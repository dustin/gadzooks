{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Exception          (SomeException)
import           Control.Monad              (when)
import           Control.Monad.Catch        (catch)
import           Control.Monad.Fix          (mfix)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Reader       (ReaderT (..), runReaderT)
import           Control.Monad.State        (StateT (..), execStateT, get, put)
import           Data.Bool                  (bool)
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.Maybe                 (fromJust, isNothing)
import           Data.Semigroup             ((<>))
import           Data.Text                  (Text, unpack)
import           Network.MQTT.Client
import           Network.URI
import           Options.Applicative
import           System.Exit                (die)
import           System.Log.Logger          (Priority (INFO), errorM, infoM,
                                             rootLoggerName, setLevel,
                                             updateGlobalLogger)
import           System.Timeout             (timeout)

import           Processor

data Options = Options {
  optSecret       :: Text
  , optAbsTimeout :: Integer
  , optMQTTURI    :: URI
  , optTopic      :: Topic
  }

newtype Env = Env { mqc :: MQTTClient }
data Counts = Counts {filesProcessed :: !Int,
                      msgsProcessed  :: !Int,
                      msgsSent       :: !Int,
                      errors         :: !Int} deriving(Show)
type Processor = ReaderT Env (StateT Counts IO)

class Stringy a where string :: a -> String

instance Stringy [Char] where string = id
instance Stringy Text where string = unpack

loginfo :: MonadIO m => Stringy a => a -> m ()
loginfo = liftIO . infoM rootLoggerName . string

logErr :: MonadIO m => Stringy a => a -> m ()
logErr = liftIO . errorM rootLoggerName . string


options :: Parser Options
options = Options
  <$> strOption (long "auth" <> help "auth secret")
  <*> option auto (long "timeout" <> showDefault <> value 1800 <> help "timeout (seconds)")
  <*> option (maybeReader parseURI) (long "mqtt-uri" <> short 'u' <>
                                     showDefault <> value (fromJust $ parseURI "mqtt://localhost/")
                                     <> help "mqtt broker URI")
  <*> option str (long "topic" <> showDefault <> value "cloud/gadzooks/" <>  help "mqtt topic prefix")

processQueue :: Text -> (HourStamp -> Processor ()) -> Processor Bool
processQueue sec f = do
  q <- liftIO $ pollQueue sec
  case q of
    Nothing   -> pure False
    (Just pt) -> processE pt >> pure True

  where
    process :: PolledTask -> Processor ()
    process (PolledTask ts qid) = f ts >> liftIO (rmQueue sec qid)

    processE :: PolledTask -> Processor ()
    processE pt@(PolledTask ts _) = catch (process pt) $ \e -> do
      logErr $ mconcat ["error processing ", show ts, ": ", show (e :: SomeException)]
      get >>= \c@Counts{..} -> put c{errors=errors+1}

notify :: Options -> Processor ()
notify Options{..} = mfix (\p -> bool p () <$> processQueue optSecret each)

  where
    each :: HourStamp -> Processor ()
    each ts = do
      loginfo $ "Processing ts = " <> show ts
      reposE <- liftIO $ loadInteresting optSecret
      let repos = either (fail <*> show) id reposE
      allEventsE <- liftIO $ processURL (archiveURL ts)
      let allEvents = either (fail <*> show) id allEventsE
          toSend = filter (combineFilters [interestingFilter repos, typeIs PushEvent]) allEvents
      loginfo $ "To send: " <> (show.length) toSend
      liftIO $ mapM_ (\r@(Repo _ nm _) -> loginfo ("Queueing for " <> nm) >> queueHook optSecret r) toSend
      c@Counts{..} <- get
      put c{filesProcessed=filesProcessed + 1,
                 msgsProcessed=msgsProcessed + length allEvents,
                 msgsSent=msgsSent + length toSend
                 }

runTrans :: Options -> Env -> Counts -> IO Counts
runTrans opts = execStateT . runReaderT (notify opts)

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName (setLevel INFO)
  o@Options{..} <- execParser opts

  mc <- connectURI mqttConfig{_protocol=Protocol50} optMQTTURI

  let to = fromIntegral $ 1000000 * optAbsTimeout

  r <- timeout to (runTrans o (Env mc) (Counts 0 0 0 0))
  when (isNothing r) $ die "timed out processing"
  let (Just Counts{..}) = r

  publishq mc (optTopic <> "files") (BC.pack $ show filesProcessed) True QoS2 [
    PropMessageExpiryInterval 7200]
  publishq mc (optTopic <> "msgs") (BC.pack $ show msgsProcessed) True QoS2 [
    PropMessageExpiryInterval 7200]
  publishq mc (optTopic <> "sent") (BC.pack $ show msgsSent) True QoS2 [
    PropMessageExpiryInterval 7200]
  publishq mc (optTopic <> "errors") (BC.pack $ show errors) True QoS2 [
    PropMessageExpiryInterval 7200]

  where opts = info (options <**> helper)
          ( fullDesc <> progDesc "Backend processing for gadzooks")
