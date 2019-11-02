{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Monad              (when)
import           Control.Monad.Reader (runReaderT, ReaderT(..))
import           Control.Monad.State (execStateT, StateT(..), get, put)
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.Maybe                 (fromJust, isNothing)
import           Data.Semigroup             ((<>))
import           Data.Text                  (Text, unpack)
import           Network.MQTT.Client
import           Network.URI
import           Options.Applicative
import           System.Exit                (die)
import           System.Log.Logger          (Priority (INFO), infoM,
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

data Env = Env { mqc :: MQTTClient }
data Counts = Counts {filesProcessed :: Int,
                      msgsProcessed :: Int,
                      msgsSent :: Int}
type Processor = ReaderT Env (StateT Counts IO)

class Stringy a where string :: a -> String

instance Stringy [Char] where string = id
instance Stringy Text where string = unpack

loginfo :: MonadIO m => Stringy a => a -> m ()
loginfo = liftIO . infoM rootLoggerName . string

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
    (Just pt) -> process pt >> pure True

  where
    process (PolledTask ts qid) = do
      f ts >> (liftIO $ rmQueue sec qid)

notify :: Options -> Processor ()
notify o@Options{..} = do
  processQueue optSecret each >>= again

  where
    again False = pure ()
    again True  = notify o

    each :: HourStamp -> Processor ()
    each ts = do
      loginfo $ "Processing ts = " <> show ts
      reposE <- liftIO $ loadInteresting optSecret
      let repos = either (fail <*> show) id reposE
      todoE <- liftIO $ processURL (archiveURL ts)
      let todo = either (fail <*> show) (filter (combineFilters [interestingFilter repos,
                                                                 typeIs PushEvent])) todoE
      loginfo $ "Todo: " <> (show.length) todo
      liftIO $ mapM_ (\r@(Repo _ nm _) -> loginfo ("Queueing for " <> nm) >> queueHook optSecret r) todo
      Counts{..} <- get
      put Counts{filesProcessed=filesProcessed + 1,
                 msgsProcessed=msgsProcessed + length todoE,
                 msgsSent=msgsSent + length todo
                 }

runTrans :: Options -> Env -> Counts -> IO Counts
runTrans opts = execStateT . runReaderT (notify opts)

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName (setLevel INFO)
  o@Options{..} <- execParser opts

  mc <- connectURI mqttConfig{_protocol=Protocol50} optMQTTURI

  let to = fromIntegral $ 1000000 * optAbsTimeout

  r <- timeout to (runTrans o (Env mc) (Counts 0 0 0))
  when (isNothing r) $ die "timed out processing"
  let (Just Counts{..}) = r

  publishq mc (optTopic <> "files") (BC.pack $ show filesProcessed) True QoS2 [
    PropMessageExpiryInterval 7200]
  publishq mc (optTopic <> "msgs") (BC.pack $ show msgsProcessed) True QoS2 [
    PropMessageExpiryInterval 7200]
  publishq mc (optTopic <> "sent") (BC.pack $ show msgsSent) True QoS2 [
    PropMessageExpiryInterval 7200]

  where opts = info (options <**> helper)
          ( fullDesc <> progDesc "Backend processing for gadzooks")
