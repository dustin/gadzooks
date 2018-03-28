{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad (when)
import Data.Maybe (isNothing)
import Data.Semigroup ((<>))
import Data.Text (Text, unpack)
import Options.Applicative
import System.Exit (die)
import System.Timeout (timeout)
import System.Log.Logger (rootLoggerName, updateGlobalLogger,
                          Priority(INFO), setLevel, infoM)

import Processor

data Options = Options {
  optSecret :: Text
  , optAbsTimeout :: Integer
  }

class Stringy a where string :: a -> String

instance Stringy [Char] where string = id
instance Stringy Text where string = unpack

loginfo :: Stringy a => a -> IO ()
loginfo = infoM rootLoggerName . string

options :: Parser Options
options = Options
  <$> strOption (long "auth" <> help "auth secret")
  <*> option auto (long "timeout" <> showDefault <> value 1800 <> help "timeout (seconds)")

processQueue :: Text -> (HourStamp -> IO ()) -> IO Bool
processQueue sec f = do
  q <- pollQueue sec
  case q of
    Nothing -> pure False
    (Just pt) -> process pt >> pure True

  where
    process :: PolledTask -> IO()
    process (PolledTask ts qid) = f ts >> rmQueue sec qid

notify :: Options -> IO ()
notify o@(Options sec _) =
  processQueue sec each >>= again

  where
    again :: Bool -> IO ()
    again False = pure ()
    again True = notify o

    each :: HourStamp -> IO ()
    each ts = do
      loginfo $ "Processing ts = " <> show ts
      reposE <- loadInteresting sec
      let repos = either (fail <*> show) id reposE
      todoE <- processURL (archiveURL ts)
      let todo = either (fail <*> show) (filter (combineFilters [interestingFilter repos,
                                                                 typeIs PushEvent])) todoE
      loginfo $ "Todo: " <> (show.length) todo
      mapM_ (\r@(Repo _ nm _) -> loginfo ("Queueing for " <> nm) >> queueHook sec r) todo

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName (setLevel INFO)
  o@(Options _ to) <- execParser opts

  r <- timeout (fromIntegral $ 1000000 * to) (notify o)
  when (isNothing r) $ die "timed out processing"

  where opts = info (options <**> helper)
          ( fullDesc <> progDesc "Backend processing for gadzooks")
