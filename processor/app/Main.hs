{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Monad (when, forever)
import Data.Either (isLeft)
import Data.Semigroup ((<>))
import Data.Text (Text, unpack)
import Options.Applicative
import System.Exit (die)
import System.Log.Logger (rootLoggerName, updateGlobalLogger,
                          Priority(INFO), setLevel, infoM)

import Processor

data Options = Options {
  optSecret :: Text
  , optAbsTimeout :: Integer
  }

loginfo :: String -> IO ()
loginfo = infoM rootLoggerName

loginfot :: Text -> IO ()
loginfot = loginfo . unpack

options :: Parser Options
options = Options
  <$> strOption (long "auth" <> help "auth secret")
  <*> option auto (long "absTimeout" <> showDefault <> value 1800 <> help "absolute timeout")

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
      mapM_ (\r@(Repo _ nm _) -> loginfot ("Queueing for " <> nm) >> queueHook sec r) todo

waitAbsolutely :: Options -> IO ()
waitAbsolutely (Options _ 0) = forever (threadDelay 10000000)
waitAbsolutely (Options _ to) = threadDelay (fromIntegral $ 1000000 * to)

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName (setLevel INFO)
  o <- execParser opts

  r <- race (waitAbsolutely o) (notify o)
  when (isLeft r) $ die "reached absolute timeout waiting for completion"

  where opts = info (options <**> helper)
          ( fullDesc <> progDesc "Backend processing for gadzooks")
