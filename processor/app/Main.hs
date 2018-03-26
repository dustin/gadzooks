{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Text (Text, unpack)
import System.Log.Logger (rootLoggerName, updateGlobalLogger,
                          Priority(INFO), setLevel, infoM)

import Processor

data Options = Options {
  optSecret :: Text
  }

loginfo :: String -> IO ()
loginfo = infoM rootLoggerName

options :: Parser Options
options = Options
  <$> strOption (long "auth" <> help "auth secret")

notify :: Options -> IO ()
notify (Options sec) = do
  reposE <- loadInteresting sec
  let repos = either (fail <*> show) id reposE
  url <- archiveURL . pred <$> currentStamp
  todoE <- processURL url
  let todo = either (fail <*> show) (filter (combineFilters [interestingFilter repos, typeIs PushEvent])) todoE
  loginfo $ "Todo:  " <> (show.length) todo
  mapM_ (\r@(Repo _ rn _) -> do
            loginfo $ unpack $ "Notifying " <> rn
            queueHook sec r) todo

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName (setLevel INFO)
  o <- execParser opts
  notify o

  where opts = info (options <**> helper)
          ( fullDesc <> progDesc "Backend processing for gadzooks")
