{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Criterion
import Criterion.Main

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set
import Data.Text (Text)

import Processor

interestingRepos :: Set.Set Text
interestingRepos = Set.fromList [
  "dotclear/dotclear",
  "google-test2/signcla-probe-repo",
  "fxtools/quote_percentages"
  ]

ps :: (Repo -> Bool) -> BL.ByteString -> Either String [Repo]
ps f d = filter f <$> processStream d

benchParsing :: B.ByteString -> (Repo -> Bool) -> Benchmark
benchParsing !d !f = bench "filter + parser" $ whnf (ps f) (BL.fromStrict d)


main :: IO ()
main = do
  d <- B.readFile "test/small.gz"
  let f = interestingFilter $! interestingRepos
  defaultMain [benchParsing d f]
