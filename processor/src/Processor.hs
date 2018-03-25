{-# LANGUAGE OverloadedStrings #-}

module Processor where

import Control.Lens
import Control.Monad (guard)
import Control.Exception as E
import Data.Aeson (Object, Value(..), json, eitherDecode)
import Data.Aeson.Lens
import Data.Either (rights)
import Data.Maybe (maybe, isJust, fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text, unpack)
import Data.Time (Day)
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds, getCurrentTime, utctDay, utctDayTime)
import Network.Wreq (Response, get, getWith, defaults, param, responseStatus, statusCode, responseBody)
import Text.Read (readMaybe)
import qualified Codec.Compression.GZip as GZip
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set

data HourStamp = HourStamp Day Int
  deriving (Eq)

instance Show HourStamp where
  show (HourStamp d h) = show d <> "-" <> show h

instance Enum HourStamp where
  fromEnum (HourStamp d h) = h + (fromEnum d * 24)
  toEnum x = let (d, h) = x `divMod` 24 in
               HourStamp (toEnum d) h

data Repo = Repo EventType Text Object
  deriving (Show)

data EventType = PushEvent
               | WatchEvent
               | IssueCommentEvent
               | PullRequestEvent
               | CreateEvent
               | IssuesEvent
               | PullRequestReviewCommentEvent
               | ForkEvent
               | ReleaseEvent
               | MemberEvent
               | DeleteEvent
               | GollumEvent
               | CommitCommentEvent
               | PublicEvent
               | UnknownEvent
               deriving (Eq, Read, Show)

archiveURL :: HourStamp -> String
archiveURL h = "http://data.githubarchive.org/" <> show h <> ".json.gz"

currentStamp :: IO HourStamp
currentStamp = tostamp <$> getCurrentTime
  where tostamp t = HourStamp (utctDay t) (((`div` 3600) . tosecs . utctDayTime) t)
        tosecs = fromIntegral . (`div` 1000000000000) . diffTimeToPicoseconds

processURL :: (Repo -> Bool) -> String -> IO (Either String [Repo])
processURL f u = do
  er <- E.try (get u)
  case er of
    (Left s) -> pure $ Left (show (s :: E.SomeException))
    (Right r) -> pure $ processStream f (r ^. responseBody)

gzd :: BL.ByteString -> B.ByteString
gzd = BL.toStrict . GZip.decompress

processStream :: (Repo -> Bool) -> BL.ByteString -> Either String [Repo]
processStream f b = filter f <$> A.parseOnly (A.many1 parseThing) (gzd b)

interestingFilter :: Set.Set Text -> Repo -> Bool
interestingFilter f (Repo _ r _) = r `elem` f

parseThing :: A.Parser Repo
parseThing = do
  j <- json
  let typ = fromMaybe UnknownEvent (readMaybe =<< unpack <$> j ^? key "type" ._String)
  let r = Repo typ
          <$> j ^? key "repo" . key "name" . _String
          <*> j ^? key "payload" ._Object
  maybe (fail "not found") (\x -> pure x) r

loadURLs :: Text -> IO (Either String (Set.Set Text))
loadURLs auth = do
  let opts = defaults & param "auth" .~ [auth]
  er <- E.try (getWith opts "https://coastal-volt-254.appspot.com/export/handlers")
  case er of
    (Left s) -> pure $ Left (show (s :: E.SomeException))
    (Right r) -> pure $ eitherDecode (r ^. responseBody)
