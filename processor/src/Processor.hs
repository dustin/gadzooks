{-# LANGUAGE OverloadedStrings #-}

module Processor (
  HourStamp(..),
  Repo(..),
  EventType(..),
  archiveURL,
  parseEvent,
  currentStamp,
  processStream,
  processURL,
  interestingFilter,
  typeIs,
  combineFilters,
  loadInteresting,
  queueHook
  ) where

import Control.Lens
import Data.Aeson (Object, json, eitherDecode, encode)
import Data.Aeson.Lens
import Data.Maybe (maybe, fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time (Day)
import Data.Time.Clock (diffTimeToPicoseconds, getCurrentTime, utctDay, utctDayTime)
import Network.Wreq (get, getWith, postWith, defaults, header, responseBody, FormParam((:=)))
import Text.Read (readMaybe)
import qualified Codec.Compression.GZip as GZip
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set
import qualified Data.Text.Lazy as L

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

data EventType = CommitCommentEvent
               | CreateEvent
               | DeleteEvent
               | DeploymentEvent
               | DeploymentStatusEvent
               | DownloadEvent
               | FollowEvent
               | ForkEvent
               | ForkApplyEvent
               | GistEvent
               | GollumEvent
               | InstallationEvent
               | InstallationRepositoriesEvent
               | IssueCommentEvent
               | IssuesEvent
               | LabelEvent
               | MarketplacePurchaseEvent
               | MemberEvent
               | MembershipEvent
               | MilestoneEvent
               | OrganizationEvent
               | OrgBlockEvent
               | PageBuildEvent
               | ProjectCardEvent
               | ProjectColumnEvent
               | ProjectEvent
               | PublicEvent
               | PullRequestEvent
               | PullRequestReviewEvent
               | PullRequestReviewCommentEvent
               | PushEvent
               | ReleaseEvent
               | RepositoryEvent
               | StatusEvent
               | TeamEvent
               | TeamAddEvent
               | WatchEvent
               | UnknownEvent
               deriving (Eq, Read, Show, Bounded, Enum)

archiveURL :: HourStamp -> String
archiveURL h = "http://data.githubarchive.org/" <> show h <> ".json.gz"

currentStamp :: IO HourStamp
currentStamp = tostamp <$> getCurrentTime
  where tostamp t = HourStamp (utctDay t) (((`div` 3600) . tosecs . utctDayTime) t)
        tosecs = fromIntegral . (`div` 1000000000000) . diffTimeToPicoseconds

processURL :: String -> IO (Either String [Repo])
processURL u = do
  r <- get u
  pure $ processStream (r ^. responseBody)

gzd :: BL.ByteString -> B.ByteString
gzd = BL.toStrict . GZip.decompress

processStream :: BL.ByteString -> Either String [Repo]
processStream b = A.parseOnly (A.many1 parseEvent) (gzd b)

interestingFilter :: Set.Set Text -> Repo -> Bool
interestingFilter f (Repo _ r _) = r `elem` f

typeIs :: EventType -> Repo -> Bool
typeIs e (Repo t _ _) = e == t

combineFilters :: [Repo -> Bool] -> Repo -> Bool
combineFilters l v = and $ map ($ v) l

parseEvent :: A.Parser Repo
parseEvent = do
  j <- json
  let typ = fromMaybe UnknownEvent (readMaybe =<< unpack <$> j ^? key "type" ._String)
  let r = Repo typ
          <$> j ^? key "repo" . key "name" . _String
          <*> j ^? key "payload" ._Object
  maybe (fail "not found") pure r

loadInteresting :: Text -> IO (Either String (Set.Set Text))
loadInteresting auth = do
  let opts = defaults & header "x-auth-secret" .~ [(BC.pack . unpack) auth]
  r <- getWith opts "https://coastal-volt-254.appspot.com/export/handlers"
  pure $ eitherDecode (r ^. responseBody)

queueHook :: Text -> Repo -> IO ()
queueHook auth (Repo _ r p) = do
  let payload = encode p
  let opts = defaults & header "x-auth-secret" .~ [(BC.pack . unpack) auth]
  let url = unpack $ "https://coastal-volt-254.appspot.com/queueHook/" <> r
  _ <- postWith opts url ["payload" := (L.toStrict . decodeUtf8) payload]
  pure ()
