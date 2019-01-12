{-# LANGUAGE OverloadedStrings #-}

module Processor (
  HourStamp(..),
  Repo(..),
  EventType(..),
  PolledTask(..),
  archiveURL,
  parseEvent,
  currentStamp,
  processStream,
  processURL,
  interestingFilter,
  typeIs,
  combineFilters,
  loadInteresting,
  queueHook,
  pollQueue,
  rmQueue
  ) where

import qualified Codec.Compression.GZip     as GZip
import           Control.Lens
import           Control.Monad              (guard)
import           Data.Aeson                 (Object, eitherDecode, encode, json)
import           Data.Aeson.Lens
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import           Data.Maybe                 (fromMaybe, maybe)
import           Data.Semigroup             ((<>))
import qualified Data.Set                   as Set
import           Data.String                (fromString)
import           Data.Text                  (Text, unpack)
import qualified Data.Text.Lazy             as L
import           Data.Text.Lazy.Encoding    (decodeUtf8)
import           Data.Time                  (Day)
import           Data.Time.Clock            (diffTimeToPicoseconds,
                                             getCurrentTime, utctDay,
                                             utctDayTime)
import           Data.Word                  (Word8)
import           Network.Wreq               (FormParam ((:=)), Options,
                                             defaults, deleteWith, get, getWith,
                                             header, postWith, responseBody)
import           Text.Read                  (readMaybe)

data HourStamp = HourStamp Day Int
  deriving (Eq)

instance Show HourStamp where
  show (HourStamp d h) = show d <> "-" <> show h

instance Read HourStamp where
  readsPrec _ x = either error (\a -> [(a,"")]) $ A.parseOnly p (fromString x)
    where p :: A.Parser HourStamp
          p = do
            ymd <- A.take 10 <* "-"
            hs <- A.many1 digit
            let h = rc hs
            guard $ h >= 0 && h <= 23
            pure $ HourStamp ((rc . B.unpack) ymd) h

              where digit = A.satisfy isDigit
                    isDigit w = w >= 48 && w <= 57
                    rc :: Read a => [Word8] -> a
                    rc = read . map (toEnum . fromEnum)

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
processURL u = get u >>= \r -> pure $ processStream (r ^. responseBody)

gzd :: BL.ByteString -> B.ByteString
gzd = BL.toStrict . GZip.decompress

processStream :: BL.ByteString -> Either String [Repo]
processStream b = A.parseOnly (A.many1 parseEvent) (gzd b)

interestingFilter :: Set.Set Text -> Repo -> Bool
interestingFilter f (Repo _ r _) = r `elem` f

typeIs :: EventType -> Repo -> Bool
typeIs e (Repo t _ _) = e == t

combineFilters :: [Repo -> Bool] -> Repo -> Bool
combineFilters l v = all ($ v) l

parseEvent :: A.Parser Repo
parseEvent = do
  j <- json
  let typ = fromMaybe UnknownEvent (readMaybe =<< unpack <$> j ^? key "type" ._String)
  let r = Repo typ
          <$> j ^? key "repo" . key "name" . _String
          <*> j ^? key "payload" ._Object
  maybe (fail "not found") pure r

authHdr :: Text -> Options
authHdr auth = defaults & header "x-auth-secret" .~ [(BC.pack . unpack) auth]

loadInteresting :: Text -> IO (Either String (Set.Set Text))
loadInteresting auth = do
  r <- getWith (authHdr auth) "https://coastal-volt-254.appspot.com/export/handlers"
  pure $ eitherDecode (r ^. responseBody)

queueHook :: Text -> Repo -> IO ()
queueHook auth (Repo _ r p) =
  let payload = encode p
      url = unpack $ "https://coastal-volt-254.appspot.com/queueHook/" <> r
  in
    postWith (authHdr auth) url ["payload" := (L.toStrict . decodeUtf8) payload] >> pure ()

data PolledTask = PolledTask HourStamp Text
  deriving (Show)

pollQueue :: Text -> IO (Maybe PolledTask)
pollQueue auth = do
  r <- getWith (authHdr auth) "https://coastal-volt-254.appspot.com/q/pull"
  let bod = (read.unpack) <$> r ^? responseBody . key "body" ._String
  let tid = r ^? responseBody . key "tid" ._String
  pure (PolledTask <$> bod <*> tid)

rmQueue :: Text -> Text -> IO ()
rmQueue auth tid =
  let url = unpack ("https://coastal-volt-254.appspot.com/q/rm/x/" <> tid) in
    deleteWith (authHdr auth) url >> pure ()
