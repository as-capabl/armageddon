{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict, StrictData #-}
{-# LANGUAGE DeriveGeneric, DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module
    BasicModel
where

import Prelude hiding ((.), id)
import Control.Category

import Control.Lens
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Time.Clock
import Data.Time.Clock.POSIX

import qualified Web.Hastodon as Hdon

type BMText = Text.Text

-- Qualified by host
type Hostname = BMText

data QHost a = QHost Hostname a

-- HastodonClient
makeClassyFor
    "HasHastodonClient"
    "hastodonClient"
    [("host", "host"), ("token", "token")]
    ''Hdon.HastodonClient

-- Config
data Config = Config
  {
    _defaultClientName :: BMText
  }

makeLenses ''Config

-- Time Format(depends on mastodon)
-- ex) 2017-10-28T07:06:42.350Z
isoTimeFormat :: String
isoTimeFormat = "%FT%H:%M:%S%Q%Z"

type DBTime = Int64 -- milliseconds
dbTimeCoeff = 1000

toDBTime :: UTCTime -> DBTime
toDBTime = truncate . (*dbTimeCoeff) . utcTimeToPOSIXSeconds

fromDBTime :: DBTime -> UTCTime
fromDBTime = posixSecondsToUTCTime . (/dbTimeCoeff) . fromIntegral

-- Host
data Host = Host
  {
    _hostname :: Hostname,
    _clientId :: BMText,
    _clientSecret :: BMText
  }
    deriving (Eq, Show)

makeLenses ''Host

-- Registration
data Registration = Registration
  {
    _registrationHost :: Hostname,
    _registrationToken :: BMText,
    _username :: BMText
  }
    deriving (Eq, Show, Generic)

makeClassy ''Registration
instance
    HasHastodonClient Registration
  where
    hastodonClient = lens getter setter
      where
        getter reg = Hdon.HastodonClient {
            Hdon.host = Text.unpack $ _registrationHost reg,
            Hdon.token = Text.unpack $ _registrationToken reg
          }
        setter reg cli = reg {
            _registrationHost = Text.pack $ Hdon.host cli,
            _registrationToken = Text.pack $ Hdon.token cli
          }

instance Hashable Registration

-- Data Source
data DSKind = DSS DSSKind | DSN DSNKind deriving (Eq, Show, Read)
data DSSKind = DSHome | DSPublic | DSUserStatus Int | DSSearch BMText | DSHashtag BMText
  deriving (Eq, Show, Read)
data DSNKind = DSNotification deriving (Eq, Show, Read)

data DataSource' kind = DataSource
  {
    _dsreg :: Registration,
    _dsKind :: kind
  }
  deriving (Eq, Show, Functor)

-- makeClassy ''DataSource'
dsreg :: Lens' (DataSource' kind) Registration
dsreg = lens _dsreg (\ds val -> ds {_dsreg = val})

instance HasRegistration (DataSource' kind) where {registration = dsreg}
instance HasHastodonClient (DataSource' kind) where {hastodonClient = dsreg . hastodonClient}

type DataSource = DataSource' DSKind

_DSSSource :: Prism' DataSource (DataSource' DSSKind)
_DSSSource = prism' fromDSS toDSS
  where
    fromDSS (DataSource k r) = DataSource k (DSS r)
    toDSS (DataSource k (DSS r)) = Just (DataSource k r)
    toDSS _ = Nothing

pattern DSSSource x <- ((^? _DSSSource) -> Just x)
  where
    DSSSource x = x ^. re _DSSSource

_DSNSource :: Prism' DataSource (DataSource' DSNKind)
_DSNSource = prism' fromDSN toDSN
  where
    fromDSN (DataSource k r) = DataSource k (DSN r)
    toDSN (DataSource k (DSN r)) = Just (DataSource k r)
    toDSN _ = Nothing

pattern DSNSource x <- ((^? _DSNSource) -> Just x)
  where
    DSNSource x = x ^. re _DSNSource

isCachableDS :: DSKind -> Bool
isCachableDS (DSS DSHome) = True
isCachableDS _ = False

-- Range Placeholder
data RPH = RPH
  {
    _rphId :: BMText,
    _rphUpper :: Maybe Int,
    _rphLower :: Maybe Int
  }
  deriving (Eq, Show)
makeLenses ''RPH

--
-- Basic functions
--
statusPrefix :: BMText
statusPrefix = "status_"

notificationPrefix :: BMText
notificationPrefix = "notification_"

statusIdInvalid :: Int
statusIdInvalid = -1

statusIdToDomId :: Int -> BMText
statusIdToDomId x = statusPrefix `mappend` Text.pack (show x)

domIdToStatusId :: BMText -> Maybe Int
domIdToStatusId x =
    case Text.decimal numPart
      of
        Left _ -> Nothing
        Right (y, _) -> Just y
  where
    (_, numPart) = Text.breakOnEnd "_" x

notificationIdToDomId :: Int -> BMText
notificationIdToDomId x = notificationPrefix `mappend` Text.pack (show x)
