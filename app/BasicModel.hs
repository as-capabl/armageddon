{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Strict, StrictData #-}
{-# LANGUAGE DeriveGeneric #-}

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

import qualified Web.Hastodon as Hdon

type BMText = Text.Text


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

-- Host
data Host = Host
  {
    _hostname :: BMText,
    _clientId :: BMText,
    _clientSecret :: BMText
  }
    deriving (Eq, Show)

makeLenses ''Host

-- Registration
data Registration = Registration
  {
    _registrationHost :: BMText,
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
data DSKind = DSHome | DSPublic | DSNotification | DSUserStatus BMText deriving (Eq, Show)

data DataSource = DataSource
  {
    _dsreg :: Registration,
    _dsKind :: DSKind
  }
  deriving (Eq, Show)

makeClassy ''DataSource
instance HasRegistration DataSource where {registration = dsreg}
instance HasHastodonClient DataSource where {hastodonClient = dsreg . hastodonClient}

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
statusPrefix = Text.pack "status_"

statusIdInvalid = -1

statusIdToDomId :: Int -> BMText
statusIdToDomId x = statusPrefix `mappend` Text.pack (show x)

domIdToStatusId :: BMText -> Int
domIdToStatusId x =
    case Text.decimal $ Text.drop (Text.length statusPrefix) x
      of
        Left _ -> statusIdInvalid
        Right (y, _) -> y
