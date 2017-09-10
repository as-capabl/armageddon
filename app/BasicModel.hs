{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Strict, StrictData #-}

module
    BasicModel
where

import Prelude hiding ((.), id)
import Control.Category

import Control.Lens
import qualified Data.Text as Text

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
    deriving (Eq, Show)

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

