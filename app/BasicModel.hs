{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Strict, StrictData #-}

module
    BasicModel
where

import Prelude hiding ((.), id)

import Control.Lens
import qualified Data.Text as Text

import qualified Web.Hastodon as Hdon

type BMText = Text.Text

data DSKind = DSHome | DSPublic | DSNotification | DSUserStatus BMText deriving (Eq, Show)

makeClassyFor
    "HasHastodonClient"
    "hastodonClient"
    [("host", "host"), ("token", "token")]
    ''Hdon.HastodonClient

data Registration = Registration
  {
    _rgcli :: Hdon.HastodonClient,
    _registrationUser :: BMText
  }

makeClassy ''Registration
instance HasHastodonClient Registration where {hastodonClient = rgcli}

instance
    Eq Registration
  where
    x == y =
        x ^. host == y ^. host &&
        x ^. registrationUser == y ^. registrationUser

instance
    Show Registration
  where
    show x = "Registration " ++ show (x ^. host) ++ " " ++ show (x ^. registrationUser)


data DataSource = DataSource
  {
    _dsreg :: Registration,
    _dsKind :: DSKind
  }
  deriving (Eq, Show)

makeClassy ''DataSource
instance HasRegistration DataSource where {registration = dsreg}
instance HasHastodonClient DataSource where {hastodonClient = rgcli}

