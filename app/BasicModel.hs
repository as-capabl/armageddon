{-# LANGUAGE TemplateHaskell #-}
module
    BasicModel
where

import Prelude hiding ((.), id)

import Control.Lens
import qualified Data.Text as Text

type BMText = Text.Text

data DSKind = DSHome | DSPublic | DSNotification | DSUserStatus BMText deriving (Eq, Show)

data DataSource = DataSource
  {
    _hostUrl :: BMText,
    _accountUser :: BMText,
    _dsKind :: DSKind
  }
  deriving (Eq, Show)

makeLenses ''DataSource
