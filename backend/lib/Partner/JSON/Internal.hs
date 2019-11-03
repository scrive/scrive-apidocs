{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
module Partner.JSON.Internal
  ( UserForUpdate(..)
  , UserGroupForUpdate(..)
  ) where

import Optics.TH

import User.Email (Email(..))
import User.Model

data UserForUpdate = UserForUpdate
  { id              :: Text
  , email           :: Email
  , firstName       :: Text
  , lastName        :: Text
  , personalNumber  :: Text
  , phone           :: Text
  , companyPosition :: Text
  , lang            :: Lang
  , hasAcceptedTOS  :: Bool
  }

data UserGroupForUpdate = UserGroupForUpdate
  { id            :: Text
  , entityName    :: Text
  , companyNumber :: Text
  , address       :: Text
  , zipCode       :: Text
  , city          :: Text
  , country       :: Text
  } deriving (Show)

makeFieldLabelsWith noPrefixFieldLabels ''UserForUpdate
makeFieldLabelsWith noPrefixFieldLabels ''UserGroupForUpdate
