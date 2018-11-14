{-# LANGUAGE ExtendedDefaultRules #-}
module Archive.View (pageArchive) where

import Data.Default
import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F

import AppView
import Context
import DataRetentionPolicy
import MinutesTime
import User.Model
import UserGroup.Types

pageArchive :: TemplatesMonad m => Context -> User -> Maybe UserGroup -> UTCTime -> m String
pageArchive ctx user mug mt = renderTemplate "pageDocumentsList" $ do
  F.value "isadmin" $ useriscompanyadmin user
  F.value "month" $ formatTime' "%m" mt
  F.value "year" $ formatTime' "%Y" mt
  F.value "hasdataretentionpolicy" $
    dataretentionpolicy (usersettings user) /= def
    || maybe False ((/= def) . get (ugsDataRetentionPolicy . ugSettings)) mug
  F.value "immediatetrash" $
    get drpImmediateTrash (dataretentionpolicy (usersettings user))
    || maybe False (get (drpImmediateTrash . ugsDataRetentionPolicy . ugSettings)) mug
  entryPointFields ctx
