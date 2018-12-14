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

pageArchive :: TemplatesMonad m => Context -> User -> UserGroupWithParents -> UTCTime -> m String
pageArchive ctx user ugwp mt = renderTemplate "pageDocumentsList" $ do
  F.value "isadmin" $ useriscompanyadmin user
  F.value "month" $ formatTime' "%m" mt
  F.value "year" $ formatTime' "%Y" mt
  F.value "hasdataretentionpolicy" $
    dataretentionpolicy (usersettings user) /= def
    || get ugsDataRetentionPolicy (ugwpSettings ugwp) /= def
  F.value "immediatetrash" $
    get drpImmediateTrash (dataretentionpolicy (usersettings user))
    || (get (drpImmediateTrash . ugsDataRetentionPolicy) . ugwpSettings) ugwp
  entryPointFields ctx
