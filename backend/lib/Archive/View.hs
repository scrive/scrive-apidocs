{-# LANGUAGE ExtendedDefaultRules #-}
module Archive.View (pageArchive) where

import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F

import AppView
import Context
import DataRetentionPolicy
import MinutesTime
import User.Model
import UserGroup.Types

pageArchive
  :: TemplatesMonad m => Context -> User -> UserGroupWithParents -> UTCTime -> m String
pageArchive ctx user ugwp mt = renderTemplate "pageDocumentsList" $ do
  F.value "isadmin" $ useriscompanyadmin user
  F.value "month" $ formatTime' "%m" mt
  F.value "year" $ formatTime' "%Y" mt
  F.value "hasdataretentionpolicy"
    $  dataretentionpolicy (usersettings user)
    /= defaultDataRetentionPolicy
    || (ugwpSettings ugwp ^. #dataRetentionPolicy)
    /= defaultDataRetentionPolicy
  F.value "immediatetrash"
    $  drpImmediateTrash (dataretentionpolicy (usersettings user))
    || (ugwpSettings ugwp ^. #dataRetentionPolicy % #drpImmediateTrash)
  entryPointFields ctx
