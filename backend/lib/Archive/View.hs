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
  F.value "isadmin" $ user ^. #isCompanyAdmin
  F.value "month" $ formatTime' "%m" mt
  F.value "year" $ formatTime' "%Y" mt
  F.value "hasdataretentionpolicy"
    $  (user ^. #settings % #dataRetentionPolicy)
    /= defaultDataRetentionPolicy
    || (ugwpSettings ugwp ^. #dataRetentionPolicy)
    /= defaultDataRetentionPolicy
  F.value "immediatetrash"
    $  (user ^. #settings % #dataRetentionPolicy % #immediateTrash)
    || (ugwpSettings ugwp ^. #dataRetentionPolicy % #immediateTrash)
  entryPointFields ctx
