{-# LANGUAGE ExtendedDefaultRules #-}
module Archive.View (pageArchive) where

import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F

import AppView
import Context
import MinutesTime
import User.Model
import UserGroup.Data

pageArchive :: TemplatesMonad m => Context -> User -> Maybe UserGroup -> UTCTime -> m String
pageArchive ctx user mug mt = renderTemplate "pageDocumentsList" $ do
  F.value "isadmin" $ useriscompanyadmin user
  F.value "month" $ formatTime' "%m" mt
  F.value "year" $ formatTime' "%Y" mt
  F.value "idledoctimeout" . join $ (fmap toInteger . get (ugsIdleDocTimeout . ugSettings)) <$> mug
  entryPointFields ctx
