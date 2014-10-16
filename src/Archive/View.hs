{-# LANGUAGE ExtendedDefaultRules #-}
module Archive.View (pageArchive) where

import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F

import MinutesTime
import User.Model

pageArchive :: TemplatesMonad m => User -> UTCTime -> m String
pageArchive user mt = renderTemplate "pageDocumentsList" $ do
  F.value "isadmin" $ useriscompanyadmin user
  F.value "month" $ formatTime' "%m" mt
  F.value "year" $ formatTime' "%Y" mt
