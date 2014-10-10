{-# LANGUAGE ExtendedDefaultRules #-}
module Archive.View (pageArchive) where

import Text.StringTemplates.Templates
import User.Model
import MinutesTime


import qualified Text.StringTemplates.Fields as F


pageArchive :: TemplatesMonad m => User -> UTCTime -> m String
pageArchive user mt = renderTemplate "pageDocumentsList" $ do
  F.value "isadmin" $ useriscompanyadmin user
  F.value "month" $ formatTime' "%m" mt
  F.value "year" $ formatTime' "%Y" mt
