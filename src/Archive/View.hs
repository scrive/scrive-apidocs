{-# LANGUAGE ExtendedDefaultRules #-}
module Archive.View (pageArchive) where

import Text.StringTemplates.Templates
import User.Model
import MinutesTime


import qualified Text.StringTemplates.Fields as F

pageArchive :: TemplatesMonad m => User -> MinutesTime -> m String
pageArchive user mt = renderTemplate "pageDocumentsList" $ do
                    F.value "isadmin" $ useriscompanyadmin user
                    F.value "month" $ mtMonth mt
                    F.value "year" $ mtYear mt
