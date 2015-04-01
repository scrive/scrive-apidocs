{-# LANGUAGE ExtendedDefaultRules #-}
module Archive.View (pageArchive) where

import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F

import Company.Model (Company, companyinfo, companyidledoctimeout)
import KontraPrelude
import MinutesTime
import User.Model

pageArchive :: TemplatesMonad m => User -> Maybe Company -> UTCTime -> m String
pageArchive user mcompany mt = renderTemplate "pageDocumentsList" $ do
  F.value "isadmin" $ useriscompanyadmin user
  F.value "month" $ formatTime' "%m" mt
  F.value "year" $ formatTime' "%Y" mt
  F.value "idledoctimeout" $ toInteger <$> (companyinfo <$> mcompany >>= companyidledoctimeout)
