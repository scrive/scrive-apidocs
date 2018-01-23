{-# LANGUAGE ExtendedDefaultRules #-}
module Branding.Adler32 (
     imageAdler32
   , brandingAdler32
  ) where

import Control.Monad.Catch
import Data.ByteString.Lazy (toStrict)
import Data.Digest.Adler32
import qualified Data.Binary as Binary
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC8

import BrandedDomain.BrandedDomain
import Company.CompanyUI
import Context
import DB
import Theme.Model
import User.Model
import VersionTH

brandingAdler32 :: (MonadDB m, MonadThrow m) => Context -> Maybe CompanyUI -> m String
brandingAdler32 ctx mcompanyui = do
  ad1 <- domainAdler32 $ get ctxbrandeddomain ctx
  ad2 <- case mcompanyui of
    Just cui1 -> companyUIAdler32 cui1
    Nothing  -> return ""
  ad3 <- do
    case getContextUser ctx of
      Nothing -> return ""
      Just user -> do
        cui2 <- dbQuery $ GetCompanyUI $ usercompany user
        companyUIAdler32 cui2
  return $ BSC8.unpack $ adler32BS $ BSC8.pack $ concat $ [ad1,ad2,ad3,show versionID]


imageAdler32 :: BSC8.ByteString -> String
imageAdler32 image = BSC8.unpack $ adler32BS $ image

domainAdler32:: (MonadDB m, MonadThrow m) => BrandedDomain -> m String
domainAdler32 bd = do
  themesMD5 <- dbQuery $ GetThemesMD5 $ [ get bdMailTheme bd
                                        , get bdSignviewTheme bd
                                        , get bdServiceTheme bd
                                        , get bdLoginTheme bd ]
  return $ BSC8.unpack $ adler32BS $ BSC8.pack $ concat $ [
      show         (get bdid      bd)
    , imageAdler32 (get bdFavicon bd)
    , get bdParticipantColor1     bd
    , get bdParticipantColor2     bd
    , get bdParticipantColor3     bd
    , get bdParticipantColor4     bd
    , get bdParticipantColor5     bd
    , get bdParticipantColor6     bd
    , get bdDraftColor            bd
    , get bdCancelledColor        bd
    , get bdInitatedColor         bd
    , get bdSentColor             bd
    , get bdDeliveredColor        bd
    , get bdOpenedColor           bd
    , get bdReviewedColor         bd
    , get bdSignedColor           bd
    , show $ get bdMailTheme      bd
    , show $ get bdSignviewTheme  bd
    , show $ get bdServiceTheme   bd
    , show $ get bdLoginTheme     bd
    ] ++ themesMD5

companyUIAdler32 :: (MonadDB m, MonadThrow m) => CompanyUI -> m String
companyUIAdler32 cui = do
  themesMD5 <- dbQuery $ GetThemesMD5 $ catMaybes [companyMailTheme cui,companySignviewTheme cui,companyServiceTheme cui]
  return $ BSC8.unpack $ adler32BS $ BSC8.pack $ concat $ [
      show $ companyuicompanyid cui
    , maybe "" imageAdler32(companyFavicon cui)
    , show $ companyMailTheme cui
    , show $ companySignviewTheme cui
    , show $ companyServiceTheme cui
    ] ++ themesMD5

adler32BS :: BSC8.ByteString -> BSC8.ByteString
adler32BS = B16.encode . toStrict . Binary.encode . adler32
