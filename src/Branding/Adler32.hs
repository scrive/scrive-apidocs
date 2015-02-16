{-# LANGUAGE ExtendedDefaultRules #-}
module Branding.Adler32 (
     imageAdler32
   , brandingAdler32
  ) where

import Control.Monad
import Control.Monad.Catch
import Data.Digest.Adler32
import Data.Maybe
import qualified Data.Binary as Binary
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC8

import BrandedDomain.BrandedDomain
import Company.CompanyUI
import Context
import DB
import Theme.Model
import Theme.Model ()
import User.Model
import Utils.String
import Version

brandingAdler32 :: (MonadDB m, MonadThrow m) => Context -> Maybe CompanyUI -> m String
brandingAdler32 ctx mcompanyui = do
  ad1 <- domainAdler32 $ ctxbrandeddomain ctx
  ad2 <- case mcompanyui of
    Just cui1 -> companyUIAdler32 cui1
    Nothing  -> return ""
  ad3 <- do
    case ((ctxmaybeuser ctx) `mplus` (ctxmaybepaduser ctx)) of
      Nothing -> return ""
      Just user -> do
        cui2 <- dbQuery $ GetCompanyUI $ usercompany user
        companyUIAdler32 cui2
  return $ BSC8.unpack $ adler32BS $ BSC8.pack $ concat $ [ad1,ad2,ad3,show versionID]


imageAdler32 :: Binary BSC8.ByteString -> String
imageAdler32 image = BSC8.unpack $ adler32BS $ unBinary $ image

domainAdler32:: (MonadDB m, MonadThrow m) => BrandedDomain -> m String
domainAdler32 bd = do
  themesMD5 <- dbQuery $ GetThemesMD5 $ [bdMailTheme bd,bdSignviewTheme bd,bdServiceTheme bd,bdLoginTheme bd]
  return $ BSC8.unpack $ adler32BS $ BSC8.pack $ concat $ [
      show (bdid bd)
    , imageAdler32 (bdFavicon bd)
    , bdParticipantColor1 bd
    , bdParticipantColor2 bd
    , bdParticipantColor3 bd
    , bdParticipantColor4 bd
    , bdParticipantColor5 bd
    , bdParticipantColor6 bd
    , bdDraftColor        bd
    , bdCancelledColor    bd
    , bdInitatedColor     bd
    , bdSentColor         bd
    , bdDeliveredColor    bd
    , bdOpenedColor       bd
    , bdReviewedColor     bd
    , bdSignedColor       bd
    , show $ bdMailTheme bd
    , show $ bdSignviewTheme bd
    , show $ bdServiceTheme bd
    , show $ bdLoginTheme bd
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
adler32BS = B16.encode . concatChunks . Binary.encode . adler32
