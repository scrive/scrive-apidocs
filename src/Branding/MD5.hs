{-# LANGUAGE ExtendedDefaultRules #-}
module Branding.MD5 (
     imageMD5
   , brandingMD5
  ) where

import Control.Monad
import Control.Monad.Catch

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.UTF8 as BS
import Data.Maybe

import BrandedDomain.BrandedDomain
import DB
import Theme.Model
import Context
import Company.CompanyUI
import Theme.Model ()
import User.Model
import Version

brandingMD5 :: (MonadDB m, MonadThrow m) => Context -> Maybe CompanyUI -> m String
brandingMD5 ctx mcompanyui = do
  md1 <- domainMD5 $ ctxbrandeddomain ctx
  md2 <- case mcompanyui of
    Just cui1 -> companyUIMD5 cui1
    Nothing  -> return ""
  md3 <- do
    case ((ctxmaybeuser ctx) `mplus` (ctxmaybepaduser ctx)) of
      Nothing -> return ""
      Just user -> do
        cui2 <- dbQuery $ GetCompanyUI $ usercompany user
        companyUIMD5 cui2
  return $ BS.toString $ B16.encode $  MD5.hash $ BS.fromString $ concat $ [md1,md2,md3,show versionID]


imageMD5 :: Binary BS.ByteString -> String
imageMD5 image = BS.toString $ B16.encode $ MD5.hash $ unBinary $ image

domainMD5 :: (MonadDB m, MonadThrow m) => BrandedDomain -> m String
domainMD5 bd = do
  themesMD5 <- dbQuery $ GetThemesMD5 $ [bdMailTheme bd,bdSignviewTheme bd,bdServiceTheme bd,bdLoginTheme bd]
  return $ BS.toString $ B16.encode $  MD5.hash $ BS.fromString $ concat $ [
      show (bdid bd)
    , imageMD5 (bdFavicon bd)
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
    ] ++ themesMD5

companyUIMD5 :: (MonadDB m, MonadThrow m) => CompanyUI -> m String
companyUIMD5 cui = do
  themesMD5 <- dbQuery $ GetThemesMD5 $ catMaybes [companyMailTheme cui,companySignviewTheme cui,companyServiceTheme cui]
  return $ BS.toString $ B16.encode $  MD5.hash $ BS.fromString $ concat $ [
      show $ companyuicompanyid cui
    , maybe "" imageMD5 (companyFavicon cui)
    ] ++ themesMD5