{-# LANGUAGE ExtendedDefaultRules #-}
{- |
   Defines the App level views.
-}
module Branding.Control(
                handleServiceBranding
              , handleLoginBranding
              , handleScriveBranding
              , handleDomainBranding
              , handleSignviewBranding
              , handleSignviewBrandingWithoutDocument
              , loginLogo
              , serviceLogo
              , emailLogo
              , signviewLogo
              , signviewLogoWithoutDocument
              , faviconIcon
              ) where

import Happstack.Server.SimpleHTTP
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as Map

import BrandedDomain.BrandedDomain
import BrandedDomain.Model
import Branding.Cache
import Branding.CSS
import Company.CompanyUI
import Company.Model
import DB
import Doc.DocStateData
import Doc.DocumentID
import Doc.Model.Query
import Kontra
import KontraPrelude
import Theme.Model
import User.Model
import User.Utils
import Util.MonadUtils
import qualified MemCache as MemCache

handleServiceBranding :: Kontrakcja m => BrandedDomainID -> String -> String -> String -> m Response
handleServiceBranding bdid uidstr brandinghash _ = do
  muser <- case uidstr of
    "_"-> return Nothing
    s -> case maybeRead s of
      Nothing -> return Nothing
      Just uid -> dbQuery $ GetUserByID uid
  theme <- getServiceTheme bdid muser
  brandingCSS <- withLessCache (ServiceBranding (themeID theme) brandinghash) $ serviceBrandingCSS theme
  return (cssResponse brandingCSS)

getServiceTheme ::  Kontrakcja m => BrandedDomainID -> Maybe User -> m Theme
getServiceTheme bdid muser = do
  bd <- dbQuery $ GetBrandedDomainByID bdid
  case muser of
    Nothing -> dbQuery $ GetTheme $ bdServiceTheme bd
    Just user -> do
      company <- getCompanyForUser user
      companyui <- dbQuery $ GetCompanyUI (companyid company)
      dbQuery $ GetTheme $ fromMaybe (bdServiceTheme bd) (companyServiceTheme $ companyui)

handleLoginBranding :: Kontrakcja m => BrandedDomainID -> String -> String -> m Response
handleLoginBranding bdid brandinghash _ = do
  bd <- dbQuery $ GetBrandedDomainByID bdid
  theme <- dbQuery $ GetTheme (bdLoginTheme bd)
  brandingCSS <- withLessCache (LoginBranding (themeID theme) brandinghash) $ loginBrandingCSS theme
  return (cssResponse brandingCSS)

-- used to deliver CSS for those pages that mimic the look of the company web ('Expression Engine').
handleScriveBranding :: Kontrakcja m => String -> String -> m Response
handleScriveBranding brandinghash _ = do
  brandingCSS <- withLessCache (ScriveBranding brandinghash) $ scriveBrandingCSS
  return (cssResponse brandingCSS)

-- Generates domain branding - enything that is onlu branded at domain level - i.e colors of status icons
handleDomainBranding :: Kontrakcja m => BrandedDomainID -> String -> String -> m Response
handleDomainBranding bdid brandinghash _ = do
  bd <- dbQuery $ GetBrandedDomainByID bdid
  brandingCSS <- withLessCache (DomainBranding bdid brandinghash) $ domainBrandingCSS bd
  return (cssResponse brandingCSS)

-- Used to brand signview
handleSignviewBranding :: Kontrakcja m => BrandedDomainID -> DocumentID -> String -> String -> m Response
handleSignviewBranding bdid did brandinghash _ = do
  theme <- getSignviewTheme bdid did
  brandingCSS <- withLessCache (SignviewBranding (themeID theme) brandinghash) $ signviewBrandingCSS theme
  return (cssResponse brandingCSS)

getSignviewTheme :: Kontrakcja m => BrandedDomainID -> DocumentID -> m Theme
getSignviewTheme bdid did = do
  bd <- dbQuery $ GetBrandedDomainByID bdid
  doc <- dbQuery $ GetDocumentByDocumentID did
  cid <- guardJust (documentauthorcompanyid doc)
  companyui <- dbQuery $ GetCompanyUI cid
  dbQuery $ GetTheme $ fromMaybe (bdSignviewTheme bd) (companySignviewTheme companyui)

-- Used to brand some view with signview branding but without any particular document. It requires some user to be logged in.
handleSignviewBrandingWithoutDocument :: Kontrakcja m => BrandedDomainID -> UserID -> String -> String -> m Response
handleSignviewBrandingWithoutDocument bdid uid brandinghash _ = do
  theme <- getSignviewThemeWithoutDocument bdid uid
  brandingCSS <- withLessCache (SignviewBranding (themeID theme) brandinghash) $ signviewBrandingCSS theme
  return (cssResponse brandingCSS)

getSignviewThemeWithoutDocument :: Kontrakcja m => BrandedDomainID -> UserID -> m Theme
getSignviewThemeWithoutDocument bdid uid = do
  bd <- dbQuery $ GetBrandedDomainByID bdid
  muser <- dbQuery $ GetUserByID uid
  user <- guardJust muser
  company <- getCompanyForUser user
  companyui <- dbQuery $ GetCompanyUI (companyid company)
  dbQuery $ GetTheme $ fromMaybe (bdSignviewTheme bd) (companySignviewTheme companyui)

loginLogo :: Kontrakcja m => BrandedDomainID -> String -> m Response
loginLogo bdid _ = do
  bd <- dbQuery $ GetBrandedDomainByID bdid
  theme <- dbQuery $ GetTheme (bdLoginTheme bd)
  return (imageResponse $ themeLogo theme)

serviceLogo :: Kontrakcja m => BrandedDomainID -> String -> String -> m Response
serviceLogo bdid uidstr _ = do
  muser <- case uidstr of
    "_" -> return Nothing
    s -> case maybeRead s of
      Nothing -> return Nothing
      Just uid -> dbQuery $ GetUserByID uid
  theme <- getServiceTheme bdid muser
  return (imageResponse $ themeLogo theme)

emailLogo :: Kontrakcja m => BrandedDomainID -> UserID -> String -> m Response
emailLogo bdid uid _ = do
  bd <- dbQuery $ GetBrandedDomainByID bdid
  muser <- dbQuery $ GetUserByID uid
  user <- guardJust muser
  company <- getCompanyForUser user
  companyui <- dbQuery $ GetCompanyUI (companyid company)
  theme <- dbQuery $ GetTheme $ fromMaybe (bdMailTheme bd) (companyMailTheme companyui)
  return (imageResponse $ themeLogo theme)

signviewLogo :: Kontrakcja m => BrandedDomainID -> DocumentID -> String -> m Response
signviewLogo bdid did _ = do
  theme <- getSignviewTheme bdid did
  return (imageResponse $ themeLogo theme)


signviewLogoWithoutDocument :: Kontrakcja m => BrandedDomainID -> UserID -> String -> m Response
signviewLogoWithoutDocument bdid uid _ = do
  theme <- getSignviewThemeWithoutDocument bdid uid
  return (imageResponse $ themeLogo theme)

faviconIcon :: Kontrakcja m => BrandedDomainID -> String -> String -> m Response
faviconIcon bdid uidstr _ = do
  mCompanyFavicon <- case uidstr of
    "_" -> return Nothing
    s -> case (maybeRead s :: Maybe UserID) of
      Nothing -> return Nothing
      Just uid -> do
        muser <- dbQuery $ GetUserByID uid
        case muser of
          Nothing -> return Nothing
          Just user -> do
            company <- getCompanyForUser user
            companyui <- dbQuery $ GetCompanyUI (companyid company)
            return (companyFavicon companyui)
  bd <- dbQuery $ GetBrandedDomainByID bdid
  let favicon = fromMaybe (bdFavicon bd) mCompanyFavicon
  return (imageResponse favicon)


-- Utils

withLessCache :: (Kontrakcja m ) => LessCacheKey -> m BSL.ByteString -> m BSL.ByteString
withLessCache key generator = do
  ctx <- getContext
  if ctxproduction ctx
    then MemCache.fetch_ (ctxlesscache ctx) key generator
    else generator

cssResponse :: BSL.ByteString -> Response
cssResponse css = setHeaderBS "Cache-Control" "max-age=31536000"
  $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "text/css")
  $ Response 200 Map.empty nullRsFlags css Nothing

imageResponse :: Binary BS.ByteString -> Response
imageResponse image = setHeaderBS "Cache-Control" "max-age=31536000"
  $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString contentType)
  $ Response 200 Map.empty nullRsFlags (BSL.fromChunks [unBinary image]) Nothing
  where content = unBinary image
        contentType | BS.take 4 content == "\x00\x00\x01\x00" = "image/x-icon"
                    | BS.take 2 content == "\xFF\xD8" = "image/jpeg"
                    | BS.take 8 content == "\x89\x50\x4E\x47\x0D\x0A\x1A\x0A" = "image/png"
                    | otherwise = "image/png"
