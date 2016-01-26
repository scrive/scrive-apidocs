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
  return (pngResponse $ themeLogo theme)

serviceLogo :: Kontrakcja m => BrandedDomainID -> String -> String -> m Response
serviceLogo bdid uidstr _ = do
  muser <- case uidstr of
    "_" -> return Nothing
    s -> case maybeRead s of
      Nothing -> return Nothing
      Just uid -> dbQuery $ GetUserByID uid
  theme <- getServiceTheme bdid muser
  return (pngResponse $ themeLogo theme)

emailLogo :: Kontrakcja m => BrandedDomainID -> UserID -> String -> m Response
emailLogo bdid uid _ = do
  bd <- dbQuery $ GetBrandedDomainByID bdid
  muser <- dbQuery $ GetUserByID uid
  user <- guardJust muser
  company <- getCompanyForUser user
  companyui <- dbQuery $ GetCompanyUI (companyid company)
  theme <- dbQuery $ GetTheme $ fromMaybe (bdMailTheme bd) (companyMailTheme companyui)
  return (pngResponse $ themeLogo theme)

signviewLogo :: Kontrakcja m => BrandedDomainID -> DocumentID -> String -> m Response
signviewLogo bdid did _ = do
  theme <- getSignviewTheme bdid did
  return (pngResponse $ themeLogo theme)


signviewLogoWithoutDocument :: Kontrakcja m => BrandedDomainID -> UserID -> String -> m Response
signviewLogoWithoutDocument bdid uid _ = do
  theme <- getSignviewThemeWithoutDocument bdid uid
  return (pngResponse $ themeLogo theme)

faviconIcon :: Kontrakcja m => String  -> m Response
faviconIcon _ = do
  ctx <- getContext
  companyFavicon <- case getContextUser ctx of
    Nothing -> return Nothing
    Just user -> do
      company <- getCompanyForUser user
      companyui <- dbQuery $ GetCompanyUI (companyid company)
      return (companyFavicon $ companyui)
  let favicon = fromMaybe (bdFavicon $ ctxbrandeddomain ctx) companyFavicon
  return (pngResponse favicon)


-- Utils

withLessCache :: (Kontrakcja m ) => LessCacheKey -> m BSL.ByteString -> m BSL.ByteString
withLessCache key generator = do
  ctx <- getContext
  if (ctxproduction ctx) -- We only use cache if we are in production mode
    then do
      let cache = ctxlesscache ctx
      mv <- MemCache.get key cache
      case mv of
        Just v -> return v
        Nothing -> do
          css <- generator
          MemCache.put key css cache
          return css
    else generator

cssResponse :: BSL.ByteString -> Response
cssResponse css = setHeaderBS "Cache-Control" "max-age=31536000"
  $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "text/css")
  $ Response 200 Map.empty nullRsFlags css Nothing

pngResponse :: Binary BS.ByteString -> Response
pngResponse png = setHeaderBS "Cache-Control" "max-age=31536000"
  $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/png")
  $ Response 200 Map.empty nullRsFlags (BSL.fromChunks [unBinary png]) Nothing
