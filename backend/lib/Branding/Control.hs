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
import Branding.CSS
import DB
import Doc.DocStateData
import Doc.DocumentID
import Doc.Model.Query
import Kontra
import Theme.Model
import User.Model
import User.Utils
import UserGroup.Model
import UserGroup.Types
import Util.MonadUtils

handleServiceBranding :: Kontrakcja m => BrandedDomainID -> String -> String -> m Response
handleServiceBranding bdID uidstr _ = do
  muser <- case uidstr of
    "_"-> return Nothing
    s -> case maybeRead s of
      Nothing -> return Nothing
      Just uid -> dbQuery $ GetUserByID uid
  theme <- getServiceTheme bdID muser
  brandingCSS <- serviceBrandingCSS theme
  return (cssResponse brandingCSS)

getServiceTheme ::  Kontrakcja m => BrandedDomainID -> Maybe User -> m Theme
getServiceTheme bdID muser = do
  bd <- dbQuery $ GetBrandedDomainByID bdID
  case muser of
    Nothing -> dbQuery $ GetTheme $ get bdServiceTheme bd
    Just user -> do
      ug <- getUserGroupForUser user
      dbQuery . GetTheme . fromMaybe (get bdServiceTheme bd) . get (uguiServiceTheme . ugUI) $ ug

handleLoginBranding :: Kontrakcja m => BrandedDomainID -> String -> m Response
handleLoginBranding bdID _ = do
  bd <- dbQuery $ GetBrandedDomainByID bdID
  theme <- dbQuery $ GetTheme (get bdLoginTheme bd)
  brandingCSS <- loginBrandingCSS theme
  return (cssResponse brandingCSS)

-- used to deliver CSS for those pages that mimic the look of the company web ('Expression Engine').
handleScriveBranding :: Kontrakcja m => String -> m Response
handleScriveBranding _ = do
  brandingCSS <- scriveBrandingCSS
  return (cssResponse brandingCSS)

-- Generates domain branding - enything that is onlu branded at domain level - i.e colors of status icons
handleDomainBranding :: Kontrakcja m => BrandedDomainID -> String -> m Response
handleDomainBranding bdID _ = do
  bd <- dbQuery $ GetBrandedDomainByID bdID
  brandingCSS <- domainBrandingCSS bd
  return (cssResponse brandingCSS)

-- Used to brand signview
handleSignviewBranding :: Kontrakcja m => BrandedDomainID -> DocumentID -> String -> m Response
handleSignviewBranding bdID did _ = do
  theme <- getSignviewTheme bdID did
  brandingCSS <- signviewBrandingCSS theme
  return (cssResponse brandingCSS)

getSignviewTheme :: Kontrakcja m => BrandedDomainID -> DocumentID -> m Theme
getSignviewTheme bdID did = do
  bd <- dbQuery $ GetBrandedDomainByID bdID
  doc <- dbQuery $ GetDocumentByDocumentID did
  ugid <- guardJust (documentauthorugid doc)
  ug <- guardJustM . dbQuery . UserGroupGet $ ugid
  dbQuery . GetTheme . fromMaybe (get bdSignviewTheme bd) . get (uguiSignviewTheme . ugUI) $ ug

-- Used to brand some view with signview branding but without any particular document. It requires some user to be logged in.
handleSignviewBrandingWithoutDocument :: Kontrakcja m => BrandedDomainID -> UserID -> String -> m Response
handleSignviewBrandingWithoutDocument bdID uid _ = do
  theme <- getSignviewThemeWithoutDocument bdID uid
  brandingCSS <- signviewBrandingCSS theme
  return (cssResponse brandingCSS)

getSignviewThemeWithoutDocument :: Kontrakcja m => BrandedDomainID -> UserID -> m Theme
getSignviewThemeWithoutDocument bdID uid = do
  bd <- dbQuery $ GetBrandedDomainByID bdID
  muser <- dbQuery $ GetUserByID uid
  user <- guardJust muser
  ug <- getUserGroupForUser user
  dbQuery . GetTheme . fromMaybe (get bdSignviewTheme bd) . get (uguiSignviewTheme . ugUI) $ ug

loginLogo :: Kontrakcja m => BrandedDomainID -> String -> m Response
loginLogo bdID _ = do
  bd <- dbQuery $ GetBrandedDomainByID bdID
  theme <- dbQuery $ GetTheme (get bdLoginTheme bd)
  return (imageResponse $ themeLogo theme)

serviceLogo :: Kontrakcja m => BrandedDomainID -> String -> String -> m Response
serviceLogo bdID uidstr _ = do
  muser <- case uidstr of
    "_" -> return Nothing
    s -> case maybeRead s of
      Nothing -> return Nothing
      Just uid -> dbQuery $ GetUserByID uid
  theme <- getServiceTheme bdID muser
  return (imageResponse $ themeLogo theme)

emailLogo :: Kontrakcja m => BrandedDomainID -> UserID -> String -> m Response
emailLogo bdID uid _ = do
  bd <- dbQuery $ GetBrandedDomainByID bdID
  muser <- dbQuery $ GetUserByID uid
  user <- guardJust muser
  ug <- getUserGroupForUser user
  theme <- dbQuery . GetTheme . fromMaybe (get bdMailTheme bd) . get (uguiMailTheme . ugUI) $ ug
  return (imageResponse $ themeLogo theme)

signviewLogo :: Kontrakcja m => BrandedDomainID -> DocumentID -> String -> m Response
signviewLogo bdID did _ = do
  theme <- getSignviewTheme bdID did
  return (imageResponse $ themeLogo theme)


signviewLogoWithoutDocument :: Kontrakcja m => BrandedDomainID -> UserID -> String -> m Response
signviewLogoWithoutDocument bdID uid _ = do
  theme <- getSignviewThemeWithoutDocument bdID uid
  return (imageResponse $ themeLogo theme)

faviconIcon :: Kontrakcja m => BrandedDomainID -> String -> String -> m Response
faviconIcon bdID uidstr _ = do
  mCompanyFavicon <- case uidstr of
    "_" -> return Nothing
    s -> case (maybeRead s :: Maybe UserID) of
      Nothing -> return Nothing
      Just uid -> do
        muser <- dbQuery $ GetUserByID uid
        case muser of
          Nothing -> return Nothing
          Just user -> do
            ug <- getUserGroupForUser user
            return . get (uguiFavicon . ugUI) $ ug

  bd <- dbQuery $ GetBrandedDomainByID bdID
  let favicon = fromMaybe (get bdFavicon bd) mCompanyFavicon
  return (imageResponse favicon)


-- Utils

cssResponse :: BSL.ByteString -> Response
cssResponse css = setHeaderBS "Cache-Control" "max-age=31536000"
  $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "text/css")
  $ Response 200 Map.empty nullRsFlags css Nothing

imageResponse :: BS.ByteString -> Response
imageResponse image = setHeaderBS "Cache-Control" "max-age=31536000"
  $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString contentType)
  $ Response 200 Map.empty nullRsFlags (BSL.fromChunks [image]) Nothing
  where content = image
        contentType | BS.take 4 content == "\x00\x00\x01\x00" = "image/x-icon"
                    | BS.take 2 content == "\xFF\xD8" = "image/jpeg"
                    | BS.take 8 content == "\x89\x50\x4E\x47\x0D\x0A\x1A\x0A" = "image/png"
                    | otherwise = "image/png"
