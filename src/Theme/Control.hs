{-  Set fo handlers for manipulating themes. Used by other handlers, should not be used on their own - since they don't do any access control,
    except for theme ownership.

    IMPORTANT: No function from this module does access control. They should not be used on their own.
-}
module Theme.Control (
    handleGetTheme
  , handleGetThemesForCompany
  , handleGetThemesForDomain
  , handleGetThemesUsedByDomain
  , handleNewThemeForDomain
  , handleNewThemeForCompany
  , handleUpdateThemeForDomain
  , handleUpdateThemeForCompany
  , handleDeleteThemeForDomain
  , handleDeleteThemeForCompany
  ) where

import Data.Unjson
import Data.Unjson as Unjson
import Happstack.Server hiding (dir, simpleHTTP)
import qualified Data.ByteString.UTF8 as BSU8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as Aeson

import BrandedDomain.BrandedDomain
import BrandedDomain.Model
import Company.Model
import DB
import Happstack.Fields
import Kontra
import Theme.Model
import Theme.View
import Util.MonadUtils
import qualified Log as Log

handleGetTheme:: Kontrakcja m => ThemeID -> m Response
handleGetTheme tid =  do
  theme <- dbQuery $ GetTheme tid
  let  res = unjsonToByteStringLazy' (Options { pretty = True, indent = 2, nulls = True }) unjsonTheme theme
  return $ toResponseBS "text/json" $ res

handleGetThemesForCompany:: Kontrakcja m => CompanyID -> m Response
handleGetThemesForCompany cid =  do
  themes <- dbQuery $ GetThemesForCompany cid
  let  res = unjsonToByteStringLazy' (Options { pretty = True, indent = 2, nulls = True }) unjsonThemesList themes
  return $ toResponseBS "text/json" $ res

handleGetThemesForDomain:: Kontrakcja m => BrandedDomainID -> m Response
handleGetThemesForDomain did =  do
  themes <- dbQuery $ GetThemesForDomain did
  let  res = unjsonToByteStringLazy' (Options { pretty = True, indent = 2, nulls = True }) unjsonThemesList themes
  return $ toResponseBS "text/json" $ res

-- Generate list of themes used by given domain. Note that order is important here - but we don't need to introduce any middle structure.
handleGetThemesUsedByDomain:: Kontrakcja m => BrandedDomain -> m Response
handleGetThemesUsedByDomain domain =  do
  mailTheme <- dbQuery $ GetTheme $ bdMailTheme domain
  signviewTheme <- dbQuery $ GetTheme $ bdSignviewTheme domain
  serviceTheme <- dbQuery $ GetTheme $ bdServiceTheme domain
  let  res = unjsonToByteStringLazy' (Options { pretty = True, indent = 2, nulls = True }) unjsonThemesList [mailTheme,signviewTheme,serviceTheme]
  return $ toResponseBS "text/json" $ res

handleUpdateThemeForDomain:: Kontrakcja m => BrandedDomainID -> ThemeID -> m ()
handleUpdateThemeForDomain did tid =  do
  guardNotMainDomain did "Main domain themes can't be changed"
  theme <- dbQuery $ GetTheme tid
  themeJSON <- guardJustM $ getField "theme"
  case Aeson.eitherDecode $ BSL.fromStrict (BSU8.fromString themeJSON) of
    Left err -> do
      Log.mixlog_ $ "Error while parsing theme for domain " ++ err
      internalError
    Right js -> case (Unjson.parse unjsonTheme js) of
      (Result newTheme []) -> do
        _ <- dbUpdate $ UpdateThemeForDomain did newTheme {themeID = themeID theme}
        return ()
      _ -> internalError

handleUpdateThemeForCompany:: Kontrakcja m => CompanyID -> ThemeID -> m ()
handleUpdateThemeForCompany cid tid =  do
  theme <- dbQuery $ GetTheme tid
  themeJSON <- guardJustM $ getField "theme"
  case Aeson.eitherDecode $ BSL.fromStrict (BSU8.fromString themeJSON) of
   Left err -> do
     Log.mixlog_ $ "Error while parsing theme for company " ++ err
     internalError
   Right js -> case (Unjson.parse unjsonTheme js) of
        (Result newTheme []) -> do
          _ <- dbUpdate $ UpdateThemeForCompany cid newTheme {themeID = themeID theme}
          return ()
        _ -> internalError


handleNewThemeForDomain:: Kontrakcja m => BrandedDomainID -> ThemeID -> m Response
handleNewThemeForDomain did tid = do
  guardNotMainDomain did "Can't create new themes for main domain"
  theme <- dbQuery $ GetTheme tid
  name <- guardJustM $ getField "name"
  newTheme <- dbUpdate $ InsertNewThemeForDomain did $ theme {themeName = name}
  let  res = unjsonToByteStringLazy' (Options { pretty = True, indent = 2, nulls = True }) unjsonTheme newTheme
  return $ toResponseBS "text/json" $ res

handleNewThemeForCompany :: Kontrakcja m => CompanyID -> ThemeID -> m Response
handleNewThemeForCompany cid tid = do
  theme <- dbQuery $ GetTheme tid
  name <- guardJustM $ getField "name"
  newTheme <- dbUpdate $ InsertNewThemeForCompany cid $ theme {themeName = name}
  let  res = unjsonToByteStringLazy' (Options { pretty = True, indent = 2, nulls = True }) unjsonTheme newTheme
  return $ toResponseBS "text/json" $ res

handleDeleteThemeForDomain:: Kontrakcja m => BrandedDomainID -> ThemeID -> m ()
handleDeleteThemeForDomain did tid = do
  guardNotMainDomain did  "Main domain themes can't be deleted"
  dbUpdate $ DeleteThemeOwnedByDomain did tid

handleDeleteThemeForCompany:: Kontrakcja m => CompanyID -> ThemeID -> m ()
handleDeleteThemeForCompany cid tid = do
  dbUpdate $ DeleteThemeOwnedByCompany cid tid


guardNotMainDomain :: Kontrakcja m => BrandedDomainID -> String -> m ()
guardNotMainDomain did msg = do
  bd <- dbQuery $ GetBrandedDomainByID did
  if (bdMainDomain bd)
   then do
    Log.mixlog_ $ msg
    internalError
   else return ()