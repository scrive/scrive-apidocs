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
import qualified Data.Aeson as Aeson

import BrandedDomain.BrandedDomain
import BrandedDomain.Model
import Company.Model
import DB
import Happstack.Fields
import Kontra
import KontraPrelude
import Theme.Model
import Theme.View
import Util.MonadUtils
import qualified Log as Log

handleGetTheme:: Kontrakcja m => ThemeID -> m Aeson.Value
handleGetTheme tid =  do
  theme <- dbQuery $ GetTheme tid
  return $ Unjson.unjsonToJSON' (Options { pretty = True, indent = 2, nulls = True }) unjsonTheme theme

handleGetThemesForCompany:: Kontrakcja m => CompanyID -> m Aeson.Value
handleGetThemesForCompany cid =  do
  themes <- dbQuery $ GetThemesForCompany cid
  return $ Unjson.unjsonToJSON' (Options { pretty = True, indent = 2, nulls = True }) unjsonThemesList themes

handleGetThemesForDomain:: Kontrakcja m => BrandedDomainID -> m Aeson.Value
handleGetThemesForDomain did =  do
  themes <- dbQuery $ GetThemesForDomain did
  return $ Unjson.unjsonToJSON'(Options { pretty = True, indent = 2, nulls = True }) unjsonThemesList themes

-- Generate list of themes used by given domain. Note that order is important here - but we don't need to introduce any middle structure.
handleGetThemesUsedByDomain:: Kontrakcja m => BrandedDomain -> m Aeson.Value
handleGetThemesUsedByDomain domain =  do
  mailTheme <- dbQuery $ GetTheme $ bdMailTheme domain
  signviewTheme <- dbQuery $ GetTheme $ bdSignviewTheme domain
  serviceTheme <- dbQuery $ GetTheme $ bdServiceTheme domain
  return $ Unjson.unjsonToJSON'  (Options { pretty = True, indent = 2, nulls = True }) unjsonThemesList [mailTheme,signviewTheme,serviceTheme]

handleUpdateThemeForDomain:: Kontrakcja m => BrandedDomainID -> ThemeID -> m ()
handleUpdateThemeForDomain did tid =  do
  guardNotMainDomain did "Main domain themes can't be changed"
  theme <- dbQuery $ GetTheme tid
  themeJSON <- guardJustM $ getFieldBS "theme"
  case Aeson.eitherDecode themeJSON of
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
  themeJSON <- guardJustM $ getFieldBS "theme"
  case Aeson.eitherDecode themeJSON of
   Left err -> do
     Log.mixlog_ $ "Error while parsing theme for company " ++ err
     internalError
   Right js -> case (Unjson.parse unjsonTheme js) of
        (Result newTheme []) -> do
          _ <- dbUpdate $ UpdateThemeForCompany cid newTheme {themeID = themeID theme}
          return ()
        _ -> internalError


handleNewThemeForDomain:: Kontrakcja m => BrandedDomainID -> ThemeID -> m Aeson.Value
handleNewThemeForDomain did tid = do
  guardNotMainDomain did "Can't create new themes for main domain"
  theme <- dbQuery $ GetTheme tid
  name <- guardJustM $ getField "name"
  newTheme <- dbUpdate $ InsertNewThemeForDomain did $ theme {themeName = name}
  return $ Unjson.unjsonToJSON' (Options { pretty = True, indent = 2, nulls = True }) unjsonTheme newTheme

handleNewThemeForCompany :: Kontrakcja m => CompanyID -> ThemeID -> m Aeson.Value
handleNewThemeForCompany cid tid = do
  theme <- dbQuery $ GetTheme tid
  name <- guardJustM $ getField "name"
  newTheme <- dbUpdate $ InsertNewThemeForCompany cid $ theme {themeName = name}
  return $ Unjson.unjsonToJSON' (Options { pretty = True, indent = 2, nulls = True }) unjsonTheme newTheme

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
