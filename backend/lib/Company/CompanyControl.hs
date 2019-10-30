{-# OPTIONS_GHC -fno-warn-orphans #-}

module Company.CompanyControl (
    routes
  , adminRoutes
  , withCompanyAdmin
  -- Exported for tests
  , handleChangeCompanyBranding
  , handleGetCompanyBranding
  , handleGetThemes
  , handleGetDomainThemes
  , handleUpdateTheme
  , handleDeleteTheme
  ) where

import Data.Unjson
import Happstack.Server hiding (dir, simpleHTTP)
import Happstack.StaticRouting (Route, choice, dir)
import Log as Log
import qualified Data.Aeson as Aeson
import qualified Data.Unjson as Unjson

import BrandedDomain.BrandedDomain
import Company.JSON
import DB
import Happstack.Fields
import Kontra
import Routing (hGet, hPost, toK0, toK1, toK2)
import Theme.Control
import Theme.ThemeID
import User.Utils
import UserGroup.Model
import UserGroup.Types
import Util.MonadUtils

routes :: Route (Kontra Response)
routes = choice
  [ dir "companybranding" $ hGet $ toK0 $ handleGetCompanyBranding Nothing
  , dir "companybranding" $ dir "themes" $ hGet $ toK0 $ handleGetThemes Nothing
  , dir "companybranding" $ dir "domainthemes" $ hGet $ toK0 $ handleGetDomainThemes
  , dir "companybranding" $ dir "change" $ hPost $ toK0 $ handleChangeCompanyBranding
    Nothing
  , dir "companybranding"
  $ dir "newtheme"
  $ hPost
  $ toK1
  $ (\themeType -> handleNewTheme themeType Nothing)
  , dir "companybranding" $ dir "updatetheme" $ hPost $ toK1 $ handleUpdateTheme Nothing
  , dir "companybranding" $ dir "deletetheme" $ hPost $ toK1 $ handleDeleteTheme Nothing
  , dir "companybranding" $ dir "signviewtheme" $ hGet $ toK0 $ handleGetSignviewTheme
  ]

adminRoutes :: Route (Kontra Response)
adminRoutes = choice
  [ dir "companybranding" $ hGet $ toK1 $ handleGetCompanyBranding . Just
  , dir "companybranding" $ dir "themes" $ hGet $ toK1 $ handleGetThemes . Just
  , dir "companybranding" $ dir "domainthemes" $ hGet $ toK0 $ handleGetDomainThemes
  , dir "companybranding"
  $ dir "change"
  $ hPost
  $ toK1
  $ handleChangeCompanyBranding
  . Just
  , dir "companybranding"
  $ dir "newtheme"
  $ hPost
  $ toK2
  $ (\cid themeType -> handleNewTheme themeType (Just cid))
  , dir "companybranding"
  $ dir "updatetheme"
  $ hPost
  $ toK2
  $ (\cid tid -> handleUpdateTheme (Just cid) tid)
  , dir "companybranding"
  $ dir "deletetheme"
  $ hPost
  $ toK2
  $ (\cid tid -> handleDeleteTheme (Just cid) tid)
  ]

{-
 - Company branding
 -}

handleGetCompanyBranding :: Kontrakcja m => Maybe UserGroupID -> m Aeson.Value
handleGetCompanyBranding mugid = do
  withCompanyAdminOrAdminOnly mugid $ \ug -> do
    return $ Unjson.unjsonToJSON' (Options { pretty = True, indent = 2, nulls = True })
                                  (unjsonUserGroupUIWithCompanyID $ ugID ug)
                                  (ugUI ug)

handleChangeCompanyBranding :: Kontrakcja m => Maybe UserGroupID -> m ()
handleChangeCompanyBranding mugid = withCompanyAdminOrAdminOnly mugid $ \ug -> do
  companyUIJSON <- guardJustM $ getFieldBS "companyui"
  case Aeson.eitherDecode $ companyUIJSON of
    Left err -> do
      logInfo "Error while parsing company branding" $ object ["error" .= err]
      internalError
    Right js -> case (Unjson.parse unjsonUserGroupUI js) of
      (Result ugui []) -> dbUpdate . UserGroupUpdate . set #ugUI ugui $ ug
      _                -> internalError

handleGetThemes :: Kontrakcja m => Maybe UserGroupID -> m Aeson.Value
handleGetThemes mugid = withCompanyAdminOrAdminOnly mugid $ \ug -> do
  handleGetThemesForUserGroup $ ugID ug

handleGetDomainThemes :: Kontrakcja m => m Aeson.Value
handleGetDomainThemes = do
  bd <- ctxBrandedDomain <$> getContext
  handleGetThemesUsedByDomain bd

handleGetSignviewTheme :: Kontrakcja m => m Aeson.Value
handleGetSignviewTheme = withUserAndGroup $ \(_, ug) -> do
  bd <- ctxBrandedDomain <$> getContext
  handleGetTheme
    . fromMaybe (bdSignviewTheme bd)
    . uguiSignviewTheme . ugUI
    $ ug

handleNewTheme :: Kontrakcja m => String -> Maybe UserGroupID -> m Aeson.Value
handleNewTheme s mugid = withCompanyAdminOrAdminOnly mugid $ \ug -> do
  bd  <- ctxBrandedDomain <$> getContext
  tid <- case s of
    "signview" -> return $ bdSignviewTheme bd
    "service"  -> return $ bdServiceTheme bd
    "mail"     -> return $ bdMailTheme bd
    _          -> internalError
  handleNewThemeForUserGroup (ugID ug) tid

handleDeleteTheme :: Kontrakcja m => Maybe UserGroupID -> ThemeID -> m ()
handleDeleteTheme mugid tid = withCompanyAdminOrAdminOnly mugid $ \ug -> do
  handleDeleteThemeForUserGroup (ugID ug) tid

handleUpdateTheme :: Kontrakcja m => Maybe UserGroupID -> ThemeID -> m ()
handleUpdateTheme mugid tid = withCompanyAdminOrAdminOnly mugid $ \ug -> do
  handleUpdateThemeForUserGroup (ugID ug) tid
