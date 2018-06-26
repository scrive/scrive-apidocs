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
  , unjsonUserGroupUI
  ) where

import Data.Functor.Invariant
import Data.Unjson
import Happstack.Server hiding (dir, simpleHTTP)
import Happstack.StaticRouting (Route, choice, dir)
import Log as Log
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Unjson as Unjson

import BrandedDomain.BrandedDomain
import Company.CompanyUI.Model
import DB
import Happstack.Fields
import Kontra
import Routing (hGet, hPost, toK0, toK1, toK2)
import Theme.Control
import Theme.ThemeID
import User.Utils
import UserGroup.Data
import UserGroup.Model
import Util.MonadUtils

routes :: Route (Kontra Response)
routes = choice
  [
    dir "companybranding" $ hGet $ toK0 $ handleGetCompanyBranding  Nothing
  , dir "companybranding" $ dir "themes" $ hGet $ toK0 $ handleGetThemes Nothing
  , dir "companybranding" $ dir "domainthemes" $ hGet $ toK0 $ handleGetDomainThemes
  , dir "companybranding" $ dir "change" $ hPost $ toK0 $ handleChangeCompanyBranding Nothing
  , dir "companybranding" $ dir "newtheme" $ hPost $ toK1 $ (\themeType -> handleNewTheme themeType Nothing)
  , dir "companybranding" $ dir "updatetheme" $ hPost $ toK1 $ handleUpdateTheme Nothing
  , dir "companybranding" $ dir "deletetheme" $ hPost $ toK1 $ handleDeleteTheme Nothing
  , dir "companybranding" $ dir "signviewtheme" $ hGet $ toK0 $ handleGetSignviewTheme
  ]

adminRoutes :: Route (Kontra Response)
adminRoutes = choice
  [
    dir "companybranding" $ hGet $ toK1 $ handleGetCompanyBranding . Just
  , dir "companybranding" $ dir "themes" $ hGet $ toK1 $ handleGetThemes . Just
  , dir "companybranding" $ dir "domainthemes" $ hGet $ toK0 $ handleGetDomainThemes
  , dir "companybranding" $ dir "change"$ hPost $ toK1 $ handleChangeCompanyBranding . Just
  , dir "companybranding" $ dir "newtheme" $ hPost $ toK2 $ (\cid themeType -> handleNewTheme themeType (Just cid))
  , dir "companybranding" $ dir "updatetheme" $ hPost $ toK2 $ (\cid tid -> handleUpdateTheme (Just cid) tid)
  , dir "companybranding" $ dir "deletetheme" $  hPost $ toK2 $ (\cid tid -> handleDeleteTheme (Just cid) tid)
  ]

handleGetCompanyBranding :: Kontrakcja m => Maybe UserGroupID -> m Aeson.Value
handleGetCompanyBranding mugid = do
  _ctx <- getContext
  withCompanyAdminOrAdminOnly mugid $ \ug -> do
    let companyui = fromUserGroupUI ug
    return $ Unjson.unjsonToJSON' (Options { pretty = True, indent = 2, nulls = True }) unjsonCompanyUI companyui

handleChangeCompanyBranding :: Kontrakcja m => Maybe UserGroupID -> m ()
handleChangeCompanyBranding mugid = withCompanyAdminOrAdminOnly mugid $ \ug -> do
  companyUIJSON <- guardJustM $ getFieldBS "companyui"
  case Aeson.eitherDecode $ companyUIJSON of
     Left err -> do
       logInfo "Error while parsing company branding" $ object [
          "error" .= err
        ]
       internalError
     Right js -> case (Unjson.parse unjsonUserGroupUI js) of
        (Result ugui []) -> dbUpdate . UserGroupUpdate . set ugUI ugui $ ug
        _ -> internalError

handleGetThemes :: Kontrakcja m =>  Maybe UserGroupID -> m Aeson.Value
handleGetThemes mugid = withCompanyAdminOrAdminOnly mugid $ \ug -> do
  handleGetThemesForUserGroup . get ugID $ ug

handleGetDomainThemes :: Kontrakcja m =>  m Aeson.Value
handleGetDomainThemes = do
  bd <- get ctxbrandeddomain <$> getContext
  handleGetThemesUsedByDomain bd

handleGetSignviewTheme :: Kontrakcja m => m Aeson.Value
handleGetSignviewTheme = withUserAndGroup $ \(_,ug) -> do
  bd <- get ctxbrandeddomain <$> getContext
  handleGetTheme . fromMaybe (get bdSignviewTheme bd) . get (uguiSignviewTheme . ugUI) $ ug

handleNewTheme :: Kontrakcja m =>  String -> Maybe UserGroupID -> m Aeson.Value
handleNewTheme s mugid = withCompanyAdminOrAdminOnly mugid $ \ug -> do
  bd <- get ctxbrandeddomain <$> getContext
  tid <- case s of
           "signview" -> return $ get bdSignviewTheme bd
           "service"  -> return $ get bdServiceTheme bd
           "mail" -> return $ get bdMailTheme bd
           _ -> internalError
  handleNewThemeForUserGroup (get ugID ug) tid

handleDeleteTheme :: Kontrakcja m =>  Maybe UserGroupID -> ThemeID -> m ()
handleDeleteTheme mugid tid =  withCompanyAdminOrAdminOnly mugid $ \ug -> do
  handleDeleteThemeForUserGroup (get ugID ug) tid

handleUpdateTheme :: Kontrakcja m =>  Maybe UserGroupID -> ThemeID -> m ()
handleUpdateTheme mugid tid = withCompanyAdminOrAdminOnly mugid $ \ug -> do
  handleUpdateThemeForUserGroup (get ugID ug) tid

unjsonCompanyUI :: UnjsonDef CompanyUI
unjsonCompanyUI = objectOf $ pure CompanyUI
  <*>  field "companyid"
      companyuicompanyid
      "Id of a company"
  <*> fieldOpt "mailTheme"
      companyMailTheme
      "Id of a mail theme"
  <*> fieldOpt "signviewTheme"
      companySignviewTheme
      "Id of a signview theme"
  <*> fieldOpt "serviceTheme"
      companyServiceTheme
      "Id of a service theme"
  <*> fieldOpt "browserTitle"
      companyBrowserTitle
      "Browser title"
  <*> fieldOpt "smsOriginator"
      companySmsOriginator
      "SMS Originator"
  <*> fieldOptBy "favicon"
      companyFavicon
      "Favicon"
       (invmap
          (\l -> B64.decodeLenient $ BSC8.pack $ drop 1 $ dropWhile ((/=) ',') $ l)
          (\l -> BSC8.unpack $ BS.append (BSC8.pack "data:image/png;base64,") $ B64.encode l)
          unjsonDef
       )

unjsonUserGroupUI :: UnjsonDef UserGroupUI
unjsonUserGroupUI = objectOf $ pure UserGroupUI
  <*> fieldOpt "mailTheme"
      _uguiMailTheme
      "Id of a mail theme"
  <*> fieldOpt "signviewTheme"
      _uguiSignviewTheme
      "Id of a signview theme"
  <*> fieldOpt "serviceTheme"
      _uguiServiceTheme
      "Id of a service theme"
  <*> fieldOpt "browserTitle"
      _uguiBrowserTitle
      "Browser title"
  <*> fieldOpt "smsOriginator"
      _uguiSmsOriginator
      "SMS Originator"
  <*> fieldOptBy "favicon"
      _uguiFavicon
      "Favicon"
       (invmap
          (\l -> B64.decodeLenient $ BSC8.pack $ drop 1 $ dropWhile ((/=) ',') $ l)
          (\l -> BSC8.unpack $ BS.append (BSC8.pack "data:image/png;base64,") $ B64.encode l)
          unjsonDef
       )
