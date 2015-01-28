{-# OPTIONS_GHC -fno-warn-orphans #-}

module Company.CompanyControl (
    handleChangeCompanyBranding
  , handleGetCompanyBranding
  , routes
  , adminRoutes
  , withCompanyAdmin
  ) where

import Control.Applicative
import Data.Functor.Invariant
import Data.Unjson
import Data.Maybe
import Happstack.Server hiding (dir, simpleHTTP)
import Happstack.StaticRouting (Route, dir, choice)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.UTF8 as BSU8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Unjson as Unjson
import qualified Data.Aeson as Aeson

import BrandedDomain.BrandedDomain
import Company.CompanyUI
import Company.Model
import DB
import Happstack.Fields
import Kontra
import Routing (hGet, hPost, toK0, toK1, toK2)
import Theme.Control
import Theme.ThemeID
import User.Utils
import Util.MonadUtils
import qualified Log as Log

routes :: Route (KontraPlus Response)
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

adminRoutes :: Route (KontraPlus Response)
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

handleGetCompanyBranding :: Kontrakcja m => Maybe CompanyID -> m Response
handleGetCompanyBranding mcid = do
  _ctx <- getContext
  withCompanyAdminOrAdminOnly mcid $ \company -> do
    companyui <- dbQuery $ GetCompanyUI (companyid company)
    let  res = unjsonToByteStringLazy' (Options { pretty = True, indent = 2, nulls = True }) unjsonCompanyUI companyui
    return $ toResponseBS "text/json" $ res

handleChangeCompanyBranding :: Kontrakcja m => Maybe CompanyID -> m ()
handleChangeCompanyBranding mcid = withCompanyAdminOrAdminOnly mcid $ \company -> do
  companyUIJSON <- guardJustM $ getField "companyui"
  case Aeson.eitherDecode $ BSL.fromStrict (BSU8.fromString companyUIJSON) of
     Left err -> do
       Log.mixlog_ $ "Error while parsing company branding " ++ err
       internalError
     Right js -> case (Unjson.parse unjsonCompanyUI js) of
        (Result cui []) -> do
           _ <- dbUpdate $ SetCompanyUI (companyid company) cui
           return ()
        _ -> internalError

handleGetThemes :: Kontrakcja m =>  Maybe CompanyID -> m Response
handleGetThemes mcid = withCompanyAdminOrAdminOnly mcid $ \company -> do
  handleGetThemesForCompany (companyid company)

handleGetDomainThemes :: Kontrakcja m =>  m Response
handleGetDomainThemes = do
  bd <- ctxbrandeddomain <$> getContext
  handleGetThemesUsedByDomain bd

handleGetSignviewTheme :: Kontrakcja m => m Response
handleGetSignviewTheme = withCompanyUser $ \(_,company) -> do
  cu <- dbQuery $ GetCompanyUI $ companyid company
  bd <- ctxbrandeddomain <$> getContext
  handleGetTheme $ fromMaybe (bdSignviewTheme bd) (companySignviewTheme cu)

handleNewTheme :: Kontrakcja m =>  String -> Maybe CompanyID -> m Response
handleNewTheme s mcid = withCompanyAdminOrAdminOnly mcid $ \company -> do
  bd <- ctxbrandeddomain <$> getContext
  tid <- case s of
           "signview" -> return $ bdSignviewTheme bd
           "service"  -> return $ bdServiceTheme bd
           "mail" -> return $ bdMailTheme bd
           _ -> internalError
  handleNewThemeForCompany (companyid company) tid

handleDeleteTheme :: Kontrakcja m =>  Maybe CompanyID -> ThemeID -> m ()
handleDeleteTheme mcid tid =  withCompanyAdminOrAdminOnly mcid $ \company -> do
  handleDeleteThemeForCompany (companyid company) tid

handleUpdateTheme :: Kontrakcja m =>  Maybe CompanyID -> ThemeID -> m ()
handleUpdateTheme mcid tid = withCompanyAdminOrAdminOnly mcid $ \company -> do
  handleUpdateThemeForCompany (companyid company) tid

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
          (\l -> Binary $ B64.decodeLenient $ BSC8.pack $ drop 1 $ dropWhile ((/=) ',') $ l)
          (\l -> BSC8.unpack $ BS.append (BSC8.pack "data:image/png;base64,") $ B64.encode $ unBinary $ l)
          unjsonDef
       )

instance Unjson CompanyUI where
  unjsonDef = unjsonCompanyUI
