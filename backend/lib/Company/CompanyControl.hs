{-# OPTIONS_GHC -fno-warn-orphans #-}

module Company.CompanyControl (
    routes
  , adminRoutes
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

import Company.JSON
import DB
import Doc.API.V2.JSON.Utils
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
  , (dir "companybranding" . dir "change" . hPost . toK0)
    $ handleChangeCompanyBranding Nothing
  , (dir "companybranding" . dir "newtheme" . hPost . toK1)
    $ \themeType -> handleNewTheme themeType Nothing
  , dir "companybranding" $ dir "updatetheme" $ hPost $ toK1 $ handleUpdateTheme Nothing
  , dir "companybranding" $ dir "deletetheme" $ hPost $ toK1 $ handleDeleteTheme Nothing
  , dir "companybranding" $ dir "signviewtheme" $ hGet $ toK0 $ handleGetSignviewTheme
  ]

adminRoutes :: Route (Kontra Response)
adminRoutes = choice
  [ dir "companybranding" $ hGet $ toK1 $ handleGetCompanyBranding . Just
  , dir "companybranding" $ dir "themes" $ hGet $ toK1 $ handleGetThemes . Just
  , dir "companybranding" $ dir "domainthemes" $ hGet $ toK0 $ handleGetDomainThemes
  , (dir "companybranding" . dir "change" . hPost . toK1)
    (handleChangeCompanyBranding . Just)
  , dir "companybranding" . dir "inherit" . hPost . toK1 $ handleInheritCompanyBranding
  , (dir "companybranding" . dir "newtheme" . hPost . toK2)
    $ \cid themeType -> handleNewTheme themeType (Just cid)
  , (dir "companybranding" . dir "updatetheme" . hPost . toK2)
    $ \cid tid -> handleUpdateTheme (Just cid) tid
  , (dir "companybranding" . dir "deletetheme" . hPost . toK2)
    $ \cid tid -> handleDeleteTheme (Just cid) tid
  ]

{-
 - Company branding
 -}

-- modelled after user group api v2 and should be moved there!
handleGetCompanyBranding :: Kontrakcja m => Maybe UserGroupID -> m Aeson.Value
handleGetCompanyBranding mugid = withCompanyAdminOrAdminOnly mugid $ \ug -> do
  ugwp <- dbQuery $ UserGroupGetWithParentsByUG ug
  let (ugforui, ui) = ugwpUIWithID ugwp
      inheritedFrom = if ugforui == ug ^. #id then Nothing else Just ugforui
      unjsonOptions = Options { pretty = True, indent = 2, nulls = True }
      unjsonUserGroupUI' =
        objectOf
          $  unjsonUserGroupUIFields
          <* fieldReadonly "companyid" (const $ ug ^. #id) "Company id"
          <* fieldReadOnlyOpt "inherited_from"
                              (const inheritedFrom)
                              "Branding is inherited"
          <* fieldReadOnlyOptBy unjsonOptions
                                "inheritable_preview"
                                (const . fmap ugwpUI $ ugwpOnlyParents ugwp)
                                "What would be inherited"
                                unjsonUserGroupUI

  -- nulls in inheritable_preview are missing...
  return $ Unjson.unjsonToJSON' unjsonOptions unjsonUserGroupUI' ui

-- Note that this fails if we try to modify an inherited branding, since we
-- don't _own_ the inherited themes!
handleChangeCompanyBranding :: Kontrakcja m => Maybe UserGroupID -> m ()
handleChangeCompanyBranding mugid = withCompanyAdminOrAdminOnly mugid $ \ug -> do
  companyUIJSON <- guardJustM $ getFieldBS "companyui"
  case Aeson.eitherDecode $ companyUIJSON of
    Left err -> do
      logInfo "Error while parsing company branding" $ object ["error" .= err]
      internalError
    Right js -> case (Unjson.parse unjsonUserGroupUI js) of
      (Result ugui []) -> dbUpdate . UserGroupUpdate . set #ui (Just ugui) $ ug
      _                -> internalError

handleInheritCompanyBranding :: Kontrakcja m => UserGroupID -> m ()
handleInheritCompanyBranding ugid = withSalesOrAdminOnly ugid $ \ug -> do
  inherit <- guardJustM $ getField "inherit"
  let newUI | inherit == "true" = Nothing
            | otherwise         = Just . fromMaybe defaultUserGroupUI $ ug ^. #ui
  dbUpdate $ UserGroupUpdateUI (ug ^. #id) newUI

handleGetThemes :: Kontrakcja m => Maybe UserGroupID -> m Aeson.Value
handleGetThemes mugid = withCompanyAdminOrAdminOnly mugid $ \ug -> do
  inherited <- getField "inherited"
  if inherited == Just "true"
    then handleGetThemesInheritableByUserGroup $ ug ^. #id
    else handleGetThemesOwnedByUserGroup $ ug ^. #id

handleGetDomainThemes :: Kontrakcja m => m Aeson.Value
handleGetDomainThemes = do
  bd <- view #brandedDomain <$> getContext
  handleGetThemesUsedByDomain bd

handleGetSignviewTheme :: Kontrakcja m => m Aeson.Value
handleGetSignviewTheme = withUserAndGroupWithParents $ \(_, ugwp) -> do
  bd <- view #brandedDomain <$> getContext
  handleGetTheme $ fromMaybe (bd ^. #signviewTheme) (ugwpUI ugwp ^. #signviewTheme)

handleNewTheme :: Kontrakcja m => String -> Maybe UserGroupID -> m Aeson.Value
handleNewTheme s mugid = withCompanyAdminOrAdminOnly mugid $ \ug -> do
  bd  <- view #brandedDomain <$> getContext
  tid <- case s of
    "signview" -> return $ bd ^. #signviewTheme
    "service"  -> return $ bd ^. #serviceTheme
    "mail"     -> return $ bd ^. #mailTheme
    _          -> internalError
  handleNewThemeForUserGroup (ug ^. #id) tid

handleDeleteTheme :: Kontrakcja m => Maybe UserGroupID -> ThemeID -> m ()
handleDeleteTheme mugid tid = withCompanyAdminOrAdminOnly mugid $ \ug -> do
  handleDeleteThemeForUserGroup (ug ^. #id) tid

handleUpdateTheme :: Kontrakcja m => Maybe UserGroupID -> ThemeID -> m ()
handleUpdateTheme mugid tid = withCompanyAdminOrAdminOnly mugid $ \ug -> do
  handleUpdateThemeForUserGroup (ug ^. #id) tid
