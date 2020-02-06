-----------------------------------------------------------------------------
-- |
-- Module      :  Administration.AdministrationView
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  develpment
-- Portability :  portable
--
-- Almost all the stuff that is visible under /adminsonly path
--
-----------------------------------------------------------------------------
module Administration.AdministrationView(
              adminMainPage
            , adminElmMainPage
            , adminUserPage
            , adminCompanyPage
          ) where

import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F

import AppView
import Kontra
import User.Model
import UserGroup.Types

adminMainPage :: TemplatesMonad m => Context -> m String
adminMainPage ctx = renderTemplate "adminsmain" $ do
  F.value "admin" $ isAdmin ctx
  entryPointFields ctx

adminElmMainPage :: TemplatesMonad m => Context -> m String
adminElmMainPage ctx = renderTemplate "adminElmMain" $ do
  F.value "admin" $ isAdmin ctx
  entryPointFields ctx

adminCompanyPage :: TemplatesMonad m => Context -> UserGroupID -> m String
adminCompanyPage ctx ugid = renderTemplate "admincompany" $ do
  F.value "companyid" $ show ugid
  F.value "admin" $ isAdmin ctx
  entryPointFields ctx

adminUserPage :: TemplatesMonad m => Context -> UserID -> m String
adminUserPage ctx uid = renderTemplate "adminuser" $ do
  F.value "userid" $ show uid
  F.value "admin" $ isAdmin ctx
  entryPointFields ctx
