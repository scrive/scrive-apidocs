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
            , adminUserPage
            , adminCompanyPage
          ) where

import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F

import AppView
import Company.Model
import Kontra
import User.Model

adminMainPage :: TemplatesMonad m => Context -> m String
adminMainPage ctx = renderTemplate "adminsmain" $ do
  F.value "admin" $ isAdmin ctx
  entryPointFields ctx

adminCompanyPage :: TemplatesMonad m => Context -> CompanyID ->  m String
adminCompanyPage ctx cid = renderTemplate "admincompany" $ do
  F.value "companyid" $ show cid
  F.value "admin" $ isAdmin ctx
  entryPointFields ctx

adminUserPage :: TemplatesMonad m => Context -> UserID -> m String
adminUserPage ctx uid = renderTemplate "adminuser" $ do
  F.value "userid" $ show uid
  F.value "admin" $ isAdmin ctx
  entryPointFields ctx

