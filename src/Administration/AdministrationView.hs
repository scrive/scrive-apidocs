-----------------------------------------------------------------------------
-- |
-- Module      :  Administration.AdministrationView
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  develpment
-- Portability :  portable
--
-- Almoust all the stuff that is visible under /adminsonly path
--
-----------------------------------------------------------------------------
module Administration.AdministrationView(
              adminMainPage
            , adminUserPage
            , adminCompanyPage
          ) where

import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F

import Company.Model
import Kontra
import KontraPrelude
import User.Model

adminMainPage :: TemplatesMonad m => Context -> m String
adminMainPage ctx = renderTemplate "adminsmain" $ F.value "admin" $ isAdmin ctx

adminCompanyPage :: TemplatesMonad m => CompanyID ->  m String
adminCompanyPage cid = renderTemplate "admincompany" $ (F.value "companyid" $ show cid)

adminUserPage :: TemplatesMonad m => UserID -> m String
adminUserPage uid = renderTemplate "adminuser" $ (F.value "userid" $ show uid)

