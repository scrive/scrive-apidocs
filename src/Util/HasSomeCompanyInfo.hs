-----------------------------------------------------------------------------
-- |
-- Module      :  Util.HasSomeCompanyInfo
-- Stability   :  development
-- Portability :  portable
--
-- Utility for abstracting away destructuring to get a
-- company number, and company name.
-----------------------------------------------------------------------------
module Util.HasSomeCompanyInfo (
    getCompanyName
  , getCompanyNumber
  , HasSomeCompanyInfo
  ) where

import Data.Maybe

import User.Model
import Doc.DocStateData
import Company.Model
import Util.SignatoryLinkUtils
import Misc

-- | Anything that might have a company name and number
class HasSomeCompanyInfo a where
  getCompanyName   :: a -> String
  getCompanyNumber :: a -> String

instance HasSomeCompanyInfo Company where
  getCompanyName   = companyname   . companyinfo
  getCompanyNumber = companynumber . companyinfo

instance HasSomeCompanyInfo (Maybe Company) where
  getCompanyName   = maybe "" getCompanyName
  getCompanyNumber = maybe "" getCompanyNumber

instance HasSomeCompanyInfo SignatoryDetails where
  getCompanyName   = getValueOfType CompanyFT
  getCompanyNumber = getValueOfType CompanyNumberFT

instance HasSomeCompanyInfo SignatoryLink where
  getCompanyName   = getCompanyName   . signatorydetails
  getCompanyNumber = getCompanyNumber . signatorydetails

instance HasSomeCompanyInfo Document where
  getCompanyName   doc = maybe "" getCompanyName   $ getAuthorSigLink doc
  getCompanyNumber doc = maybe "" getCompanyNumber $ getAuthorSigLink doc

instance HasSomeCompanyInfo UserInfo where
  getCompanyName   = usercompanyname
  getCompanyNumber = usercompanynumber

instance HasSomeCompanyInfo User where
  getCompanyName   user = "" <| (isJust $ usercompany user) |> getCompanyName   (userinfo user)
  getCompanyNumber user = "" <| (isJust $ usercompany user) |> getCompanyNumber (userinfo user)

instance HasSomeCompanyInfo (User, Maybe Company) where
  getCompanyName   (u, mc) = maybe (getCompanyName   u) getCompanyName   mc
  getCompanyNumber (u, mc) = maybe (getCompanyNumber u) getCompanyNumber mc
