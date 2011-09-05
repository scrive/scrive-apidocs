{-# LANGUAGE OverloadedStrings #-}
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
  getCompanyName,
  getCompanyNumber
  ) where

import Doc.DocStateData
import Company.Model

import qualified Data.ByteString as BS

-- | Anything that might have a company name and number
class HasSomeUserInfo a where
  getCompanyName   :: a -> BS.ByteString
  getCompanyNumber :: a -> BS.ByteString

instance HasSomeUserInfo Company where
  getCompanyName   = companyname . companyinfo
  getCompanyNumber = companynumber . companyinfo
  
instance HasSomeUserInfo (Maybe Company) where
  getCompanyName   = maybe BS.empty getCompanyName
  getCompanyNumber = maybe BS.empty getCompanyNumber

instance HasSomeUserInfo SignatoryDetails where
  getCompanyName   = getValueOfType CompanyFT
  getCompanyNumber = getValueOfType CompanyNumberFT

instance HasSomeUserInfo SignatoryLink where
  getCompanyName   = getCompanyName . signatorydetails
  getCompanyNumber = getCompanyNumber . signatorydetails
