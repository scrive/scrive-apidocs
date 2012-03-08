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
  getCompanyNumber,
  HasSomeCompanyInfo
  ) where

import Doc.DocStateData
import Company.Model
import Util.SignatoryLinkUtils

import qualified Data.ByteString as BS

-- | Anything that might have a company name and number
class HasSomeCompanyInfo a where
  getCompanyName   :: a -> BS.ByteString
  getCompanyNumber :: a -> BS.ByteString

instance HasSomeCompanyInfo Company where
  getCompanyName   = companyname   . companyinfo
  getCompanyNumber = companynumber . companyinfo

instance HasSomeCompanyInfo (Maybe Company) where
  getCompanyName   = maybe BS.empty getCompanyName
  getCompanyNumber = maybe BS.empty getCompanyNumber

instance HasSomeCompanyInfo SignatoryDetails where
  getCompanyName   = getValueOfType CompanyFT
  getCompanyNumber = getValueOfType CompanyNumberFT

instance HasSomeCompanyInfo SignatoryLink where
  getCompanyName   = getCompanyName   . signatorydetails
  getCompanyNumber = getCompanyNumber . signatorydetails

instance HasSomeCompanyInfo Document where
  getCompanyName  doc  = maybe BS.empty getCompanyName   $ getAuthorSigLink doc
  getCompanyNumber doc = maybe BS.empty getCompanyNumber $ getAuthorSigLink doc
