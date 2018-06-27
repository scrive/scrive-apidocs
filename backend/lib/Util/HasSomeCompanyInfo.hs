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

import qualified Data.Text as T

import Doc.DocStateData
import UserGroup.Data
import Util.SignatoryLinkUtils

-- | Anything that might have a company name and number
class HasSomeCompanyInfo a where
  getCompanyName   :: a -> String
  getCompanyNumber :: a -> String

instance (HasSomeCompanyInfo a) => HasSomeCompanyInfo (Maybe a) where
  getCompanyName   = maybe "" getCompanyName
  getCompanyNumber = maybe "" getCompanyNumber

instance HasSomeCompanyInfo SignatoryLink where
  getCompanyName   = getCompanyName . signatoryfields
  getCompanyNumber = getCompanyNumber . signatoryfields

instance HasSomeCompanyInfo [SignatoryField] where
  getCompanyName   = getTextValueOfField CompanyFI
  getCompanyNumber = getTextValueOfField CompanyNumberFI

instance HasSomeCompanyInfo Document where
  getCompanyName   doc = maybe "" getCompanyName   $ getAuthorSigLink doc
  getCompanyNumber doc = maybe "" getCompanyNumber $ getAuthorSigLink doc

instance HasSomeCompanyInfo UserGroup where
  getCompanyName   = T.unpack . get ugName
  getCompanyNumber = T.unpack . get (ugaCompanyNumber . ugAddress)
