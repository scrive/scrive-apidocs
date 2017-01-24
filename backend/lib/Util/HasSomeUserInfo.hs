-----------------------------------------------------------------------------
-- |
-- Module      :  Util.HasSomeUserInfo
-- Author      :  Eric Normand
-- Stability   :  development
-- Portability :  portable
--
-- Utility for abstracting away destructuring to get a name, email,
-- and personal number.
-----------------------------------------------------------------------------
module Util.HasSomeUserInfo (
  getMailAddress,
  getFullName,
  getSmartName,
  getSmartNameOrPlaceholder,
  HasSomeUserInfo(..)
  ) where

import Data.String.Utils
import Text.StringTemplates.Templates

import KontraPrelude
import Mails.MailsData
import Utils.String

-- | Anything that might have a first name and last name, or personalnumber
class HasSomeUserInfo a where
  getEmail          :: a -> String
  getFirstName      :: a -> String
  getLastName       :: a -> String
  getPersonalNumber :: a -> String
  getMobile         :: a -> String

-- | Get the full name (first last)
getFullName :: (HasSomeUserInfo a) => a -> String
getFullName a = strip $ (strip $ getFirstName a) ++ " " ++ (strip $ getLastName  a)

-- | Return the first non-empty of full name, email or mobile number,
-- for use in frontend.  Not guaranteed to be unique.
getSmartName :: (HasSomeUserInfo a) => a -> String
getSmartName a = firstNonEmpty $ [getFullName a, getEmail a, getMobile a]

-- | Uses '_notNamedParty' localized source text with `getSmartNameFromMaybe`.
getSmartNameOrPlaceholder :: (HasSomeUserInfo a, TemplatesMonad m) => a -> m String
getSmartNameOrPlaceholder a = do
    notNamed <- renderTemplate_ "_notNamedParty"
    return $ firstNonEmpty [getSmartName a, notNamed]

-- | Get a MailAddress
getMailAddress :: (HasSomeUserInfo a) => a -> MailAddress
getMailAddress a = MailAddress {
    fullname = getFullName a
  , email    = getEmail a
  }
