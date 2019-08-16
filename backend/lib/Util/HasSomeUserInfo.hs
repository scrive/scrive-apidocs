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

import Text.StringTemplates.Templates
import qualified Data.Text as T

import Mails.MailsData
import Templates (renderTextTemplate_)
import Utils.String

-- | Anything that might have a first name and last name, or personalnumber
class HasSomeUserInfo a where
  getEmail          :: a -> Text
  getFirstName      :: a -> Text
  getLastName       :: a -> Text
  getPersonalNumber :: a -> Text
  getMobile         :: a -> Text

-- | Get the full name (first last)
getFullName :: (HasSomeUserInfo a) => a -> Text
getFullName a = T.strip $ (T.strip $ getFirstName a) <> " " <> (T.strip $ getLastName  a)

-- | Return the first non-empty of full name, email or mobile number,
-- for use in frontend.  Not guaranteed to be unique.
getSmartName :: (HasSomeUserInfo a) => a -> Text
getSmartName a = firstNonEmpty $ [getFullName a, getEmail a, getMobile a]

-- | Uses '_notNamedParty' localized source text with `getSmartNameFromMaybe`.
getSmartNameOrPlaceholder :: (HasSomeUserInfo a, TemplatesMonad m) => a -> m Text
getSmartNameOrPlaceholder a = do
    notNamed <- renderTextTemplate_ "_notNamedParty"
    return $ firstNonEmpty [getSmartName a, notNamed]

-- | Get a MailAddress
getMailAddress :: (HasSomeUserInfo a) => a -> MailAddress
getMailAddress a = MailAddress {
    fullname = getFullName a
  , email    = getEmail a
  }
