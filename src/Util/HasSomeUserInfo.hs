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
  emailFromSigLink,
  getMailAddress,
  getFullName,
  getSmartName,
  getSmartNameOrPlaceholder,
  HasSomeUserInfo(..)
  ) where


import Data.String.Utils
import Text.StringTemplates.Templates

import Doc.DocStateData
import KontraPrelude
import Mails.MailsData
import User.Email
import User.Model
import Utils.String

-- | Anything that might have a first name and last name, or personalnumber
class HasSomeUserInfo a where
  getEmail          :: a -> String
  getFirstName      :: a -> String
  getLastName       :: a -> String
  getPersonalNumber :: a -> String
  getMobile         :: a -> String

instance HasSomeUserInfo UserInfo where
  getEmail          = strip . unEmail . useremail
  getFirstName      = userfstname
  getLastName       = usersndname
  getPersonalNumber = userpersonalnumber
  getMobile         = userphone

instance HasSomeUserInfo User where
  getEmail          = strip . unEmail . useremail . userinfo
  getFirstName      = userfstname         . userinfo
  getLastName       = usersndname         . userinfo
  getPersonalNumber = userpersonalnumber  . userinfo
  getMobile         = userphone          . userinfo


instance HasSomeUserInfo SignatoryLink where
  getEmail          = getEmail . signatoryfields
  getFirstName      = getFirstName . signatoryfields
  getLastName       = getLastName . signatoryfields
  getPersonalNumber = getPersonalNumber . signatoryfields
  getMobile         = getMobile . signatoryfields

instance HasSomeUserInfo [SignatoryField] where
  getEmail          = strip . getTextValueOfField EmailFI
  getFirstName      = getTextValueOfField $ NameFI (NameOrder 1)
  getLastName       = getTextValueOfField $ NameFI (NameOrder 2)
  getPersonalNumber = getTextValueOfField PersonalNumberFI
  getMobile         = getTextValueOfField MobileFI


{- |
   Given a SignatoryLink, returns a tuple containing the name and the email address.

   Useful for sending emails.
   Refactor note: change this to getNameEmailPair, move to Util.HasSomeUserInfo
 -}
emailFromSigLink :: SignatoryLink -> (String, String)
emailFromSigLink sl = (getFullName sl, getEmail sl)

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
