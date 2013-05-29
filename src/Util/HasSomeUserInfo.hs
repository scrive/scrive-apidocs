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
  HasSomeUserInfo(..)
  ) where


import Doc.DocStateData
import User.Model
import Mails.MailsData
import Data.Char
import Data.String.Utils


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

instance HasSomeUserInfo SignatoryDetails where
  getEmail          = strip . getValueOfType EmailFT
  getFirstName      = getValueOfType FirstNameFT
  getLastName       = getValueOfType LastNameFT
  getPersonalNumber = getValueOfType PersonalNumberFT
  getMobile         = getValueOfType MobileFT

instance HasSomeUserInfo SignatoryLink where
  getEmail          = strip . getEmail          . signatorydetails
  getFirstName      = getFirstName      . signatorydetails
  getLastName       = getLastName       . signatorydetails
  getPersonalNumber = getPersonalNumber . signatorydetails
  getMobile         = getMobile         . signatorydetails

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


-- | If the full name is empty, return the email
-- (no check if email is empty)
getSmartName :: (HasSomeUserInfo a) => a -> String
getSmartName a = if (all isSpace (getFullName a))
                    then strip $ getEmail a
                    else strip $ getFullName a

-- | Get a MailAddress
getMailAddress :: (HasSomeUserInfo a) => a -> MailAddress
getMailAddress a = MailAddress {
    fullname = getFullName a
  , email    = getEmail a
  }
