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
  getEmail,
  getMailAddress,
  getFirstName,
  getFullName,
  getLastName,
  getPersonalNumber,
  getSmartName,
  HasSomeUserInfo
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

instance HasSomeUserInfo UserInfo where
  getEmail          = unEmail . useremail
  getFirstName      = userfstname
  getLastName       = usersndname
  getPersonalNumber = userpersonalnumber

instance HasSomeUserInfo User where
  getEmail          = unEmail . useremail . userinfo
  getFirstName      = userfstname         . userinfo
  getLastName       = usersndname         . userinfo
  getPersonalNumber = userpersonalnumber  . userinfo

instance HasSomeUserInfo SignatoryDetails where
  getEmail          = getValueOfType EmailFT
  getFirstName      = getValueOfType FirstNameFT
  getLastName       = getValueOfType LastNameFT
  getPersonalNumber = getValueOfType PersonalNumberFT

instance HasSomeUserInfo SignatoryLink where
  getEmail          = getEmail          . signatorydetails
  getFirstName      = getFirstName      . signatorydetails
  getLastName       = getLastName       . signatorydetails
  getPersonalNumber = getPersonalNumber . signatorydetails

{- |
   Given a SignatoryLink, returns a tuple containing the name and the email address.

   Useful for sending emails.
   Refactor note: change this to getNameEmailPair, move to Util.HasSomeUserInfo
 -}
emailFromSigLink :: SignatoryLink -> (String, String)
emailFromSigLink sl = (getFullName sl, getEmail sl)

-- | Get the full name (first last)
getFullName :: (HasSomeUserInfo a) => a -> String
getFullName a = (strip $ getFirstName a) ++ " " ++ (strip $ getLastName  a)


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
