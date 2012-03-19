{-# LANGUAGE OverloadedStrings #-}
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
import Data.List
import qualified Data.ByteString.UTF8 as BS

-- | Anything that might have a first name and last name, or personalnumber
class HasSomeUserInfo a where
  getEmail          :: a -> BS.ByteString
  getFirstName      :: a -> BS.ByteString
  getLastName       :: a -> BS.ByteString
  getPersonalNumber :: a -> BS.ByteString

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
emailFromSigLink :: SignatoryLink -> (BS.ByteString, BS.ByteString)
emailFromSigLink sl = (getFullName sl, getEmail sl)

-- | Get the full name (first last)
getFullName :: (HasSomeUserInfo a) => a -> BS.ByteString
getFullName a =
  let fn = strip $ BS.toString $ getFirstName a
      ln = strip $ BS.toString  $ getLastName  a
  in BS.fromString  $ strip $ intercalate " " [fn, ln]

-- | If the full name is empty, return the email
-- (no check if email is empty)
getSmartName :: (HasSomeUserInfo a) => a -> BS.ByteString
getSmartName a =
  let fn = strip $ BS.toString $ getFullName a
      em = strip $ BS.toString $ getEmail    a
  in if all isSpace fn
     then BS.fromString  em
     else BS.fromString  fn

-- | Get a MailAddress
getMailAddress :: (HasSomeUserInfo a) => a -> MailAddress
getMailAddress a =
  MailAddress { fullname = getFullName a
              , email    = getEmail a
              }
