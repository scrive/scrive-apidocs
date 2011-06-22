{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror -XOverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Util.HasSomeUserInfo
-- Author      :  Eric Normand
-- Stability   :  development
-- Portability :  portable
--
-- Utility for abstracting away destructuring to get a name.
-----------------------------------------------------------------------------
module Util.HasSomeUserInfo ( 
  getCompanyName,
  getCompanyNumber,
  getEmail,
  getFirstName,
  getFullName,
  getLastName,
  getPersonalNumber,
  getSmartName
  
  ) where

import Doc.DocStateData
import User.UserState

import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import qualified Data.ByteString as BS

-- | Get the full name (first last)
getFullName :: (HasSomeUserInfo a) => a -> BS.ByteString
getFullName a =
  let fn = T.strip $ E.decodeUtf8 $ getFirstName a
      ln = T.strip $ E.decodeUtf8 $ getLastName  a
  in E.encodeUtf8 $ T.strip $ T.intercalate " " [fn, ln]

-- | If the full name is empty, return the email
-- (no check if email is empty)
getSmartName :: (HasSomeUserInfo a) => a -> BS.ByteString
getSmartName a =
  let fn = T.strip $ E.decodeUtf8 $ getFullName a
      em = T.strip $ E.decodeUtf8 $ getEmail    a
  in if T.all isSpace fn 
     then E.encodeUtf8 em 
     else E.encodeUtf8 fn    

-- | Anything that might have a first name and last name.
class HasSomeUserInfo a where
  getCompanyName    :: a -> BS.ByteString
  getCompanyNumber  :: a -> BS.ByteString
  getEmail          :: a -> BS.ByteString
  getFirstName      :: a -> BS.ByteString
  getLastName       :: a -> BS.ByteString
  getPersonalNumber :: a -> BS.ByteString
  
instance HasSomeUserInfo UserInfo where
  getCompanyName    = usercompanyname
  getCompanyNumber  = usercompanynumber
  getEmail          = unEmail . useremail
  getFirstName      = userfstname
  getLastName       = usersndname
  getPersonalNumber = userpersonalnumber
  
instance HasSomeUserInfo User where
  getCompanyName    = usercompanyname     . userinfo
  getCompanyNumber  = usercompanynumber   . userinfo
  getEmail          = unEmail . useremail . userinfo
  getFirstName      = userfstname         . userinfo
  getLastName       = usersndname         . userinfo
  getPersonalNumber = userpersonalnumber  . userinfo

instance HasSomeUserInfo SignatoryDetails where
  getCompanyName    = signatorycompany
  getCompanyNumber  = signatorycompanynumber
  getEmail          = signatoryemail
  getFirstName      = signatoryfstname
  getLastName       = signatorysndname
  getPersonalNumber = signatorypersonalnumber
  
instance HasSomeUserInfo SignatoryLink where
  getCompanyName    = signatorycompany        . signatorydetails
  getCompanyNumber  = signatorycompanynumber  . signatorydetails
  getEmail          = signatoryemail          . signatorydetails
  getFirstName      = signatoryfstname        . signatorydetails
  getLastName       = signatorysndname        . signatorydetails
  getPersonalNumber = signatorypersonalnumber . signatorydetails
