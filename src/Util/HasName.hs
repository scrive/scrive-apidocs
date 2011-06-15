{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror -XOverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Util.HasName
-- Author      :  Eric Normand
-- Stability   :  development
-- Portability :  portable
--
-- Utility for abstracting away destructuring to get a name.
-----------------------------------------------------------------------------
module Util.HasName where

import Doc.DocStateData
import User.UserState
import Util.HasEmail

import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import qualified Data.ByteString as BS

-- | Get the full name (first last)
getFullName :: (HasName a) => a -> BS.ByteString
getFullName a =
  let fn = T.strip $ E.decodeUtf8 $ getFirstName a
      ln = T.strip $ E.decodeUtf8 $ getLastName  a
  in E.encodeUtf8 $ T.strip $ T.intercalate " " [fn, ln]

-- | If the full name is empty, return the email
-- (no check if email is empty)
getSmartName :: (HasName a, HasEmail a) => a -> BS.ByteString
getSmartName a =
  let fn = T.strip $ E.decodeUtf8 $ getFullName a
      em = T.strip $ E.decodeUtf8 $ getEmail    a
  in if T.all isSpace fn 
     then E.encodeUtf8 em 
     else E.encodeUtf8 fn    

-- | Anything that might have a first name and last name.
class HasName a where
  getFirstName :: a -> BS.ByteString
  getLastName  :: a -> BS.ByteString
  
instance HasName UserInfo where
  getFirstName = userfstname
  getLastName  = usersndname
  
instance HasName User where
  getFirstName = getFirstName . userinfo
  getLastName  = getLastName  . userinfo

instance HasName SignatoryDetails where
  getFirstName = signatoryfstname
  getLastName  = signatorysndname
  
instance HasName SignatoryLink where
  getFirstName = getFirstName . signatorydetails
  getLastName  = getLastName  . signatorydetails
