{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror -XOverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Util.HasEmail
-- Author      :  Eric Normand
-- Stability   :  development
-- Portability :  portable
--
-- Utility for abstracting away destructuring to get an email.
-----------------------------------------------------------------------------
module Util.HasEmail where

import User.UserState
import Doc.DocStateData

import qualified Data.ByteString as BS

-- | Anything that might have an email address.
class HasEmail a where
  getEmail :: a -> BS.ByteString
  
instance HasEmail UserInfo where
  getEmail = unEmail . useremail
  
instance HasEmail User where
  getEmail = getEmail . userinfo

instance HasEmail SignatoryDetails where
  getEmail = signatoryemail
  
instance HasEmail SignatoryLink where
  getEmail = getEmail . signatorydetails
