{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Doc.HasEmail
-- Author      :  Eric Normand
-- Stability   :  development
-- Portability :  portable
--
-- Utility for abstracting away destructuring to get an email.
-----------------------------------------------------------------------------

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS

-- | Anything that might have an email address.
class HasEmail a where
  getEmail :: a -> BS.ByteString
  
instance HasName UserDetails where
  getEmail = useremail
  
instance HasName User where
  getEmail = getEmail . userdetails

instance HasName SignatoryDetails where
  getEmail = signatoryemail
  
instance HasName SignatoryLink where
  getEmail = getEmail . signatorydetails
