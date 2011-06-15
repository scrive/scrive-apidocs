{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Doc.HasName
-- Author      :  Eric Normand
-- Stability   :  development
-- Portability :  portable
--
-- Utility for abstracting away destructuring to get a name.
-----------------------------------------------------------------------------

import Util.HasEmail

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS

-- | Get the full name (first last)
getFullName :: (HasName a) => a -> BS.ByteString
getFullName a = getFirstName a `BS.concat` BS.fromString " " `BS.concat` getLastName a

getSmartName :: (HasName a, HasEmail a) => a -> BS.ByteString
getSmartName a =
  let fn = BS.toString . getFullName a
      em = getEmail a
  in if all isSpace fn then em else BS.fromString fn    

-- | Anything that might have a first name and last name.
class HasName a where
  getFirstName :: a -> BS.ByteString
  getLastName  :: a -> BS.ByteString
  
instance HasName UserDetails where
  getFirstName = userfstname
  getLastName  = usersndname
  
instance HasName User where
  getFirstName = getFirstName . userdetails
  getLastName  = getLastName  . userdetails

instance HasName SignatoryDetails where
  getFirstName = signatoryfstname
  getLastName  = signatorysndname
  
instance HasName SignatoryLink where
  getFirstName = getFirstName . signatorydetails
  getLastName  = getLastName  . signatorydetails
