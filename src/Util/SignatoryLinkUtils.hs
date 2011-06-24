{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Util.SignatoryLinkUtils
-- Author      :  Eric Normand
-- Stability   :  development
-- Portability :  portable
--
-- Utility for abstracting away finding signatory links for given information
-----------------------------------------------------------------------------
module Util.SignatoryLinkUtils where

import Doc.DocStateData
import User.UserState
import Util.HasSomeUserInfo
import Data.List

import qualified Data.ByteString as BS

class SignatoryLinkIdentity a where
  isSigLinkFor :: a -> SignatoryLink -> Bool

instance SignatoryLinkIdentity BS.ByteString where
  isSigLinkFor email sl = email == getEmail sl
  
instance SignatoryLinkIdentity Email where
  isSigLinkFor (Email email) sl = email == getEmail sl

instance SignatoryLinkIdentity UserID where
  isSigLinkFor uid sl = Just uid == maybesignatory sl
  
instance SignatoryLinkIdentity Signatory where
  isSigLinkFor (Signatory uid) sl = isSigLinkFor uid sl
  
instance (SignatoryLinkIdentity a, SignatoryLinkIdentity b) => SignatoryLinkIdentity (a, b) where
  isSigLinkFor (a, b) sl = isSigLinkFor a sl || isSigLinkFor b sl
  
instance SignatoryLinkIdentity SignatoryLinkID where
  isSigLinkFor slid sl = slid == signatorylinkid sl

instance SignatoryLinkIdentity User where
  isSigLinkFor u sl = isSigLinkFor (userid u, getEmail u) sl
  
instance SignatoryLinkIdentity Supervisor where
  isSigLinkFor (Supervisor uid) sl = Just uid == maybesupervisor sl

instance (SignatoryLinkIdentity a) => SignatoryLinkIdentity (Maybe a) where
  isSigLinkFor (Just a) sl = isSigLinkFor a sl
  isSigLinkFor Nothing  _  = False

getSigLinkFor :: (SignatoryLinkIdentity a) => Document -> a -> Maybe SignatoryLink
getSigLinkFor d a = find (isSigLinkFor a) (documentsignatorylinks d)

class SignatoryLinkQueries a where
  {- |
   Is this SignatoryLink an author?
   -}
  isAuthor :: a -> Bool
  {- |
   Is the given SignatoryLink marked as a signatory (someone who can must sign)?
   -}
  isSignatory :: a -> Bool
  
instance SignatoryLinkQueries SignatoryLink where
  isAuthor sl = SignatoryAuthor `elem` signatoryroles sl
  isSignatory sl = SignatoryPartner `elem` signatoryroles sl

instance (SignatoryLinkQueries a) => SignatoryLinkQueries (Maybe a) where
  isAuthor msl = maybe False isAuthor msl
  isSignatory msl = maybe False isSignatory msl
  
instance (SignatoryLinkIdentity a) => SignatoryLinkQueries (Document, a) where
  isAuthor (doc, a) = isAuthor (getSigLinkFor doc a)
  isSignatory (doc, a) = isSignatory (getSigLinkFor doc a)
