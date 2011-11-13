
-----------------------------------------------------------------------------
-- |
-- Module      :  Util.SignatoryLinkUtils
-- Author      :  Eric Normand
-- Stability   :  development
-- Portability :  portable
--
-- Utility for abstracting away finding signatory links for given information
-----------------------------------------------------------------------------
module Util.SignatoryLinkUtils (

  isSigLinkFor,
  isSigLinkSavedFor,
  isAuthorSignatory,
  getAuthorSigLink,
  getAuthorName,
  isUndelivered,
  isDeferred,
  hasSigned,
  isAuthor,
  isSignatory,
  isViewer,
  isDeletedFor,
  getSigLinkFor,
  validSigLink,
  getSignatoryPartnerLinks,
  hasSeen,  
  hasUser,
  hasCompany,
  SignatoryLinkIdentity,
  MaybeSignatoryLink(..),
       ) where

import Company.Model
import DB.Types
import Doc.DocStateData
import Mails.MailsUtil
import User.Model
import Util.HasSomeUserInfo
import Data.Semantic
import Misc

import Data.List
import Data.Maybe
import Data.Functor

import qualified Data.ByteString as BS

{- |
   Anything that could identify a SignatoryLink
 -}
class SignatoryLinkIdentity a where
  isJustSigLinkFor :: a -> SignatoryLink -> Bool

instance SignatoryLinkIdentity BS.ByteString where
  isJustSigLinkFor email sl = email == getEmail sl

instance SignatoryLinkIdentity Email where
  isJustSigLinkFor (Email email) sl = email == getEmail sl

instance SignatoryLinkIdentity UserID where
  isJustSigLinkFor uid sl = Just uid == maybesignatory sl

instance SignatoryLinkIdentity Signatory where
  isJustSigLinkFor (Signatory uid) sl = isSigLinkFor uid sl

instance SignatoryLinkIdentity (SignatoryLink -> Bool) where
  isJustSigLinkFor p sl = p sl
  
--instance MaybeSignatoryLink a => SignatoryLinkIdentity (a -> Bool) where
--  isJustSigLinkFor p sl = p sl

{- |
   Identify a SignatoryLink based on a; if that does not match, identify based on b.
 -}
instance (SignatoryLinkIdentity a, SignatoryLinkIdentity b) => SignatoryLinkIdentity (Or a b) where
  isJustSigLinkFor (Or a b) sl = isSigLinkFor a sl || isSigLinkFor b sl
  
instance (SignatoryLinkIdentity a, SignatoryLinkIdentity b) => SignatoryLinkIdentity (And a b) where
  isJustSigLinkFor (And a b) sl = isSigLinkFor a sl && isSigLinkFor b sl

instance SignatoryLinkIdentity SignatoryLinkID where
  isJustSigLinkFor slid sl = slid == signatorylinkid sl
  
instance SignatoryLinkIdentity SignatoryRole where
  isJustSigLinkFor role sl = role `elem` signatoryroles sl

{- |
   Identify a User based on UserID or Email (if UserID fails).
 -}
instance SignatoryLinkIdentity User where
  isJustSigLinkFor u sl = isSigLinkFor (Or (userid u) (getEmail u)) sl

instance SignatoryLinkIdentity Author where
  isJustSigLinkFor (Author uid) sl = isSigLinkFor (And SignatoryAuthor uid) sl

instance SignatoryLinkIdentity CompanyID where
  isJustSigLinkFor cid sl = Just cid == maybecompany sl

instance (SignatoryLinkIdentity a) => SignatoryLinkIdentity (Maybe a) where
  isJustSigLinkFor (Just a) sl = isSigLinkFor a sl
  isJustSigLinkFor Nothing  _  = False
  
instance SignatoryLinkIdentity MagicHash where
  isJustSigLinkFor mh sl = mh == signatorymagichash sl

{- |
   Anything that could resolve to a SignatoryLink.
 -}
class MaybeSignatoryLink a where
  getMaybeSignatoryLink :: a -> Maybe SignatoryLink

instance MaybeSignatoryLink SignatoryLink where
  getMaybeSignatoryLink = Just

{- |
   Nothing always resolves to Nothing.
 -}
instance (MaybeSignatoryLink msl) => MaybeSignatoryLink (Maybe msl) where
  getMaybeSignatoryLink (Just sl) = getMaybeSignatoryLink sl
  getMaybeSignatoryLink Nothing   = Nothing

instance (SignatoryLinkIdentity a) => MaybeSignatoryLink (Document, a) where
  getMaybeSignatoryLink (d, a) = getSigLinkFor d a

{- |
   Is the Author of this Document a signatory (not a Secretary)?
 -}
isAuthorSignatory :: Document -> Bool
isAuthorSignatory document =
  case getAuthorSigLink document of
    Just siglink -> isSignatory siglink
    _ -> False

{- |
   Get the author's signatory link.
 -}
getAuthorSigLink :: Document -> Maybe SignatoryLink
getAuthorSigLink doc = getSigLinkFor doc SignatoryAuthor

{- |
   Given a Document, return the best guess at the author's name:
     * First Name + Last Name
     * email address if no name info
-}
getAuthorName :: Document -> BS.ByteString
getAuthorName doc =
  let Just authorsiglink = getAuthorSigLink doc
  in getSmartName authorsiglink

{- |
   Is this SignatoryLink undelivered?
 -}
isUndelivered :: (MaybeSignatoryLink msl) => msl -> Bool
isUndelivered msl = maybe False ((==) Undelivered . invitationdeliverystatus) (getMaybeSignatoryLink msl)

{- |
   Is this SignatoryLink Deferred?
 -}
isDeferred :: (MaybeSignatoryLink msl) => msl -> Bool
isDeferred msl = maybe False ((==) Delivered . invitationdeliverystatus) (getMaybeSignatoryLink msl)

{- |
   Does the given SignatoryLink have SignInfo (meaning the signatory has signed)?
 -}
hasSigned :: (MaybeSignatoryLink msl) => msl -> Bool
hasSigned msl = maybe False (isJust . maybesigninfo) (getMaybeSignatoryLink msl)

{- |
   Does the given SignatoryLink have a maybeseeninfo (is has been seen)?
 -}
hasSeen :: (MaybeSignatoryLink msl) => msl -> Bool
hasSeen msl = maybe False (isJust . maybeseeninfo) (getMaybeSignatoryLink msl)

{- |
   Is this SignatoryLink an author?
 -}
isAuthor :: (MaybeSignatoryLink msl) => msl -> Bool
isAuthor msl = maybe False (elem SignatoryAuthor . signatoryroles) (getMaybeSignatoryLink msl)

{- |
   Is the given SignatoryLink marked as a signatory (someone who can must sign)?
 -}
isSignatory :: (MaybeSignatoryLink msl) => msl -> Bool
isSignatory = isSigLinkFor SignatoryPartner

{- |
   Is the user able to view the doc?
 -}
isViewer :: (MaybeSignatoryLink msl) => msl -> Bool
isViewer msl = isJust (getMaybeSignatoryLink msl)

{- |
   Is the document deleted for this signatory link?
 -}
isDeletedFor :: (MaybeSignatoryLink a) => a -> Bool
isDeletedFor msl = maybe False signatorylinkdeleted (getMaybeSignatoryLink msl)

{- |
  Get the SignatoryLink from a document given a matching value.
 -}
getSigLinkFor :: (SignatoryLinkIdentity a) => Document -> a -> Maybe SignatoryLink
getSigLinkFor d a = find (isSigLinkFor a) (documentsignatorylinks d)

{- 
  Checks if siglink with magic hash is valid for this document
-}

validSigLink ::  SignatoryLinkID -> MagicHash-> Maybe Document -> Bool
validSigLink a mh (Just doc) =  joinB $ (== mh)  <$> signatorymagichash <$> (getSigLinkFor doc a)
validSigLink _ _ _ = False

{- |
   Gets the signatory links from the document that are
   signing the document, rather than just viewing.
-}
getSignatoryPartnerLinks :: Document -> [SignatoryLink] 
getSignatoryPartnerLinks doc = filter isSignatory $ documentsignatorylinks doc

{- |
  Does this siglink have a user (maybesignatory)?
 -}
hasUser :: (MaybeSignatoryLink a) => a -> Bool
hasUser msl = maybe False (isJust . maybesignatory) (getMaybeSignatoryLink msl)

{- |
  Does this siglink have a company?
 -}
hasCompany :: (MaybeSignatoryLink a) => a -> Bool
hasCompany msl = maybe False (isJust . maybecompany) (getMaybeSignatoryLink msl)

{- |
    This is counts a user as being for the signatory link
    if the userid is mentioned on the sig link, or if the user
    is an admin for a company that is mentioned on the sig link.
    The idea of this function is to be used when we need to be a bit
    more strict that just isSigLinkFor, as in particular isSigLinkFor
    would link by email.
-}
isSigLinkSavedFor :: User -> SignatoryLink -> Bool
isSigLinkSavedFor User{userid, useriscompanyadmin, usercompany} sl =
  isSigLinkFor userid sl || 
    (useriscompanyadmin && isSigLinkFor usercompany sl)

{- |
   Does i identify sl?
 -}
isSigLinkFor :: (MaybeSignatoryLink sl, SignatoryLinkIdentity i) => i -> sl -> Bool
isSigLinkFor i sl = maybe False (isJustSigLinkFor i) (getMaybeSignatoryLink sl)
