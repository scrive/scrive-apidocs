
-----------------------------------------------------------------------------
-- |
-- Module      :  Util.SignatoryLinkUtils
-- Author      :  Eric Normand
-- Stability   :  development
-- Portability :  portable
--
-- Utility for abstracting away finding signatory links for given information
--
-- It forms a kind of DSL for querying SignatoryLinks out of documents
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
  HasSignatoryLinks,
  filterSigLinksFor,
  hasSigLinkFor
       ) where

import Company.Model
import Doc.DocStateData
import MagicHash (MagicHash)
import User.Model
import Util.HasSomeUserInfo
import Utils.Prelude

import Data.List
import Data.Maybe
import Data.Functor

{- |
   Anything that could identify a SignatoryLink
 -}
class SignatoryLinkIdentity a where
  isJustSigLinkFor :: a -> SignatoryLink -> Bool

instance SignatoryLinkIdentity String where
  isJustSigLinkFor email sl = email == getEmail sl

instance SignatoryLinkIdentity Email where
  isJustSigLinkFor (Email email) sl = email == getEmail sl

instance SignatoryLinkIdentity UserID where
  isJustSigLinkFor uid sl = Just uid == maybesignatory sl

instance SignatoryLinkIdentity (SignatoryLink -> Bool) where
  isJustSigLinkFor p sl = p sl
  
--instance MaybeSignatoryLink a => SignatoryLinkIdentity (a -> Bool) where
--  isJustSigLinkFor p sl = p sl

{- |
   Identify a SignatoryLink based on a; if that does not match, identify based on b.
 -}

instance SignatoryLinkIdentity SignatoryLinkID where
  isJustSigLinkFor slid sl = slid == signatorylinkid sl
  
instance SignatoryLinkIdentity SignatoryRole where
  isJustSigLinkFor role sl = role `elem` signatoryroles sl

{- |
   Identify a User based on UserID or Email (if UserID fails).
 -}
instance SignatoryLinkIdentity User where
  isJustSigLinkFor u sl = isSigLinkFor (userid u) sl || isSigLinkFor (getEmail u) sl

instance SignatoryLinkIdentity CompanyID where
  isJustSigLinkFor cid sl = Just cid == maybecompany sl

instance (SignatoryLinkIdentity a) => SignatoryLinkIdentity (Maybe a) where
  isJustSigLinkFor (Just a) sl = isSigLinkFor a sl
  isJustSigLinkFor Nothing  _  = False
  
instance SignatoryLinkIdentity MagicHash where
  isJustSigLinkFor mh sl = mh == signatorymagichash sl

instance SignatoryLinkIdentity MailsDeliveryStatus where
  isJustSigLinkFor mds sl = mds == invitationdeliverystatus sl

instance (SignatoryLinkIdentity a, SignatoryLinkIdentity b) => SignatoryLinkIdentity (a, b) where
  isJustSigLinkFor (a, b) sl = isJustSigLinkFor a sl && isJustSigLinkFor b sl

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

instance (SignatoryLinkIdentity a, HasSignatoryLinks b) => MaybeSignatoryLink (b, a) where
  getMaybeSignatoryLink (d, a) = getSigLinkFor d a

{- |
  A source of signatory links
 -}
class HasSignatoryLinks a where
  getSignatoryLinks :: a -> [SignatoryLink]
  
instance HasSignatoryLinks Document where
  getSignatoryLinks = documentsignatorylinks
  
instance HasSignatoryLinks [SignatoryLink] where
  getSignatoryLinks = id
  
instance HasSignatoryLinks SignatoryLink where
  getSignatoryLinks sl = [sl]
  
instance (HasSignatoryLinks a) => HasSignatoryLinks (Maybe a) where
  getSignatoryLinks = maybe [] getSignatoryLinks
  
instance (HasSignatoryLinks a, SignatoryLinkIdentity b) => HasSignatoryLinks (a, b) where
  getSignatoryLinks (sls, i) = filter (isSigLinkFor i) (getSignatoryLinks sls)

{- |
   Is the Author of this Document a signatory (not a Secretary)?
 -}
isAuthorSignatory :: HasSignatoryLinks a => a -> Bool
isAuthorSignatory hsl = hasSigLinkFor (SignatoryAuthor, SignatoryPartner) hsl

{- |
   Get the author's signatory link.
 -}
getAuthorSigLink :: HasSignatoryLinks a => a -> Maybe SignatoryLink
getAuthorSigLink doc = getSigLinkFor doc SignatoryAuthor

{- |
   Given a Document, return the best guess at the author's name:
     * First Name + Last Name
     * email address if no name info
-}
getAuthorName :: Document -> String
getAuthorName doc = maybe "" getSmartName $ getAuthorSigLink doc


{- |
   Is this SignatoryLink undelivered?
 -}
isUndelivered :: (MaybeSignatoryLink msl) => msl -> Bool
isUndelivered = isSigLinkFor Undelivered

{- |
   Is this SignatoryLink Deferred?
 -}
isDeferred :: (MaybeSignatoryLink msl) => msl -> Bool
isDeferred = isSigLinkFor Delivered

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
isAuthor = isSigLinkFor SignatoryAuthor

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
getSignatoryPartnerLinks :: HasSignatoryLinks hsl => hsl -> [SignatoryLink] 
getSignatoryPartnerLinks doc = filterSigLinksFor SignatoryPartner doc

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

{- |
   Filters the signatory links based on the query
 -}
filterSigLinksFor :: (SignatoryLinkIdentity i, HasSignatoryLinks sls) => i -> sls -> [SignatoryLink]
filterSigLinksFor i sls = filter (isSigLinkFor i) (getSignatoryLinks sls)

{- |
   True if one or more signatory links matches the query
 -}
hasSigLinkFor :: (SignatoryLinkIdentity i, HasSignatoryLinks sls) => i -> sls -> Bool
hasSigLinkFor i sls = isJust $ getSigLinkFor sls i

{- |
  Get the SignatoryLink from a document given a matching value.
 -}
getSigLinkFor :: (SignatoryLinkIdentity a, HasSignatoryLinks b) => b -> a -> Maybe SignatoryLink
getSigLinkFor d a = find (isSigLinkFor a) (getSignatoryLinks d)
