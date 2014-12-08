
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
  getAuthorSigLink,
  getAuthorName,
  hasSigned,
  isAuthor,
  isAuthorOrAuthorsAdmin,
  isDocumentVisibleToUser,
  isSignatory,
  isDeletedFor,
  getSigLinkFor,
  validSigLink,
  getSignatoryPartnerLinks,
  hasSeen,
  hasUser,
  SignatoryLinkIdentity,
  MaybeSignatoryLink(..),
  HasSignatoryLinks,
  filterSigLinksFor,
  hasSigLinkFor
       ) where

import Data.Functor
import Data.List
import Data.Maybe

import Doc.DocStateData
import Doc.SignatoryLinkID
import MagicHash (MagicHash)
import User.Email
import User.Model
import Util.HasSomeUserInfo
import Utils.Prelude

-- TODO: clean up this mess. this is useless as it's so obscured by
-- typeclasses that checking for a function that interests you takes
-- ages.

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

{- |
   Identify a User based on UserID or Email (if UserID fails).
 -}
instance SignatoryLinkIdentity User where
  isJustSigLinkFor u sl = isSigLinkFor (userid u) sl || isSigLinkFor (getEmail u) sl

instance (SignatoryLinkIdentity a) => SignatoryLinkIdentity (Maybe a) where
  isJustSigLinkFor (Just a) sl = isSigLinkFor a sl
  isJustSigLinkFor Nothing  _  = False

instance SignatoryLinkIdentity MagicHash where
  isJustSigLinkFor mh sl = mh == signatorymagichash sl

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
  getMaybeSignatoryLink (d, a) = getSigLinkFor a d

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
   Get the author's signatory link.
 -}
getAuthorSigLink :: HasSignatoryLinks a => a -> Maybe SignatoryLink
getAuthorSigLink doc = getSigLinkFor signatoryisauthor doc

{- |
   Given a Document, return the best guess at the author's name:
     * First Name + Last Name
     * email address if no name info
-}
getAuthorName :: Document -> String
getAuthorName doc = maybe "" getSmartName $ getAuthorSigLink doc

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
isAuthor = isSigLinkFor signatoryisauthor

isAuthorOrAuthorsAdmin :: User -> Document -> Bool
isAuthorOrAuthorsAdmin user doc = isAuthor (doc, user) || (useriscompanyadmin user && documentauthorcompanyid doc == Just (usercompany user))

isDocumentVisibleToUser :: User -> Document -> Bool
isDocumentVisibleToUser user doc = isJust (getSigLinkFor user doc) || isAuthorOrAuthorsAdmin user doc

{- |
   Is the given SignatoryLink marked as a signatory (someone who can must sign)?
 -}
isSignatory :: (MaybeSignatoryLink msl) => msl -> Bool
isSignatory = isSigLinkFor signatoryispartner

{- |
   Is the document deleted for this signatory link?
 -}
isDeletedFor :: (MaybeSignatoryLink a) => a -> Bool
isDeletedFor msl = maybe False (isJust . signatorylinkdeleted) (getMaybeSignatoryLink msl)


{-
  Checks if siglink with magic hash is valid for this document
-}

validSigLink ::  SignatoryLinkID -> MagicHash-> Maybe Document -> Bool
validSigLink a mh (Just doc) =  joinB $ (== mh)  <$> signatorymagichash <$> (getSigLinkFor a doc)
validSigLink _ _ _ = False

{- |
   Gets the signatory links from the document that are
   signing the document, rather than just viewing.
-}
getSignatoryPartnerLinks :: HasSignatoryLinks hsl => hsl -> [SignatoryLink]
getSignatoryPartnerLinks = filterSigLinksFor signatoryispartner

{- |
  Does this siglink have a user (maybesignatory)?
 -}
hasUser :: (MaybeSignatoryLink a) => a -> Bool
hasUser msl = maybe False (isJust . maybesignatory) (getMaybeSignatoryLink msl)


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
hasSigLinkFor i sls = isJust $ getSigLinkFor i sls

{- |
  Get the SignatoryLink from a document given a matching value.
 -}
getSigLinkFor :: (SignatoryLinkIdentity a, HasSignatoryLinks b) => a -> b -> Maybe SignatoryLink
getSigLinkFor a d = find (isSigLinkFor a) (getSignatoryLinks d)
