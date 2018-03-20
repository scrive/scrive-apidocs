
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
  getSigLinkFor,
  hasSeen,
  SignatoryLinkIdentity,
  MaybeSignatoryLink(..),
  authenticationMethodsCanMix,
  authViewMatchesAuth,
  authToViewNeedsPersonalNumber,
  authToViewNeedsMobileNumber,
  authToSignNeedsPersonalNumber,
  authToSignNeedsMobileNumber
       ) where

import Doc.DocStateData
import Doc.SignatoryLinkID
import EID.Authentication.Model
import MagicHash (MagicHash)
import User.Email
import User.Model
import Util.HasSomeUserInfo

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

instance (SignatoryLinkIdentity a) => MaybeSignatoryLink (Document, a) where
  getMaybeSignatoryLink (d, a) = getSigLinkFor a d

{- |
   Get the author's signatory link.
 -}
getAuthorSigLink :: Document -> Maybe SignatoryLink
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
   Does i identify sl?
 -}
isSigLinkFor :: (MaybeSignatoryLink sl, SignatoryLinkIdentity i) => i -> sl -> Bool
isSigLinkFor i sl = maybe False (isJustSigLinkFor i) (getMaybeSignatoryLink sl)

{- |
  Get the SignatoryLink from a document given a matching value.
 -}
getSigLinkFor :: (SignatoryLinkIdentity a) => a -> Document -> Maybe SignatoryLink
getSigLinkFor a d = find (isSigLinkFor a) (documentsignatorylinks d)

authenticationMethodsCanMix :: AuthenticationToViewMethod -> AuthenticationToSignMethod -> Bool
authenticationMethodsCanMix NOBankIDAuthenticationToView SEBankIDAuthenticationToSign = False
authenticationMethodsCanMix DKNemIDAuthenticationToView  SEBankIDAuthenticationToSign = False
authenticationMethodsCanMix DKNemIDAuthenticationToView  NOBankIDAuthenticationToSign = False
authenticationMethodsCanMix SEBankIDAuthenticationToView NOBankIDAuthenticationToSign = False
authenticationMethodsCanMix _ _ = True

authViewMatchesAuth :: AuthenticationToViewMethod -> EAuthentication -> Bool
authViewMatchesAuth NOBankIDAuthenticationToView NetsNOBankIDAuthentication_{} = True
authViewMatchesAuth SEBankIDAuthenticationToView CGISEBankIDAuthentication_{}  = True
authViewMatchesAuth DKNemIDAuthenticationToView  NetsDKNemIDAuthentication_{}  = True
authViewMatchesAuth SMSPinAuthenticationToView   SMSPinAuthentication_{}  = True
authViewMatchesAuth _ _ = False

-- Functions to determine if AuthenticationToViewMethod or
-- AuthenticationToSignMethod needs Personal Number or Mobile Number

authToViewNeedsPersonalNumber :: AuthenticationToViewMethod -> Bool
authToViewNeedsPersonalNumber StandardAuthenticationToView = False
authToViewNeedsPersonalNumber SEBankIDAuthenticationToView = True
authToViewNeedsPersonalNumber NOBankIDAuthenticationToView = True
authToViewNeedsPersonalNumber DKNemIDAuthenticationToView  = True
authToViewNeedsPersonalNumber SMSPinAuthenticationToView = False

authToViewNeedsMobileNumber :: AuthenticationToViewMethod -> Bool
authToViewNeedsMobileNumber StandardAuthenticationToView = False
authToViewNeedsMobileNumber SEBankIDAuthenticationToView = False
authToViewNeedsMobileNumber NOBankIDAuthenticationToView = True
authToViewNeedsMobileNumber DKNemIDAuthenticationToView  = False
authToViewNeedsMobileNumber SMSPinAuthenticationToView = True

authToSignNeedsPersonalNumber :: AuthenticationToSignMethod -> Bool
authToSignNeedsPersonalNumber StandardAuthenticationToSign = False
authToSignNeedsPersonalNumber SMSPinAuthenticationToSign   = False
authToSignNeedsPersonalNumber SEBankIDAuthenticationToSign = True
authToSignNeedsPersonalNumber NOBankIDAuthenticationToSign = False

authToSignNeedsMobileNumber :: AuthenticationToSignMethod -> Bool
authToSignNeedsMobileNumber StandardAuthenticationToSign = False
authToSignNeedsMobileNumber SMSPinAuthenticationToSign   = True
authToSignNeedsMobileNumber SEBankIDAuthenticationToSign = False
authToSignNeedsMobileNumber NOBankIDAuthenticationToSign = False
