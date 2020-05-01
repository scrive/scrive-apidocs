
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
  getCompanyName,
  getCompanyNumber,
  getAuthorCompanyName,
  getAuthorCompanyNumber,
  isSignatoryAndHasSigned,
  isSignatoryAndHasNotSigned,
  isApproverAndHasApproved,
  isApproverAndHasNotApproved,
  isAuthor,
  isAuthorOrAuthorsAdmin,
  isDocumentVisibleToUser,
  isSignatory,
  isApprover,
  isViewer,
  isForwarded,
  getSigLinkFor,
  hasSeen,
  hasConfirmationDelivery,
  SignatoryLinkIdentity,
  MaybeSignatoryLink(..),
  authenticationMethodsCanMix,
  authViewMatchesAuth,
  authToViewNeedsPersonalNumber,
  authToViewNeedsMobileNumber,
  authToSignNeedsPersonalNumber,
  authToSignNeedsMobileNumber
       ) where

import qualified Data.Set as S

import Doc.DocStateData
import Doc.SignatoryLinkID
import EID.Authentication.Model
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

instance SignatoryLinkIdentity Text where
  isJustSigLinkFor email sl = email == getEmail sl

instance SignatoryLinkIdentity Email where
  isJustSigLinkFor (Email email) sl = email == getEmail sl

instance SignatoryLinkIdentity UserID where
  isJustSigLinkFor uid sl = Just uid == maybesignatory sl

instance SignatoryLinkIdentity (SignatoryLink -> Bool) where
  isJustSigLinkFor p = p

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
  isJustSigLinkFor u sl = isSigLinkFor (u ^. #id) sl || isSigLinkFor (getEmail u) sl

instance (SignatoryLinkIdentity a) => SignatoryLinkIdentity (Maybe a) where
  isJustSigLinkFor (Just a) sl = isSigLinkFor a sl
  isJustSigLinkFor Nothing  _  = False

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
getAuthorSigLink = getSigLinkFor signatoryisauthor

-- | Given a Document, return the best guess at the author's name:
--     * First Name + Last Name
--     * email address if no name info
getAuthorName :: Document -> Text
getAuthorName doc = maybe "" getSmartName $ getAuthorSigLink doc

getCompanyName :: SignatoryLink -> Text
getCompanyName = getTextValueOfField CompanyFI . signatoryfields

getCompanyNumber :: SignatoryLink -> Text
getCompanyNumber = getTextValueOfField CompanyNumberFI . signatoryfields

getAuthorCompanyName :: Document -> Text
getAuthorCompanyName doc = maybe "" getCompanyName $ getAuthorSigLink doc

getAuthorCompanyNumber :: Document -> Text
getAuthorCompanyNumber doc = maybe "" getCompanyNumber $ getAuthorSigLink doc

hasSignInfoAndRoleIs :: (MaybeSignatoryLink msl) => SignatoryRole -> msl -> Bool
hasSignInfoAndRoleIs role msl = maybe
  False
  (\sl -> (isJust . maybesigninfo $ sl) && (signatoryrole sl == role))
  (getMaybeSignatoryLink msl)

hasNoSignInfoAndRoleIs :: (MaybeSignatoryLink msl) => SignatoryRole -> msl -> Bool
hasNoSignInfoAndRoleIs role msl = maybe
  False
  (\sl -> (isNothing . maybesigninfo $ sl) && (signatoryrole sl == role))
  (getMaybeSignatoryLink msl)

-- | Does the given SignatoryLink belong to a signing party and does
-- it have a SignInfo (meaning the signatory has signed)?
isSignatoryAndHasSigned :: (MaybeSignatoryLink msl) => msl -> Bool
isSignatoryAndHasSigned = hasSignInfoAndRoleIs SignatoryRoleSigningParty

-- | Does the given SignatoryLink belong to a signing party that has
-- NOT signed yet?
--
-- Note that @not . isSignatoryAndHasSigned@ is subtly different from
-- @isSignatoryAndHasNotSigned@ - the former can be (vacuously) True
-- when the signlink belongs to a viewer or an approver.
isSignatoryAndHasNotSigned :: (MaybeSignatoryLink msl) => msl -> Bool
isSignatoryAndHasNotSigned = hasNoSignInfoAndRoleIs SignatoryRoleSigningParty

-- | Does the given SignatoryLink belong to an approver and does it have a
-- SignInfo (meaning the document was approved)?
isApproverAndHasApproved :: (MaybeSignatoryLink msl) => msl -> Bool
isApproverAndHasApproved = hasSignInfoAndRoleIs SignatoryRoleApprover

-- | Does the given SignatoryLink belong to an approver that has NOT
-- approved the document yet?
isApproverAndHasNotApproved :: (MaybeSignatoryLink msl) => msl -> Bool
isApproverAndHasNotApproved = hasNoSignInfoAndRoleIs SignatoryRoleApprover

{- |
   Does the given SignatoryLink have a maybeseeninfo (is has been seen)?
 -}
hasSeen :: (MaybeSignatoryLink msl) => msl -> Bool
hasSeen msl = maybe False (isJust . maybeseeninfo) (getMaybeSignatoryLink msl)

hasConfirmationDelivery :: SignatoryLink -> Bool
hasConfirmationDelivery sl =
  signatorylinkconfirmationdeliverymethod sl /= NoConfirmationDelivery

{- |
   Is this SignatoryLink an author?
 -}
isAuthor :: (MaybeSignatoryLink msl) => msl -> Bool
isAuthor = isSigLinkFor signatoryisauthor

isAuthorOrAuthorsAdmin :: User -> Document -> Bool
isAuthorOrAuthorsAdmin user doc =
  isAuthor (doc, user)
    || (user ^. #isCompanyAdmin && documentauthorugid doc == Just (user ^. #groupID))

isDocumentVisibleToUser :: User -> Document -> Bool
isDocumentVisibleToUser user doc =
  isJust (getSigLinkFor user doc) || isAuthorOrAuthorsAdmin user doc


-- | Is the given SignatoryLink marked as a signatory (someone who must sign)?
isSignatory :: (MaybeSignatoryLink msl) => msl -> Bool
isSignatory = isSigLinkFor ((==) SignatoryRoleSigningParty . signatoryrole)

-- | Is the given SignatoryLink marked as an approver (someone who
-- must approve the document)?
isApprover :: (MaybeSignatoryLink msl) => msl -> Bool
isApprover = isSigLinkFor ((==) SignatoryRoleApprover . signatoryrole)

-- | Is the given SignatoryLink marked as a viewer (someone who can
-- review the document)?
isViewer :: (MaybeSignatoryLink msl) => msl -> Bool
isViewer = isSigLinkFor ((==) SignatoryRoleViewer . signatoryrole)

-- | Is the given SignatoryLink marked as a forwarder
isForwarded :: (MaybeSignatoryLink msl) => msl -> Bool
isForwarded = isSigLinkFor ((`elem` forwardedRoles) . signatoryrole)
  where
    forwardedRoles = [SignatoryRoleForwardedSigningParty, SignatoryRoleForwardedApprover]

-- | Does i identify sl?
isSigLinkFor :: (MaybeSignatoryLink sl, SignatoryLinkIdentity i) => i -> sl -> Bool
isSigLinkFor i sl = maybe False (isJustSigLinkFor i) (getMaybeSignatoryLink sl)


-- | Get the SignatoryLink from a document given a matching value.
getSigLinkFor :: (SignatoryLinkIdentity a) => a -> Document -> Maybe SignatoryLink
getSigLinkFor a d = find (isSigLinkFor a) (documentsignatorylinks d)

----------------------------------------

{-# ANN type MixAuthMethod ("HLint: ignore Use camelCase" :: String) #-}
-- | Internal helper type for checking whether authentication types can mix.
data MixAuthMethod
  = MAM_Standard
  | MAM_SEBankID
  | MAM_NOBankID
  | MAM_DKNemID
  | MAM_SMSPin
  | MAM_FITupas
  | MAM_Verimi
  | MAM_NLIDIN
  | MAM_Onfido
  deriving (Eq, Ord, Show)

atvToMix :: AuthenticationToViewMethod -> MixAuthMethod
atvToMix StandardAuthenticationToView = MAM_Standard
atvToMix SEBankIDAuthenticationToView = MAM_SEBankID
atvToMix NOBankIDAuthenticationToView = MAM_NOBankID
atvToMix DKNemIDAuthenticationToView  = MAM_DKNemID
atvToMix SMSPinAuthenticationToView   = MAM_SMSPin
atvToMix FITupasAuthenticationToView  = MAM_FITupas
atvToMix VerimiAuthenticationToView   = MAM_Verimi
atvToMix IDINAuthenticationToView     = MAM_NLIDIN

atsToMix :: AuthenticationToSignMethod -> MixAuthMethod
atsToMix StandardAuthenticationToSign            = MAM_Standard
atsToMix SEBankIDAuthenticationToSign            = MAM_SEBankID
atsToMix SMSPinAuthenticationToSign              = MAM_SMSPin
atsToMix NOBankIDAuthenticationToSign            = MAM_NOBankID
atsToMix DKNemIDAuthenticationToSign             = MAM_DKNemID
atsToMix IDINAuthenticationToSign                = MAM_NLIDIN
atsToMix FITupasAuthenticationToSign             = MAM_FITupas
atsToMix OnfidoDocumentCheckAuthenticationToSign = MAM_Onfido
atsToMix OnfidoDocumentAndPhotoCheckAuthenticationToSign = MAM_Onfido

authenticationMethodsCanMix
  :: AuthenticationToViewMethod
  -> AuthenticationToSignMethod
  -> AuthenticationToViewMethod
  -> Bool
-- We allow at most one EID authentication method within a signatory.
authenticationMethodsCanMix authToView authToSign authToViewArchived =
  let auths = S.fromList
        [atvToMix authToView, atsToMix authToSign, atvToMix authToViewArchived]
      eids = S.fromList
        [MAM_SEBankID, MAM_NOBankID, MAM_DKNemID, MAM_FITupas, MAM_Verimi, MAM_NLIDIN]
  in  length (auths `S.intersection` eids) <= 1

----------------------------------------

authViewMatchesAuth :: AuthenticationToViewMethod -> EAuthentication -> Bool
authViewMatchesAuth NOBankIDAuthenticationToView NetsNOBankIDAuthentication_{}     = True
authViewMatchesAuth SEBankIDAuthenticationToView CGISEBankIDAuthentication_{}      = True
authViewMatchesAuth DKNemIDAuthenticationToView  NetsDKNemIDAuthentication_{}      = True
authViewMatchesAuth FITupasAuthenticationToView  NetsFITupasAuthentication_{}      = True
authViewMatchesAuth SMSPinAuthenticationToView   SMSPinAuthentication_{}           = True
authViewMatchesAuth VerimiAuthenticationToView   EIDServiceVerimiAuthentication_{} = True
authViewMatchesAuth IDINAuthenticationToView     EIDServiceIDINAuthentication_{}   = True
authViewMatchesAuth DKNemIDAuthenticationToView  EIDServiceNemIDAuthentication_{}  = True
authViewMatchesAuth NOBankIDAuthenticationToView EIDServiceNOBankIDAuthentication_{} =
  True
authViewMatchesAuth FITupasAuthenticationToView EIDServiceFITupasAuthentication_{} = True
authViewMatchesAuth _ _ = False

-- Functions to determine if AuthenticationToViewMethod or
-- AuthenticationToSignMethod needs Personal Number or Mobile Number

authToViewNeedsPersonalNumber :: AuthenticationToViewMethod -> Bool
authToViewNeedsPersonalNumber StandardAuthenticationToView = False
authToViewNeedsPersonalNumber SEBankIDAuthenticationToView = True
authToViewNeedsPersonalNumber NOBankIDAuthenticationToView = True
authToViewNeedsPersonalNumber DKNemIDAuthenticationToView  = True
authToViewNeedsPersonalNumber SMSPinAuthenticationToView   = False
authToViewNeedsPersonalNumber FITupasAuthenticationToView  = False
authToViewNeedsPersonalNumber VerimiAuthenticationToView   = False
authToViewNeedsPersonalNumber IDINAuthenticationToView     = False

authToViewNeedsMobileNumber :: AuthenticationToViewMethod -> Bool
authToViewNeedsMobileNumber StandardAuthenticationToView = False
authToViewNeedsMobileNumber SEBankIDAuthenticationToView = False
authToViewNeedsMobileNumber NOBankIDAuthenticationToView = True
authToViewNeedsMobileNumber DKNemIDAuthenticationToView  = False
authToViewNeedsMobileNumber SMSPinAuthenticationToView   = True
authToViewNeedsMobileNumber FITupasAuthenticationToView  = False
authToViewNeedsMobileNumber VerimiAuthenticationToView   = False
authToViewNeedsMobileNumber IDINAuthenticationToView     = False

authToSignNeedsPersonalNumber :: AuthenticationToSignMethod -> Bool
authToSignNeedsPersonalNumber StandardAuthenticationToSign            = False
authToSignNeedsPersonalNumber SMSPinAuthenticationToSign              = False
authToSignNeedsPersonalNumber SEBankIDAuthenticationToSign            = True
authToSignNeedsPersonalNumber NOBankIDAuthenticationToSign            = False
authToSignNeedsPersonalNumber DKNemIDAuthenticationToSign             = True
authToSignNeedsPersonalNumber IDINAuthenticationToSign                = False
authToSignNeedsPersonalNumber FITupasAuthenticationToSign             = False
authToSignNeedsPersonalNumber OnfidoDocumentCheckAuthenticationToSign = False
authToSignNeedsPersonalNumber OnfidoDocumentAndPhotoCheckAuthenticationToSign = False

authToSignNeedsMobileNumber :: AuthenticationToSignMethod -> Bool
authToSignNeedsMobileNumber StandardAuthenticationToSign            = False
authToSignNeedsMobileNumber SMSPinAuthenticationToSign              = True
authToSignNeedsMobileNumber SEBankIDAuthenticationToSign            = False
authToSignNeedsMobileNumber NOBankIDAuthenticationToSign            = False
authToSignNeedsMobileNumber DKNemIDAuthenticationToSign             = False
authToSignNeedsMobileNumber IDINAuthenticationToSign                = False
authToSignNeedsMobileNumber FITupasAuthenticationToSign             = False
authToSignNeedsMobileNumber OnfidoDocumentCheckAuthenticationToSign = False
authToSignNeedsMobileNumber OnfidoDocumentAndPhotoCheckAuthenticationToSign = False
