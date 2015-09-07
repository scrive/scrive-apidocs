module Doc.API.V2.Guards (
-- * Document guards
  guardThatDocumentIs
, guardDocumentStatus
, guardThatDocumentCanBeStarted
, guardThatObjectVersionMatchesIfProvided
-- * User guards
, guardThatUserIsAuthor
, guardThatUserIsAuthorOrCompanyAdmin
, guardThatUserIsAuthorOrDocumentIsShared
, guardThatUserIsAuthorOrCompanyAdminOrDocumentIsShared
-- * Signatory guards
, guardSignatoryNeedsToIdentifyToView
, guardSignatoryHasNotSigned
-- * Access / Session guard
, guardDocumentAccessSessionOrUser
) where

import Control.Conditional (unlessM, whenM)
import qualified Data.Text as T

import API.V2
import DB
import Doc.API.V2.DocumentAccess
import Doc.API.V2.Parameters
import Doc.Conditions
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocUtils
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.Model.Query
import Doc.SignatoryLinkID
import InputValidation
import Kontra
import KontraPrelude
import OAuth.Model (APIPrivilege)
import User.Model
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

-- | Guard a given condition on the document, throws an error with
-- `documentStateError` when this does not match.
--
-- Prefer to use a more specific guard if this satisfies your need.
guardThatDocumentIs :: (DocumentMonad m, Kontrakcja m) => (Document -> Bool) -> T.Text -> m ()
guardThatDocumentIs f text = unlessM (f <$> theDocument) $ apiError $ documentStateError text

-- | Guard that the document status matches, otherwise throw a `documentStateError`
guardDocumentStatus :: (Kontrakcja m, DocumentMonad m) => DocumentStatus -> m ()
guardDocumentStatus s = unlessM ((\d -> documentstatus d == s) <$> theDocument) $ apiError $ documentStateError errorMsg
  where errorMsg = "The document status should be '" `T.append` (T.pack $ show s) `T.append` "'."

-- | Internal function used in all guards on User
-- Helps code reuse and keep error messages consistent
guardDocumentAuthorIs :: (Kontrakcja m, DocumentMonad m) => (User -> Bool) -> m ()
guardDocumentAuthorIs condition = do
  let msgNoAuthor = "Document doesn't have author signatory link connected with user account"
  authorUserId <- apiGuardJustM (serverError msgNoAuthor) $ ((maybesignatory =<<) . getAuthorSigLink) <$> theDocument
  let msgNoUser = "Document doesn't have author user account for the author signatory link"
  author <- apiGuardJustM (serverError msgNoUser) $ dbQuery $ GetUserByIDIncludeDeleted authorUserId
  when (not $ condition author) $ do
    apiError documentActionForbidden

guardThatUserIsAuthor :: (DocumentMonad m, Kontrakcja m) => User -> m ()
guardThatUserIsAuthor user = guardDocumentAuthorIs (\a -> userid user == userid a)

guardThatUserIsAuthorOrCompanyAdmin :: (Kontrakcja m, DocumentMonad m) => User -> m ()
guardThatUserIsAuthorOrCompanyAdmin user = guardDocumentAuthorIs
  (\a -> userid user == userid a
      || (usercompany user == usercompany a && useriscompanyadmin user)
  )

guardThatUserIsAuthorOrDocumentIsShared :: (Kontrakcja m, DocumentMonad m) => User -> m ()
guardThatUserIsAuthorOrDocumentIsShared user = do
  doc <- theDocument
  guardDocumentAuthorIs (\a -> userid user == userid a
    || (usercompany user == usercompany a && isDocumentShared doc)
    )

guardThatUserIsAuthorOrCompanyAdminOrDocumentIsShared :: (Kontrakcja m, DocumentMonad m) => User -> m ()
guardThatUserIsAuthorOrCompanyAdminOrDocumentIsShared user = do
  doc <- theDocument
  guardDocumentAuthorIs (\a -> userid user == userid a
    || (usercompany user == usercompany a && (useriscompanyadmin user || isDocumentShared doc))
    )

guardThatObjectVersionMatchesIfProvided :: Kontrakcja m => DocumentID -> m ()
guardThatObjectVersionMatchesIfProvided did = do
  reqObjectVersion <- apiV2ParameterOptional (ApiV2ParameterInt "object_version")
  case reqObjectVersion of
    Nothing -> return ()
    Just ov -> (dbQuery $ CheckDocumentObjectVersionIs did (fromIntegral ov))
      `catchKontra` (\e@DocumentObjectVersionDoesNotMatch {} -> apiError $ documentObjectVersionMismatch e)

guardSignatoryNeedsToIdentifyToView :: (Kontrakcja m, DocumentMonad m) => SignatoryLinkID -> m ()
guardSignatoryNeedsToIdentifyToView slid = do
  msl <- getSigLinkFor slid <$> theDocument
  identifyToView <- signatoryNeedsToIdentifyToView ($fromJust msl)
  when (identifyToView && (not $ isAuthor msl))
    (apiError $ signatoryStateError "Authorisation to view needed before signing")

guardSignatoryHasNotSigned :: (Kontrakcja m, DocumentMonad m) => SignatoryLinkID -> m ()
guardSignatoryHasNotSigned slid =
  whenM (hasSigned . $fromJust . getSigLinkFor slid <$> theDocument) $ do
    (apiError $ signatoryStateError "The signatory has already signed")

-- Checks if document can be strated. Throws matching API exception if it does not
guardThatDocumentCanBeStarted :: (DocumentMonad m, Kontrakcja m) => m ()
guardThatDocumentCanBeStarted = do
    whenM (isTemplate <$> theDocument) $ do
       apiError $ (documentStateError "Document is a template, templates can not be started")
    unlessM (((all signatoryHasValidDeliverySettings) . documentsignatorylinks) <$> theDocument) $ do
       apiError $ documentStateError "Some signatories have invalid email address or phone number, and it is required for invitation delivery."
    unlessM (((all signatoryHasValidSSNForIdentifyToView) . documentsignatorylinks) <$> theDocument) $ do
       apiError $ documentStateError "Some signatories have invalid personal numbers: valid numbers are required for identification to view."
    whenM (isNothing . documentfile <$> theDocument) $ do
       apiError $ documentStateError "Document must have a file before it can be started"
    return ()
 where
    signatoryHasValidDeliverySettings sl = (isAuthor sl) || case (signatorylinkdeliverymethod sl) of
      EmailDelivery  ->  isGood $ asValidEmail $ getEmail sl
      MobileDelivery ->  isGood $ asValidPhoneForSMS $ getMobile sl
      EmailAndMobileDelivery -> (isGood $ asValidPhoneForSMS $ getMobile sl) && (isGood $ asValidEmail $ getEmail sl)
      _ -> True
    signatoryHasValidSSNForIdentifyToView sl = case (signatorylinkauthenticationtoviewmethod sl) of
      SEBankIDAuthenticationToView -> isGood $ asValidSwedishSSN   $ getPersonalNumber sl
      NOBankIDAuthenticationToView -> isGood $ asValidNorwegianSSN $ getPersonalNumber sl
      _ -> True

-- | For the given DocumentID:
--
-- 1. Try to get a valid session for the given `Maybe SignatoryLinkID`
--
-- if that fails or no SignatoryLinkID is given, then:
--
-- Get permissions using `getAPIUser` with given privileges.
-- If the user account is not linked to the document then also guard extra
-- permissions using the given $User -> DocumentT m ()$ function, which can be
-- any guard from this module.
--
-- This is useful in all situations where a signatory or other users could use
-- the API call (e.g. document GET call)
guardDocumentAccessSessionOrUser :: Kontrakcja m => DocumentID -> Maybe SignatoryLinkID -> APIPrivilege -> (User -> DocumentT m ()) -> m DocumentAccess
guardDocumentAccessSessionOrUser did mslid apiPermission guardOnUser = do
  mSessionSignatory <- maybe (return Nothing) (getDocumentSignatoryMagicHash did) mslid
  case mSessionSignatory of
    Just sl -> withDocumentID did (documentAccessForSlid (signatorylinkid sl) <$> theDocument)
    Nothing -> withDocumentID did $ do
      (user,_) <- getAPIUser apiPermission
      doc <- theDocument
      case getSigLinkFor user doc of
        Just _ -> return ()
        Nothing -> guardOnUser user
      return $ documentAccessForUser user doc
