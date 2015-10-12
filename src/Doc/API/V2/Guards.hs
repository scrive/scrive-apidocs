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
, guardSignatoryHasNotIdentifiedToView
, guardCanSetAuthenticationToViewForSignatoryWithValues
, guardCanSetAuthenticationToSignForSignatoryWithValue
, guardSignatoryHasNotSigned
-- * Joined guard for read-only functions
, guardDocumentReadAccess
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
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.DocUtils
import Doc.Model.Query
import Doc.SignatoryLinkID
import InputValidation
import Kontra
import KontraPrelude
import OAuth.Model (APIPrivilege(..))
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
  where errorMsg = "The document status should be '" <> (T.pack $ show s) <> "'."

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
    (apiError $ signatoryStateError "Authorization to view needed before signing")

guardSignatoryHasNotIdentifiedToView :: (Kontrakcja m, DocumentMonad m) => SignatoryLinkID -> m ()
guardSignatoryHasNotIdentifiedToView slid =
  whenM (signatorylinkidentifiedtoview . $fromJust . getSigLinkFor slid <$> theDocument)
    (apiError $ signatoryStateError "The signatory has already identified to view")

guardCanSetAuthenticationToViewForSignatoryWithValues :: (Kontrakcja m, DocumentMonad m) => SignatoryLinkID -> AuthenticationToViewMethod -> Maybe String -> Maybe String -> m ()
guardCanSetAuthenticationToViewForSignatoryWithValues slid authToView mSSN mPhone = do
  sl <- $fromJust . getSigLinkFor slid <$> theDocument
  -- Do not allow mixing of Swedish and Norwegian BankID
  when (authToView == NOBankIDAuthenticationToView && signatorylinkauthenticationtosignmethod sl == SEBankIDAuthenticationToSign)
    (apiError $ signatoryStateError "Can't mix Norwegian and Swedish BankID for the same signatory")
  -- Check if either a valid SSN for authToView is set or is provided
  case mSSN of
    Nothing -> unless (isValidSSNForAuthenticationToView authToView $ getPersonalNumber sl) $
      (apiError $ signatoryStateError "Signatory does not have a valid personal number for the authentication method and you did not provide one")
    Just ssn -> unless (isValidSSNForAuthenticationToView authToView ssn) $
      (apiError $ signatoryStateError "The personal number you provided is not valid for the authentication method")
  -- Check if either a valid phone for authToView is set or is provided
  case mPhone of
    Nothing -> unless (isValidPhoneForAuthenticationToView authToView $ getMobile sl) $
      (apiError $ signatoryStateError "Signatory does not have a valid phone number set for the authentication method and you did not provide one")
    Just phone -> unless (isValidPhoneForAuthenticationToView authToView phone) $
      (apiError $ signatoryStateError "The phone number you provided is not valid for the authentication method")
  where
    isValidSSNForAuthenticationToView :: AuthenticationToViewMethod -> String -> Bool
    isValidSSNForAuthenticationToView StandardAuthenticationToView _ = True
    isValidSSNForAuthenticationToView SEBankIDAuthenticationToView ssn = isGood $ asValidSwedishSSN   ssn
    isValidSSNForAuthenticationToView NOBankIDAuthenticationToView ssn = isGood $ asValidNorwegianSSN ssn
    isValidPhoneForAuthenticationToView :: AuthenticationToViewMethod -> String -> Bool
    isValidPhoneForAuthenticationToView StandardAuthenticationToView _ = True
    isValidPhoneForAuthenticationToView SEBankIDAuthenticationToView _ = True
    isValidPhoneForAuthenticationToView NOBankIDAuthenticationToView phone = isGood phoneValidation || isEmpty phoneValidation
      where phoneValidation = asValidPhoneForNorwegianBankID phone

guardCanSetAuthenticationToSignForSignatoryWithValue :: (Kontrakcja m, DocumentMonad m) => SignatoryLinkID -> AuthenticationToSignMethod -> Maybe String -> Maybe String -> m ()
guardCanSetAuthenticationToSignForSignatoryWithValue slid authToSign mSSN mPhone = do
  sl <- $fromJust . getSigLinkFor slid <$> theDocument
  case authToSign of
    StandardAuthenticationToSign -> return ()
    SEBankIDAuthenticationToSign -> do
      -- Do not allow mixing of Swedish and Norwegian BankID
      when (signatorylinkauthenticationtoviewmethod sl == NOBankIDAuthenticationToView) $
        apiError $ signatoryStateError "Can't mix Norwegian and Swedish BankID for the same signatory"
      case mSSN of
        Nothing -> return ()
        -- If we are given a Swedish SSN
        Just ssn -> do
          when (signatorylinkidentifiedtoview sl && ssn /= getPersonalNumber sl) $
            apiError $ signatoryStateError "The signatory has authenticated to view, therefore you can't change the authentication value"
          case asValidSwedishSSN ssn of
            -- Empty is allowed only if we don't need it for AuthenticationToViewMethod
            Empty -> when (signatorylinkauthenticationtoviewmethod sl == SEBankIDAuthenticationToView) $
              apiError $ signatoryStateError "You provided an empty authentication value, needs a value for authentication to view"
            Bad -> apiError $ signatoryStateError "The authentication value provided is not a valid for Swedish BankID"
            Good _ -> return ()
    SMSPinAuthenticationToSign -> case mPhone of
      Nothing -> return ()
      Just phone -> do
        -- If the signatory has authenticated to view with NOBankIDAuthenticationToView and a valid number, then we can't change the phone number!
        when (signatorylinkauthenticationtoviewmethod sl == NOBankIDAuthenticationToView && signatorylinkidentifiedtoview sl && getMobile sl /= "" && phone /= getMobile sl) $
          apiError $ signatoryStateError "The signatory has authenticated to view with Norwegian BankID, therefore you can't change the phone number"
        -- If given a phone number we need to make sure it doesn't invalidate NOBankIDAuthenticationToView
        when (signatorylinkauthenticationtoviewmethod sl == NOBankIDAuthenticationToView) $
          case asValidPhoneForNorwegianBankID phone of
            Bad -> apiError $ signatoryStateError "Phone number needs to be a valid Norwegian number as Norwegian BankID is set as authentication to view"
            Empty -> return ()
            Good _ -> return ()

guardSignatoryHasNotSigned :: (Kontrakcja m, DocumentMonad m) => SignatoryLinkID -> m ()
guardSignatoryHasNotSigned slid =
  whenM (hasSigned . $fromJust . getSigLinkFor slid <$> theDocument)
    (apiError $ signatoryStateError "The signatory has already signed")

-- Checks if document can be strated. Throws matching API exception if it does not
guardThatDocumentCanBeStarted :: (DocumentMonad m, Kontrakcja m) => m ()
guardThatDocumentCanBeStarted = do
    whenM (isTemplate <$> theDocument) $ do
       apiError $ (documentStateError "Document is a template, templates can not be started")
    unlessM (((all signatoryHasValidDeliverySettings) . documentsignatorylinks) <$> theDocument) $ do
       apiError $ documentStateError "Some signatories have invalid email address or phone number, their invitation 'delivery_method' requires it to be valid and not empty."
    unlessM (((all signatoryHasValidSSNForIdentifyToView) . documentsignatorylinks) <$> theDocument) $ do
       apiError $ documentStateError "Some signatories have invalid personal numbers, their 'authentication_to_view' requires it to be valid and not empty."
    unlessM (((all signatoryHasValidAuthSettings) . documentsignatorylinks) <$> theDocument) $ do
       apiError $ documentStateError "Some signatories have invalid personal numbers, their 'authentication_to_sign' requires it to be valid or empty."
    unlessM (((all signatoryHasValidPhoneForIdentifyToView) . documentsignatorylinks) <$> theDocument) $ do
       apiError $ documentStateError "Some signatories have invalid phone number and it is required for identification to view document."
    whenM (isNothing . documentfile <$> theDocument) $ do
       apiError $ documentStateError "Document must have a file before it can be started"
    return ()
 where
    signatoryHasValidDeliverySettings sl = (isAuthor sl) || case (signatorylinkdeliverymethod sl) of
      EmailDelivery  ->  isGood $ asValidEmail $ getEmail sl
      MobileDelivery ->  isGood $ asValidPhoneForSMS $ getMobile sl
      EmailAndMobileDelivery -> (isGood $ asValidPhoneForSMS $ getMobile sl) && (isGood $ asValidEmail $ getEmail sl)
      _ -> True
    signatoryHasValidAuthSettings sl = authToSignIsValid sl
    authToSignIsValid sl = getPersonalNumber sl == "" || case signatorylinkauthenticationtosignmethod sl of
      SEBankIDAuthenticationToSign -> isGood $ asValidSEBankIdPersonalNumber $ getPersonalNumber sl
      _ -> True
    signatoryHasValidSSNForIdentifyToView sl = case (signatorylinkauthenticationtoviewmethod sl) of
      SEBankIDAuthenticationToView -> isGood $ asValidSwedishSSN   $ getPersonalNumber sl
      NOBankIDAuthenticationToView -> isGood $ asValidNorwegianSSN $ getPersonalNumber sl
      _ -> True
    signatoryHasValidPhoneForIdentifyToView sl =
      let resultValidPhone = asValidPhoneForNorwegianBankID $ getMobile sl in
      if (signatorylinkauthenticationtoviewmethod sl == NOBankIDAuthenticationToView)
         then isGood resultValidPhone || isEmpty resultValidPhone
         else True

-- | For the given DocumentID:
--
-- 1. Try to get a valid session for the given `Maybe SignatoryLinkID`
--
-- if that fails or no SignatoryLinkID is given, then:
--
-- Get permissions using `getAPIUser` with given privileges.
-- If the user account is not linked to the document then also guard extra
-- permissions using guardThatUserIsAuthorOrCompanyAdminOrDocumentIsShared
--
-- This is useful in all situations where a signatory or other users could use
-- the API call (e.g. document GET call), but not that this function is focused only
-- on ability to read document

guardDocumentReadAccess :: Kontrakcja m => DocumentID -> Maybe SignatoryLinkID -> m DocumentAccess
guardDocumentReadAccess did mslid   = do
  mSessionSignatory <- maybe (return Nothing) (getDocumentSignatoryMagicHash did) mslid
  case mSessionSignatory of
    Just sl -> withDocumentID did (documentAccessForSlid (signatorylinkid sl) <$> theDocument)
    Nothing -> withDocumentID did $ do
      (user,_) <- getAPIUser APIDocCheck
      doc <- theDocument
      case getSigLinkFor user doc of
        Just _ -> return ()
        Nothing -> guardThatUserIsAuthorOrCompanyAdminOrDocumentIsShared user
      return $ documentAccessForUser user doc
