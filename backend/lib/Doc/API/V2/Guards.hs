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
, guardThatAllAttachmentsAreAcceptedOrIsAuthor
-- * Joined guard for read-only functions
, guardDocumentReadAccess
) where

import qualified Data.Text as T

import API.V2
import API.V2.Parameters
import DB
import Doc.API.V2.DocumentAccess
import Doc.Conditions
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocumentID
import Doc.DocUtils
import Doc.Model.Query
import Doc.SignatoryLinkID
import File.FileID
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
guardThatDocumentIs :: Kontrakcja m => (Document -> Bool) -> T.Text -> Document -> m ()
guardThatDocumentIs f text doc = unless (f doc) $ apiError $ documentStateError text

-- | Guard that the document status matches, otherwise throw a `documentStateError`
guardDocumentStatus :: Kontrakcja m => DocumentStatus -> Document -> m ()
guardDocumentStatus s doc = unless (documentstatus doc == s) $ apiError $ documentStateError errorMsg
  where errorMsg = "The document status should be '" <> (T.pack $ show s) <> "'."

-- | Internal function used in all guards on User
-- Helps code reuse and keep error messages consistent
guardDocumentAuthorIs :: Kontrakcja m => (User -> Bool) -> Document -> m ()
guardDocumentAuthorIs condition doc = do
  let msgNoAuthor = "Document doesn't have author signatory link connected with user account"
  authorUserId <- apiGuardJust (serverError msgNoAuthor) $ (getAuthorSigLink doc >>= maybesignatory)
  let msgNoUser = "Document doesn't have author user account for the author signatory link"
  author <- apiGuardJustM (serverError msgNoUser) $ dbQuery $ GetUserByIDIncludeDeleted authorUserId
  when (not $ condition author) $ do
    apiError documentActionForbidden

guardThatUserIsAuthor :: Kontrakcja m => User -> Document -> m ()
guardThatUserIsAuthor user = guardDocumentAuthorIs (\a -> userid user == userid a)

guardThatUserIsAuthorOrCompanyAdmin :: Kontrakcja m => User -> Document -> m ()
guardThatUserIsAuthorOrCompanyAdmin user = guardDocumentAuthorIs
  (\a -> userid user == userid a
      || (usercompany user == usercompany a && useriscompanyadmin user)
  )

guardThatUserIsAuthorOrDocumentIsShared :: Kontrakcja m => User -> Document -> m ()
guardThatUserIsAuthorOrDocumentIsShared user doc = do
  guardDocumentAuthorIs (\a -> userid user == userid a
    || (usercompany user == usercompany a && isDocumentShared doc)
    ) doc

guardThatUserIsAuthorOrCompanyAdminOrDocumentIsShared :: Kontrakcja m => User -> Document -> m ()
guardThatUserIsAuthorOrCompanyAdminOrDocumentIsShared user doc = do
  guardDocumentAuthorIs (\a -> userid user == userid a
    || (usercompany user == usercompany a && (useriscompanyadmin user || isDocumentShared doc))
    ) doc

guardThatObjectVersionMatchesIfProvided :: Kontrakcja m => DocumentID -> m ()
guardThatObjectVersionMatchesIfProvided did = do
  reqObjectVersion <- apiV2ParameterOptional (ApiV2ParameterInt "object_version")
  case reqObjectVersion of
    Nothing -> return ()
    Just ov -> (dbQuery $ CheckDocumentObjectVersionIs did (fromIntegral ov))
      `catchDBExtraException` (\e@DocumentObjectVersionDoesNotMatch {} -> apiError $ documentObjectVersionMismatch e)

guardSignatoryNeedsToIdentifyToView :: Kontrakcja m => SignatoryLinkID -> Document -> m ()
guardSignatoryNeedsToIdentifyToView slid doc = do
  let msl = getSigLinkFor slid doc
  identifyToView <- signatoryNeedsToIdentifyToView (fromJust msl)
  when (identifyToView && (not $ isAuthor msl))
    (apiError $ signatoryStateError "Authorization to view needed before signing")

guardSignatoryHasNotIdentifiedToView :: Kontrakcja m => SignatoryLinkID -> Document -> m ()
guardSignatoryHasNotIdentifiedToView slid doc =
  when (signatorylinkidentifiedtoview . fromJust $ getSigLinkFor slid doc)
    (apiError $ signatoryStateError "The party has already identified to view")

guardCanSetAuthenticationToViewForSignatoryWithValues :: Kontrakcja m => SignatoryLinkID -> AuthenticationToViewMethod -> Maybe String -> Maybe String -> Document -> m ()
guardCanSetAuthenticationToViewForSignatoryWithValues slid authToView mSSN mMobile doc = do
  let sl = fromJust $ getSigLinkFor slid doc
  -- Do not allow mixing of Swedish and Norwegian BankID
  when (authToView == NOBankIDAuthenticationToView && signatorylinkauthenticationtosignmethod sl == SEBankIDAuthenticationToSign)
    (apiError $ signatoryStateError "Can't mix Norwegian and Swedish BankID for the same party")
  -- Check if either a valid SSN for authToView is set or is provided
  case mSSN of
    Nothing -> unless (isValidSSNForAuthenticationToView authToView $ getPersonalNumber sl) $
      (apiError $ signatoryStateError "Signatory does not have a valid personal number for the authentication method and you did not provide one")
    Just ssn -> unless (isValidSSNForAuthenticationToView authToView ssn) $
      (apiError $ signatoryStateError "The personal number you provided is not valid for the authentication method")
  -- Check if either a valid mobile for authToView is set or is provided
  case mMobile of
    Nothing -> unless (isValidMobileForAuthenticationToView authToView $ getMobile sl) $
      (apiError $ signatoryStateError "Party does not have a valid mobile number set for the authentication method and you did not provide one")
    Just mobile -> unless (isValidMobileForAuthenticationToView authToView mobile) $
      (apiError $ signatoryStateError "The mobile number you provided is not valid for the authentication method")
  where
    isValidSSNForAuthenticationToView :: AuthenticationToViewMethod -> String -> Bool
    isValidSSNForAuthenticationToView StandardAuthenticationToView _ = True
    isValidSSNForAuthenticationToView SEBankIDAuthenticationToView ssn = isGood $ asValidSwedishSSN   ssn
    isValidSSNForAuthenticationToView NOBankIDAuthenticationToView ssn = isGood $ asValidNorwegianSSN ssn
    isValidMobileForAuthenticationToView :: AuthenticationToViewMethod -> String -> Bool
    isValidMobileForAuthenticationToView StandardAuthenticationToView _ = True
    isValidMobileForAuthenticationToView SEBankIDAuthenticationToView _ = True
    isValidMobileForAuthenticationToView NOBankIDAuthenticationToView mobile = isGood phoneValidation || isEmpty phoneValidation
      where phoneValidation = asValidPhoneForNorwegianBankID mobile

guardCanSetAuthenticationToSignForSignatoryWithValue :: Kontrakcja m => SignatoryLinkID -> AuthenticationToSignMethod -> Maybe String -> Maybe String -> Document -> m ()
guardCanSetAuthenticationToSignForSignatoryWithValue slid authToSign mSSN mMobile doc = do
  let sl = fromJust $ getSigLinkFor slid doc
  case authToSign of
    StandardAuthenticationToSign -> return ()
    SEBankIDAuthenticationToSign -> do
      -- Do not allow mixing of Swedish and Norwegian BankID
      when (signatorylinkauthenticationtoviewmethod sl == NOBankIDAuthenticationToView) $
        apiError $ signatoryStateError "Can't mix Norwegian and Swedish BankID for the same party"
      case mSSN of
        Nothing -> return ()
        -- If we are given a Swedish SSN
        Just ssn -> do
          when (signatorylinkidentifiedtoview sl && ssn /= getPersonalNumber sl) $
            apiError $ signatoryStateError "The party has authenticated to view, therefore you can't change the authentication value"
          case asValidSwedishSSN ssn of
            -- Empty is allowed only if we don't need it for AuthenticationToViewMethod
            Empty -> when (signatorylinkauthenticationtoviewmethod sl == SEBankIDAuthenticationToView) $
              apiError $ signatoryStateError "You provided an empty authentication value, needs a value for authentication to view"
            Bad -> apiError $ signatoryStateError "The authentication value provided is not a valid for Swedish BankID"
            Good _ -> return ()
    SMSPinAuthenticationToSign -> case mMobile of
      Nothing -> return ()
      Just mobile -> do
        -- If the signatory has authenticated to view with NOBankIDAuthenticationToView and a valid number, then we can't change the mobile number!
        when (signatorylinkauthenticationtoviewmethod sl == NOBankIDAuthenticationToView && signatorylinkidentifiedtoview sl && getMobile sl /= "" && mobile /= getMobile sl) $
          apiError $ signatoryStateError "The party has authenticated to view with Norwegian BankID, therefore you can't change the mobile number"
        -- If given a mobile number we need to make sure it doesn't invalidate NOBankIDAuthenticationToView
        when (signatorylinkauthenticationtoviewmethod sl == NOBankIDAuthenticationToView) $
          case asValidPhoneForNorwegianBankID mobile of
            Bad -> apiError $ signatoryStateError "Mobile number needs to be a valid Norwegian number as Norwegian BankID is set as authentication to view"
            Empty -> return ()
            Good _ -> return ()

guardSignatoryHasNotSigned :: Kontrakcja m => SignatoryLinkID -> Document -> m ()
guardSignatoryHasNotSigned slid doc =
  when (hasSigned . fromJust $ getSigLinkFor slid doc)
    (apiError $ signatoryStateError "The signatory has already signed")

-- Checks if document can be strated. Throws matching API exception if it does not
guardThatDocumentCanBeStarted :: Kontrakcja m => Document -> m ()
guardThatDocumentCanBeStarted doc = do
    when (isTemplate doc) $ do
       apiError $ (documentStateError "Document is a template, templates can not be started")
    unless (all signatoryHasValidDeliverySettings $ documentsignatorylinks doc) $ do
       apiError $ documentStateError "Some parties have an invalid email address or mobile number, their invitation 'delivery_method' requires it to be valid and not empty."
    unless (all signatoryHasValidConfirmationSettings $ documentsignatorylinks doc) $ do
       apiError $ documentStateError "Some parties have an invalid email address or mobile number, their 'confirmation_delivery_method' requires it to be valid or empty."
    unless (all signatoryHasValidSSNForIdentifyToView $ documentsignatorylinks doc) $ do
       apiError $ documentStateError "Some parties have an invalid personal numbers, their 'authentication_to_view' requires it to be valid and not empty."
    unless (all signatoryHasValidAuthSettings $ documentsignatorylinks doc) $ do
       apiError $ documentStateError "Some parties have an invalid personal numbers, their 'authentication_to_sign' requires it to be valid or empty."
    unless (all signatoryHasValidMobileForIdentifyToView $ documentsignatorylinks doc) $ do
       apiError $ documentStateError "Some parties have an invalid mobile number and it is required for identification to view document."
    when (isNothing $ documentfile doc) $ do
       apiError $ documentStateError "Document must have a file before it can be started"
    return ()
 where
    signatoryHasValidDeliverySettings sl = (isAuthor sl) || case (signatorylinkdeliverymethod sl) of
      EmailDelivery  ->  isGood $ asValidEmail $ getEmail sl
      MobileDelivery ->  isGood $ asValidPhoneForSMS $ getMobile sl
      EmailAndMobileDelivery -> (isGood $ asValidPhoneForSMS $ getMobile sl) && (isGood $ asValidEmail $ getEmail sl)
      _ -> True
    signatoryHasValidConfirmationSettings sl = isAuthor sl || case signatorylinkconfirmationdeliverymethod sl of
      EmailConfirmationDelivery -> null (getEmail sl) || isGood (asValidEmail $ getEmail sl)
      MobileConfirmationDelivery -> null (getMobile sl) || isGood (asValidPhoneForSMS $ getMobile sl)
      EmailAndMobileConfirmationDelivery -> (null (getEmail sl) || isGood (asValidEmail $ getEmail sl)) && (null (getMobile sl) || isGood (asValidPhoneForSMS $ getMobile sl))
      NoConfirmationDelivery -> True
    signatoryHasValidAuthSettings sl = authToSignIsValid sl
    authToSignIsValid sl = null (getPersonalNumber sl) || case signatorylinkauthenticationtosignmethod sl of
      SEBankIDAuthenticationToSign -> isGood $ asValidSEBankIdPersonalNumber $ getPersonalNumber sl
      _ -> True
    signatoryHasValidSSNForIdentifyToView sl = case (signatorylinkauthenticationtoviewmethod sl) of
      SEBankIDAuthenticationToView -> isGood $ asValidSwedishSSN   $ getPersonalNumber sl
      NOBankIDAuthenticationToView -> isGood $ asValidNorwegianSSN $ getPersonalNumber sl
      _ -> True
    signatoryHasValidMobileForIdentifyToView sl =
      let resultValidPhone = asValidPhoneForNorwegianBankID $ getMobile sl in
      if (signatorylinkauthenticationtoviewmethod sl == NOBankIDAuthenticationToView)
         then isGood resultValidPhone || isEmpty resultValidPhone
         else True

guardThatAllAttachmentsAreAcceptedOrIsAuthor :: Kontrakcja m => SignatoryLinkID -> [FileID] -> Document -> m ()
guardThatAllAttachmentsAreAcceptedOrIsAuthor slid acceptedAttachments doc = do
  unless (allRequiredAttachmentsAreOnList acceptedAttachments doc) $
    unless (isAuthor $ fromJust $ getSigLinkFor slid doc) $ -- Author does not need to accept attachments
      apiError $ (signatoryStateError "Some mandatory author attachments aren't accepted")

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

guardDocumentReadAccess :: Kontrakcja m => Maybe SignatoryLinkID -> Document -> m DocumentAccess
guardDocumentReadAccess mslid doc = do
  mSessionSignatory <- maybe (return Nothing) (getDocumentSignatoryMagicHash $ documentid doc) mslid
  case mSessionSignatory of
    Just sl -> return $ documentAccessForSlid (signatorylinkid sl) doc
    Nothing -> do
      (user,_) <- getAPIUser APIDocCheck
      case getSigLinkFor user doc of
        Just _ -> return ()
        Nothing -> guardThatUserIsAuthorOrCompanyAdminOrDocumentIsShared user doc
      return $ documentAccessForUser user doc
