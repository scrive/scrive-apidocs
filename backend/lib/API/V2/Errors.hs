{-# LANGUAGE FunctionalDependencies, ExtendedDefaultRules #-}
module API.V2.Errors (
    serverError
  , requestFailed
  , requestParameterMissing
  , requestParameterParseError
  , requestParameterInvalid
  , documentObjectVersionMismatch
  , documentStateError
  , documentStateErrorWithCode
  , signatoryStateError
  , signatoryLinkForDocumentNotFound
  , documentActionForbidden
  , documentNotFound
  , resourceNotFound
  -- * Internal to API.V2
  , APIError(..)
  , APIErrorType(..)
  , endpointNotFound
  , invalidAuthorization
  , invalidAuthorizationWithMsg
  , insufficientPrivileges
  , actionNotPermitted
  , httpCodeFromSomeDBExtraException
  , jsonFromSomeDBExtraException
  , tryToConvertConditionalExceptionIntoAPIError
) where

import Data.Typeable
import Text.JSON
import Text.JSON.Gen hiding (object)
import qualified Data.Text as T

import DB
import Doc.Conditions
import Doc.DocStateData
import Doc.DocumentID
import Doc.SignatoryLinkID

data APIError = APIError {
      errorType     :: APIErrorType
    , errorHttpCode :: Int
    , errorMessage  :: T.Text
  }
  deriving (Show, Eq, Typeable)

instance ToJSValue APIError where
  toJSValue a = runJSONGen $ do
    value "error_type" (T.unpack $ errorIDFromAPIErrorType $ errorType a)
    value "error_message" (T.unpack $ errorMessage a)
    value "http_code" (errorHttpCode $ a)

instance DBExtraException APIError

data APIErrorType = ServerError
               | RequestFailed
               | EndpointNotFound
               | InvalidAuthorization
               | InsufficientPrivileges
               | ResourceNotFound
               | DocumentActionForbidden
               | RequestParametersMissing
               | RequestParametersParseError
               | RequestParametersInvalid
               | DocumentObjectVersionMismatch
               | DocumentStateError
               | SignatoryStateError
               | ActionNotPermitted
  deriving (Show, Eq, Typeable)

errorIDFromAPIErrorType :: APIErrorType -> T.Text
errorIDFromAPIErrorType ServerError                   = "server_error"
errorIDFromAPIErrorType RequestFailed                 = "request_failed"
errorIDFromAPIErrorType EndpointNotFound              = "endpoint_not_found"
errorIDFromAPIErrorType InvalidAuthorization          = "invalid_authorisation"
errorIDFromAPIErrorType InsufficientPrivileges        = "insufficient_privileges"
errorIDFromAPIErrorType ResourceNotFound              = "resource_not_found"
errorIDFromAPIErrorType DocumentActionForbidden       = "document_action_forbidden"
errorIDFromAPIErrorType RequestParametersMissing      = "request_parameters_missing"
errorIDFromAPIErrorType RequestParametersParseError   = "request_parameters_parse_error"
errorIDFromAPIErrorType RequestParametersInvalid      = "request_parameters_invalid"
errorIDFromAPIErrorType DocumentObjectVersionMismatch = "document_object_version_mismatch"
errorIDFromAPIErrorType DocumentStateError            = "document_state_error"
errorIDFromAPIErrorType SignatoryStateError           = "signatory_state_error"
errorIDFromAPIErrorType ActionNotPermitted            = "action_not_permitted"

jsonFromSomeDBExtraException :: SomeDBExtraException -> JSValue
jsonFromSomeDBExtraException (SomeDBExtraException ex)  = toJSValue ex

httpCodeFromSomeDBExtraException :: SomeDBExtraException -> Int
httpCodeFromSomeDBExtraException (SomeDBExtraException ex) =
  case cast ex of
    Just (apierror :: APIError) -> errorHttpCode apierror
    Nothing -> 500

-- General errors that have to be handled, but are generally not expected
serverError :: T.Text -> APIError
serverError reason = APIError { errorType = ServerError, errorHttpCode = 500, errorMessage = msg}
  where msg = "We encountered an unexpected error. Please contact Scrive"
              <+> "support and include as much details about what caused"
              <+> "the error, including the document id or any other details."
              <+> "Error details:" <+> reason

-- General errors that have to be handled and are expected
requestFailed :: T.Text -> APIError
requestFailed reason = APIError { errorType = RequestFailed, errorHttpCode = 400, errorMessage = msg}
  where msg = "Request failed. Additional information:" <+> reason

-- | Used internally by API.V2 for reporting bad API endpoints
endpointNotFound :: T.Text -> APIError
endpointNotFound ep = APIError { errorType = EndpointNotFound, errorHttpCode = 404, errorMessage = msg}
  where msg = "The endpoint" <+> ep <+> "was not found. See our website for API documentation"

-- | Used interally by this module and API.V2.User
invalidAuthorization :: APIError
invalidAuthorization = APIError { errorType = InvalidAuthorization, errorHttpCode = 401, errorMessage = msg}
  where msg = "No valid access credentials were provided. Please refer to our API documentation"

-- | Used interally by this module and API.V2.User
invalidAuthorizationWithMsg :: T.Text -> APIError
invalidAuthorizationWithMsg problem = invalidAuthorization { errorMessage = msg}
  where msg = errorMessage invalidAuthorization <+>  "The problem was:" <+> problem

-- | Used interally by this module and API.V2.User
insufficientPrivileges :: APIError
insufficientPrivileges = APIError { errorType = InsufficientPrivileges, errorHttpCode = 403, errorMessage = msg}
  where msg = "The access credentials provided do not have sufficient privileges for this request"

-- Request specific errors
requestParameterMissing :: T.Text -> APIError
requestParameterMissing param = APIError { errorType = RequestParametersMissing, errorHttpCode = 400, errorMessage = msg}
  where msg = "The parameter '"<> param <> "' was missing. Please refer to our API documentation"

requestParameterParseError :: T.Text -> T.Text -> APIError
requestParameterParseError param err = APIError { errorType = RequestParametersParseError, errorHttpCode = 400, errorMessage = msg}
  where msg = "The parameter '" <> param <> "' could not be parsed."
            <+> "Please refer to our API documentation. Error details:"
            <+> err

requestParameterInvalid :: T.Text -> T.Text -> APIError
requestParameterInvalid param reason = APIError { errorType = RequestParametersInvalid, errorHttpCode = 400, errorMessage = msg}
  where msg = "The parameter '" <> param <> "' had the following problems:" <+> reason

-- Document calls errors

documentObjectVersionMismatch :: DocumentObjectVersionDoesNotMatch -> APIError
documentObjectVersionMismatch (DocumentObjectVersionDoesNotMatch {..}) = APIError { errorType = DocumentObjectVersionMismatch, errorHttpCode = 409, errorMessage = msg}
  where msg = "The document has a different object_version to the one provided and so the request was not processed."
              <+> "You gave" <+> (T.pack $ show documentObjectVersionShouldBe)
              <+> "but the document had" <+> (T.pack $ show documentObjectVersionIs)

documentStateError :: T.Text -> APIError
documentStateError msg = APIError { errorType = DocumentStateError, errorHttpCode = 409, errorMessage = msg}

documentStateErrorWithCode :: Int -> T.Text -> APIError
documentStateErrorWithCode code msg = (documentStateError msg) {errorHttpCode = code}

signatoryStateError :: T.Text -> APIError
signatoryStateError msg = APIError { errorType = SignatoryStateError, errorHttpCode = 409, errorMessage = msg}

signatoryLinkForDocumentNotFound :: DocumentID -> SignatoryLinkID -> APIError
signatoryLinkForDocumentNotFound did slid =
    resourceNotFound $ "A signatory with id" <+> slidText <+> "was not found for document id" <+> didText
  where didText  = T.pack (show did)
        slidText = T.pack (show slid)

documentActionForbidden :: APIError
documentActionForbidden = APIError { errorType = DocumentActionForbidden, errorHttpCode = 403, errorMessage = msg}
  where msg = "You do not have permission to perform this action on the document"

documentNotFound :: DocumentID -> APIError
documentNotFound did = resourceNotFound $ "A document with id" <+> didText <+> "was not found"
  where didText = T.pack (show did)

documentShortIDNotFound :: DocumentID -> APIError
documentShortIDNotFound sdid = resourceNotFound $ "A document matching short id" <+> sdidText <+> "was not found"
  where sdidText = T.pack (show sdid)

resourceNotFound :: T.Text -> APIError
resourceNotFound info = APIError { errorType = ResourceNotFound, errorHttpCode = 404, errorMessage = msg}
  where msg = "The resource was not found." <+>  info

actionNotPermitted :: T.Text -> APIError
actionNotPermitted msg = APIError
  { errorType     = ActionNotPermitted
  , errorHttpCode = 403
  , errorMessage  = "Action not permitted." <+> msg
  }

-- Conversion of DB exception / document conditionals into API errors

tryToConvertConditionalExceptionIntoAPIError :: SomeDBExtraException -> SomeDBExtraException
tryToConvertConditionalExceptionIntoAPIError  =  foldr (.) id [
      convertDocumentDoesNotExist
    , convertShortDocumentIDHasNoMatch
    , convertDocumentTypeShouldBe
    , convertDocumentStatusShouldBe
    , convertUserShouldBeSelfOrCompanyAdmin
    , convertUserShouldBeDirectlyOrIndirectlyRelatedToDocument
    , convertSignatoryLinkDoesNotExist
    , convertSignatoryHasNotYetSigned
    , convertSignatoryIsNotPartner
    , convertSignatoryHasAlreadySigned
    , convertSignatoryTokenDoesNotMatch
    , convertDocumentObjectVersionDoesNotMatch
    , convertDocumentWasPurged
    , convertDocumentIsDeleted
    , convertDocumentIsNotDeleted
    , convertDocumentIsReallyDeleted
    , convertSignatoryAuthenticationToSignDoesNotMatch
  ]


convertDocumentDoesNotExist :: SomeDBExtraException -> SomeDBExtraException
convertDocumentDoesNotExist (SomeDBExtraException ex) =
  case cast ex of
    Just (DocumentDoesNotExist did) ->  SomeDBExtraException $ documentNotFound did
    Nothing -> (SomeDBExtraException ex)

convertShortDocumentIDHasNoMatch :: SomeDBExtraException -> SomeDBExtraException
convertShortDocumentIDHasNoMatch (SomeDBExtraException ex) =
  case cast ex of
    Just (ShortDocumentIDHasNoMatch sdid) -> SomeDBExtraException $ documentShortIDNotFound sdid
    Nothing -> (SomeDBExtraException ex)

convertDocumentTypeShouldBe :: SomeDBExtraException -> SomeDBExtraException
convertDocumentTypeShouldBe (SomeDBExtraException ex) =
  case cast ex of
    Just (DocumentTypeShouldBe  {documentTypeShouldBe = Template}) ->  SomeDBExtraException $ documentStateError $ "Document is not a template"
    Just (DocumentTypeShouldBe  {documentTypeShouldBe = Signable}) ->  SomeDBExtraException $ documentStateError $ "Document is a template"
    Nothing -> (SomeDBExtraException ex)

convertDocumentStatusShouldBe :: SomeDBExtraException -> SomeDBExtraException
convertDocumentStatusShouldBe (SomeDBExtraException ex) =
  case cast ex of
    Just (DocumentStatusShouldBe{}) ->  SomeDBExtraException $ documentStateError $ "Invalid document state"
    Nothing -> (SomeDBExtraException ex)

convertUserShouldBeSelfOrCompanyAdmin :: SomeDBExtraException -> SomeDBExtraException
convertUserShouldBeSelfOrCompanyAdmin (SomeDBExtraException ex) =
  case cast ex of
    Just (UserShouldBeSelfOrCompanyAdmin{}) ->  SomeDBExtraException $ insufficientPrivileges
    Nothing -> (SomeDBExtraException ex)

convertUserShouldBeDirectlyOrIndirectlyRelatedToDocument :: SomeDBExtraException -> SomeDBExtraException
convertUserShouldBeDirectlyOrIndirectlyRelatedToDocument (SomeDBExtraException ex) =
  case cast ex of
    Just (UserShouldBeDirectlyOrIndirectlyRelatedToDocument {}) ->  SomeDBExtraException $ insufficientPrivileges
    Nothing -> (SomeDBExtraException ex)

convertSignatoryLinkDoesNotExist :: SomeDBExtraException -> SomeDBExtraException
convertSignatoryLinkDoesNotExist (SomeDBExtraException ex) =
  case cast ex of
    Just (SignatoryLinkDoesNotExist sig) ->  SomeDBExtraException $ signatoryStateError $ "Signatory"  <+> T.pack (show sig) <+> "does not exists"
    Nothing -> (SomeDBExtraException ex)

convertSignatoryHasNotYetSigned :: SomeDBExtraException -> SomeDBExtraException
convertSignatoryHasNotYetSigned (SomeDBExtraException ex) =
  case cast ex of
    Just (SignatoryHasNotYetSigned {}) ->  SomeDBExtraException $ signatoryStateError $ "Signatory has not signed yet"
    Nothing -> (SomeDBExtraException ex)

convertSignatoryIsNotPartner :: SomeDBExtraException -> SomeDBExtraException
convertSignatoryIsNotPartner (SomeDBExtraException ex) =
  case cast ex of
    Just (SignatoryIsNotPartner {}) ->  SomeDBExtraException $ signatoryStateError $ "Signatory should not sign this document"
    Nothing -> (SomeDBExtraException ex)

convertSignatoryHasAlreadySigned :: SomeDBExtraException -> SomeDBExtraException
convertSignatoryHasAlreadySigned (SomeDBExtraException ex) =
  case cast ex of
    Just (SignatoryHasAlreadySigned {}) ->  SomeDBExtraException $ signatoryStateError $ "Signatory already signed"
    Nothing -> (SomeDBExtraException ex)

convertSignatoryTokenDoesNotMatch :: SomeDBExtraException -> SomeDBExtraException
convertSignatoryTokenDoesNotMatch (SomeDBExtraException ex) =
  case cast ex of
    Just (SignatoryTokenDoesNotMatch {}) -> SomeDBExtraException $ invalidAuthorizationWithMsg "Signatory token does not match"
    Nothing -> (SomeDBExtraException ex)

convertDocumentObjectVersionDoesNotMatch :: SomeDBExtraException -> SomeDBExtraException
convertDocumentObjectVersionDoesNotMatch (SomeDBExtraException ex) =
  case cast ex of
    Just (e@DocumentObjectVersionDoesNotMatch {}) -> SomeDBExtraException $ documentObjectVersionMismatch e
    Nothing -> (SomeDBExtraException ex)

convertDocumentWasPurged ::  SomeDBExtraException -> SomeDBExtraException
convertDocumentWasPurged (SomeDBExtraException ex) =
  case cast ex of
    Just (DocumentWasPurged {}) -> SomeDBExtraException $ documentStateError $ "Document was purged"
    Nothing -> (SomeDBExtraException ex)

convertDocumentIsDeleted ::  SomeDBExtraException -> SomeDBExtraException
convertDocumentIsDeleted (SomeDBExtraException ex) =
  case cast ex of
    Just (DocumentIsDeleted {}) -> SomeDBExtraException $ documentStateError $ "The document is in Trash"
    Nothing -> (SomeDBExtraException ex)

convertDocumentIsNotDeleted ::  SomeDBExtraException -> SomeDBExtraException
convertDocumentIsNotDeleted (SomeDBExtraException ex) =
  case cast ex of
    Just (DocumentIsNotDeleted {}) -> SomeDBExtraException $ documentStateError $ "The document is not in Trash"
    Nothing -> (SomeDBExtraException ex)

convertDocumentIsReallyDeleted ::  SomeDBExtraException -> SomeDBExtraException
convertDocumentIsReallyDeleted (SomeDBExtraException ex) =
  case cast ex of
    Just (DocumentIsReallyDeleted {}) -> SomeDBExtraException $ documentStateError $ "The document is deleted. It is not avaialbe and will be purged soon"
    Nothing -> (SomeDBExtraException ex)

convertSignatoryAuthenticationToSignDoesNotMatch ::  SomeDBExtraException -> SomeDBExtraException
convertSignatoryAuthenticationToSignDoesNotMatch (SomeDBExtraException ex) =
  case cast ex of
    Just (SignatoryAuthenticationToSignDoesNotMatch {}) -> SomeDBExtraException $ signatoryStateError $ "Invalid authorization for signatory"
    Nothing -> (SomeDBExtraException ex)
