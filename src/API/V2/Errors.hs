{-# LANGUAGE FunctionalDependencies, ExtendedDefaultRules #-}
module API.V2.Errors (
    serverError
  , requestParameterMissing
  , requestParameterParseError
  , requestParameterInvalid
  , documentObjectVersionMismatch
  , documentStateError
  , documentStateErrorWithCode
  , signatoryStateError
  , documentActionForbidden
  , documentNotFound
  , resourceNotFound
  -- * Internal to API.V2
  , APIError
  , endpointNotFound
  , invalidAuthorisation
  , invalidAuthorisationWithMsg
  , insufficientPrivileges
  , httpCodeFromSomeKontraException
  , jsonFromSomeKontraException
  , tryToConvertConditionalExpectionIntoAPIError
) where

import Data.Text
import Data.Typeable
import Text.JSON
import Text.JSON.Gen hiding (object)

import DB
import Doc.Conditions
import Doc.DocStateData
import Doc.DocumentID
import KontraPrelude

data APIError = APIError {
      errorType     :: APIErrorType
    , errorHttpCode :: Int
    , errorMessage  :: Text
  }
  deriving (Show, Eq, Typeable)

instance ToJSValue APIError where
  toJSValue a = runJSONGen $ do
    value "error_type" (unpack $ errorIDFromAPIErrorType $ errorType a)
    value "error_message" (unpack $ errorMessage a)
    value "http_code" (errorHttpCode $ a)

instance KontraException APIError

data APIErrorType = ServerError
               | EndpointNotFound
               | InvalidAuthorisation
               | InsufficientPrivileges
               | ResourceNotFound
               | DocumentActionForbidden
               | RequestParametersMissing
               | RequestParametersParseError
               | RequestParametersInvalid
               | DocumentObjectVersionMismatch
               | DocumentStateError
               | SignatoryStateError
  deriving (Show, Eq, Typeable)



errorIDFromAPIErrorType :: APIErrorType -> Text
errorIDFromAPIErrorType ServerError                   = "server_error"
errorIDFromAPIErrorType EndpointNotFound              = "endpoint_not_found"
errorIDFromAPIErrorType InvalidAuthorisation          = "invalid_authorisation"
errorIDFromAPIErrorType InsufficientPrivileges        = "insufficient_privileges"
errorIDFromAPIErrorType ResourceNotFound              = "resource_not_found"
errorIDFromAPIErrorType DocumentActionForbidden       = "document_action_forbidden"
errorIDFromAPIErrorType RequestParametersMissing      = "request_parameters_missing"
errorIDFromAPIErrorType RequestParametersParseError   = "request_parameters_parse_error"
errorIDFromAPIErrorType RequestParametersInvalid      = "request_parameters_invalid"
errorIDFromAPIErrorType DocumentObjectVersionMismatch = "document_object_version_mismatch"
errorIDFromAPIErrorType DocumentStateError            = "document_state_error"
errorIDFromAPIErrorType SignatoryStateError           = "signatory_state_error"

jsonFromSomeKontraException :: SomeKontraException -> JSValue
jsonFromSomeKontraException (SomeKontraException ex)  = toJSValue ex

httpCodeFromSomeKontraException :: SomeKontraException -> Int
httpCodeFromSomeKontraException (SomeKontraException ex) =
  case cast ex of
    Just (apierror :: APIError) -> errorHttpCode apierror
    Nothing -> 500



-- General errors
serverError :: Text -> APIError
serverError reason = APIError { errorType = ServerError, errorHttpCode = 500, errorMessage = msg}
  where msg = "We encountered an unexpected error. Please contact Scrive "
              `append` "support and include as much details about what caused "
              `append` "the error, including the document id or any other details. "
              `append` "Error details: " `append` reason

-- | Used internally by API.V2 for reporting bad API endpoints
endpointNotFound :: Text -> APIError
endpointNotFound ep = APIError { errorType = EndpointNotFound, errorHttpCode = 404, errorMessage = msg}
  where msg = "The endpoint " `append` ep `append` " was not found. See our website for API documentation."

-- | Used interally by this module and API.V2.User
invalidAuthorisation :: APIError
invalidAuthorisation = APIError { errorType = InvalidAuthorisation, errorHttpCode = 401, errorMessage = msg}
  where msg = "No valid access credentials were provided. Please refer to our API documentation."

-- | Used interally by this module and API.V2.User
invalidAuthorisationWithMsg :: Text -> APIError
invalidAuthorisationWithMsg problem = invalidAuthorisation { errorMessage = msg}
  where msg = errorMessage invalidAuthorisation `append` " The problem was: " `append` problem

-- | Used interally by this module and API.V2.User
insufficientPrivileges :: APIError
insufficientPrivileges = APIError { errorType = InsufficientPrivileges, errorHttpCode = 403, errorMessage = msg}
  where msg = "The access credentials provided do not have sufficient privileges for this request."

-- Request specific errors
requestParameterMissing :: Text -> APIError
requestParameterMissing param = APIError { errorType = RequestParametersMissing, errorHttpCode = 400, errorMessage = msg}
  where msg = "The parameter '" `append` param `append` "' was missing. Please refer to our API documentation."

requestParameterParseError :: Text -> Text -> APIError
requestParameterParseError param error = APIError { errorType = RequestParametersParseError, errorHttpCode = 400, errorMessage = msg}
  where msg = "The parameter '" `append` param `append` "' could not be parsed."
            `append` " Please refer to our API documentation. Error details: "
            `append` error

requestParameterInvalid :: Text -> Text -> APIError
requestParameterInvalid param reason = APIError { errorType = RequestParametersInvalid, errorHttpCode = 400, errorMessage = msg}
  where msg = "The parameter '" `append` param `append` "' had the following problems: " `append` reason

-- Document calls errors

documentObjectVersionMismatch :: DocumentObjectVersionDoesNotMatch -> APIError
documentObjectVersionMismatch (DocumentObjectVersionDoesNotMatch {..}) = APIError { errorType = DocumentObjectVersionMismatch, errorHttpCode = 409, errorMessage = msg}
  where msg = "The document has a different object_version to the one provided and so the request was not processed."
              `append` " You gave " `append` (pack $ show documentObjectVersionShouldBe)
              `append` " but the document had " `append` (pack $ show documentObjectVersionIs)

documentStateError :: Text -> APIError
documentStateError msg = APIError { errorType = DocumentStateError, errorHttpCode = 409, errorMessage = msg}

documentStateErrorWithCode :: Int -> Text -> APIError
documentStateErrorWithCode code msg = (documentStateError msg) {errorHttpCode = code}

signatoryStateError :: Text -> APIError
signatoryStateError msg = APIError { errorType = SignatoryStateError, errorHttpCode = 409, errorMessage = msg}

documentActionForbidden :: APIError
documentActionForbidden = APIError { errorType = DocumentActionForbidden, errorHttpCode = 403, errorMessage = msg}
  where msg = "You do not have permission to perform this action on the document."

documentNotFound :: DocumentID -> APIError
documentNotFound did = resourceNotFound $ "A document with id " `append` didText `append` " was not found."
  where didText = pack (show did)

resourceNotFound :: Text -> APIError
resourceNotFound info = APIError { errorType = ResourceNotFound, errorHttpCode = 404, errorMessage = msg}
  where msg = "The resource was not found. " `append` info

-- Conversion of DB exception / document conditionals into API errors

tryToConvertConditionalExpectionIntoAPIError :: SomeKontraException -> SomeKontraException
tryToConvertConditionalExpectionIntoAPIError  =  compose [
      convertDocumentDoesNotExist
    , convertDocumentTypeShouldBe
    , convertDocumentStatusShouldBe
    , convertUserShouldBeSelfOrCompanyAdmin
    , convertUserShouldBeDirectlyOrIndirectlyRelatedToDocument
    , convertSignatoryLinkDoesNotExist
    , convertSignatoryHasNotYetSigned
    , convertSignatoryIsNotPartner
    , convertSignatoryIsAuthor
    , convertSignatoryHasAlreadySigned
    , convertSignatoryTokenDoesNotMatch
    , convertDocumentObjectVersionDoesNotMatch
    , convertDocumentWasPurged
    , convertDocumentIsDeleted
    , convertDocumentIsNotDeleted
    , convertDocumentIsReallyDeleted
    , convertSignatoryAuthenticationToSignDoesNotMatch
  ]
  where
    compose [] = id
    compose (f:fs) = f . compose fs

convertDocumentDoesNotExist :: SomeKontraException -> SomeKontraException
convertDocumentDoesNotExist (SomeKontraException ex) =
  case cast ex of
    Just (DocumentDoesNotExist did) ->  SomeKontraException $ documentNotFound did
    Nothing -> (SomeKontraException ex)

convertDocumentTypeShouldBe :: SomeKontraException -> SomeKontraException
convertDocumentTypeShouldBe (SomeKontraException ex) =
  case cast ex of
    Just (DocumentTypeShouldBe  {documentTypeShouldBe = Template}) ->  SomeKontraException $ documentStateError $ "Document is not a template"
    Just (DocumentTypeShouldBe  {documentTypeShouldBe = Signable}) ->  SomeKontraException $ documentStateError $ "Document is a template"
    Nothing -> (SomeKontraException ex)

convertDocumentStatusShouldBe :: SomeKontraException -> SomeKontraException
convertDocumentStatusShouldBe (SomeKontraException ex) =
  case cast ex of
    Just (DocumentStatusShouldBe{}) ->  SomeKontraException $ documentStateError $ "Invalid document state "
    Nothing -> (SomeKontraException ex)

convertUserShouldBeSelfOrCompanyAdmin :: SomeKontraException -> SomeKontraException
convertUserShouldBeSelfOrCompanyAdmin (SomeKontraException ex) =
  case cast ex of
    Just (UserShouldBeSelfOrCompanyAdmin{}) ->  SomeKontraException $ insufficientPrivileges
    Nothing -> (SomeKontraException ex)

convertUserShouldBeDirectlyOrIndirectlyRelatedToDocument :: SomeKontraException -> SomeKontraException
convertUserShouldBeDirectlyOrIndirectlyRelatedToDocument (SomeKontraException ex) =
  case cast ex of
    Just (UserShouldBeDirectlyOrIndirectlyRelatedToDocument {}) ->  SomeKontraException $ insufficientPrivileges
    Nothing -> (SomeKontraException ex)

convertSignatoryLinkDoesNotExist :: SomeKontraException -> SomeKontraException
convertSignatoryLinkDoesNotExist (SomeKontraException ex) =
  case cast ex of
    Just (SignatoryLinkDoesNotExist sig) ->  SomeKontraException $ signatoryStateError $ "Signatory "  `append` pack (show sig) `append` " does not exists"
    Nothing -> (SomeKontraException ex)

convertSignatoryHasNotYetSigned :: SomeKontraException -> SomeKontraException
convertSignatoryHasNotYetSigned (SomeKontraException ex) =
  case cast ex of
    Just (SignatoryHasNotYetSigned {}) ->  SomeKontraException $ signatoryStateError $ "Signatory has not signed yet"
    Nothing -> (SomeKontraException ex)

convertSignatoryIsNotPartner :: SomeKontraException -> SomeKontraException
convertSignatoryIsNotPartner (SomeKontraException ex) =
  case cast ex of
    Just (SignatoryIsNotPartner {}) ->  SomeKontraException $ signatoryStateError $ "Signatory should not sign this document "
    Nothing -> (SomeKontraException ex)

convertSignatoryIsAuthor :: SomeKontraException -> SomeKontraException
convertSignatoryIsAuthor (SomeKontraException ex) =
  case cast ex of
    Just (SignatoryIsAuthor {}) -> SomeKontraException $ signatoryStateError $ "Signatory is author"
    Nothing -> (SomeKontraException ex)

convertSignatoryHasAlreadySigned :: SomeKontraException -> SomeKontraException
convertSignatoryHasAlreadySigned (SomeKontraException ex) =
  case cast ex of
    Just (SignatoryHasAlreadySigned {}) ->  SomeKontraException $ signatoryStateError $ "Signatory already signed"
    Nothing -> (SomeKontraException ex)

convertSignatoryTokenDoesNotMatch :: SomeKontraException -> SomeKontraException
convertSignatoryTokenDoesNotMatch (SomeKontraException ex) =
  case cast ex of
    Just (SignatoryTokenDoesNotMatch {}) -> SomeKontraException $ invalidAuthorisationWithMsg "Signatory token does not match"
    Nothing -> (SomeKontraException ex)

convertDocumentObjectVersionDoesNotMatch :: SomeKontraException -> SomeKontraException
convertDocumentObjectVersionDoesNotMatch (SomeKontraException ex) =
  case cast ex of
    Just (e@DocumentObjectVersionDoesNotMatch {}) -> SomeKontraException $ documentObjectVersionMismatch e
    Nothing -> (SomeKontraException ex)

convertDocumentWasPurged ::  SomeKontraException -> SomeKontraException
convertDocumentWasPurged (SomeKontraException ex) =
  case cast ex of
    Just (DocumentWasPurged {}) -> SomeKontraException $ documentStateError $ "Document was purged"
    Nothing -> (SomeKontraException ex)

convertDocumentIsDeleted ::  SomeKontraException -> SomeKontraException
convertDocumentIsDeleted (SomeKontraException ex) =
  case cast ex of
    Just (DocumentIsDeleted {}) -> SomeKontraException $ documentStateError $ "The document is in Trash."
    Nothing -> (SomeKontraException ex)

convertDocumentIsNotDeleted ::  SomeKontraException -> SomeKontraException
convertDocumentIsNotDeleted (SomeKontraException ex) =
  case cast ex of
    Just (DocumentIsNotDeleted {}) -> SomeKontraException $ documentStateError $ "The document is not in Trash."
    Nothing -> (SomeKontraException ex)

convertDocumentIsReallyDeleted ::  SomeKontraException -> SomeKontraException
convertDocumentIsReallyDeleted (SomeKontraException ex) =
  case cast ex of
    Just (DocumentIsReallyDeleted {}) -> SomeKontraException $ documentStateError $ "The document is deleted. It is not avaialbe and will be purged soon."
    Nothing -> (SomeKontraException ex)

convertSignatoryAuthenticationToSignDoesNotMatch ::  SomeKontraException -> SomeKontraException
convertSignatoryAuthenticationToSignDoesNotMatch (SomeKontraException ex) =
  case cast ex of
    Just (SignatoryAuthenticationToSignDoesNotMatch {}) -> SomeKontraException $ signatoryStateError $ "Invalid authorization for signatory"
    Nothing -> (SomeKontraException ex)
