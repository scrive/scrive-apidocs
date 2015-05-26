{-# LANGUAGE FunctionalDependencies, ExtendedDefaultRules #-}
module API.Monad.V2Errors (
                 APIError(),
                 serverError,
                 endPointNotFound,
                 invalidAuthorisation,
                 insufficientPrivileges,
                 requestParametersMissing,
                 requestParametersParseError,
                 requestParametersInvalid,
                 objectVersionMismatch,
                 documentStateError,
                 signatoryStateError,
                 documentAccessForbidden,
                 documentNotFound,
                 httpCodeFromSomeKontraException,
                 jsonFromSomeKontraException,
                 tryToConvertConditionalExpectionIntoAPIError
                 )
  where

import Data.Typeable
import Text.JSON.Gen hiding (object)
import Data.Text
import Text.JSON

import DB
import KontraPrelude
import Doc.Conditions
import Doc.DocStateData

data APIError = APIError {
      errorType    :: APIErrorType
    , errorCode    :: Int
    , errorMessage :: Text
  }
  deriving (Show, Eq, Typeable)



data APIErrorType = ServerError
               | EndPointNoFound
               | InvalidAuthorisation
               | InsufficientPrivileges
               | DocumentAccessForbidden
               | DocumentNotFound
               | RequestParametersMissing
               | RequestParametersParseError
               | RequestParametersInvalid
               | ObjectVersionMismatch
               | DocumentStateError
               | SignatoryStateError
  deriving (Show, Eq, Typeable)

instance KontraException APIError


instance ToJSValue APIError where
  toJSValue a = runJSONGen $ do
    value "code" (errorCode $ a)
    value "error_id" (unpack $ errorIDFromAPIErrorType $ errorType a)
    value "message" (unpack $ errorMessage a)

errorIDFromAPIErrorType :: APIErrorType -> Text
errorIDFromAPIErrorType ServerError      = "server_error"
errorIDFromAPIErrorType EndPointNoFound    = "end_point_not_found"
errorIDFromAPIErrorType InvalidAuthorisation  = "invalid_authorization"
errorIDFromAPIErrorType InsufficientPrivileges     = "insufficient_privileges"
errorIDFromAPIErrorType DocumentAccessForbidden = "document_access_forbidden"
errorIDFromAPIErrorType DocumentNotFound   = "document_not_found"
errorIDFromAPIErrorType RequestParametersMissing = "request_parameters_missing"
errorIDFromAPIErrorType RequestParametersParseError   = "request_parameters_parse_error"
errorIDFromAPIErrorType RequestParametersInvalid   = "request_parameters_invalid"
errorIDFromAPIErrorType ObjectVersionMismatch   = "object_version_mismatch"
errorIDFromAPIErrorType DocumentStateError = "document_state_error"
errorIDFromAPIErrorType SignatoryStateError   = "signatory_state_error"

jsonFromSomeKontraException :: SomeKontraException -> JSValue
jsonFromSomeKontraException (SomeKontraException ex)  = toJSValue ex

httpCodeFromSomeKontraException :: SomeKontraException -> Int
httpCodeFromSomeKontraException (SomeKontraException ex) =
  case cast ex of
    Just (apierror :: APIError) -> errorCode apierror
    Nothing -> 400


-- General errors
serverError :: Text -> APIError
serverError msg = APIError { errorType = ServerError, errorCode = 500, errorMessage = msg}

endPointNotFound :: Text -> APIError
endPointNotFound msg = APIError { errorType = EndPointNoFound, errorCode = 404, errorMessage = msg}

invalidAuthorisation :: Text -> APIError
invalidAuthorisation msg = APIError { errorType = InvalidAuthorisation, errorCode = 401, errorMessage = msg}

insufficientPrivileges :: Text -> APIError
insufficientPrivileges msg = APIError { errorType = InsufficientPrivileges, errorCode = 403, errorMessage = msg}

-- Request specific errors
requestParametersMissing :: Text -> APIError
requestParametersMissing msg = APIError { errorType = RequestParametersMissing, errorCode = 400, errorMessage = msg}

requestParametersParseError :: Text -> APIError
requestParametersParseError msg = APIError { errorType = RequestParametersParseError, errorCode = 400, errorMessage = msg}

requestParametersInvalid :: Text -> APIError
requestParametersInvalid msg = APIError { errorType = RequestParametersParseError, errorCode = 400, errorMessage = msg}

-- Document calls errors

objectVersionMismatch :: Text -> APIError
objectVersionMismatch msg = APIError { errorType = ObjectVersionMismatch, errorCode = 409, errorMessage = msg}

documentStateError :: Text -> APIError
documentStateError msg = APIError { errorType = DocumentStateError, errorCode = 409, errorMessage = msg}

signatoryStateError :: Text -> APIError
signatoryStateError msg = APIError { errorType = SignatoryStateError, errorCode = 409, errorMessage = msg}

documentAccessForbidden :: Text -> APIError
documentAccessForbidden msg = APIError { errorType = DocumentAccessForbidden, errorCode = 403, errorMessage = msg}

documentNotFound :: Text -> APIError
documentNotFound msg = APIError { errorType = DocumentNotFound, errorCode = 404, errorMessage = msg}

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
    , convertSignatoryAuthenticationDoesNotMatch
  ]
  where
    compose [] = id
    compose (f:fs) = f . compose fs

convertDocumentDoesNotExist :: SomeKontraException -> SomeKontraException
convertDocumentDoesNotExist (SomeKontraException ex) =
  case cast ex of
    Just (DocumentDoesNotExist did) ->  SomeKontraException $ documentNotFound $ "Document " `append` pack (show did) `append` " does not exists"
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
    Just (UserShouldBeSelfOrCompanyAdmin{}) ->  SomeKontraException $ invalidAuthorisation $ "You can not perform this action with current authorization."
    Nothing -> (SomeKontraException ex)

convertUserShouldBeDirectlyOrIndirectlyRelatedToDocument :: SomeKontraException -> SomeKontraException
convertUserShouldBeDirectlyOrIndirectlyRelatedToDocument (SomeKontraException ex) =
  case cast ex of
    Just (UserShouldBeDirectlyOrIndirectlyRelatedToDocument {}) ->  SomeKontraException $ invalidAuthorisation $ "You don't have rights not perform action on this document"
    Nothing -> (SomeKontraException ex)


convertSignatoryLinkDoesNotExist :: SomeKontraException -> SomeKontraException
convertSignatoryLinkDoesNotExist (SomeKontraException ex) =
  case cast ex of
    Just (SignatoryLinkDoesNotExist sig) ->  SomeKontraException $ signatoryStateError $ "Signatory"  `append` pack (show sig) `append` " does not exists"
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
    Just (SignatoryTokenDoesNotMatch {}) -> SomeKontraException $ invalidAuthorisation $ "Signatory token does not match"
    Nothing -> (SomeKontraException ex)

convertDocumentObjectVersionDoesNotMatch :: SomeKontraException -> SomeKontraException
convertDocumentObjectVersionDoesNotMatch (SomeKontraException ex) =
  case cast ex of
    Just (DocumentObjectVersionDoesNotMatch {}) -> SomeKontraException $ objectVersionMismatch $ "Object version does not match"
    Nothing -> (SomeKontraException ex)

convertDocumentWasPurged ::  SomeKontraException -> SomeKontraException
convertDocumentWasPurged (SomeKontraException ex) =
  case cast ex of
    Just (DocumentWasPurged {}) -> SomeKontraException $ documentStateError $ "Document was purged"
    Nothing -> (SomeKontraException ex)

convertDocumentIsDeleted ::  SomeKontraException -> SomeKontraException
convertDocumentIsDeleted (SomeKontraException ex) =
  case cast ex of
    Just (DocumentIsDeleted {}) -> SomeKontraException $ documentStateError $ "Document is deleted"
    Nothing -> (SomeKontraException ex)

convertDocumentIsNotDeleted ::  SomeKontraException -> SomeKontraException
convertDocumentIsNotDeleted (SomeKontraException ex) =
  case cast ex of
    Just (DocumentIsNotDeleted {}) -> SomeKontraException $ documentStateError $ "Document is not deleted"
    Nothing -> (SomeKontraException ex)


convertDocumentIsReallyDeleted ::  SomeKontraException -> SomeKontraException
convertDocumentIsReallyDeleted (SomeKontraException ex) =
  case cast ex of
    Just (DocumentIsReallyDeleted {}) -> SomeKontraException $ documentStateError $ "Document is really deleted. It is not avaialbe and will be purged soon"
    Nothing -> (SomeKontraException ex)


convertSignatoryAuthenticationDoesNotMatch ::  SomeKontraException -> SomeKontraException
convertSignatoryAuthenticationDoesNotMatch (SomeKontraException ex) =
  case cast ex of
    Just (SignatoryAuthenticationDoesNotMatch {}) -> SomeKontraException $ signatoryStateError $ "Invalid authorization for signatory"
    Nothing -> (SomeKontraException ex)