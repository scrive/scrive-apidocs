module Flow.Error (
    AuthError(..)
  , throwAuthenticationError
  , throwAuthenticationErrorHTML
  , throwTemplateNotFoundError
  , throwTemplateAlreadyCommittedError
  , throwTemplateNotCommittedError
  , throwInstanceNotFoundError
  , throwDSLValidationError
  , TemplateStartErrorType(..)
  , throwTemplateCannotBeStartedError
  , throwFlowCannotBeRejectedError
  , throwFlowCannotBeCancelledError
  , throwInternalServerError
  , throwDocumentCouldNotBeStarted
  , throwUnableToAddDocumentSessionHTML
  , throwProcessFailedHTML
  ) where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Casing
import Database.PostgreSQL.PQTypes hiding (JSON(..))
import GHC.Generics
import Servant
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html.Renderer.Text as Blaze

import BrandedDomain.Model
import Flow.Server.Types
import qualified Flow.Html as Html

data FlowError = FlowError
  { code :: Int
  , message :: Text
  , explanation :: Text
  , details :: Maybe Value
  } deriving Generic

instance ToJSON FlowError where
  toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = snakeCase
                                                , omitNothingFields  = True
                                                }

data AuthError
  = OAuthHeaderParseFailureError
  | InvalidTokenError
  | SessionCookieMissingError
  | XTokenMissingError
  | SessionCookieOrXTokenInvalidError
  | AccessControlError
  | AdditionalAuthenticationNeeded
  | RejectForbiddenError
  | InvalidInstanceAccessTokenError

instance Show AuthError where
  show = \case
    OAuthHeaderParseFailureError -> "Cannot parse OAuth header"
    InvalidTokenError            -> "The provided OAuth token is not valid"
    SessionCookieMissingError    -> "Session cookie is missing"
    XTokenMissingError
      -> "XToken is missing. This request should contain an X-Scrive-XToken header.\
         \ You can set it to the value from the \"xtoken\" cookie."
    SessionCookieOrXTokenInvalidError ->
      "The provided session cookie and/or xtoken header are invalid"
    AccessControlError -> "You do not have permission to perform the requested action"
    RejectForbiddenError            -> "You are not allowed to reject the flow instance"
    AdditionalAuthenticationNeeded  -> "Additional authentication required"
    InvalidInstanceAccessTokenError -> "This invitation link is invalid"

makeJSONError :: FlowError -> ServerError
makeJSONError err@FlowError {..} = ServerError
  { errHTTPCode     = code
  , errReasonPhrase = T.unpack message
  , errBody         = encode err
  , errHeaders      = [("Content-Type", "application/json")]
  }

makeHTMLError :: Int -> Text -> Text -> ServerError
makeHTMLError code reason body = ServerError
  { errHTTPCode     = code
  , errReasonPhrase = T.unpack reason
  , errBody         = BSL.fromString $ T.unpack body
  , errHeaders      = [("Content-Type", "text/html; charset=utf-8")]
  }

throwAuthenticationError :: MonadError ServerError m => AuthError -> m a
throwAuthenticationError explanation = throwError $ makeJSONError FlowError
  { code        = 401
  , message     = "Authentication Error"
  , explanation = T.pack $ show explanation
  , details     = Nothing
  }

throwTemplateNotFoundError :: MonadError ServerError m => m a
throwTemplateNotFoundError = throwError $ makeJSONError FlowError
  { code        = 404
  , message     = "Template not found"
  , explanation = "There is no template associated with this ID"
  , details     = Nothing
  }

throwTemplateAlreadyCommittedError :: MonadError ServerError m => m a
throwTemplateAlreadyCommittedError = throwError $ makeJSONError FlowError
  { code        = 409
  , message     = "Template already committed"
  , explanation = "This template has already been committed and cannot be altered"
  , details     = Nothing
  }

throwTemplateNotCommittedError :: MonadError ServerError m => m a
throwTemplateNotCommittedError = throwError $ makeJSONError FlowError
  { code        = 409
  , message     = "Committed template not found"
  , explanation = "The template associated with this ID has not yet been committed"
  , details     = Nothing
  }

throwInstanceNotFoundError :: MonadError ServerError m => m a
throwInstanceNotFoundError = throwError $ makeJSONError FlowError
  { code        = 404
  , message     = "Instance not found"
  , explanation = "There is no instance associated with this ID"
  , details     = Nothing
  }

throwDSLValidationError :: MonadError ServerError m => Text -> m a
throwDSLValidationError explanation = throwError $ makeJSONError FlowError
  { code        = 409
  , message     = "Template DSL failed validation"
  , explanation = explanation
  , details     = Nothing
  }

throwFlowCannotBeRejectedError :: MonadError ServerError m => m a
throwFlowCannotBeRejectedError = throwError $ makeJSONError FlowError
  { code        = 409
  , message     = "Flow cannot be rejected"
  , explanation = "You cannot reject a flow which is in Completed or Failed state"
  , details     = Nothing
  }

throwFlowCannotBeCancelledError :: MonadError ServerError m => m a
throwFlowCannotBeCancelledError = throwError $ makeJSONError FlowError
  { code        = 409
  , message     = "Flow cannot be cancelled"
  , explanation = "You cannot cancel a flow which is in Completed or Failed state"
  , details     = Nothing
  }

data TemplateStartErrorType = TemplateStartBadRequest | TemplateStartConflict

throwTemplateCannotBeStartedError
  :: MonadError ServerError m => TemplateStartErrorType -> Text -> Value -> m a
throwTemplateCannotBeStartedError errorType explanation details =
  throwError $ makeJSONError FlowError
    { code        = case errorType of
                      TemplateStartBadRequest -> 400
                      TemplateStartConflict   -> 409
    , message     = "Template cannot be started"
    , explanation = explanation
    , details     = Just details
    }

throwDocumentCouldNotBeStarted
  :: (MonadError ServerError m, ToJSON b) => Int -> Maybe b -> m a
throwDocumentCouldNotBeStarted statusCode details = throwError $ makeJSONError FlowError
  { code        = statusCode
  , message     = msg
  , explanation = msg
  , details     = toJSON <$> details
  }
  where msg = "Document could not be started"

throwInternalServerError :: Text -> MonadError ServerError m => m a
throwInternalServerError explanation = throwError $ makeJSONError FlowError
  { code        = 500
  , message     = "Internal server error"
  , explanation = explanation
  , details     = Nothing
  }

throwHTMLErrorPage
  :: (MonadError ServerError m, MonadReader FlowContext m, MonadDB m, MonadThrow m)
  => BrandedDomain
  -> Int
  -> Text
  -> Text
  -> m a
throwHTMLErrorPage bd code msg longMsg =
  throwError
    .   makeHTMLError code msg
    .   TL.toStrict
    .   Blaze.renderHtml
    .   Html.renderErrorPage
    =<< Html.mkErrorPageVars bd msg longMsg

throwAuthenticationErrorHTML
  :: (MonadError ServerError m, MonadReader FlowContext m, MonadDB m, MonadThrow m)
  => BrandedDomain
  -> AuthError
  -> m a
throwAuthenticationErrorHTML bd explanation = throwHTMLErrorPage bd code msg longMsg
  where
    code    = 401
    msg     = "Authentication Error"
    longMsg = showt explanation

throwUnableToAddDocumentSessionHTML
  :: (MonadError ServerError m, MonadReader FlowContext m, MonadDB m, MonadThrow m)
  => BrandedDomain
  -> m a
throwUnableToAddDocumentSessionHTML bd = throwHTMLErrorPage bd code msg longMsg
  where
    code    = 500
    msg     = "Unable to add a document session"
    longMsg = "We were unable to create a session for this document"

throwProcessFailedHTML
  :: (MonadError ServerError m, MonadReader FlowContext m, MonadDB m, MonadThrow m)
  => BrandedDomain
  -> m a
throwProcessFailedHTML bd = throwHTMLErrorPage bd code msg longMsg
  where
    code    = 403
    msg     = "Document workflow has failed"
    longMsg = "This document workflow is no longer accessible to participants"
