{-# LANGUAGE StrictData #-}
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
  | AuthCookiesParseError
  | InvalidAuthCookiesError
  | AccessControlError
  | InvalidInstanceAccessTokenError

instance Show AuthError where
  show = \case
    OAuthHeaderParseFailureError    -> "Cannot parse OAuth header"
    InvalidTokenError               -> "The provided OAuth token is not valid"
    AuthCookiesParseError           -> "Cannot parse the authentication cookies"
    InvalidAuthCookiesError         -> "The provided authentication cookies are invalid"
    AccessControlError -> "You do not have permission to perform the requested action"
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

throwAuthenticationErrorHTML
  :: (MonadError ServerError m, MonadReader FlowContext m, MonadDB m, MonadThrow m)
  => BrandedDomain
  -> AuthError
  -> m a
throwAuthenticationErrorHTML bd explanation = do
  renderedHtml <- do
    pageVars <- Html.mkErrorPageVars bd "Authentication Error" (showt explanation)
    pure . TL.toStrict . Blaze.renderHtml $ Html.renderErrorPage pageVars
  throwError $ makeHTMLError 401 "Authentication Error" renderedHtml

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

throwUnableToAddDocumentSessionHTML
  :: (MonadError ServerError m, MonadReader FlowContext m, MonadDB m, MonadThrow m)
  => BrandedDomain
  -> m a
throwUnableToAddDocumentSessionHTML bd = do
  renderedHtml <- do
    pageVars <- Html.mkErrorPageVars
      bd
      msg
      "We were unable to create a session for this document"
    pure . TL.toStrict . Blaze.renderHtml $ Html.renderErrorPage pageVars
  throwError $ makeHTMLError 500 msg renderedHtml
  where msg = "Unable to add a document session"

throwProcessFailedHTML
  :: (MonadError ServerError m, MonadReader FlowContext m, MonadDB m, MonadThrow m)
  => BrandedDomain
  -> m a
throwProcessFailedHTML bd = do
  renderedHtml <- do
    pageVars <- Html.mkErrorPageVars
      bd
      msg
      "This document workflow is no longer accessible to participants."
    pure . TL.toStrict . Blaze.renderHtml $ Html.renderErrorPage pageVars
  throwError $ makeHTMLError 403 msg renderedHtml
  where msg = "Document workflow has failed"

throwInternalServerError :: Text -> MonadError ServerError m => m a
throwInternalServerError explanation = throwError $ makeJSONError FlowError
  { code        = 500
  , message     = "Internal server error"
  , explanation = explanation
  , details     = Nothing
  }
