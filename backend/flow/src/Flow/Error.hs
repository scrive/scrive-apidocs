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
  , throwUnableToAddDocumentSession
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Casing
import GHC.Generics
import Servant
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html.Renderer.Text as Blaze

import Flow.Server.Types
import VersionTH (versionID)
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

makeHTMLError :: FlowError -> ServerError
makeHTMLError FlowError {..} = ServerError
  { errHTTPCode     = code
  , errReasonPhrase = T.unpack message
  , errBody         = BSL.fromString $ T.unpack explanation
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
  :: (MonadError ServerError m, MonadReader FlowContext m) => AuthError -> m a
throwAuthenticationErrorHTML explanation' = do
  FlowContext { cdnBaseUrl, production } <- ask
  throwError $ makeHTMLError FlowError
    { code        = 401
    , message     = "Authentication Error"
    , explanation = renderedHTML (fromMaybe "" cdnBaseUrl) production
    , details     = Nothing
    }
  where
    -- TODO: The error page should use BrandedDomains like the overview page.
    renderedHTML cdnBaseUrl production =
      TL.toStrict . Blaze.renderHtml . Html.renderAuthErrorPage $ Html.AuthErrorPageVars
        { commonVars  =
          Html.CommonPageVars
            { production     = production
            , cdnBaseUrl     = cdnBaseUrl
            , brandingCssUrl = cdnBaseUrl <> "/elm-assets/flow-dummy-branding.css"
            , logoUrl        = cdnBaseUrl <> "/elm-assets/flow-images/scrive-logo.png"
            , versionCode    = T.decodeUtf8 . B16.encode $ BS.fromString versionID
            , browserTitle   = "Scrive Flow Authorisation Error"
            }
        , explanation = showt explanation'
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

throwUnableToAddDocumentSession :: MonadError ServerError m => m a
throwUnableToAddDocumentSession = throwError $ makeJSONError FlowError
  { code        = 500
  , message     = "Unable to add a document session"
  , explanation = "We were unable to create a session for this document"
  , details     = Nothing
  }

throwInternalServerError :: Text -> MonadError ServerError m => m a
throwInternalServerError explanation = throwError $ makeJSONError FlowError
  { code        = 500
  , message     = "Internal server error"
  , explanation = explanation
  , details     = Nothing
  }
