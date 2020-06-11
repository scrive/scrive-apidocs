module Flow.Error (
    throwAuthenticationError
  , throwTemplateNotFoundError
  , throwTemplateAlreadyCommittedError
  , throwTemplateNotCommittedError
  , throwInstanceNotFoundError
  , throwDSLValidationError
  , AuthError(..)
  ) where

import Control.Monad.Except
import Data.Aeson
import Data.Aeson.Casing
import GHC.Generics
import Servant
import qualified Data.Text as T

data FlowError = FlowError {
    code :: Int
  , message :: Text
  , explanation :: Text
  } deriving Generic

instance ToJSON FlowError where
  toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = snakeCase }

data AuthError
  = OAuthHeaderParseFailureError
  | InvalidTokenError
  | AuthCookiesParseError
  | InvalidAuthCookiesError
  | AccessControlError

instance Show AuthError where
  show = \case
    OAuthHeaderParseFailureError -> "Cannot parse OAuth header"
    InvalidTokenError            -> "The provided OAuth token is not valid"
    AuthCookiesParseError        -> "Cannot parse the authentication cookies"
    InvalidAuthCookiesError      -> "The provided authentication cookies are invalid"
    AccessControlError -> "You do not have permission to perform the requested action"

makeError :: FlowError -> ServerError
makeError err@FlowError {..} = ServerError
  { errHTTPCode     = code
  , errReasonPhrase = T.unpack message
  , errBody         = encode err
  , errHeaders      = [("Content-Type", "application/json")]
  }

throwAuthenticationError :: MonadError ServerError m => AuthError -> m a
throwAuthenticationError explanation = throwError $ makeError FlowError
  { code        = 401
  , message     = "Authentication Error"
  , explanation = T.pack $ show explanation
  }

throwTemplateNotFoundError :: MonadError ServerError m => m a
throwTemplateNotFoundError = throwError $ makeError FlowError
  { code        = 404
  , message     = "Template not found"
  , explanation = "There is no template associated with this ID"
  }

throwTemplateAlreadyCommittedError :: MonadError ServerError m => m a
throwTemplateAlreadyCommittedError = throwError $ makeError FlowError
  { code        = 409
  , message     = "Template already committed"
  , explanation = "This template has already been committed and cannot be altered"
  }

throwTemplateNotCommittedError :: MonadError ServerError m => m a
throwTemplateNotCommittedError = throwError $ makeError FlowError
  { code        = 409
  , message     = "Committed template not found"
  , explanation = "The template associated with this ID has not yet been committed"
  }

throwInstanceNotFoundError :: MonadError ServerError m => m a
throwInstanceNotFoundError = throwError $ makeError FlowError
  { code        = 404
  , message     = "Instance not found"
  , explanation = "There is no instance associated with this ID"
  }

throwDSLValidationError :: MonadError ServerError m => Text -> m a
throwDSLValidationError explanation = throwError $ makeError FlowError
  { code        = 409
  , message     = "Template DSL failed validation"
  , explanation = explanation
  }
