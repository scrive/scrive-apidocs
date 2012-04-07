module KontraError where

import Control.Monad.Base
import Data.Typeable
import qualified Control.Exception.Lifted as E

data KontraError =
    Respond404
  | InternalError
  deriving (Show, Typeable)

instance E.Exception KontraError

internalError :: MonadBase IO m => m a
internalError = E.throwIO InternalError

respond404 :: MonadBase IO m => m a
respond404 = E.throwIO Respond404
