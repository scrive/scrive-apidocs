module Flow.Guards where

import Control.Monad.Except (MonadError)
import Servant (ServerError)

import AccessControl.Check (accessControlCheckAll)
import AccessControl.Types (Permission)
import Flow.Error
import Flow.Server.Types

guardUserHasPermission :: MonadError ServerError m => Account -> [Permission] -> m ()
guardUserHasPermission Account {..} perms =
  unless (accessControlCheckAll roles perms) $ throwAuthenticationError AccessControlError
