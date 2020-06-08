module Flow.Guards where

import AccessControl.Check (accessControlCheckAll)
import AccessControl.Types (Permission)
import Flow.Error
import Flow.Server.Types

guardUserHasPermission :: Account -> [Permission] -> AppM ()
guardUserHasPermission Account {..} perms =
  unless (accessControlCheckAll roles perms) $ throwAuthenticationError AccessControlError
