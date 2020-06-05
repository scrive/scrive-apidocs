module Flow.Guards where

import AccessControl.Check (accessControlCheckAll)
import AccessControl.Model (GetRolesIncludingInherited(..))
import AccessControl.Types (Permission)
import DB
import Flow.Error
import Flow.Server.Types
import User.Model (GetUserByID(..))
import UserGroup.Model (UserGroupGet(..))

guardUserHasPermission :: Account -> [Permission] -> AppM ()
guardUserHasPermission Account {..} perms = do
  user  <- fmap fromJust . dbQuery $ GetUserByID userId -- TODO: fix fromJust
  ug    <- fmap fromJust . dbQuery $ UserGroupGet userGroupId -- TODO: fix fromJust
  roles <- dbQuery $ GetRolesIncludingInherited user ug
  unless (accessControlCheckAll roles perms) $ throwAuthenticationError AccessControlError
