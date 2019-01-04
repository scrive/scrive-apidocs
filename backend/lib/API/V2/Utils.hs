module API.V2.Utils (
    apiAccessControl
) where

import AccessControl.Types
import API.V2
import API.V2.Errors
import DB.Query
import Kontra (Kontrakcja)
import OAuth.Model
import User.Model.Query

apiAccessControl :: (Kontrakcja m) => AccessPolicy -> m a -> m a
apiAccessControl acc ma = do
  apiuser <- fst <$> getAPIUser APIPersonal
  roles <- dbQuery . GetUserRoles $ apiuser
  accessControl roles acc (apiError insufficientPrivileges) ma
