module AccessControl.API (
    accessControlAPI
  , accessControlAPIV2GetUserRoles
) where

import Control.Monad.Extra (concatForM)
import Data.Unjson
import Happstack.Server.Types
import Happstack.StaticRouting

import AccessControl.JSON
import AccessControl.Model
import AccessControl.Types
import API.V2
import API.V2.Utils
import DB
import Kontra
import Routing
import User.Model.Query
import User.UserID
import UserGroup.Model
import UserGroup.Types

accessControlAPI :: Route (Kontra Response)
accessControlAPI = dir "api" $ choice
  [
    dir "frontend" $ accessControlAPIV2
  , dir "v2" $ accessControlAPIV2
  ]

accessControlAPIV2 :: Route (Kontra Response)
accessControlAPIV2 = choice
  [
    dir "getuserroles" . hGet . toK1 $ accessControlAPIV2GetUserRoles
  ]

accessControlAPIV2GetUserRoles :: Kontrakcja m => UserID -> m Response
accessControlAPIV2GetUserRoles uid = api $ do
  -- Check user has permissions to view User
  apiAccessControlOrIsAdmin [mkAccPolicyItem (ReadA, UserR, uid)] $ do
    -- Get roles for user
    dbQuery (GetUserByID uid) >>= \case
      Nothing ->
        apiError $ serverError "Impossible happened: No user with ID, or deleted."
      Just user -> do
        roles0 <- dbQuery $ GetRoles user
        roles <- concatForM roles0 $ \role -> do
          case accessRoleTarget role of
            UserAdminAR ugid -> do
              ugwcs <- dbQuery $ UserGroupGetAllChildrenRecursive ugid
              return . (role:) . for (ugwcToList ugwcs) $ \ug ->
                accessRoleSetTarget (UserAdminAR $ get ugID ug) role
            _ -> return [role]

        Ok <$> return (arrayOf unjsonAccessRole, nub roles)