module AccessControl.API (
    accessControlAPI
  , accessControlAPIV2GetUserRoles
) where

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
        roles <- addInheritedRoles roles0
        Ok <$> return (arrayOf unjsonAccessRole, nub roles)
