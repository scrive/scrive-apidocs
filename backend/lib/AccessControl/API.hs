module AccessControl.API (
    accessControlAPI
  , accessControlAPIV2GetUserRoles
) where

import Data.Unjson
import Happstack.Server.Types
import Happstack.StaticRouting

import AccessControl.JSON
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
    dir "v2" $ accessControlAPIV2
  ]

accessControlAPIV2 :: Route (Kontra Response)
accessControlAPIV2 = choice
  [
    dir "getuserroles" . hGet . toK1 $ accessControlAPIV2GetUserRoles
  ]

accessControlAPIV2GetUserRoles :: Kontrakcja m => UserID -> m Response
accessControlAPIV2GetUserRoles uid = api $ do
  -- Check user has permissions to view User
  (acc, user) <- makeAPIUserAccessPolicyReq ReadA UserR uid
  apiAccessControlOrIsAdmin [acc] $ do
    -- Get roles for user
    roles <- dbQuery $ GetRoles user
    Ok <$> return (arrayOf unjsonAccessRole, roles)
