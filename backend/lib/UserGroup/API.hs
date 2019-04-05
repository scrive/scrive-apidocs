module UserGroup.API (
    userGroupAPI
  , userGroupApiV2Get
) where

import Happstack.Server.Types
import Happstack.StaticRouting

import AccessControl.Types
import API.V2
import API.V2.Utils
import Kontra
import Routing
import UserGroup.JSON
import UserGroup.Types

userGroupAPI :: Route (Kontra Response)
userGroupAPI = dir "api" $ choice
  [
    dir "frontend" $ userGroupAPIV2
  , dir "v2" $ userGroupAPIV2
  ]

userGroupAPIV2 :: Route (Kontra Response)
userGroupAPIV2 = dir "usergroups" $ choice
  [
    hGet . toK1 $ userGroupApiV2Get
  ]

userGroupApiV2Get :: Kontrakcja m => UserGroupID -> m Response
userGroupApiV2Get ugid = api $ do
  -- Check user has permissions to view UserGroup
  (acc, ug) <- makeAPIUserGroupAccessPolicyReq ReadA UserGroupR ugid
  apiAccessControlOrIsAdmin [acc] $ do
    Ok <$> return (unjsonUserGroup, ug)
