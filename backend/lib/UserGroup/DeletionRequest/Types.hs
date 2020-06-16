module UserGroup.DeletionRequest.Types where

import Data.Time (Day, UTCTime)
import Text.JSON.Gen

import User.UserID (UserID)
import UserGroup.Types (UserGroupID)

data UserGroupDeletionRequest = UserGroupDeletionRequest
  { requestedFor          :: UserGroupID
  , requestedBy           :: UserID
  , requestedDeletionDate :: Day
  , signedOffBy           :: Maybe UserID
  , expires               :: Maybe UTCTime
  }

instance ToJSValue UserGroupDeletionRequest where
  toJSValue UserGroupDeletionRequest {..} = runJSONGen $ do
    value "requested_by" $ show requestedBy
    value "requested_deletion_date" $ show requestedDeletionDate
    whenJust signedOffBy $ value "signed_off_by" . show
