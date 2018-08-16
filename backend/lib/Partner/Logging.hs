module Partner.Logging (
    logPartner
  , logPartnerAndUser
  , logPartnerAndUserGroup
  ) where

import Log

import Log.Identifier
import Partner.PartnerID
import User.UserID
import UserGroup.Data

logPartner :: MonadLog m => PartnerID -> m r -> m r
logPartner pid = localData [identifier pid]

logPartnerAndUserGroup :: MonadLog m => PartnerID -> UserGroupID -> m r -> m r
logPartnerAndUserGroup pid ugid = localData [identifier pid, identifier ugid]

logPartnerAndUser :: MonadLog m => PartnerID -> UserID -> m r -> m r
logPartnerAndUser pid uid = localData [identifier pid, identifier uid]
