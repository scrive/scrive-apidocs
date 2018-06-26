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
logPartner pid = localData [identifier_ pid]

logPartnerAndUserGroup :: MonadLog m => PartnerID -> UserGroupID -> m r -> m r
logPartnerAndUserGroup pid ugid = localData [identifier_ pid, identifier_ ugid]

logPartnerAndUser :: MonadLog m => PartnerID -> UserID -> m r -> m r
logPartnerAndUser pid uid = localData [identifier_ pid, identifier_ uid]
