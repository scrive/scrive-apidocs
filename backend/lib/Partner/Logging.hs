module Partner.Logging (
    logPartner
  , logPartnerAndUser
  , logPartnerAndUserGroup
  ) where

import Log

import Log.Identifier
import User.UserID
import UserGroup.Types

logPartner
  :: MonadLog m
  => UserGroupID
           -- ^ The user group for the partner or the root of the
           -- user group structure
  -> m r
  -> m r
logPartner pugid = localData $ [identifierMapLabel ("partner_" <>) pugid]

logPartnerAndUserGroup
  :: MonadLog m
  => UserGroupID
                       -- ^ The user group for the partner or the root of the
                       -- user group structure
  -> UserGroupID
                       -- ^ The user group being touched under the above partner
                       -- (the root)
  -> m r
  -> m r
logPartnerAndUserGroup pugid ugid =
  localData $ [identifierMapLabel ("partner_" <>) pugid, identifier ugid]

logPartnerAndUser
  :: MonadLog m
  => UserGroupID
                  -- ^ The user group for the partner
  -> UserID
                  -- ^ The user being touched
  -> m r
  -> m r
logPartnerAndUser pugid uid =
  localData $ [identifierMapLabel ("partner_" <>) pugid, identifier uid]
