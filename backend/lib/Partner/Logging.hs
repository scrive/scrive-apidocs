module Partner.Logging (
    logPartner
  , logPartnerAndUser
  , logPartnerAndUserGroup
  ) where

import Log

import Log.Identifier
import Partner.PartnerID
import User.UserID
import UserGroup.Types

logPartner
  :: MonadLog m
  => Maybe PartnerID
           -- ^ Any legacy partner ID that we want to log
  -> UserGroupID
           -- ^ The user group for the partner or the root of the
           -- user group structure
  -> m r
  -> m r
logPartner mpid pugid =
  localData $ [identifier mpid, identifierMapLabel ("partner_" <>) pugid]

logPartnerAndUserGroup
  :: MonadLog m
  => Maybe PartnerID
                       -- ^ Any legacy partner ID that we want to log
  -> UserGroupID
                       -- ^ The user group for the partner or the root of the
                       -- user group structure
  -> UserGroupID
                       -- ^ The user group being touched under the above partner
                       -- (the root)
  -> m r
  -> m r
logPartnerAndUserGroup mpid pugid ugid =
  localData $ [identifier mpid, identifierMapLabel ("partner_" <>) pugid, identifier ugid]

logPartnerAndUser
  :: MonadLog m
  => Maybe PartnerID
                  -- ^ Any legacy partner ID that we want to log
  -> UserGroupID
                  -- ^ The user group for the partner
  -> UserID
                  -- ^ The user being touched
  -> m r
  -> m r
logPartnerAndUser mpid pugid uid =
  localData $ [identifier mpid, identifierMapLabel ("partner_" <>) pugid, identifier uid]
