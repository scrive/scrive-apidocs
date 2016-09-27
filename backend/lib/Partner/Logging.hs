module Partner.Logging (
    logPartner
  , logPartnerAndCompany
  , logPartnerAndUser
  ) where

import Log

import Company.CompanyID
import Log.Identifier
import Partner.PartnerID
import User.UserID

logPartner :: MonadLog m => PartnerID -> m r -> m r
logPartner pid = localData [identifier_ pid]

logPartnerAndCompany :: MonadLog m => PartnerID -> CompanyID -> m r -> m r
logPartnerAndCompany pid cid = localData [identifier_ pid, identifier_ cid]

logPartnerAndUser :: MonadLog m => PartnerID -> UserID -> m r -> m r
logPartnerAndUser pid uid = localData [identifier_ pid, identifier_ uid]
