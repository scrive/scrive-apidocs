module User.Migrations where

import DB
import User.Tables

makeAssociatedDomainObligatoryForUsers :: MonadDB m => Migration m
makeAssociatedDomainObligatoryForUsers =
  Migration {
    mgrTable = tableUsers
  , mgrFrom = 19
  , mgrDo = do
      runSQL_ "UPDATE users SET associated_domain_id = (SELECT branded_domains.id FROM branded_domains WHERE branded_domains.main_domain) WHERE associated_domain_id IS NULL"
      runSQL_ "ALTER TABLE users ALTER associated_domain_id SET NOT NULL"
  }
