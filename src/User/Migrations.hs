module User.Migrations where

import Control.Monad
import Control.Monad.Catch
import Data.Monoid.Utils

import Company.Model (CompanyID)
import DB
import User.Model (UserID)
import User.Tables

addUserCustomFooter :: MonadDB m => Migration m
addUserCustomFooter =
  Migration {
    mgrTable = tableUsers
  , mgrFrom = 3
  , mgrDo = do
      runSQL_ "ALTER TABLE users ADD COLUMN customfooter TEXT"
      runSQL_ "UPDATE users SET customfooter = NULL"
  }

removeSystemServer :: MonadDB m => Migration m
removeSystemServer =
  Migration {
    mgrTable = tableUsers
  , mgrFrom = 2
  , mgrDo = do
      runSQL_ "ALTER TABLE users DROP COLUMN system_server CASCADE"
  }

addRegionToUserSettings :: MonadDB m => Migration m
addRegionToUserSettings =
  Migration {
    mgrTable = tableUsers
  , mgrFrom = 1
  , mgrDo = do
      runSQL_ "ALTER TABLE users ADD COLUMN region SMALLINT"
      runSQL_ "UPDATE users SET region = 1" -- default region
      runSQL_ "ALTER TABLE users ALTER COLUMN region SET NOT NULL"
  }

addIdSerialOnUsers :: MonadDB m => Migration m
addIdSerialOnUsers =
  Migration {
    mgrTable = tableUsers
  , mgrFrom = 4
  , mgrDo = do
      runSQL_ "CREATE SEQUENCE users_id_seq"
      runSQL_ "SELECT setval('users_id_seq',(SELECT COALESCE(max(id)+1,1000) FROM users))"
      runSQL_ "ALTER TABLE users ALTER id SET DEFAULT nextval('users_id_seq')"
  }

addCompanyNameNumberOnUsers :: MonadDB m => Migration m
addCompanyNameNumberOnUsers =
  Migration {
    mgrTable = tableUsers
  , mgrFrom = 5
  , mgrDo = do
      runSQL_ "ALTER TABLE users ADD COLUMN company_name   TEXT NOT NULL DEFAULT ''"
      runSQL_ "ALTER TABLE users ADD COLUMN company_number TEXT NOT NULL DEFAULT ''"
  }

addCheckLowercaseEmailsUsers :: MonadDB m => Migration m
addCheckLowercaseEmailsUsers =
  Migration {
    mgrTable = tableUsers
  , mgrFrom = 6
  , mgrDo = do
      runSQL_ "UPDATE users SET email = lower(email)"
      runSQL_ "ALTER TABLE users ADD CONSTRAINT users_email_lowercase_chk CHECK (email = lower(email))"
  }

removePreferedDesignMode :: MonadDB m => Migration m
removePreferedDesignMode =
  Migration {
    mgrTable = tableUsers
  , mgrFrom = 7
  , mgrDo = do
      runSQL_ "ALTER TABLE users DROP COLUMN preferred_design_mode"
  }

addIsFree :: MonadDB m => Migration m
addIsFree =
  Migration {
      mgrTable = tableUsers
    , mgrFrom = 8
    , mgrDo = do
      runSQL_ "ALTER TABLE users ADD COLUMN is_free BOOL NOT NULL DEFAULT FALSE"
    }

removeServiceIDFromUsers :: MonadDB m => Migration m
removeServiceIDFromUsers = Migration {
    mgrTable = tableUsers
  , mgrFrom = 9
  , mgrDo = do
    -- check if service_id field is empty for all users
    runSQL_ "SELECT DISTINCT service_id IS NULL FROM users"
    check <- fetchMany runIdentity
    case check of
      []     -> return () -- no records, ok
      [True] -> return () -- only nulls, ok
      _      -> error "Users have rows with non-null service_id"
    runSQL_ "ALTER TABLE users DROP CONSTRAINT fk_users_services"
    runSQL_ "DROP INDEX idx_users_service_id"
    runSQL_ "ALTER TABLE users DROP COLUMN service_id"
}

removeRegionFromUsers :: MonadDB m => Migration m
removeRegionFromUsers = Migration {
    mgrTable = tableUsers
  , mgrFrom = 10
  , mgrDo = runSQL_ "ALTER TABLE users DROP COLUMN region"
}

dropCustomFooterFromUsers :: MonadDB m => Migration m
dropCustomFooterFromUsers = Migration {
    mgrTable = tableUsers
  , mgrFrom = 11
  , mgrDo = runSQL_ "ALTER TABLE users DROP COLUMN customfooter"
}

addAssociatedDomainToUsers :: MonadDB m => Migration m
addAssociatedDomainToUsers = Migration {
    mgrTable = tableUsers
  , mgrFrom = 12
  , mgrDo = runSQL_ "ALTER TABLE users ADD COLUMN associated_domain TEXT NULL"

}

dropMobileFromUsers :: MonadDB m => Migration m
dropMobileFromUsers = Migration {
    mgrTable = tableUsers
  , mgrFrom = 13
  , mgrDo = runSQL_ "ALTER TABLE users DROP COLUMN mobile"

}
removeIsFree :: MonadDB m => Migration m
removeIsFree =
  Migration {
      mgrTable = tableUsers
    , mgrFrom = 14
    , mgrDo = runSQL_ "ALTER TABLE users DROP COLUMN is_free"
    }

allUsersMustHaveCompany :: (MonadDB m, MonadThrow m) => Migration m
allUsersMustHaveCompany =
  Migration {
      mgrTable = tableUsers
    , mgrFrom = 15
    , mgrDo = do
       runQuery_ . sqlSelect "users" $ do
                  sqlResult "id, company_name, company_number"
                  sqlWhere "company_id IS NULL"
       usersWithoutCompany <- fetchMany id
       forM_ usersWithoutCompany $ \(userid::UserID, companyname::String, companynumber::String) -> do
            runQuery_ . sqlInsert "companies" $ do
                            sqlSet "name" companyname
                            sqlSet "number" companynumber
                            sqlResult "id"
            companyidx :: CompanyID <- fetchOne runIdentity

            runQuery_ . sqlInsert "company_uis" $ do
                sqlSet "company_id" companyidx

            runQuery_ . sqlUpdate "users" $ do
                sqlSet "company_id" companyidx
                sqlSet "is_company_admin" True
                sqlWhereEq "id" userid
       runSQL_ "ALTER TABLE users DROP COLUMN company_name"
       runSQL_ "ALTER TABLE users DROP COLUMN company_number"
    }

migrateUsersDeletedTime :: MonadDB m => Migration m
migrateUsersDeletedTime =
  Migration {
      mgrTable = tableUsers
    , mgrFrom = 16
    , mgrDo = do
       runSQL_ $ "ALTER TABLE users"
                  <+> "ALTER deleted DROP NOT NULL,"
                  <+> "ALTER deleted DROP DEFAULT,"
                  <+> "ALTER deleted TYPE TIMESTAMPTZ USING (CASE WHEN deleted THEN now() ELSE NULL END)"
    }

migrateUsersUniqueIndexOnEmail :: MonadDB m => Migration m
migrateUsersUniqueIndexOnEmail =
  Migration {
      mgrTable = tableUsers
    , mgrFrom = 17
    , mgrDo = do
       runQuery_ . sqlDelete "users" $ do
                 sqlWhereNotExists (sqlSelect "signatory_links" $ do -- there are not documents connected to this account)
                                      sqlWhere "signatory_links.user_id = users.id")
                 -- but there is another account for this email address
                 sqlWhereExists $ sqlSelect "users u2" $ do
                    sqlWhere "u2.email = users.email"    -- same email
                    sqlWhere "u2.id <> users.id"         -- different id
                    sqlWhere "u2.deleted IS NULL"        -- the other one is not deleted
         -- note: this may delete both accounts, but that is ok


       runQuery_ $ sqlDropIndex "users" (indexOnColumn "email")
       runQuery_ $ sqlCreateIndex "users" ((indexOnColumn "email") { idxUnique = True, idxWhere = Just ("deleted IS NULL") })
    }

usersTableChangeAssociatedDomainToForeignKey :: MonadDB m => Migration m
usersTableChangeAssociatedDomainToForeignKey =
  Migration {
    mgrTable = tableUsers
  , mgrFrom = 18
  , mgrDo = do
      runQuery_ $ sqlAlterTable "users" [ sqlAddColumn (tblColumn { colName = "associated_domain_id", colType = BigIntT }) ]
      runSQL_ "UPDATE users SET associated_domain_id = (SELECT branded_domains.id FROM branded_domains WHERE branded_domains.url = users.associated_domain)"
      runQuery_ $ sqlAlterTable "users" [sqlDropColumn "associated_domain"]
      runQuery_ $ sqlAlterTable "users" [sqlAddFK "users" (fkOnColumn "associated_domain_id" "branded_domains" "id")]
  }

makeAssociatedDomainObligatoryForUsers :: MonadDB m => Migration m
makeAssociatedDomainObligatoryForUsers =
  Migration {
    mgrTable = tableUsers
  , mgrFrom = 19
  , mgrDo = do
      runSQL_ "UPDATE users SET associated_domain_id = (SELECT branded_domains.id FROM branded_domains WHERE branded_domains.main_domain) WHERE associated_domain_id IS NULL"
      runSQL_ "ALTER TABLE users ALTER associated_domain_id SET NOT NULL"
  }
