module Company.Migrations where

import DB.Classes
import DB.Model
import Company.Tables

addEmailBrandingToCompany :: Migration
addEmailBrandingToCompany =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 1
  , mgrDo = do
      kRunRaw "ALTER TABLE users ADD COLUMN bars_background TEXT NULL"
      kRunRaw "ALTER TABLE users ADD COLUMN logo BYTEA NULL"
      return ()
  }
