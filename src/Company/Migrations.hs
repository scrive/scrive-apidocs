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
      kRunRaw "ALTER TABLE companies ADD COLUMN bars_background TEXT NULL"
      kRunRaw "ALTER TABLE companies ADD COLUMN logo BYTEA NULL"
      return ()
  }

addTextColourToEmailBranding :: Migration
addTextColourToEmailBranding =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 2
  , mgrDo = kRunRaw "ALTER TABLE companies ADD COLUMN bars_textcolour TEXT NULL"
  }

addIdSerialOnCompanies :: Migration
addIdSerialOnCompanies =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 3
  , mgrDo = do
      kRunRaw $ "CREATE SEQUENCE companies_id_seq"
      kRunRaw $ "SELECT setval('companies_id_seq',(SELECT COALESCE(max(id)+1,1000) FROM companies))"
      kRunRaw $ "ALTER TABLE companies ALTER id SET DEFAULT nextval('companies_id_seq')"
  }
