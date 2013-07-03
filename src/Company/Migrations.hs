{-# LANGUAGE ExtendedDefaultRules #-}
module Company.Migrations where

import DB
import Company.Tables

default (SQL)

addIPAddressMaskListToCompanies :: MonadDB m => Migration m
addIPAddressMaskListToCompanies = Migration {
    mgrTable = tableCompanies
  , mgrFrom = 7
  , mgrDo = do
    kRunRaw "ALTER TABLE companies ADD COLUMN ip_address_mask_list TEXT NULL"
    kRunRaw "ALTER SEQUENCE companies_id_seq OWNED BY companies.id"
}

removeServiceIDFromCompanies :: MonadDB m => Migration m
removeServiceIDFromCompanies = Migration {
    mgrTable = tableCompanies
  , mgrFrom = 6
  , mgrDo = do
    -- check if service_id field is empty for all companies
    check <- getMany "SELECT DISTINCT service_id IS NULL FROM companies"
    case check of
      []     -> return () -- no records, ok
      [True] -> return () -- only nulls, ok
      _      -> error "Companies have rows with non-null service_id"
    kRunRaw "ALTER TABLE companies DROP CONSTRAINT fk_companies_services"
    kRunRaw "DROP INDEX idx_companies_service_id"
    kRunRaw "ALTER TABLE companies DROP COLUMN service_id"
}

removeExternalIDFromCompanies :: MonadDB m => Migration m
removeExternalIDFromCompanies = Migration {
    mgrTable = tableCompanies
  , mgrFrom = 11
  , mgrDo = do
    kRunRaw "DROP INDEX idx_companies_external_id"
    kRunRaw "ALTER TABLE companies DROP COLUMN external_id"
}

addEmailBrandingToCompany :: MonadDB m => Migration m
addEmailBrandingToCompany =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 1
  , mgrDo = do
      kRunRaw "ALTER TABLE companies ADD COLUMN bars_background TEXT NULL"
      kRunRaw "ALTER TABLE companies ADD COLUMN logo BYTEA NULL"
      return ()
  }

addTextColourToEmailBranding :: MonadDB m => Migration m
addTextColourToEmailBranding =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 2
  , mgrDo = kRunRaw "ALTER TABLE companies ADD COLUMN bars_textcolour TEXT NULL"
  }

addIdSerialOnCompanies :: MonadDB m => Migration m
addIdSerialOnCompanies =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 3
  , mgrDo = do
      kRunRaw $ "CREATE SEQUENCE companies_id_seq"
      kRunRaw $ "SELECT setval('companies_id_seq',(SELECT COALESCE(max(id)+1,1000) FROM companies))"
      kRunRaw $ "ALTER TABLE companies ALTER id SET DEFAULT nextval('companies_id_seq')"
  }

addEmailDomainOnCompanies :: MonadDB m => Migration m
addEmailDomainOnCompanies =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 4
  , mgrDo = kRunRaw $ "ALTER TABLE companies ADD COLUMN email_domain TEXT NULL"
  }

addDefaultEmptyStringsToSomeColumnsInCompaniesTable :: MonadDB m => Migration m
addDefaultEmptyStringsToSomeColumnsInCompaniesTable =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 5
  , mgrDo = kRunRaw $ "ALTER TABLE companies"
    <> " ALTER name SET DEFAULT '',"
    <> " ALTER number SET DEFAULT '',"
    <> " ALTER address SET DEFAULT '',"
    <> " ALTER zip SET DEFAULT '',"
    <> " ALTER city SET DEFAULT '',"
    <> " ALTER country SET DEFAULT ''"
  }

addNewCompanyBrandingOptions :: MonadDB m => Migration m
addNewCompanyBrandingOptions =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 8
  , mgrDo = do
      kRunRaw "ALTER TABLE companies ADD COLUMN email_bordercolour TEXT NULL"
      kRunRaw "ALTER TABLE companies ADD COLUMN email_font TEXT NULL"
      kRunRaw "ALTER TABLE companies ADD COLUMN email_buttoncolour TEXT NULL"
      kRunRaw "ALTER TABLE companies ADD COLUMN email_emailbackgroundcolour TEXT NULL"
  }

addSignviewBrandingOptions :: MonadDB m => Migration m
addSignviewBrandingOptions =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 9
  , mgrDo = do
      kRunRaw "ALTER TABLE companies ADD COLUMN email_backgroundcolour TEXT NULL"
      kRunRaw "ALTER TABLE companies ADD COLUMN email_textcolour TEXT NULL"
      kRunRaw "ALTER TABLE companies ADD COLUMN email_logo BYTEA NULL"
      kRunRaw "ALTER TABLE companies ADD COLUMN signview_logo BYTEA NULL"
      kRunRaw "ALTER TABLE companies ADD COLUMN signview_textcolour TEXT NULL"
      kRunRaw "ALTER TABLE companies ADD COLUMN signview_textfont TEXT NULL"
      kRunRaw "ALTER TABLE companies ADD COLUMN signview_barscolour TEXT NULL"
      kRunRaw "ALTER TABLE companies ADD COLUMN signview_barstextcolour TEXT NULL"
      kRunRaw "ALTER TABLE companies ADD COLUMN signview_backgroundcolour TEXT NULL"

      kRunRaw "UPDATE companies SET signview_barscolour = bars_background"
      kRunRaw "UPDATE companies SET signview_barstextcolour = bars_textcolour"
      kRunRaw "UPDATE companies SET signview_logo = logo"
      kRunRaw "UPDATE companies SET email_emailbackgroundcolour = bars_background"
      kRunRaw "UPDATE companies SET email_logo = logo"

      kRunRaw "ALTER TABLE companies DROP COLUMN IF EXISTS email_headerfont"
      kRunRaw "ALTER TABLE companies DROP COLUMN bars_background"
      kRunRaw "ALTER TABLE companies DROP COLUMN bars_textcolour"
      kRunRaw "ALTER TABLE companies DROP COLUMN logo"
  }

addCustomBrandingOptions :: MonadDB m => Migration m
addCustomBrandingOptions =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 10
  , mgrDo = do
      kRunRaw "ALTER TABLE companies ADD COLUMN custom_logo BYTEA NULL"
      kRunRaw "ALTER TABLE companies ADD COLUMN custom_barscolour TEXT NULL"
      kRunRaw "ALTER TABLE companies ADD COLUMN custom_barstextcolour TEXT NULL"
      kRunRaw "ALTER TABLE companies ADD COLUMN custom_barssecondarycolour TEXT NULL"
      kRunRaw "ALTER TABLE companies ADD COLUMN custom_backgroundcolour TEXT NULL"
  }

removeEmailDomainFromCompany :: MonadDB m => Migration m
removeEmailDomainFromCompany =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 12
  , mgrDo = do
      kRunRaw "ALTER TABLE companies DROP COLUMN email_domain"
  }
