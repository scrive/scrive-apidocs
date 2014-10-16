module Company.Migrations where

import Data.Monoid
import Data.Monoid.Space

import Company.Tables
import DB

addPrimaryAndSecondaryColoursToCompanyUIs :: MonadDB m => Migration m
addPrimaryAndSecondaryColoursToCompanyUIs = Migration {
  mgrTable = tableCompanyUIs
, mgrFrom = 2
, mgrDo = do
    runSQL_ "ALTER TABLE company_uis ADD COLUMN signview_primarycolour TEXT NULL"
    runSQL_ "ALTER TABLE company_uis ADD COLUMN signview_primarytextcolour TEXT NULL"
    runSQL_ "ALTER TABLE company_uis ADD COLUMN signview_secondarycolour TEXT NULL"
    runSQL_ "ALTER TABLE company_uis ADD COLUMN signview_secondarytextcolour TEXT NULL"
}

removeDuplicateIndexFromCompanyUIs :: MonadDB m => Migration m
removeDuplicateIndexFromCompanyUIs = Migration {
  mgrTable = tableCompanyUIs
, mgrFrom = 1
, mgrDo = do
  let Table{..} = tableCompanyUIs
  runQuery_ . sqlDropIndex tblName $ indexOnColumn "company_id"
}

addIPAddressMaskListToCompanies :: MonadDB m => Migration m
addIPAddressMaskListToCompanies = Migration {
    mgrTable = tableCompanies
  , mgrFrom = 7
  , mgrDo = do
    runSQL_ "ALTER TABLE companies ADD COLUMN ip_address_mask_list TEXT NULL"
    runSQL_ "ALTER SEQUENCE companies_id_seq OWNED BY companies.id"
}

removeServiceIDFromCompanies :: MonadDB m => Migration m
removeServiceIDFromCompanies = Migration {
    mgrTable = tableCompanies
  , mgrFrom = 6
  , mgrDo = do
    -- check if service_id field is empty for all companies
    runSQL_ "SELECT DISTINCT service_id IS NULL FROM companies"
    check <- fetchMany unSingle
    case check of
      []     -> return () -- no records, ok
      [True] -> return () -- only nulls, ok
      _      -> error "Companies have rows with non-null service_id"
    runSQL_ "ALTER TABLE companies DROP CONSTRAINT fk_companies_services"
    runSQL_ "DROP INDEX idx_companies_service_id"
    runSQL_ "ALTER TABLE companies DROP COLUMN service_id"
}

removeExternalIDFromCompanies :: MonadDB m => Migration m
removeExternalIDFromCompanies = Migration {
    mgrTable = tableCompanies
  , mgrFrom = 11
  , mgrDo = do
    runSQL_ "DROP INDEX idx_companies_external_id"
    runSQL_ "ALTER TABLE companies DROP COLUMN external_id"
}

addEmailBrandingToCompany :: MonadDB m => Migration m
addEmailBrandingToCompany =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 1
  , mgrDo = do
      runSQL_ "ALTER TABLE companies ADD COLUMN bars_background TEXT NULL"
      runSQL_ "ALTER TABLE companies ADD COLUMN logo BYTEA NULL"
      return ()
  }

addTextColourToEmailBranding :: MonadDB m => Migration m
addTextColourToEmailBranding =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 2
  , mgrDo = runSQL_ "ALTER TABLE companies ADD COLUMN bars_textcolour TEXT NULL"
  }

addIdSerialOnCompanies :: MonadDB m => Migration m
addIdSerialOnCompanies =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 3
  , mgrDo = do
      runSQL_ $ "CREATE SEQUENCE companies_id_seq"
      runSQL_ $ "SELECT setval('companies_id_seq',(SELECT COALESCE(max(id)+1,1000) FROM companies))"
      runSQL_ $ "ALTER TABLE companies ALTER id SET DEFAULT nextval('companies_id_seq')"
  }

addEmailDomainOnCompanies :: MonadDB m => Migration m
addEmailDomainOnCompanies =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 4
  , mgrDo = runSQL_ $ "ALTER TABLE companies ADD COLUMN email_domain TEXT NULL"
  }

addDefaultEmptyStringsToSomeColumnsInCompaniesTable :: MonadDB m => Migration m
addDefaultEmptyStringsToSomeColumnsInCompaniesTable =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 5
  , mgrDo = runSQL_ $ "ALTER TABLE companies"
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
      runSQL_ "ALTER TABLE companies ADD COLUMN email_bordercolour TEXT NULL"
      runSQL_ "ALTER TABLE companies ADD COLUMN email_font TEXT NULL"
      runSQL_ "ALTER TABLE companies ADD COLUMN email_buttoncolour TEXT NULL"
      runSQL_ "ALTER TABLE companies ADD COLUMN email_emailbackgroundcolour TEXT NULL"
  }

addSignviewBrandingOptions :: MonadDB m => Migration m
addSignviewBrandingOptions =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 9
  , mgrDo = do
      runSQL_ "ALTER TABLE companies ADD COLUMN email_backgroundcolour TEXT NULL"
      runSQL_ "ALTER TABLE companies ADD COLUMN email_textcolour TEXT NULL"
      runSQL_ "ALTER TABLE companies ADD COLUMN email_logo BYTEA NULL"
      runSQL_ "ALTER TABLE companies ADD COLUMN signview_logo BYTEA NULL"
      runSQL_ "ALTER TABLE companies ADD COLUMN signview_textcolour TEXT NULL"
      runSQL_ "ALTER TABLE companies ADD COLUMN signview_textfont TEXT NULL"
      runSQL_ "ALTER TABLE companies ADD COLUMN signview_barscolour TEXT NULL"
      runSQL_ "ALTER TABLE companies ADD COLUMN signview_barstextcolour TEXT NULL"
      runSQL_ "ALTER TABLE companies ADD COLUMN signview_backgroundcolour TEXT NULL"

      runSQL_ "UPDATE companies SET signview_barscolour = bars_background"
      runSQL_ "UPDATE companies SET signview_barstextcolour = bars_textcolour"
      runSQL_ "UPDATE companies SET signview_logo = logo"
      runSQL_ "UPDATE companies SET email_emailbackgroundcolour = bars_background"
      runSQL_ "UPDATE companies SET email_logo = logo"

      runSQL_ "ALTER TABLE companies DROP COLUMN IF EXISTS email_headerfont"
      runSQL_ "ALTER TABLE companies DROP COLUMN bars_background"
      runSQL_ "ALTER TABLE companies DROP COLUMN bars_textcolour"
      runSQL_ "ALTER TABLE companies DROP COLUMN logo"
  }

addCustomBrandingOptions :: MonadDB m => Migration m
addCustomBrandingOptions =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 10
  , mgrDo = do
      runSQL_ "ALTER TABLE companies ADD COLUMN custom_logo BYTEA NULL"
      runSQL_ "ALTER TABLE companies ADD COLUMN custom_barscolour TEXT NULL"
      runSQL_ "ALTER TABLE companies ADD COLUMN custom_barstextcolour TEXT NULL"
      runSQL_ "ALTER TABLE companies ADD COLUMN custom_barssecondarycolour TEXT NULL"
      runSQL_ "ALTER TABLE companies ADD COLUMN custom_backgroundcolour TEXT NULL"
  }

removeEmailDomainFromCompany :: MonadDB m => Migration m
removeEmailDomainFromCompany =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 12
  , mgrDo = do
      runSQL_ "ALTER TABLE companies DROP COLUMN email_domain"
  }

moveCompanyUIsToSeparateTable:: MonadDB m => Migration m
moveCompanyUIsToSeparateTable =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 13
  , mgrDo = do
      let columnsToMove = [ "email_bordercolour"
                          , "email_font"
                          , "email_buttoncolour"
                          , "email_emailbackgroundcolour"
                          , "email_backgroundcolour"
                          , "email_textcolour"
                          , "email_logo"
                          , "signview_logo"
                          , "signview_textcolour"
                          , "signview_textfont"
                          , "signview_barscolour"
                          , "signview_barstextcolour"
                          , "signview_backgroundcolour"
                          , "custom_logo"
                          , "custom_barscolour"
                          , "custom_barstextcolour"
                          , "custom_barssecondarycolour"
                          , "custom_backgroundcolour"
                          ]
      runQuery_ . sqlInsertSelect "company_uis" "companies" $ do
          sqlSetCmd "company_id" "companies.id"
          mapM_ (\column -> sqlSetCmd column ("companies." <> column)) columnsToMove

      runQuery_ $ "ALTER TABLE companies" <+> sqlConcatComma (map (\column -> "DROP COLUMN" <+> column) columnsToMove)
  }

addAllowSaveSafetyCopyToCompanies :: MonadDB m => Migration m
addAllowSaveSafetyCopyToCompanies = Migration {
    mgrTable = tableCompanies
  , mgrFrom = 14
  , mgrDo = do
    runSQL_ "ALTER TABLE companies ADD COLUMN allow_save_safety_copy BOOL NOT NULL DEFAULT true"
}

addIdleDocTimeout :: MonadDB m => Migration m
addIdleDocTimeout = Migration {
    mgrTable = tableCompanies
  , mgrFrom = 15
  , mgrDo = do
    runQuery_ $ sqlAlterTable (tblName tableCompanies) [ sqlAddColumn (tblColumn { colName = "idle_doc_timeout", colType = SmallIntT }) ]
}

