module Company.Migrations where

import Control.Monad
import Control.Monad.Catch
import Data.Int
import Data.Monoid
import Data.Monoid.Space
import qualified Data.ByteString as BS

import Company.Tables
import DB
import Utils.Color
import Utils.Font

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

addThemesAndOthersToCompanyUIs :: (MonadDB m,MonadThrow m) => Migration m
addThemesAndOthersToCompanyUIs = Migration {
  mgrTable = tableCompanyUIs
, mgrFrom = 3
, mgrDo = do
    runSQL_ "ALTER TABLE company_uis ADD COLUMN mail_theme BIGINT NULL"
    runSQL_ "ALTER TABLE company_uis ADD COLUMN signview_theme BIGINT NULL"
    runSQL_ "ALTER TABLE company_uis ADD COLUMN service_theme BIGINT NULL"
    runSQL_ $ "ALTER TABLE company_uis  ADD CONSTRAINT fk__company_uis__mail_theme__themes FOREIGN KEY (mail_theme)  "
           <> "REFERENCES themes (id) MATCH SIMPLE  "
           <> "ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE INITIALLY IMMEDIATE"
    runSQL_ $ "ALTER TABLE company_uis  ADD CONSTRAINT fk__company_uis__signview_theme__themes FOREIGN KEY (signview_theme)  "
           <> "REFERENCES themes (id) MATCH SIMPLE  "
           <> "ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE INITIALLY IMMEDIATE"
    runSQL_ $ "ALTER TABLE company_uis  ADD CONSTRAINT fk__company_uis__service_theme__themes FOREIGN KEY (service_theme)  "
           <> "REFERENCES themes (id) MATCH SIMPLE  "
           <> "ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE INITIALLY IMMEDIATE"
    runSQL_ "ALTER TABLE company_uis ADD COLUMN browser_title TEXT NULL"
    runSQL_ "ALTER TABLE company_uis ADD COLUMN sms_originator TEXT NULL"
    runSQL_ "ALTER TABLE company_uis ADD COLUMN favicon BYTEA NULL"

    runSQL_ $ "UPDATE company_uis"
          <+> "SET sms_originator = companies.sms_originator"
          <+> "FROM companies"
          <+> "WHERE companies.id = company_uis.company_id"

    migrateCompanyUIEmailSettingsToTheme
    migrateCompanyUISignviewSettingsToTheme
    migrateCompanyUIServiceSettingsToTheme

    runSQL_ "ALTER TABLE company_uis DROP COLUMN email_bordercolour"
    runSQL_ "ALTER TABLE company_uis DROP COLUMN email_font"
    runSQL_ "ALTER TABLE company_uis DROP COLUMN email_buttoncolour"
    runSQL_ "ALTER TABLE company_uis DROP COLUMN email_emailbackgroundcolour"
    runSQL_ "ALTER TABLE company_uis DROP COLUMN email_backgroundcolour"
    runSQL_ "ALTER TABLE company_uis DROP COLUMN email_textcolour"
    runSQL_ "ALTER TABLE company_uis DROP COLUMN email_logo"
    runSQL_ "ALTER TABLE company_uis DROP COLUMN signview_logo"
    runSQL_ "ALTER TABLE company_uis DROP COLUMN signview_textcolour"
    runSQL_ "ALTER TABLE company_uis DROP COLUMN signview_textfont"
    runSQL_ "ALTER TABLE company_uis DROP COLUMN signview_barscolour"
    runSQL_ "ALTER TABLE company_uis DROP COLUMN signview_barstextcolour"
    runSQL_ "ALTER TABLE company_uis DROP COLUMN signview_backgroundcolour"
    runSQL_ "ALTER TABLE company_uis DROP COLUMN custom_logo"
    runSQL_ "ALTER TABLE company_uis DROP COLUMN custom_barscolour"
    runSQL_ "ALTER TABLE company_uis DROP COLUMN custom_barstextcolour"
    runSQL_ "ALTER TABLE company_uis DROP COLUMN custom_barssecondarycolour"
    runSQL_ "ALTER TABLE company_uis DROP COLUMN custom_backgroundcolour"
    runSQL_ "ALTER TABLE company_uis DROP COLUMN signview_primarycolour"
    runSQL_ "ALTER TABLE company_uis DROP COLUMN signview_primarytextcolour"
    runSQL_ "ALTER TABLE company_uis DROP COLUMN signview_secondarycolour"
    runSQL_ "ALTER TABLE company_uis DROP COLUMN signview_secondarytextcolour"
}

migrateCompanyUIEmailSettingsToTheme:: (MonadDB m,MonadThrow m) => m ()
migrateCompanyUIEmailSettingsToTheme = do
    runQuery_ $ sqlSelect "themes" $ do
      sqlWhereExists $ sqlSelect "branded_domains as d" $ do
          sqlWhere "d.mail_theme = themes.id"
          sqlWhere "d.main_domain"
      sqlResult "logo"
      sqlResult "brand_color"
      sqlResult "brand_text_color"
      sqlResult "action_color"
      sqlResult "action_text_color"
      sqlResult "action_secondary_color"
      sqlResult "action_secondary_text_color"
      sqlResult "positive_color"
      sqlResult "positive_text_color"
      sqlResult "negative_color"
      sqlResult "negative_text_color"
      sqlResult "font"
    ((tlogo,tbrandcolor,tbrandtextcolor,
     tactioncolor,tactiontextcolor,tactionsecondarycolor,
     tactionsecondarytextcolor,tpositivecolor,tpositivetextcolor,
     tnegativecolor,tnegativetextcolor,tfont) :: (Binary BS.ByteString,String,String,String,String,String,String,String,String,String,String,String)) <- fetchOne id

    runQuery_ $ sqlSelect "company_uis" $ do
                 sqlWhereAny [
                    sqlWhere "email_logo IS NOT NULL AND email_logo != ''::bytea"
                  , sqlWhere "email_emailbackgroundcolour IS NOT NULL AND email_emailbackgroundcolour != ''"
                  , sqlWhere "email_buttoncolour IS NOT NULL AND email_buttoncolour != ''"
                  , sqlWhere "email_font IS NOT NULL AND email_font != ''"
                  ]
                 sqlResult "company_id"
                 sqlResult "email_logo"
                 sqlResult "email_emailbackgroundcolour"
                 sqlResult "email_buttoncolour"
                 sqlResult "email_font"
    company_uis :: [(Int64, Maybe (Binary BS.ByteString),Maybe String,Maybe String,Maybe String)] <- fetchMany id
    forM_ company_uis $ \(cid,mclogo,mcbackground,mcbutton,mcfont) -> do
      runQuery_ $ sqlInsert "themes" $ do
        sqlSet "name" $ ("Email" ::String)
        sqlSet "logo" $ fromLogo mclogo tlogo
        sqlSet "brand_color" $ fromColor mcbackground tbrandcolor
        sqlSet "brand_text_color" $ tbrandtextcolor
        sqlSet "action_color" $ fromColor mcbutton tactioncolor
        sqlSet "action_text_color" $ tactiontextcolor
        sqlSet "action_secondary_color" $ tactionsecondarycolor
        sqlSet "action_secondary_text_color" $ tactionsecondarytextcolor
        sqlSet "positive_color" $ tpositivecolor
        sqlSet "positive_text_color" $ tpositivetextcolor
        sqlSet "negative_color" $ tnegativecolor
        sqlSet "negative_text_color" $ tnegativetextcolor
        sqlSet "font" $ fromFont  mcfont tfont
        sqlResult "id"
      (themeid :: Int64) <- fetchOne unSingle
      runQuery_ $ sqlInsert "theme_owners" $ do
        sqlSet "company_id" $ cid
        sqlSet "theme_id" $ themeid
      runQuery_ $ sqlUpdate "company_uis" $ do
        sqlSet "mail_theme" $ themeid
        sqlWhereEq "company_id" $ cid


migrateCompanyUISignviewSettingsToTheme:: (MonadDB m,MonadThrow m) => m ()
migrateCompanyUISignviewSettingsToTheme = do
    runQuery_ $ sqlSelect "themes" $ do
      sqlWhereExists $ sqlSelect "branded_domains as d" $ do
          sqlWhere "d.signview_theme = themes.id"
          sqlWhere "d.main_domain"
      sqlResult "logo"
      sqlResult "brand_color"
      sqlResult "brand_text_color"
      sqlResult "action_color"
      sqlResult "action_text_color"
      sqlResult "action_secondary_color"
      sqlResult "action_secondary_text_color"
      sqlResult "positive_color"
      sqlResult "positive_text_color"
      sqlResult "negative_color"
      sqlResult "negative_text_color"
      sqlResult "font"
    ((tlogo,tbrandcolor,tbrandtextcolor,
     tactioncolor,tactiontextcolor,tactionsecondarycolor,
     tactionsecondarytextcolor,tpositivecolor,tpositivetextcolor,
     tnegativecolor,tnegativetextcolor,tfont) :: (Binary BS.ByteString,String,String,String,String,String,String,String,String,String,String,String)) <- fetchOne id

    runQuery_ $ sqlSelect "company_uis" $ do
                 sqlWhereAny [
                    sqlWhere "signview_logo IS NOT NULL AND signview_logo != ''::bytea"
                  , sqlWhere "signview_barscolour IS NOT NULL AND signview_barscolour != ''"
                  , sqlWhere "signview_barstextcolour IS NOT NULL AND signview_barstextcolour != ''"
                  , sqlWhere "signview_primarycolour IS NOT NULL AND signview_primarycolour != ''"
                  , sqlWhere "signview_primarytextcolour IS NOT NULL AND signview_primarytextcolour != ''"
                  , sqlWhere "signview_secondarycolour IS NOT NULL AND signview_secondarycolour != ''"
                  , sqlWhere "signview_secondarytextcolour IS NOT NULL AND signview_secondarytextcolour != ''"
                  , sqlWhere "signview_textfont IS NOT NULL AND signview_textfont != ''"
                  ]
                 sqlResult "company_id"
                 sqlResult "signview_logo"
                 sqlResult "signview_barscolour"
                 sqlResult "signview_barstextcolour"
                 sqlResult "signview_primarycolour"
                 sqlResult "signview_primarytextcolour"
                 sqlResult "signview_secondarycolour"
                 sqlResult "signview_secondarytextcolour"
                 sqlResult "signview_textfont"
    company_uis :: [(Int64, Maybe (Binary BS.ByteString),Maybe String,Maybe String,Maybe String,Maybe String,Maybe String,Maybe String,Maybe String)] <- fetchMany id
    forM_ company_uis $ \(cid,mclogo,mcbarscolor,mcbarstextcolor,mcactioncolor,mcactiontextscolor,mcactionsecondarycolor,mcactionsecondarytextcolor,mcfont) -> do
      runQuery_ $ sqlInsert "themes" $ do
        sqlSet "name" $ ("Signing page" ::String)
        sqlSet "logo" $ fromLogo mclogo tlogo
        sqlSet "brand_color" $ fromColor mcbarscolor tbrandcolor
        sqlSet "brand_text_color" $ fromColor mcbarstextcolor tbrandtextcolor
        sqlSet "action_color" $ fromColor mcactioncolor tactioncolor
        sqlSet "action_text_color" $ fromColor mcactiontextscolor tactiontextcolor
        sqlSet "action_secondary_color" $ fromColor mcactionsecondarycolor tactionsecondarycolor
        sqlSet "action_secondary_text_color" $ fromColor mcactionsecondarytextcolor tactionsecondarytextcolor
        sqlSet "positive_color" $ tpositivecolor
        sqlSet "positive_text_color" $ tpositivetextcolor
        sqlSet "negative_color" $ tnegativecolor
        sqlSet "negative_text_color" $ tnegativetextcolor
        sqlSet "font" $ fromFont  mcfont tfont
        sqlResult "id"
      (themeid :: Int64) <- fetchOne unSingle
      runQuery_ $ sqlInsert "theme_owners" $ do
        sqlSet "company_id" $ cid
        sqlSet "theme_id" $ themeid
      runQuery_ $ sqlUpdate "company_uis" $ do
        sqlSet "signview_theme" $ themeid
        sqlWhereEq "company_id" $ cid


migrateCompanyUIServiceSettingsToTheme:: (MonadDB m,MonadThrow m) => m ()
migrateCompanyUIServiceSettingsToTheme = do
    runQuery_ $ sqlSelect "themes" $ do
      sqlWhereExists $ sqlSelect "branded_domains as d" $ do
          sqlWhere "d.service_theme = themes.id"
          sqlWhere "d.main_domain"
      sqlResult "logo"
      sqlResult "brand_color"
      sqlResult "brand_text_color"
      sqlResult "action_color"
      sqlResult "action_text_color"
      sqlResult "action_secondary_color"
      sqlResult "action_secondary_text_color"
      sqlResult "positive_color"
      sqlResult "positive_text_color"
      sqlResult "negative_color"
      sqlResult "negative_text_color"
      sqlResult "font"
    ((tlogo,tbrandcolor,tbrandtextcolor,
     tactioncolor,tactiontextcolor,tactionsecondarycolor,
     tactionsecondarytextcolor,tpositivecolor,tpositivetextcolor,
     tnegativecolor,tnegativetextcolor,tfont) :: (Binary BS.ByteString,String,String,String,String,String,String,String,String,String,String,String)) <- fetchOne id

    runQuery_ $ sqlSelect "company_uis" $ do
                 sqlWhereAny [
                    sqlWhere "custom_logo IS NOT NULL AND custom_logo != ''::bytea"
                  , sqlWhere "custom_barscolour IS NOT NULL AND custom_barscolour != ''"
                  , sqlWhere "custom_barstextcolour IS NOT NULL AND custom_barstextcolour != ''"
                  ]
                 sqlResult "company_id"
                 sqlResult "custom_logo"
                 sqlResult "custom_barscolour"
                 sqlResult "custom_barstextcolour"

    company_uis :: [(Int64, Maybe (Binary BS.ByteString),Maybe String,Maybe String)] <- fetchMany id
    forM_ company_uis $ \(cid,mclogo,mcbarscolor,mcbarstextcolor) -> do
      runQuery_ $ sqlInsert "themes" $ do
        sqlSet "name" $ ("Service" ::String)
        sqlSet "logo" $ fromLogo mclogo tlogo
        sqlSet "brand_color" $ fromColor mcbarscolor tbrandcolor
        sqlSet "brand_text_color" $ fromColor mcbarstextcolor tbrandtextcolor
        sqlSet "action_color" $ tactioncolor
        sqlSet "action_text_color" $ tactiontextcolor
        sqlSet "action_secondary_color" $ tactionsecondarycolor
        sqlSet "action_secondary_text_color" $ tactionsecondarytextcolor
        sqlSet "positive_color" $ tpositivecolor
        sqlSet "positive_text_color" $ tpositivetextcolor
        sqlSet "negative_color" $ tnegativecolor
        sqlSet "negative_text_color" $ tnegativetextcolor
        sqlSet "font" $ tfont
        sqlResult "id"
      (themeid :: Int64) <- fetchOne unSingle
      runQuery_ $ sqlInsert "theme_owners" $ do
        sqlSet "company_id" $ cid
        sqlSet "theme_id" $ themeid
      runQuery_ $ sqlUpdate "company_uis" $ do
        sqlSet "service_theme" $ themeid
        sqlWhereEq "company_id" $ cid

-- Utils for themes migration
fromLogo :: Maybe (Binary BS.ByteString) -> (Binary BS.ByteString) -> (Binary BS.ByteString)
fromLogo mclogo tlogo =  case mclogo of
  Nothing -> tlogo
  Just clogo -> if (BS.null $ unBinary clogo)
                  then tlogo
                  else clogo

fromColor :: Maybe String -> String -> String
fromColor mccolor tcolor =  case (mccolor) of
  Nothing ->  tcolor
  Just ccolor -> if (isValidColor ccolor)
                   then ccolor
                   else tcolor

fromFont :: Maybe String -> String -> String
fromFont mcfont tfont =  case (mcfont) of
  Nothing ->  tfont
  Just cfont -> if (isValidFont cfont)
                   then cfont
                   else tfont

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

companiesAddCgiDisplayName :: MonadDB m => Migration m
companiesAddCgiDisplayName = Migration {
  mgrTable = tableCompanies
, mgrFrom = 16
, mgrDo = runQuery_ $ sqlAlterTable (tblName tableCompanies) [
    sqlAddColumn $ tblColumn { colName = "cgi_display_name", colType = TextT }
  ]
}
removeSMSOriginatorFromCompany:: MonadDB m => Migration m
removeSMSOriginatorFromCompany =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 17
  , mgrDo = do
     runSQL_ $ "ALTER TABLE companies DROP COLUMN sms_originator"
  }
