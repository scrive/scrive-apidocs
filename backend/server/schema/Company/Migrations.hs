module Company.Migrations where

import Control.Monad.Catch
import Data.Char
import Data.Int
import Data.String.Utils
import Text.Regex.TDFA
import qualified Data.ByteString as BS

import Company.Tables
import DB
import KontraPrelude

addThemesAndOthersToCompanyUIs :: (MonadDB m,MonadThrow m) => Migration m
addThemesAndOthersToCompanyUIs = Migration {
  mgrTable = tableCompanyUIs
, mgrFrom = 3
, mgrDo = do
    runSQL_ "ALTER TABLE company_uis ADD COLUMN mail_theme BIGINT NULL"
    runSQL_ "ALTER TABLE company_uis ADD COLUMN signview_theme BIGINT NULL"
    runSQL_ "ALTER TABLE company_uis ADD COLUMN service_theme BIGINT NULL"

    runQuery_ $ sqlAlterTable "company_uis" [sqlAddFK "company_uis" $  (fkOnColumn "mail_theme" "themes" "id")]
    runQuery_ $ sqlAlterTable "company_uis" [sqlAddFK "company_uis" $  (fkOnColumn "signview_theme" "themes" "id")]
    runQuery_ $ sqlAlterTable "company_uis" [sqlAddFK "company_uis" $  (fkOnColumn "service_theme" "themes" "id")]

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
     tnegativecolor,tnegativetextcolor,tfont) :: (BS.ByteString,String,String,String,String,String,String,String,String,String,String,String)) <- fetchOne id

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
    company_uis :: [(Int64, Maybe BS.ByteString,Maybe String,Maybe String,Maybe String)] <- fetchMany id
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
      (themeid :: Int64) <- fetchOne runIdentity
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
     tnegativecolor,tnegativetextcolor,tfont) :: (BS.ByteString,String,String,String,String,String,String,String,String,String,String,String)) <- fetchOne id

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
    company_uis :: [(Int64, Maybe BS.ByteString,Maybe String,Maybe String,Maybe String,Maybe String,Maybe String,Maybe String,Maybe String)] <- fetchMany id
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
      (themeid :: Int64) <- fetchOne runIdentity
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
     tnegativecolor,tnegativetextcolor,tfont) :: (BS.ByteString,String,String,String,String,String,String,String,String,String,String,String)) <- fetchOne id

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

    company_uis :: [(Int64, Maybe BS.ByteString,Maybe String,Maybe String)] <- fetchMany id
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
      (themeid :: Int64) <- fetchOne runIdentity
      runQuery_ $ sqlInsert "theme_owners" $ do
        sqlSet "company_id" $ cid
        sqlSet "theme_id" $ themeid
      runQuery_ $ sqlUpdate "company_uis" $ do
        sqlSet "service_theme" $ themeid
        sqlWhereEq "company_id" $ cid

-- Utils for themes migration
fromLogo :: Maybe BS.ByteString -> BS.ByteString -> BS.ByteString
fromLogo mclogo tlogo =  case mclogo of
  Nothing -> tlogo
  Just clogo -> if (BS.null clogo)
                  then tlogo
                  else clogo

fromColor :: Maybe String -> String -> String
fromColor Nothing template = template
fromColor (Just color) template
  | norm ==~ "[a-z]+" = named
  | norm ==~ "#[0-9a-f]{3}" = three
  | norm ==~ "#[0-9a-f]{6}" = norm
  | otherwise = template
  where (==~) s (p :: String) = (s =~ p) == s
        norm = map toLower $ strip color
        three = '#':[norm !! 1, norm !! 1, norm !! 2, norm !! 2, norm !! 3, norm !! 3]
        named = case norm of -- http://www.w3.org/TR/CSS21/syndata.html#value-def-color
          "maroon" -> "#800000"
          "red" -> "#ff0000"
          "orange" -> "#ffa500"
          "yellow" -> "#ffff00"
          "olive" -> "#808000"
          "purple" -> "#800080"
          "fuchsia" -> "#ff00ff"
          "white" -> "#ffffff"
          "lime" -> "#00ff00"
          "green" -> "#008000"
          "navy" -> "#000080"
          "blue" -> "#0000ff"
          "aqua" -> "#00ffff"
          "teal" -> "#008080"
          "black" -> "#000000"
          "silver" -> "#c0c0c0"
          "gray" -> "#808080"
          _ -> template

-- | WARNING: when adding fonts make sure standard fonts always return themselves.
-- | i.e fromFont (Just "arial,helvetica,sans-serif") "" == "arial,helvetica,sans-serif".
fromFont :: Maybe String -> String -> String
fromFont Nothing template = template
fromFont (Just font) template
  | norm ==~ "arial black" = "\"arial black\",sans-serif"
  | norm ==~ "narrow" = "\"arial narrow\",sans-serif"
  | norm ==~ "comic" = "\"comic sans ms\",sans-serif"
  | norm ==~ "courier" = "\"courier new\",monospace"
  | norm ==~ "source" = "\"Source Sans Pro\", \"Helvetica Neue\", Arial, sans-serif"
  | norm ==~ "garamond" = "garamond,serif"
  | norm ==~ "georgia" = "georgia,serif"
  | norm ==~ "times" = "\"times new roman\",serif"
  | norm ==~ "tahoma" = "tahoma,sans-serif"
  | norm ==~ "trebuchet" = "\"trebuchet ms\",sans-serif"
  | norm ==~ "verdana" = "verdana,sans-serif"
  | norm ==~ "arial" = "arial,helvetica,sans-serif"
  | norm ==~ "helvetica" = "helvetica,sans-serif"
  | otherwise = template
  where (==~) s (p :: String) = (s =~ p)
        norm = map toLower $ strip font

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

addSMSProviderToCompanies :: MonadDB m => Migration m
addSMSProviderToCompanies =
  Migration {
    mgrTable = tableCompanies
  , mgrFrom = 18
  , mgrDo = do
      runSQL_ "ALTER TABLE companies ADD COLUMN sms_provider SMALLINT NOT NULL DEFAULT 1"
  }

companiesAddCgiServiceID :: MonadDB m => Migration m
companiesAddCgiServiceID = Migration {
  mgrTable = tableCompanies
, mgrFrom = 19
, mgrDo = runQuery_ $ sqlAlterTable (tblName tableCompanies) [
    sqlAddColumn $ tblColumn { colName = "cgi_service_id", colType = TextT }
  ]
}

companiesAddPartnerID :: (MonadThrow m, MonadDB m) => Migration m
companiesAddPartnerID = Migration {
  mgrTable = tableCompanies
, mgrFrom = 20
, mgrDo = do
    runQuery_ $ sqlAlterTable (tblName tableCompanies)
                              [ sqlAddColumn $ tblColumn
                                                 { colName = "partner_id"
                                                 , colType = BigIntT
                                                 , colNullable = True
                                                 }
                              , sqlAddFK (tblName tableCompanies) $
                                    (fkOnColumn "partner_id" "partners" "id" )
                                      { fkOnDelete = ForeignKeySetNull } ]
    runSQL_ "UPDATE companies SET partner_id = (SELECT partners.id FROM partners WHERE partners.default_partner) WHERE partner_id IS NULL"
    runSQL_ "ALTER TABLE companies ALTER partner_id SET NOT NULL"
}

companiesAddPaymentPlan :: (MonadThrow m, MonadDB m) => Migration m
companiesAddPaymentPlan = Migration {
  mgrTable = tableCompanies
, mgrFrom = 22
, mgrDo = do
    runQuery_ $ sqlAlterTable (tblName tableCompanies)  [ sqlAddColumn $
        tblColumn { colName = "payment_plan", colType = SmallIntT, colNullable = True, colDefault = Just "0"}
      ]
    -- Migrate One plan to One plan
    runSQL_ "UPDATE companies SET payment_plan = 1 WHERE companies.id IN (SELECT company_id FROM payment_plans WHERE plan = 6)"
    -- Migrate Team plan to Team plan
    runSQL_ "UPDATE companies SET payment_plan = 2 WHERE companies.id IN (SELECT company_id FROM payment_plans WHERE plan = 1)"
    -- Migrate Form, Company and Enterprise plans to Enterprise plan
    runSQL_ "UPDATE companies SET payment_plan = 3 WHERE companies.id IN (SELECT company_id FROM payment_plans WHERE plan = 2 OR plan = 3 OR plan = 5)"
    -- Migrate Trial plan to Trial plan
    runSQL_ "UPDATE companies SET payment_plan = 4 WHERE companies.id IN (SELECT company_id FROM payment_plans WHERE plan = 4)"
    runSQL_ "ALTER TABLE companies ALTER COLUMN payment_plan SET NOT NULL"
}

companiesAddPadAppModeAndEArchiveEnabled :: MonadDB m => Migration m
companiesAddPadAppModeAndEArchiveEnabled = Migration {
  mgrTable = tableCompanies
, mgrFrom = 21
, mgrDo = runQuery_ $ sqlAlterTable (tblName tableCompanies) [
    sqlAddColumn $ tblColumn { colName = "pad_app_mode", colType = SmallIntT, colNullable = False, colDefault = Just "1" }
  , sqlAddColumn $ tblColumn { colName = "pad_earchive_enabled", colType = BoolT, colNullable = False, colDefault = Just "true" }
  ]
}
