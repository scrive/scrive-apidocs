module Company.Tables where

import DB

tableCompanies :: Table
tableCompanies = tblTable {
    tblName = "companies"
  , tblVersion = 11
  , tblCreateOrValidate = \desc -> case desc of
      [  ("id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("external_id", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("name", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("number", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("address", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("zip", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("city", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("country", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("email_domain", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("ip_address_mask_list", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("email_bordercolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("email_font", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("email_buttoncolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("email_emailbackgroundcolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("email_backgroundcolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("email_textcolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("email_logo", SqlColDesc {colType = SqlVarBinaryT, colNullable = Just True})
       , ("signview_logo", SqlColDesc {colType = SqlVarBinaryT, colNullable = Just True})
       , ("signview_textcolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("signview_textfont", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("signview_barscolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("signview_barstextcolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("signview_backgroundcolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("custom_logo", SqlColDesc {colType = SqlVarBinaryT, colNullable = Just True})
       , ("custom_barscolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("custom_barstextcolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("custom_barssecondarycolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("custom_backgroundcolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE companies ("
          <> "  id              BIGSERIAL NOT NULL"
          <> ", external_id     TEXT       NULL"
          <> ", name            TEXT   NOT NULL DEFAULT ''"
          <> ", number          TEXT   NOT NULL DEFAULT ''"
          <> ", address         TEXT   NOT NULL DEFAULT ''"
          <> ", zip             TEXT   NOT NULL DEFAULT ''"
          <> ", city            TEXT   NOT NULL DEFAULT ''"
          <> ", country         TEXT   NOT NULL DEFAULT ''"
          <> ", email_domain    TEXT       NULL"
          <> ", ip_address_mask_list    TEXT       NULL"
          <> ", email_bordercolour TEXT    NULL"
          <> ", email_font         TEXT    NULL"
          <> ", email_buttoncolour TEXT    NULL"
          <> ", email_emailbackgroundcolour TEXT    NULL"
          <> ", email_backgroundcolour TEXT    NULL"
          <> ", email_textcolour TEXT    NULL"
          <> ", email_logo BYTEA      NULL"
          <> ", signview_logo BYTEA      NULL"
          <> ", signview_textcolour TEXT NULL"
          <> ", signview_textfont TEXT NULL"
          <> ", signview_barscolour TEXT NULL"
          <> ", signview_barstextcolour TEXT NULL"
          <> ", signview_backgroundcolour TEXT NULL"
          <> ", custom_logo BYTEA      NULL"
          <> ", custom_barscolour TEXT NULL"
          <> ", custom_barstextcolour TEXT NULL"
          <> ", custom_barssecondarycolour TEXT NULL"
          <> ", custom_backgroundcolour TEXT NULL"
          <> ", CONSTRAINT pk_companies PRIMARY KEY (id)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblIndexes = [ tblIndexOnColumn "external_id" ]
  }
