module Company.Tables where

import DB

tableCompanies :: Table
tableCompanies = tblTable {
    tblName = "companies"
  , tblVersion = 10
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
       , ("email_headerfont", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("email_font", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("email_buttoncolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("email_emailbackgroundcolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("email_backgroundcolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("email_textcolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("email_logo", SqlColDesc {colType = SqlVarBinaryT, colNullable = Just True})
       , ("signview_logo", SqlColDesc {colType = SqlVarBinaryT, colNullable = Just True})
       , ("signview_textcolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("signview_textfont", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("signview_footertextcolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("signview_footertextfont", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("signview_headertextcolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("signview_headertextfont", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("signview_headerbackgroundcolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("signview_footerbackgroundcolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("signview_backgroundcolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
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
          <> ", email_headerfont   TEXT    NULL"
          <> ", email_font         TEXT    NULL"
          <> ", email_buttoncolour TEXT    NULL"
          <> ", email_emailbackgroundcolour TEXT    NULL"
          <> ", email_backgroundcolour TEXT    NULL"
          <> ", email_textcolour TEXT    NULL"
          <> ", email_logo BYTEA      NULL"
          <> ", signview_logo BYTEA      NULL"
          <> ", signview_textcolour TEXT NULL"
          <> ", signview_textfont TEXT NULL"
          <> ", signview_footertextcolour TEXT NULL"
          <> ", signview_footertextfont TEXT NULL"
          <> ", signview_headertextcolour TEXT NULL"
          <> ", signview_headertextfont TEXT NULL"
          <> ", signview_headerbackgroundcolour TEXT NULL"
          <> ", signview_footerbackgroundcolour TEXT NULL"
          <> ", signview_backgroundcolour TEXT NULL"
          <> ", CONSTRAINT pk_companies PRIMARY KEY (id)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblIndexes = [ tblIndexOnColumn "external_id" ]
  }
