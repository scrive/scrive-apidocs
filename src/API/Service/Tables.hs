module API.Service.Tables where

import DB

tableServices :: Table
tableServices = Table {
    tblName = "services"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
         [  ("id", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
          , ("password", SqlColDesc {colType = SqlVarBinaryT, colNullable = Just True})
          , ("salt", SqlColDesc {colType = SqlVarBinaryT, colNullable = Just True})
          , ("admin_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
          , ("location", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
          , ("email_from_address", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
          , ("mail_footer", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
          , ("button1", SqlColDesc {colType = SqlVarBinaryT, colNullable = Just True})
          , ("button2", SqlColDesc {colType = SqlVarBinaryT, colNullable = Just True})
          , ("buttons_text_color", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
          , ("background", SqlColDesc {colType = SqlVarCharT, colNullable = Just True}), ("overlay_background", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
          , ("bars_background", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
          , ("logo", SqlColDesc {colType = SqlVarBinaryT, colNullable = Just True})
          ] -> return TVRvalid
         [] -> do
           kRunRaw $ "CREATE TABLE services ("
             ++ "  id TEXT NOT NULL"
             ++ ", password BYTEA NULL"
             ++ ", salt BYTEA NULL"
             ++ ", admin_id BIGINT NOT NULL"
             ++ ", location TEXT NULL"
             ++ ", email_from_address TEXT NULL"
             ++ ", mail_footer TEXT NULL"
             ++ ", button1 BYTEA NULL"
             ++ ", button2 BYTEA NULL"
             ++ ", buttons_text_color TEXT NULL"
             ++ ", background TEXT NULL"
             ++ ", overlay_background TEXT NULL"
             ++ ", bars_background TEXT NULL"
             ++ ", logo BYTEA NULL"
             ++ ", CONSTRAINT pk_services PRIMARY KEY (id)"
             ++ ", CONSTRAINT idx_services_location UNIQUE (location)"
             ++ ")"
           return TVRcreated
         _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw "CREATE INDEX idx_services_admin_id ON services(admin_id)"
    kRunRaw $ "ALTER TABLE services"
      ++ " ADD CONSTRAINT fk_services_users FOREIGN KEY (admin_id)"
      ++ " REFERENCES users(id) ON DELETE RESTRICT ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }
