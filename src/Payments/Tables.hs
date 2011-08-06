module Payments.Tables where

import Database.HDBC

import DB.Classes
import DB.Model

tableUserPaymentPolicies :: Table
tableUserPaymentPolicies = Table {
    tblName = "user_payment_policies"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> wrapDB $ \conn -> do
    case desc of
      [("user_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False}), ("account_type", SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})] -> return TVRvalid
      [] -> do
        runRaw conn $ "CREATE TABLE user_payment_policies ("
          ++ "  user_id BIGINT NOT NULL"
          ++ ", account_type SMALLINT NOT NULL"
          ++ ", CONSTRAINT pk_user_payment_policies PRIMARY KEY (user_id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = wrapDB $ \conn -> do
    runRaw conn $ "ALTER TABLE user_payment_policies"
      ++ " ADD CONSTRAINT fk_user_payment_policies_users FOREIGN KEY(user_id)"
      ++ " REFERENCES users(id) ON DELETE CASCADE ON UPDATE CASCADE"
  }
