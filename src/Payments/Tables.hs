module Payments.Tables where

import Database.HDBC

import DB

{- 

  A payment plan relates a User or Company to a Recurly Account_Code
  and remembers the plan they are using.

-}

tablePaymentPlans :: Table
tablePaymentPlans = Table {
  tblName = "payment_plans"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> do
    case desc of
      [("account_code", SqlColDesc { colType     = SqlBigIntT
                                   , colNullable = Just False}),
       ("account_type", SqlColDesc { colType     = SqlSmallIntT
                                   , colNullable = Just False}),
       ("user_id",      SqlColDesc { colType     = SqlBigIntT
                                   , colNullable = Just True}),
       ("company_id",   SqlColDesc { colType     = SqlBigIntT
                                   , colNullable = Just True}),
       ("plan",         SqlColDesc { colType     = SqlSmallIntT
                                   , colNullable = Just False}),
       ("plan_pending", SqlColDesc { colType     = SqlSmallIntT
                                   , colNullable = Just False}),
       ("status",       SqlColDesc { colType     = SqlSmallIntT
                                   , colNullable = Just False}),
       ("status_pending", SqlColDesc { colType     = SqlSmallIntT
                                     , colNullable = Just False}),
       ("quantity",         SqlColDesc { colType = SqlBigIntT
                                       , colSize = Just 4
                                      , colNullable = Just False}),
       ("quantity_pending", SqlColDesc { colType = SqlBigIntT
                                       , colSize = Just 4
                              , colNullable = Just False}),
       ("sync_date", SqlColDesc { colType = SqlTimestampWithZoneT
                               , colNullable = Just False}),
       ("provider", SqlColDesc { colType = SqlSmallIntT
                               , colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE payment_plans ("
          ++ "  account_code BIGSERIAL NOT NULL"
          ++ ", account_type SMALLINT  NOT NULL"          
          ++ ", user_id      BIGINT        NULL"          
          ++ ", company_id   BIGINT        NULL"
          ++ ", plan         SMALLINT  NOT NULL"
          ++ ", plan_pending SMALLINT  NOT NULL"          
          ++ ", status       SMALLINT  NOT NULL"
          ++ ", status_pending SMALLINT  NOT NULL"          
          ++ ", quantity     INTEGER NOT NULL"
          ++ ", quantity_pending INTEGER NOT NULL"
          ++ ", sync_date TIMESTAMPTZ NOT NULL"
          ++ ", provider SMALLINT NOT NULL"
          ++ ", CONSTRAINT pk_payment_plans PRIMARY KEY (account_code)"
          ++ ", CONSTRAINT un_payment_plans_user_id UNIQUE (user_id)"
          ++ ", CONSTRAINT un_payment_plans_company_id UNIQUE (company_id)"
          --- the following constraint implements an Either UserID CompanyID
          ++ ", CONSTRAINT ch_payment_plans_type_id CHECK ((account_type = 1 AND user_id    IS NOT NULL AND company_id IS NULL) OR " -- 1 is UserID
          ++ "                                             (account_type = 2 AND company_id IS NOT NULL AND user_id    IS NULL))"    -- 2 is CompanyID
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE payment_plans"
      ++ " ADD CONSTRAINT fk_payment_plans_users FOREIGN KEY(user_id)"
      ++ " REFERENCES users(id) ON UPDATE RESTRICT ON DELETE NO ACTION"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE payment_plans"
      ++ " ADD CONSTRAINT fk_payment_plans_companies FOREIGN KEY(company_id)"
      ++ " REFERENCES companies(id) ON UPDATE RESTRICT ON DELETE NO ACTION"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

tablePaymentStats :: Table
tablePaymentStats = Table {
  tblName = "payment_stats"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> do
    case desc of
      [("time", SqlColDesc { colType = SqlTimestampWithZoneT
                           , colNullable = Just False}),
       ("provider", SqlColDesc { colType = SqlSmallIntT
                               , colNullable = Just False}),
       ("action", SqlColDesc { colType = SqlSmallIntT
                             , colNullable = Just False}),
       ("quantity",         SqlColDesc { colType = SqlBigIntT
                                       , colSize = Just 4
                                      , colNullable = Just False}),
       ("plan",         SqlColDesc { colType     = SqlSmallIntT
                                   , colNullable = Just False}),
       ("account_type", SqlColDesc { colType     = SqlSmallIntT
                                   , colNullable = Just False}),
       ("user_id",      SqlColDesc { colType     = SqlBigIntT
                                   , colNullable = Just True}),
       ("company_id",   SqlColDesc { colType     = SqlBigIntT
                                   , colNullable = Just True}),
       ("account_code", SqlColDesc { colType     = SqlBigIntT
                                   , colNullable = Just False})       
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE payment_stats ("
          ++ "  time       TIMESTAMPTZ NOT NULL"
          ++ ", provider     SMALLINT  NOT NULL"
          ++ ", action       SMALLINT  NOT NULL"
          ++ ", quantity     INTEGER   NOT NULL"
          ++ ", plan         SMALLINT  NOT NULL"          
          ++ ", account_type SMALLINT  NOT NULL"          
          ++ ", user_id      BIGINT        NULL"          
          ++ ", company_id   BIGINT        NULL"
          ++ ", account_code BIGINT    NOT NULL"
          --- the following constraint implements an Either UserID CompanyID
          ++ ", CONSTRAINT ch_payment_statss_type_id CHECK ((account_type = 1 AND user_id    IS NOT NULL AND company_id IS NULL) OR " -- 1 is UserID
          ++ "                                              (account_type = 2 AND company_id IS NOT NULL AND user_id    IS NULL))"    -- 2 is CompanyID
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE payment_stats"
      ++ " ADD CONSTRAINT fk_payment_stats_users FOREIGN KEY(user_id)"
      ++ " REFERENCES users(id) ON UPDATE RESTRICT ON DELETE NO ACTION"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE payment_stats"
      ++ " ADD CONSTRAINT fk_payment_stats_companies FOREIGN KEY(company_id)"
      ++ " REFERENCES companies(id) ON UPDATE RESTRICT ON DELETE NO ACTION"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }
