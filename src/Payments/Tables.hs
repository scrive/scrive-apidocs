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
       ("status",       SqlColDesc { colType     = SqlSmallIntT
                                   , colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE payment_plans ("
          ++ "  account_code BIGSERIAL NOT NULL"
          ++ ", account_type SMALLINT  NOT NULL"          
          ++ ", user_id      BIGINT        NULL"          
          ++ ", company_id   BIGINT        NULL"
          ++ ", plan         SMALLINT  NOT NULL"
          ++ ", status       SMALLINT  NOT NULL"
          ++ ", CONSTRAINT pk_payment_plans PRIMARY KEY (account_code)"
          ++ ", CONSTRAINT un_payment_plans_user_id UNIQUE (user_id)"
          ++ ", CONSTRAINT un_payment_plans_company_id UNIQUE (company_id)"
          ++ ", CONSTRAINT ch_payment_plans_type CHECK (account_type IN (1, 2))" -- 1 User, 2 Company
          ++ ", CONSTRAINT ch_payment_plans_type_id CHECK ((account_type = 1 AND user_id IS NOT NULL AND company_id IS NULL) OR (account_type = 2 AND company_id IS NOT NULL AND user_id IS NULL))"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE payment_plans"
      ++ " ADD CONSTRAINT fk_payment_plans_users FOREIGN KEY(user_id)"
      -- we want the api tokens to disappear when the User disappears
      ++ " REFERENCES users(id) ON UPDATE RESTRICT ON DELETE CASCADE"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE payment_plans"
      ++ " ADD CONSTRAINT fk_payment_plans_companies FOREIGN KEY(company_id)"
      -- we want the api tokens to disappear when the User disappears
      ++ " REFERENCES companies(id) ON UPDATE RESTRICT ON DELETE CASCADE"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }
