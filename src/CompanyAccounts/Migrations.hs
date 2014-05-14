module CompanyAccounts.Migrations where

import Data.Monoid
import DB
import CompanyAccounts.Tables
import Company.CompanyID
import Control.Monad
import Data.Maybe
import User.Model
import Data.Char

normalizeCompanyInvites :: MonadDB m => Migration m
normalizeCompanyInvites = Migration {
    mgrTable = tableCompanyInvites
  , mgrFrom = 1
  , mgrDo = do
      runQuery_ $ sqlSelect "companyinvites as ci LEFT JOIN users as u ON (ci.email = u.email AND u.deleted IS NULL)" $ do
                  sqlResult "u.id, u.company_id,  ci.company_id, ci.email,ci.first_name, ci.last_name"
      values <- fetchMany id
      runSQL_ "DELETE FROM companyinvites"
      runSQL_ "ALTER TABLE companyinvites ADD COLUMN user_id BIGINT NOT NULL"
      runQuery_ $ "ALTER TABLE companyinvites " <> sqlDropPK (tblName tableCompanyInvites)
      runQuery_ $ sqlDropIndex (tblName tableCompanyInvites) (indexOnColumn "email")
      runQuery_ $ sqlCreateIndex (tblName tableCompanyInvites) (indexOnColumn "user_id")
      runQuery_ $ "ALTER TABLE companyinvites " <> sqlAddPK (tblName tableCompanyInvites) (fromJust $ pkOnColumns ["company_id","user_id"])
      runQuery_ $ "ALTER TABLE companyinvites " <> sqlAddFK (tblName tableCompanyInvites) (fkOnColumn "user_id" "users" "id")
      runSQL_ "ALTER TABLE companyinvites DROP COLUMN email"
      runSQL_ "ALTER TABLE companyinvites DROP COLUMN first_name"
      runSQL_ "ALTER TABLE companyinvites DROP COLUMN last_name"
      forM_ values $ \v -> case v of
        (_       , Just ucid , icid, _    , _      , _      ) | ucid == icid -> do
          -- We drop invitations that were already accepted
          return ()
        (Just uid :: Maybe UserID, Just ucid , icid, _, _, _) | ucid /= icid -> do
              runQuery_ . sqlInsert "companyinvites" $ do
                sqlSet "company_id" icid
                sqlSet "user_id" uid
        (_ , _ , icid::CompanyID, email :: String, fstname :: String, lstname :: String) -> do -- There should be Nothing,Nothing at the begining
          -- We need user for this invite
          runQuery_ $ sqlInsertSelect "users" "" $ do
            sqlSet "is_company_admin" False
            sqlSet "account_suspended" False
            sqlSet "signup_method" AccountRequest
            sqlSet "company_id" icid
            sqlSet "first_name" fstname
            sqlSet "last_name" lstname
            sqlSet "personal_number" ("" :: String)
            sqlSet "company_position" ("" :: String)
            sqlSet "phone" ("" :: String)
            sqlSet "email" $ map toLower email
            sqlSet "lang" LANG_SV
            sqlWhereNotExists (sqlSelect "users AS users2" $ sqlWhereEq "users2.email" (map toLower email))
  }
