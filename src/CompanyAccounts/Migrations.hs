module CompanyAccounts.Migrations where

import DB
import CompanyAccounts.Tables
import DB.SQL2
import Control.Monad
import Data.Maybe
import User.Model
normalizeCompanyInvites :: MonadDB m => Migration m
normalizeCompanyInvites = Migration {
    mgrTable = tableCompanyInvites
  , mgrFrom = 1
  , mgrDo = do
      kRun_ $ sqlSelect "companyinvites as ci LEFT JOIN users as u ON (ci.email = u.email AND u.deleted IS NULL)" $ do
                  sqlResult "u.id, u.company_id,  ci.company_id, ci.email,ci.first_name, ci.last_name"
      values <- kFold (\acc (uid :: Maybe UserID) ucid icid email fstname lstname -> (uid, ucid , icid, email, fstname, lstname) : acc) []
      kRunRaw "DELETE FROM companyinvites"
      kRunRaw "ALTER TABLE companyinvites ADD COLUMN user_id BIGINT NOT NULL"
      kRun_ $ sqlDropPK (tblName tableCompanyInvites)
      kRun_ $ sqlDropIndex (tblName tableCompanyInvites) (indexOnColumn "email")
      kRun_ $ sqlCreateIndex (tblName tableCompanyInvites) (indexOnColumn "user_id")
      kRun_ $ sqlAddPK (tblName tableCompanyInvites) (fromJust $ pkOnColumns ["company_id","user_id"])
      kRun_ $ sqlAddFK (tblName tableCompanyInvites) (fkOnColumn "user_id" "users" "id")
      kRunRaw "ALTER TABLE companyinvites DROP COLUMN email"
      kRunRaw "ALTER TABLE companyinvites DROP COLUMN first_name"
      kRunRaw "ALTER TABLE companyinvites DROP COLUMN last_name"
      forM_ values $ \v -> case v of
        (_       , Just ucid , icid, _    , _      , _      ) | ucid == icid -> do
          -- We drop invitations that were already accepted
          return ()
        (Just uid, Just ucid , icid, _, _, _) | ucid /= icid -> do
              kRun_ $ sqlInsert "attachments" $ do
                sqlSet "company_id" icid
                sqlSet "user_id" uid
        (_ , _ , icid, email, fstname, lstname) -> do -- There should be Nothing,Nothing at the begining
          -- We need user for this invite
          void $ dbUpdate $ AddUser (fstname,lstname) email Nothing (icid,False) LANG_SV Nothing
  }