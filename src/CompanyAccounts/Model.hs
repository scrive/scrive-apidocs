module CompanyAccounts.Model (
    module User.Model
  , module Company.Model
  , CompanyInvite(..)
  , AddCompanyInvite(..)
  , RemoveCompanyInvite(..)
  , RemoveUserCompanyInvites(..)
  , GetCompanyInvite(..)
  , GetCompanyInvites(..)
  , GetCompanyInvitesWithUsersData(..)
  ) where

import Control.Monad.Catch
import Control.Monad.State

import Company.Model
import DB
import KontraPrelude
import User.Model

{- |
    A CompanyInvite is a record
    of an invitation made by a company
    to takeover an existing user.
-}
data CompanyInvite = CompanyInvite {
    inviteduserid   :: UserID
  , invitingcompany :: CompanyID --the company they are invited to
  } deriving (Eq, Ord, Show)

data AddCompanyInvite = AddCompanyInvite CompanyInvite
instance (MonadDB m, MonadThrow m) => DBUpdate m AddCompanyInvite CompanyInvite where
  update (AddCompanyInvite CompanyInvite{..}) = do
    runSQL_ "LOCK TABLE companyinvites IN ACCESS EXCLUSIVE MODE"
    runQuery_ . sqlDelete "companyinvites" $ do
      sqlWhereEq "company_id" invitingcompany
      sqlWhereEq "user_id" inviteduserid
    runQuery_ . sqlInsert "companyinvites" $ do
      sqlSet "user_id" inviteduserid
      sqlSet "company_id" invitingcompany
    $fromJust `liftM` query (GetCompanyInvite invitingcompany inviteduserid)

data RemoveCompanyInvite = RemoveCompanyInvite CompanyID UserID
instance (MonadDB m, MonadThrow m) => DBUpdate m RemoveCompanyInvite Bool where
  update (RemoveCompanyInvite companyid user_id) = do
    runQuery01 . sqlDelete "companyinvites" $ do
      sqlWhereEq "company_id" companyid
      sqlWhereEq "user_id"user_id

data RemoveUserCompanyInvites = RemoveUserCompanyInvites UserID
instance (MonadDB m, MonadThrow m) => DBUpdate m RemoveUserCompanyInvites Bool where
  update (RemoveUserCompanyInvites user_id) = do
    runQuery01 . sqlDelete "companyinvites" $ do
      sqlWhereEq "user_id" user_id

data GetCompanyInvite = GetCompanyInvite CompanyID UserID
instance (MonadDB m, MonadThrow m) => DBQuery m GetCompanyInvite (Maybe CompanyInvite) where
  query (GetCompanyInvite companyid uid) = do
    runQuery_ . selectCompanyInvites $ do
      sqlWhereEq "ci.company_id" companyid
      sqlWhereEq "ci.user_id" uid
    fetchMaybe fetchCompanyInvite

data GetCompanyInvites = GetCompanyInvites CompanyID
instance MonadDB m => DBQuery m GetCompanyInvites [CompanyInvite] where
  query (GetCompanyInvites companyid) = do
    runQuery_ . selectCompanyInvites $ do
      sqlWhereEq "ci.company_id" companyid
    fetchMany fetchCompanyInvite

data GetCompanyInvitesWithUsersData = GetCompanyInvitesWithUsersData CompanyID
instance MonadDB m => DBQuery m GetCompanyInvitesWithUsersData [(CompanyInvite,String,String,String)] where
  query (GetCompanyInvitesWithUsersData companyid) = do
    runQuery_ . sqlSelect2 "companyinvites as i, users as u" $ do
      sqlWhere "i.user_id = u.id"
      sqlWhereEq "i.company_id" companyid
      sqlResult "i.user_id"
      sqlResult "i.company_id"
      sqlResult "u.first_name"
      sqlResult "u.last_name"
      sqlResult "u.email"
    fetchMany $ \(uid, cid, fn, ln, eml) -> (CompanyInvite uid cid, fn, ln, eml)

-- helpers

selectCompanyInvites :: State SqlSelect () -> SqlSelect
selectCompanyInvites refine =
  sqlSelect "companyinvites ci" $ do
    sqlResult "ci.user_id"
    sqlResult "ci.company_id"
    refine

fetchCompanyInvite :: (UserID, CompanyID) -> CompanyInvite
fetchCompanyInvite (uid, cid) = CompanyInvite {
  inviteduserid = uid
, invitingcompany = cid
}
