{-# LANGUAGE NoImplicitPrelude #-}
module CompanyAccounts.Model (
    module User.Model
  , module Company.Model
  , CompanyInvite(..)
  , AddCompanyInvite(..)
  , RemoveCompanyInvite(..)
  , GetCompanyInvite(..)
  , GetCompanyInvites(..)
  , GetCompanyInvitesWithUsersData(..)
  ) where

import Control.Monad

import Company.Model
import DB
import OurPrelude
import User.Model
import DB.SQL2

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
instance MonadDB m => DBUpdate m AddCompanyInvite CompanyInvite where
  update (AddCompanyInvite CompanyInvite{..}) = do
    _ <- kRunRaw "LOCK TABLE companyinvites IN ACCESS EXCLUSIVE MODE"
    kRun_ $ SQL "DELETE FROM companyinvites WHERE (company_id = ? AND user_id = ?)"
            [toSql invitingcompany, toSql inviteduserid]
    kRun_ $ SQL ("INSERT INTO companyinvites ("
      <> "  user_id"
      <> ", company_id) VALUES (?, ?)")
      [ toSql inviteduserid
      , toSql invitingcompany
      ]
    $fromJust `liftM` query (GetCompanyInvite invitingcompany inviteduserid)

data RemoveCompanyInvite = RemoveCompanyInvite CompanyID UserID
instance MonadDB m => DBUpdate m RemoveCompanyInvite Bool where
  update (RemoveCompanyInvite companyid user_id) = do
    kRun01 $ SQL "DELETE FROM companyinvites WHERE (company_id = ? AND user_id = ?)"
             [toSql companyid, toSql user_id]

data GetCompanyInvite = GetCompanyInvite CompanyID UserID
instance MonadDB m => DBQuery m GetCompanyInvite (Maybe CompanyInvite) where
  query (GetCompanyInvite companyid uid) = do
    kRun_ $ SQL (selectCompanyInvitesSQL <> "WHERE (ci.company_id = ? AND ci.user_id = ?)")
            [toSql companyid, toSql uid]
    fetchCompanyInvites >>= oneObjectReturnedGuard

data GetCompanyInvites = GetCompanyInvites CompanyID
instance MonadDB m => DBQuery m GetCompanyInvites [CompanyInvite] where
  query (GetCompanyInvites companyid) = do
    kRun_ $ SQL (selectCompanyInvitesSQL <> "WHERE (ci.company_id = ?)")
            [toSql companyid]
    fetchCompanyInvites

data GetCompanyInvitesWithUsersData = GetCompanyInvitesWithUsersData CompanyID
instance MonadDB m => DBQuery m GetCompanyInvitesWithUsersData [(CompanyInvite,String,String,String)] where
  query (GetCompanyInvitesWithUsersData companyid) = do
    kRun_ $ sqlSelect2 "companyinvites as i, users as u" $ do
      sqlWhere "i.user_id = u.id"
      sqlWhereEq "i.company_id" companyid
      sqlResult "i.user_id"
      sqlResult "i.company_id"
      sqlResult "u.first_name"
      sqlResult "u.last_name"
      sqlResult "u.email"
    kFold decode []
   where
    decode acc uid cid  fn ln eml = (CompanyInvite uid cid, fn, ln,eml) : acc


-- helpers
selectCompanyInvitesSQL :: RawSQL
selectCompanyInvitesSQL = "SELECT"
  <> "  ci.user_id"
  <> ", ci.company_id"
  <> "  FROM companyinvites ci"
  <> " "

fetchCompanyInvites :: MonadDB m => m [CompanyInvite]
fetchCompanyInvites = kFold decoder []
  where
    decoder acc uid cid = CompanyInvite {
        inviteduserid = uid
      , invitingcompany = cid
      } : acc
