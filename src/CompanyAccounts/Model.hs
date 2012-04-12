module CompanyAccounts.Model (
    module User.Model
  , module Company.Model
  , CompanyInvite(..)
  , AddCompanyInvite(..)
  , RemoveCompanyInvite(..)
  , GetCompanyInvite(..)
  , GetCompanyInvites(..)
  ) where

import Data.Monoid
import Database.HDBC
import qualified Control.Exception as E

import Company.Model
import DB.Classes
import DB.Fetcher2
import DB.Utils
import User.Model

{- |
    A CompanyInvite is a record
    of an invitation made by a company
    to takeover an existing user.
-}
data CompanyInvite = CompanyInvite {
    invitedemail    :: Email --who was invited
  , invitedfstname  :: String --the fstname they were invited as
  , invitedsndname  :: String --the sndname they were invited as
  , invitingcompany :: CompanyID --the company they are invited to
  } deriving (Eq, Ord, Show)

data AddCompanyInvite = AddCompanyInvite CompanyInvite
instance MonadDB m => DBUpdate m AddCompanyInvite CompanyInvite where
  update (AddCompanyInvite CompanyInvite{..}) = do
    _ <- kRunRaw "LOCK TABLE companyinvites IN ACCESS EXCLUSIVE MODE"
    kPrepare "DELETE FROM companyinvites WHERE (company_id = ? AND email = ?)"
    _ <- kExecute [toSql invitingcompany, toSql invitedemail]
    kPrepare $ "INSERT INTO companyinvites ("
      ++ "  email"
      ++ ", first_name"
      ++ ", last_name"
      ++ ", company_id) VALUES (?, ?, ?, ?)"
    _ <- kExecute [
        toSql invitedemail
      , toSql invitedfstname
      , toSql invitedsndname
      , toSql invitingcompany
      ]
    query (GetCompanyInvite invitingcompany invitedemail)
      >>= maybe (E.throw $ NoObject mempty) return

data RemoveCompanyInvite = RemoveCompanyInvite CompanyID Email
instance MonadDB m => DBUpdate m RemoveCompanyInvite Bool where
  update (RemoveCompanyInvite companyid email) = do
    kPrepare "DELETE FROM companyinvites WHERE (company_id = ? AND email = ?)"
    kExecute01 [toSql companyid, toSql email]

data GetCompanyInvite = GetCompanyInvite CompanyID Email
instance MonadDB m => DBQuery m GetCompanyInvite (Maybe CompanyInvite) where
  query (GetCompanyInvite companyid email) = do
    kPrepare $ selectCompanyInvitesSQL ++ "WHERE (ci.company_id = ? AND ci.email = ?)"
    _ <- kExecute [toSql companyid, toSql email]
    fetchCompanyInvites >>= oneObjectReturnedGuard

data GetCompanyInvites = GetCompanyInvites CompanyID
instance MonadDB m => DBQuery m GetCompanyInvites [CompanyInvite] where
  query (GetCompanyInvites companyid) = do
    kPrepare $ selectCompanyInvitesSQL ++ "WHERE (ci.company_id = ?)"
    _ <- kExecute [toSql companyid]
    fetchCompanyInvites

-- helpers
selectCompanyInvitesSQL :: String
selectCompanyInvitesSQL = "SELECT"
  ++ "  ci.email"
  ++ ", ci.first_name"
  ++ ", ci.last_name"
  ++ ", ci.company_id"
  ++ "  FROM companyinvites ci"
  ++ " "

fetchCompanyInvites :: MonadDB m => DBEnv m [CompanyInvite]
fetchCompanyInvites = foldDB decoder []
  where
    decoder acc email fstname sndname cid = CompanyInvite {
        invitedemail = email
      , invitedfstname = fstname
      , invitedsndname = sndname
      , invitingcompany = cid
      } : acc
