module UserGroupAccounts.Model (
    module User.Model
  , UserGroupInvite(..)
  , AddUserGroupInvite(..)
  , RemoveUserGroupInvite(..)
  , RemoveUserUserGroupInvites(..)
  , GetUserGroupInvite(..)
  , UserGroupGetInvites(..)
  , UserGroupGetInvitesWithUsersData(..)
  ) where

import Control.Monad.Catch
import Control.Monad.State

import DB
import User.Model
import UserGroup.Data

{- |
    A UserGroupInvite is a record
    of an invitation made by a usergroup
    to takeover an existing user.
-}
data UserGroupInvite = UserGroupInvite {
    inviteduserid   :: UserID
  , invitingusergroup :: UserGroupID --the usergroup they are invited to
  } deriving (Eq, Ord, Show)

data AddUserGroupInvite = AddUserGroupInvite UserGroupInvite
instance (MonadDB m, MonadThrow m) => DBUpdate m AddUserGroupInvite UserGroupInvite where
  update (AddUserGroupInvite UserGroupInvite{..}) = do
    runSQL_ "LOCK TABLE companyinvites IN ACCESS EXCLUSIVE MODE"
    runQuery_ . sqlDelete "companyinvites" $ do
      sqlWhereEq "company_id" invitingusergroup
      sqlWhereEq "user_id" inviteduserid
    runQuery_ . sqlInsert "companyinvites" $ do
      sqlSet "user_id" inviteduserid
      sqlSet "company_id" invitingusergroup
    fromJust `liftM` query (GetUserGroupInvite invitingusergroup inviteduserid)

data RemoveUserGroupInvite = RemoveUserGroupInvite UserGroupID UserID
instance (MonadDB m, MonadThrow m) => DBUpdate m RemoveUserGroupInvite Bool where
  update (RemoveUserGroupInvite ugid user_id) = do
    runQuery01 . sqlDelete "companyinvites" $ do
      sqlWhereEq "company_id" ugid
      sqlWhereEq "user_id" user_id

data RemoveUserUserGroupInvites = RemoveUserUserGroupInvites UserID
instance (MonadDB m, MonadThrow m) => DBUpdate m RemoveUserUserGroupInvites Bool where
  update (RemoveUserUserGroupInvites user_id) = do
    runQuery01 . sqlDelete "companyinvites" $ do
      sqlWhereEq "user_id" user_id

data GetUserGroupInvite = GetUserGroupInvite UserGroupID UserID
instance (MonadDB m, MonadThrow m) => DBQuery m GetUserGroupInvite (Maybe UserGroupInvite) where
  query (GetUserGroupInvite ugid uid) = do
    runQuery_ . selectUserGroupInvites $ do
      sqlWhereEq "ci.company_id" ugid
      sqlWhereEq "ci.user_id" uid
    fetchMaybe fetchUserGroupInvite

data UserGroupGetInvites = UserGroupGetInvites UserGroupID
instance MonadDB m => DBQuery m UserGroupGetInvites [UserGroupInvite] where
  query (UserGroupGetInvites ugid) = do
    runQuery_ . selectUserGroupInvites $ do
      sqlWhereEq "ci.company_id" ugid
    fetchMany fetchUserGroupInvite

data UserGroupGetInvitesWithUsersData = UserGroupGetInvitesWithUsersData UserGroupID
instance MonadDB m => DBQuery m UserGroupGetInvitesWithUsersData [(UserGroupInvite,String,String,String)] where
  query (UserGroupGetInvitesWithUsersData ugid) = do
    runQuery_ . sqlSelect "companyinvites as i, users as u" $ do
      sqlWhere "i.user_id = u.id"
      sqlWhereEq "i.company_id" ugid
      sqlResult "i.user_id"
      sqlResult "i.company_id"
      sqlResult "u.first_name"
      sqlResult "u.last_name"
      sqlResult "u.email"
    fetchMany $ \(uid, ugid', fn, ln, eml) -> (UserGroupInvite uid ugid', fn, ln, eml)

-- helpers

selectUserGroupInvites :: State SqlSelect () -> SqlSelect
selectUserGroupInvites refine =
  sqlSelect "companyinvites ci" $ do
    sqlResult "ci.user_id"
    sqlResult "ci.company_id"
    refine

fetchUserGroupInvite :: (UserID, UserGroupID) -> UserGroupInvite
fetchUserGroupInvite (uid, ugid) = UserGroupInvite {
  inviteduserid = uid
, invitingusergroup = ugid
}
