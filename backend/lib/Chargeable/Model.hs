module Chargeable.Model (
    insertChargeableItem
  , getAuthorAndAuthorsUserGroupIDs
  ) where

import Control.Monad.Catch
import Control.Monad.Time
import Data.Int
import Data.Time

import Chargeable.Types
import DB
import Doc.DocumentID
import User.UserID
import UserGroup.Types

insertChargeableItem
  :: (MonadDB m, MonadThrow m, MonadTime m)
  => UTCTime
  -> ChargeableItem
  -> DocumentID
  -> UserID
  -> UserGroupID
  -> Int32
  -> m ()
insertChargeableItem now item documentId userId userGroupId quantity = do
  runQuery_ . sqlInsert "chargeable_items" $ do
    sqlSet "time"          now
    sqlSet "type"          item
    sqlSet "user_id"       userId
    sqlSet "document_id"   documentId
    sqlSet "quantity"      quantity
    sqlSet "user_group_id" userGroupId

-- | Fetch id of the author of the document.
getAuthorAndAuthorsUserGroupIDs
  :: (MonadDB m, MonadThrow m) => DocumentID -> m (UserID, UserGroupID)
getAuthorAndAuthorsUserGroupIDs did = do
  runQuery_ . sqlSelect "documents d" $ do
    sqlJoinOn "users u" "d.author_user_id = u.id"
    sqlResult "u.id"
    sqlResult "u.user_group_id"
    sqlWhereEq "d.id" did
  fetchOne identity
