module Chargeable (
    ChargeableItem(..)
  , chargeForItem
  , chargeForItemSimple
  , chargeForItemSingle
  , chargeForItemSpecificUserGroup
  ) where

import Control.Monad.Catch
import Control.Monad.Time
import Data.Int
import Data.Text

import Chargeable.Model
import Chargeable.Types
import DB
import Doc.DocumentID
import EventStream
import User.UserID
import UserGroup.Types

-- Note: We charge the user group of the author of the document
-- at a time of the event, therefore the user_group_id never
-- changes, even if the corresponding user moves to the other
-- user group.
chargeForItem
  :: (MonadDB m, MonadTime m, MonadEventStream m, MonadThrow m, MonadCatch m)
  => ChargeableItem
  -> DocumentID
  -> UserID
  -> UserGroupID
  -> Int32
  -> m ()
chargeForItem item documentId userId userGroupId quantity = do
  now <- currentTime
  insertChargeableItem now item documentId userId userGroupId quantity
  pushEvent chargeableItemsStreamId (pack $ show now)
    $ ChargeableItemEvent now item documentId userId userGroupId quantity

chargeForItemSingle
  :: (MonadDB m, MonadTime m, MonadEventStream m, MonadThrow m, MonadCatch m)
  => ChargeableItem
  -> DocumentID
  -> m ()
chargeForItemSingle item documentId = chargeForItemSimple item documentId 1

chargeForItemSimple
  :: (MonadDB m, MonadTime m, MonadEventStream m, MonadThrow m, MonadCatch m)
  => ChargeableItem
  -> DocumentID
  -> Int32
  -> m ()
chargeForItemSimple item documentId quantity = do
  (userId, userGroupId) <- getAuthorAndAuthorsUserGroupIDs documentId
  chargeForItem item documentId userId userGroupId quantity

chargeForItemSpecificUserGroup
  :: (MonadDB m, MonadTime m, MonadEventStream m, MonadThrow m, MonadCatch m)
  => ChargeableItem
  -> DocumentID
  -> UserGroupID
  -> Int32
  -> m ()
chargeForItemSpecificUserGroup item documentId userGroupId quantity = do
  (userId, _) <- getAuthorAndAuthorsUserGroupIDs documentId
  chargeForItem item documentId userId userGroupId quantity

