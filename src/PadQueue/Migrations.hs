module PadQueue.Migrations where

import DB
import PadQueue.Tables

setCascadeOnPadQueue :: MonadDB m => Migration m
setCascadeOnPadQueue = Migration {
    mgrTable = tablePadQueue
  , mgrFrom = 1
  , mgrDo = do
    -- this is supposed to aid in the signatory_links renumeration step that follows
    kRunRaw $ "ALTER TABLE padqueue"
              <> " DROP CONSTRAINT fk_padqueue_signatorylinks,"
              <> " ADD CONSTRAINT fk_padqueue_signatorylinks FOREIGN KEY(document_id,signatorylink_id)"
              <> " REFERENCES signatory_links(document_id,id) ON DELETE RESTRICT ON UPDATE CASCADE"
              <> " DEFERRABLE INITIALLY IMMEDIATE"
  }

dropSLForeignKeyOnPadQueue :: MonadDB m => Migration m
dropSLForeignKeyOnPadQueue = Migration {
    mgrTable = tablePadQueue
  , mgrFrom = 2
  , mgrDo = do
    kRunRaw $ "ALTER TABLE padqueue"
           <> " DROP CONSTRAINT fk_padqueue_signatorylinks"
  }

setPadQueueForeignKeyToSLIDOnly :: MonadDB m => Migration m
setPadQueueForeignKeyToSLIDOnly = Migration {
    mgrTable = tablePadQueue
  , mgrFrom = 3
  , mgrDo = do
    kRunRaw $ "ALTER TABLE padqueue"
      <> " ADD CONSTRAINT fk_padqueue_signatorylinks FOREIGN KEY(signatorylink_id)"
      <> " REFERENCES signatory_links(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      <> " DEFERRABLE INITIALLY IMMEDIATE"
  }
