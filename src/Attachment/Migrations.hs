module Attachment.Migrations where

import Attachment.Tables
import DB

addProbablyMissingIndexesOnAttachments :: MonadDB m => Migration m
addProbablyMissingIndexesOnAttachments =
  Migration {
      mgrTable = tableAttachments
    , mgrFrom = 1
    , mgrDo = do
       runQuery_ $ sqlAlterTable (tblName tableAttachments) ["DROP CONSTRAINT IF EXISTS fk__attachments__file_id__files"]
       runQuery_ $ sqlAlterTable (tblName tableAttachments) [sqlAddFK (tblName tableAttachments) $ fkOnColumn "file_id" "files" "id"]
       runSQL_ $ "DROP INDEX IF EXISTS idx__attachments__file_id"
       runQuery_ $ sqlCreateIndex (tblName tableAttachments) $ indexOnColumn "file_id"
       return ()
    }
