{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Attachment.Migrations where

import DB
import Attachment.Tables

default (SQL)

addProbablyMissingIndexesOnAttachments :: MonadDB m => Migration m
addProbablyMissingIndexesOnAttachments =
  Migration {
      mgrTable = tableAttachments
    , mgrFrom = 1
    , mgrDo = do
       kRun_ $ sqlAlterTable (tblName tableAttachments) ["DROP CONSTRAINT IF EXISTS fk__attachments__file_id__files"]
       kRun_ $ sqlAlterTable (tblName tableAttachments) [sqlAddFK (tblName tableAttachments) $ fkOnColumn "file_id" "files" "id"]
       kRun_ $ "DROP INDEX IF EXISTS idx__attachments__file_id"
       kRun_ $ sqlCreateIndex (tblName tableAttachments) $ indexOnColumn "file_id"
       return ()
    }
