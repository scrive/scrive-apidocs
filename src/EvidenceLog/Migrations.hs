{-# LANGUAGE ExtendedDefaultRules #-}
module EvidenceLog.Migrations where

import DB
import EvidenceLog.Tables

default (SQL)

expandEventsWithAffectedSignatoryAndTextMessage :: MonadDB m => Migration m
expandEventsWithAffectedSignatoryAndTextMessage =
  Migration {
    mgrTable = tableEvidenceLog
  , mgrFrom = 1
  , mgrDo = do
      kRunRaw "ALTER TABLE evidence_log ADD COLUMN affected_signatory_link_id BIGINT NULL"
      kRunRaw "ALTER TABLE evidence_log ADD COLUMN message_text   VARCHAR NULL"
      return ()
  }

