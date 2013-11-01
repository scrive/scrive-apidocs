{-# LANGUAGE ExtendedDefaultRules #-}
module EvidenceLog.Migrations where

import DB
import EvidenceLog.Tables

default (SQL)

addClientTimeNameToEvidenceLog :: MonadDB m => Migration m
addClientTimeNameToEvidenceLog =
  Migration {
    mgrTable = tableEvidenceLog
  , mgrFrom = 2
  , mgrDo = do
      kRunRaw "ALTER TABLE evidence_log ADD COLUMN client_time TIMESTAMPTZ NULL"
      kRunRaw "ALTER TABLE evidence_log ADD COLUMN client_name VARCHAR NULL"
      return ()
  }

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

