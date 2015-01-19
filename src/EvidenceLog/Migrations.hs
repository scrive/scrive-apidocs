module EvidenceLog.Migrations where

import Data.Monoid

import DB
import EvidenceLog.Tables

evidenceLogAddActor :: MonadDB m => Migration m
evidenceLogAddActor =
  Migration {
    mgrTable = tableEvidenceLog
  , mgrFrom = 4
  , mgrDo = do
    -- ip addresses are 32bits long
    runSQL_ $ "ALTER TABLE evidence_log ADD COLUMN actor TEXT NOT NULL DEFAULT ''"
  }

evidenceLogFixColumns :: MonadDB m => Migration m
evidenceLogFixColumns =
  Migration {
    mgrTable = tableEvidenceLog
  , mgrFrom = 3
  , mgrDo = do
    -- ip addresses are 32bits long
    runSQL_ $ "ALTER TABLE evidence_log ALTER COLUMN request_ip_v4 TYPE INTEGER," <>
                                        -- this is completely useless
                                        "DROP COLUMN request_ip_v6," <>

                                        "ALTER COLUMN email TYPE TEXT," <>
                                        "ALTER COLUMN text TYPE TEXT," <>
                                        "ALTER COLUMN version_id TYPE TEXT," <>
                                        "ALTER COLUMN api_user TYPE TEXT," <>
                                        "ALTER COLUMN message_text TYPE TEXT," <>
                                        "ALTER COLUMN client_name TYPE TEXT"
  }

addClientTimeNameToEvidenceLog :: MonadDB m => Migration m
addClientTimeNameToEvidenceLog =
  Migration {
    mgrTable = tableEvidenceLog
  , mgrFrom = 2
  , mgrDo = do
      runSQL_ "ALTER TABLE evidence_log ADD COLUMN client_time TIMESTAMPTZ NULL"
      runSQL_ "ALTER TABLE evidence_log ADD COLUMN client_name VARCHAR NULL"
  }

expandEventsWithAffectedSignatoryAndTextMessage :: MonadDB m => Migration m
expandEventsWithAffectedSignatoryAndTextMessage =
  Migration {
    mgrTable = tableEvidenceLog
  , mgrFrom = 1
  , mgrDo = do
      runSQL_ "ALTER TABLE evidence_log ADD COLUMN affected_signatory_link_id BIGINT NULL"
      runSQL_ "ALTER TABLE evidence_log ADD COLUMN message_text   VARCHAR NULL"
  }
