module EvidenceLog.Migrations where

import DB
import EvidenceLog.Tables

evidenceLogFixColumns :: MonadDB m => Migration m
evidenceLogFixColumns =
  Migration {
    mgrTable = tableEvidenceLog
  , mgrFrom = 3
  , mgrDo = do
    -- ip addresses are 32bits long
    runSQL_ "ALTER TABLE evidence_log ALTER COLUMN request_ip_v4 TYPE INTEGER"
    -- this is completely useless
    runSQL_ "ALTER TABLE evidence_log DROP COLUMN request_ip_v6"

    runSQL_ "ALTER TABLE evidence_log ALTER COLUMN email TYPE TEXT"
    runSQL_ "ALTER TABLE evidence_log ALTER COLUMN text TYPE TEXT"
    runSQL_ "ALTER TABLE evidence_log ALTER COLUMN version_id TYPE TEXT"
    runSQL_ "ALTER TABLE evidence_log ALTER COLUMN api_user TYPE TEXT"
    runSQL_ "ALTER TABLE evidence_log ALTER COLUMN message_text TYPE TEXT"
    runSQL_ "ALTER TABLE evidence_log ALTER COLUMN client_name TYPE TEXT"
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
