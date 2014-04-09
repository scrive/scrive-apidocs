module Session.Migrations where


import DB
import Session.Tables

addDomainToSession :: MonadDB m => Migration m
addDomainToSession =
  Migration {
    mgrTable = tableSessions
  , mgrFrom = 1
  , mgrDo = do
      runSQL_ "ALTER TABLE sessions ADD COLUMN domain TEXT"
      runSQL_ "UPDATE sessions SET domain='scrive.com'"
      runSQL_ "ALTER TABLE sessions  ALTER COLUMN domain SET NOT NULL"
  }
