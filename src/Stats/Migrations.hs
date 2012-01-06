module Stats.Migrations where

import Database.HDBC

import DB.Classes
import Stats.Tables
import Doc.DocStateData
import DB.Model

mapStatement :: ([SqlValue] -> a) -> Statement -> IO [a]
mapStatement f st = mapStatement' []
  where mapStatement' acc = fetchRow st >>= maybe (return acc) (\a -> mapStatement' $ f a : acc)

addServiceAndCompanyToStats :: Migration
addServiceAndCompanyToStats =
  Migration {
    mgrTable = tableDocStatEvents
  , mgrFrom = 1
  , mgrDo = wrapDB $ \conn -> do
      _ <- run conn "ALTER TABLE doc_stat_events DROP CONSTRAINT pk_doc_stat_events" []
      _ <- run conn "ALTER TABLE doc_stat_events ADD CONSTRAINT pk_doc_stat_events PRIMARY KEY (quantity, document_id)" []
      _ <- run conn "ALTER TABLE doc_stat_events ADD COLUMN service_id    TEXT       NULL" []
      _ <- run conn "ALTER TABLE doc_stat_events ADD COLUMN company_id    BIGINT     NULL" []
      _ <- run conn "ALTER TABLE doc_stat_events ADD COLUMN document_type TEXT       NULL" []
      _ <- run conn "UPDATE doc_stat_events SET service_id = NULL, company_id = NULL, document_type = ? "
           [toSql $ show $ Signable Contract]
      _ <- run conn "ALTER TABLE doc_stat_events ALTER COLUMN document_type SET   NOT NULL" []
      _ <- runRaw conn $ "ALTER TABLE doc_stat_events"
           ++ " ADD CONSTRAINT fk_doc_stat_events_company FOREIGN KEY(company_id)"
           ++ " REFERENCES companies(id) ON UPDATE RESTRICT ON DELETE NO ACTION"
           ++ " DEFERRABLE INITIALLY IMMEDIATE"
      _ <- runRaw conn $ "ALTER TABLE doc_stat_events"
           ++ " ADD CONSTRAINT fk_doc_stat_events_service FOREIGN KEY(service_id)"
           ++ " REFERENCES services(id) ON UPDATE RESTRICT ON DELETE NO ACTION"
           ++ " DEFERRABLE INITIALLY IMMEDIATE"
      return ()
  }

makeUserStatsRepeatableByChangingPK :: Migration
makeUserStatsRepeatableByChangingPK =
  Migration {
    mgrTable = tableUserStatEvents
  , mgrFrom = 1
  , mgrDo = wrapDB $ \conn -> do
      _ <- runRaw conn "CREATE SEQUENCE user_stat_events_id_seq"
      _ <- runRaw conn $ "ALTER TABLE user_stat_events"
           ++ " ADD COLUMN id BIGINT NOT NULL DEFAULT nextval('user_stat_events_id_seq')"
      _ <- runRaw conn $ "ALTER TABLE user_stat_events"
           ++ " DROP CONSTRAINT pk_user_stat_events"
      _ <- runRaw conn $ "ALTER TABLE user_stat_events"
           ++ " ADD CONSTRAINT pk_user_stat_events PRIMARY KEY (id)"
      return ()
  }
