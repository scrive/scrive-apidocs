module Stats.Migrations where

import Database.HDBC

import DB.Classes
import DB.Model
import Stats.Tables
import Doc.DocState

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
--      _ <- run conn "ALTER TABLE doc_stat_events ADD CONSTRAINT pk_doc_stat_events (quantity, document_id)" []
      _ <- run conn "ALTER TABLE doc_stat_events ADD COLUMN service_id    TEXT       NULL" []
      _ <- run conn "ALTER TABLE doc_stat_events ADD COLUMN company_id    BIGINT     NULL" []
      _ <- run conn "ALTER TABLE doc_stat_events ADD COLUMN document_type TEXT   NOT NULL" []
      _ <- run conn "UPDATE doc_stat_events SET service_id = NULL, company_id = NULL, document_type = ? " 
           [toSql $ show $ Signable Contract]
           
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

