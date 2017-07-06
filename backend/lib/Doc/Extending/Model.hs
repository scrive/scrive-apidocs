module Doc.Extending.Model (
    ScheduleDocumentExtending(..)
  ) where

import Control.Monad.Catch
import Data.Int
import Database.PostgreSQL.PQTypes
import Log.Class

import DB
import Doc.DocumentID
import Doc.DocumentMonad
import KontraPrelude

data ScheduleDocumentExtending = ScheduleDocumentExtending DocumentID UTCTime
instance (MonadDB m, DocumentMonad m, MonadLog m, MonadMask m) => DBUpdate m ScheduleDocumentExtending () where
  update (ScheduleDocumentExtending did _seal_time) = do
    runQuery_ . sqlDelete "document_extending_jobs" $ do
      sqlWhereEq "id" did
    runQuery_ . sqlInsert "document_extending_jobs" $ do
      sqlSet "id" did
      -- Documents can be extended only after the next GuardTime publications file release. Now, when is that going to happen?
      -- In a publications file, there are all signatures signed before midnight 15th day of the month. The publications file itself
      -- is released a few days after. So all documents closed BEFORE 15th day midnight will be scheduled for extending on 20th day.
      -- All documents closed AFTER 15th day midnight will be scheduled for extending on 20th day next month.
      sqlSetCmd "run_at" "date_trunc('month', now() - interval '15 days') + interval '1 month' + interval '20 days'"
      sqlSet "attempts" (0::Int32)
