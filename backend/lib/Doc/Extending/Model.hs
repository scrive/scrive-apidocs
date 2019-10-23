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

data ScheduleDocumentExtending = ScheduleDocumentExtending DocumentID UTCTime
instance (MonadDB m, DocumentMonad m, MonadLog m, MonadMask m, MonadTime m) => DBUpdate m ScheduleDocumentExtending () where
  update (ScheduleDocumentExtending did _seal_time) = do
    now <- currentTime
    runQuery_ . sqlDelete "document_extending_jobs" $ do
      sqlWhereEq "id" did
    runQuery_ . sqlInsert "document_extending_jobs" $ do
      sqlSet "id" did
      -- Documents can be extended whenever the next GuardTime publications file
      -- is released. In a publications file all documents closed before
      -- midnight 15th day of the month are collected. The publications file
      -- itself is released a few days after. So theoretically we could schedule
      -- all documents closed BEFORE 15th day 00:00:00 for extending on 20th
      -- day. However, as GuardTime Java tools induce a relatively high CPU
      -- load, we want to spread this out more evenly; we thus agreed to
      -- schedule it 40 days after we close it.
      sqlSetCmd "run_at" $ (sqlParam now) <+> "+ interval '40 days'"
      sqlSet "attempts" (0 :: Int32)
