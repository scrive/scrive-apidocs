module Doc.Sealing.Model (
    documentSealingNotificationChannel
  , ScheduleDocumentSealing(..)
  ) where

import Control.Monad.Catch
import Data.Int
import Log.Class

import BrandedDomain.Model
import DB
import Doc.DocumentMonad
import Doc.Types.Document

documentSealingNotificationChannel :: Channel
documentSealingNotificationChannel = "document_sealing"

data ScheduleDocumentSealing = ScheduleDocumentSealing BrandedDomainID
instance (MonadDB m, DocumentMonad m, MonadLog m, MonadMask m, MonadTime m) => DBUpdate m ScheduleDocumentSealing () where
  update (ScheduleDocumentSealing bdid) = do
    did <- documentid <$> theDocument
    now <- currentTime
    logInfo_ "Attempting to schedule document sealing"
    -- There can be only one document sealing job for a given
    -- document. If the job already exists, ignore unique violation
    -- error and don't do anything.
    let logErr = logAttention_ "Document already scheduled for sealing"
    (`onUniqueViolation` logErr) . withSavepoint "schedule_document_sealing" $ do
      runQuery_ . sqlInsert "document_sealing_jobs" $ do
        sqlSet "id" did
        sqlSetCmd "run_at" $ sqlParam now
        sqlSet "attempts"          (0 :: Int32)
        sqlSet "branded_domain_id" bdid
      notify documentSealingNotificationChannel ""
      logInfo_ "Document sealing scheduled"
