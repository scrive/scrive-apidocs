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

newtype ScheduleDocumentSealing = ScheduleDocumentSealing BrandedDomainID
instance (MonadDB m, DocumentMonad m, MonadLog m, MonadMask m, MonadTime m) => DBUpdate m ScheduleDocumentSealing () where
  update (ScheduleDocumentSealing bdid) = do
    did <- documentid <$> theDocument
    now <- currentTime
    logInfo_ "Attempting to schedule document sealing"
    -- There can be only one document sealing job for a given document. If the
    -- job already exists, ignore unique violation error and don't do anything.
    success <- runQuery01 . sqlInsert "document_sealing_jobs" $ do
      sqlSet "id" did
      sqlSetCmd "run_at" $ sqlParam now
      sqlSet "attempts"          (0 :: Int32)
      sqlSet "branded_domain_id" bdid
      sqlOnConflictDoNothing
    if success
      then do
        notify documentSealingNotificationChannel ""
        logInfo_ "Document sealing scheduled"
      else logAttention_ "Document already scheduled for sealing"
