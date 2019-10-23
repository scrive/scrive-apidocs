module CronStats.Model (
    CronStats(..)
  , GetCronStats(..)
  ) where

import Control.Monad.Catch
import Data.Int

import DB

data CronStats = CronStats {
      sealingJobsCount          :: Int64
    , signingJobsCount          :: Int64
    , extendingJobsCount        :: Int64
    , overdueExtendingJobsCount :: Int64
    , mailSendoutsCount         :: Int64
    , smsSendoutsCount          :: Int64
    , filePurgingJobsCount      :: Int64
    , apiCallbacksCount         :: Int64
    , retriedCallbacksCount     :: Int64
  }

data GetCronStats = GetCronStats
instance (MonadDB m, MonadThrow m) => DBQuery m GetCronStats CronStats where
  query GetCronStats = do
    sealingJobsCount          <- fetchCount "document_sealing_jobs"
    signingJobsCount          <- fetchCount "document_signing_jobs"
    extendingJobsCount        <- fetchCount "document_extending_jobs"
    overdueExtendingJobsCount <- fetchCountWhere "document_extending_jobs" $ do
      sqlWhere "run_at < now()"
    mailSendoutsCount     <- fetchCountWhere "mails" $ sqlWhereIsNULL "finished_at"
    smsSendoutsCount      <- fetchCountWhere "smses" $ sqlWhereIsNULL "finished_at"
    filePurgingJobsCount  <- fetchCount "file_purge_jobs"
    apiCallbacksCount     <- fetchCount "document_api_callbacks"
    retriedCallbacksCount <- fetchCountWhere "document_api_callbacks"
      $ sqlWhere "attempts > 1"
    return $ CronStats { .. }


    where
      fetchCountWhere table sqlwhere = fetchCountWhereM table (Just sqlwhere)
      fetchCount table = fetchCountWhereM table Nothing
      fetchCountWhereM table msqlwhere = do
        runQuery_ . sqlSelect table $ do
          case msqlwhere of
            Just sqlwhere -> sqlwhere
            Nothing       -> return ()
          sqlResult "count(*)"
        countR :: Int64 <- fetchOne runIdentity
        return countR
