{-# LANGUAGE ExistentialQuantification #-}
module JobQueue.Config (
    Action(..)
  , Result(..)
  , ConsumerConfig(..)
  ) where

import Database.PostgreSQL.PQTypes

data Action
  = MarkProcessed
  | RetryAfter Interval
  | Remove
    deriving (Eq, Ord, Show)

data Result = Ok Action | Failed Action
  deriving (Eq, Ord, Show)

data ConsumerConfig m idx job = forall row. FromRow row => ConsumerConfig {
  ccJobsTable             :: !(RawSQL ())
, ccConsumersTable        :: !(RawSQL ())
, ccJobSelectors          :: ![SQL]
, ccJobFetcher            :: !(row -> job)
, ccJobIndex              :: !(job -> idx)
, ccNotificationChannel   :: !(Maybe Channel)
, ccNotificationTimeout   :: !Int
, ccMaxRunningJobs        :: !Int
, ccProcessJob            :: !(job -> m Result)
, ccOnException           :: !(job -> m Action)
}
