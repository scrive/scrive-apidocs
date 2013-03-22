module HostClock.Model 
  ( InsertClockOffsetFrequency(..)
  , ClockErrorEstimate(..)
  , GetLatestClockErrorEstimate(..)
  , ClockErrorStatistics(..)
  , GetClockErrorStatistics(..)
  , maxClockError
  ) where

import Control.Monad (when)
import DB (DBUpdate(..), DBQuery(..), kRun, kRun_, tblName, MonadDB, kFold, (<?>))
import DB.SQL2 (sqlInsert, sqlSet, sqlSetCmd, sqlSelect, sqlWhere, sqlResult)
import Data.Maybe (listToMaybe, isJust)
import HostClock.Tables (tableHostClock)
import MinutesTime (MinutesTime, toSeconds)

data InsertClockOffsetFrequency = InsertClockOffsetFrequency (Maybe Double) Double
instance MonadDB m => DBUpdate m InsertClockOffsetFrequency Integer where
  update (InsertClockOffsetFrequency moffset frequency) =
    kRun $ sqlInsert (tblName tableHostClock) $ do
      sqlSetCmd "time" "now()"
      sqlSet "clock_offset" moffset
      sqlSet "clock_frequency" frequency

data ClockErrorEstimate = ClockErrorEstimate
  { time :: MinutesTime
  , offset :: Double
  , frequency :: Double
  }
  deriving (Eq, Ord, Show)

data GetLatestClockErrorEstimate = GetLatestClockErrorEstimate
instance MonadDB m => DBQuery m GetLatestClockErrorEstimate (Maybe ClockErrorEstimate) where
  query (GetLatestClockErrorEstimate) = do
    kRun_ $ sqlSelect (tblName tableHostClock) $ do
      sqlWhere "time = (SELECT MAX(time) FROM host_clock WHERE clock_offset IS NOT NULL)"
      sqlResult "time"
      sqlResult "clock_offset"
      sqlResult "clock_frequency"
    es <- flip kFold [] $ \a t o f -> ClockErrorEstimate t o f:a
    return $ listToMaybe es

-- | Estimate maximum clock error at a given time and a previous clock
-- error estimate, assuming that the host clock had been
-- unsynchronized since the estimate, and that the clock was drifting
-- according to the estimate's PLL frequency.
maxClockError :: MinutesTime -> ClockErrorEstimate -> Double
maxClockError t e = fromIntegral (toSeconds t - toSeconds (time e)) * abs (frequency e) + abs (offset e)

data ClockErrorStatistics = ClockErrorStatistics
  { max       :: Maybe Double -- ^ clock error maximum (ignoring frequency)
  , mean      :: Maybe Double -- ^ clock error sample mean
  , std_dev   :: Maybe Double -- ^ clock error sample standard deviation
  , collected :: Int          -- ^ number of samples
  , missed    :: Int          -- ^ missed number of samples
  }
  deriving Show

data GetClockErrorStatistics = GetClockErrorStatistics (Maybe MinutesTime) (Maybe MinutesTime)
instance MonadDB m => DBQuery m GetClockErrorStatistics ClockErrorStatistics where
  query (GetClockErrorStatistics from to) = do
    kRun_ $ sqlSelect (tblName tableHostClock) $ do
      when (isJust from) $ sqlWhere $ "time >=" <?> from
      when (isJust to)   $ sqlWhere $ "time <=" <?> to
      sqlResult "max(abs(clock_offset))"
      sqlResult "avg(clock_offset)"
      sqlResult "stddev_samp(clock_offset)"
      sqlResult "count(clock_offset)"
      sqlResult "count(*)"
    [s] <- flip kFold [] $ \a max' mean' std_dev' collected' total -> ClockErrorStatistics max' mean' std_dev' collected' (total - collected') : a
    return s

{- 
TODO: 

In evidence log, put

  * max clock error
  * mean clock error
  * clock error std deviation
  * number of collected error estimates
  * number of missed error estimates

over all hosts recorded in the event log and with time interval covering event log

-}