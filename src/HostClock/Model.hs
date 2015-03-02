module HostClock.Model 
  ( InsertClockOffsetFrequency(..)
  , ClockErrorEstimate(..)
  , GetLatestClockErrorEstimate(..)
  , ClockErrorStatistics(..)
  , GetClockErrorStatistics(..)
  , maxClockError
  ) where

import Control.Monad (when)
import Control.Monad.Catch
import Data.Int
import Data.Maybe (isJust)
import Data.Time

import DB
import MinutesTime.Class

data InsertClockOffsetFrequency = InsertClockOffsetFrequency (Maybe Double) Double
instance (MonadDB m, MonadTime m) => DBUpdate m InsertClockOffsetFrequency Int where
  update (InsertClockOffsetFrequency moffset frequency) = do
    now <- currentTime
    runQuery . sqlInsert "host_clock" $ do
      sqlSet "time" now
      sqlSet "clock_offset" moffset
      sqlSet "clock_frequency" frequency

data ClockErrorEstimate = ClockErrorEstimate
  { time :: UTCTime
  , offset :: Double
  , frequency :: Double
  }
  deriving (Eq, Ord, Show)

data GetLatestClockErrorEstimate = GetLatestClockErrorEstimate
instance (MonadDB m, MonadThrow m) => DBQuery m GetLatestClockErrorEstimate (Maybe ClockErrorEstimate) where
  query (GetLatestClockErrorEstimate) = do
    runQuery_ . sqlSelect "host_clock" $ do
      sqlWhere "time = (SELECT MAX(time) FROM host_clock WHERE clock_offset IS NOT NULL)"
      sqlResult "time"
      sqlResult "clock_offset"
      sqlResult "clock_frequency"
    fetchMaybe $ \(t, o, f) -> ClockErrorEstimate t o f

-- | Estimate maximum clock error at a given time and a previous clock
-- error estimate, assuming that the host clock had been
-- unsynchronized since the estimate, and that the clock was drifting
-- according to the estimate's PLL frequency.
maxClockError :: UTCTime -> ClockErrorEstimate -> Double
maxClockError t e = realToFrac (diffUTCTime t $ time e) * abs (frequency e) + abs (offset e)

data ClockErrorStatistics = ClockErrorStatistics
  { max       :: Maybe Double -- ^ clock error maximum (ignoring frequency)
  , mean      :: Maybe Double -- ^ clock error sample mean
  , std_dev   :: Maybe Double -- ^ clock error sample standard deviation
  , collected :: Int64          -- ^ number of samples
  , missed    :: Int64          -- ^ missed number of samples
  }
  deriving Show

data GetClockErrorStatistics = GetClockErrorStatistics (Maybe UTCTime) (Maybe UTCTime)
instance (MonadDB m, MonadThrow m) => DBQuery m GetClockErrorStatistics ClockErrorStatistics where
  query (GetClockErrorStatistics from to) = do
    runQuery_ $ sqlSelect "host_clock" $ do
      when (isJust from) $ sqlWhere $ "time >=" <?> from
      when (isJust to)   $ sqlWhere $ "time <=" <?> to
      sqlResult "max(abs(clock_offset))"
      sqlResult "avg(clock_offset)"
      sqlResult "stddev_samp(clock_offset)"
      sqlResult "count(clock_offset)"
      sqlResult "count(*)"
    fetchOne $ \(max', mean', std_dev', collected', total) ->
      ClockErrorStatistics max' mean' std_dev' collected' (total - collected')

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
