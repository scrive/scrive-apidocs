module HostClock.Model
  ( InsertClockOffsetFrequency(..)
  , ClockErrorEstimate(..)
  , GetLatestClockErrorEstimate(..)
  , GetNClockErrorEstimates(..)
  , maxClockError
  , showClockError
  , enoughClockErrorOffsetSamples
  ) where

import Control.Monad.Catch
import Control.Monad.Time
import Data.Decimal (realFracToDecimal)
import Data.Time
import Data.Word (Word8)

import DB

data InsertClockOffsetFrequency = InsertClockOffsetFrequency (Maybe Double) Double
instance (MonadDB m, MonadTime m) => DBUpdate m InsertClockOffsetFrequency Int where
  dbUpdate (InsertClockOffsetFrequency moffset frequency) = do
    now <- currentTime
    runQuery . sqlInsert "host_clock" $ do
      sqlSet "time"            now
      sqlSet "clock_offset"    moffset
      sqlSet "clock_frequency" frequency

data ClockErrorEstimate = ClockErrorEstimate
  { time :: UTCTime
  , offset :: Double
  , frequency :: Double
  }
  deriving (Eq, Ord, Show)

data GetLatestClockErrorEstimate = GetLatestClockErrorEstimate
instance (MonadDB m, MonadThrow m) => DBQuery m GetLatestClockErrorEstimate (Maybe ClockErrorEstimate) where
  dbQuery GetLatestClockErrorEstimate = do
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
maxClockError t e =
  realToFrac (diffUTCTime t $ time e) * abs (frequency e) + abs (offset e)

-- | Get the last 'limit' clock error estimates wher clock_offset is not NULL,
-- used in Evidence of Time
newtype GetNClockErrorEstimates = GetNClockErrorEstimates Integer
instance (MonadDB m, MonadThrow m) => DBQuery m GetNClockErrorEstimates [ClockErrorEstimate] where
  dbQuery (GetNClockErrorEstimates limit) = do
    runQuery_ . sqlSelect "host_clock" $ do
      sqlResult "time"
      sqlResult "clock_offset"
      sqlResult "clock_frequency"
      sqlWhere "clock_offset IS NOT NULL"
      sqlOrderBy "time DESC"
      sqlLimit limit
    reverse <$> fetchMany
      (\(time', offset', frequency') -> ClockErrorEstimate time' offset' frequency')

-- | Simple way to see that there are more than 2 different offset values
enoughClockErrorOffsetSamples :: [ClockErrorEstimate] -> Bool
enoughClockErrorOffsetSamples xs = case group . map offset $ xs of
  (_ : _ : _) -> True
  _           -> False

showClockError :: Word8 -> Double -> String
showClockError decimals e = show (realFracToDecimal decimals (e * 1000)) ++ " ms"
