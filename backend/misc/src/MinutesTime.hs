{-# OPTIONS_GHC -fno-warn-orphans #-}
module MinutesTime (
    UTCTime
  , module Control.Monad.Time
  , unixEpoch
  , formatTime'
  , parseTime'
  , formatTimeAPI
  , formatTimeYMD
  , formatTimeUTC
  , parseTimeUTC
  , formatTimeISO
  , parseTimeISO
  , formatTimeSimple
  , parseTimeSimple
  , formatTimeForMail
  , minutesAfter
  , minutesBefore
  , secondsAfter
  , secondsBefore
  , daysAfter
  , daysBefore
  , monthsBefore
  , beginingOfMonth
  , beginingOfMonthUTC
  , beginningOfDay
  , nextDayMidnight
  , nextDayAt
  , nextDayAtHour
  , todayAtHour
  , beginningOfNextMonthAtHour
  ) where

import Control.Monad.Time
import Data.Int
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Binary as B

-- | FIXME: this really needs to go.
instance B.Binary UTCTime where
  put t = B.put (floor $ utcTimeToPOSIXSeconds t :: Int64)
  get = do
    n :: Int64 <- B.get
    return . posixSecondsToUTCTime . fromIntegral $ n

unixEpoch :: UTCTime
unixEpoch = posixSecondsToUTCTime 0

----------------------------------------

formatTime' :: FormatTime t => String -> t -> String
formatTime' = formatTime defaultTimeLocale

parseTime' :: ParseTime t => String -> String -> Maybe t
parseTime' = parseTimeM True defaultTimeLocale

-- | Show time as %Y-%m-%d %H:%M:%S %Z.
-- This change was requested by Upsales. Should not affect much.
formatTimeAPI :: UTCTime -> String
formatTimeAPI = formatTime' "%Y-%m-%d %H:%M:%S %Z"

-- | Format time as %Y-%m-%d.
formatTimeYMD :: UTCTime -> String
formatTimeYMD = formatTime' "%Y-%m-%d"

-- | Format time as %Y-%m-%d %H:%M.
formatTimeSimple :: UTCTime -> String
formatTimeSimple = formatTime' "%Y-%m-%d %H:%M"

-- | Parse time as %Y-%m-%d %H:%M.
parseTimeSimple :: String -> Maybe UTCTime
parseTimeSimple = parseTime' "%Y-%m-%d %H:%M"

-- | Format time as %Y-%m-%d %H:%M:%S%Q.
formatTimeUTC :: UTCTime -> String
formatTimeUTC = formatTime' "%Y-%m-%d %H:%M:%S%Q"

-- | Parse time as %Y-%m-%d %H:%M:%S%Q.
parseTimeUTC :: String -> Maybe UTCTime
parseTimeUTC = parseTime' "%Y-%m-%d %H:%M:%S%Q"

-- | Format time as %Y-%m-%dT%H:%M:%SZ. Microseconds (%Q) are
-- not included because this may break integrations. It should
-- be fixed to contain them in the next API version.
formatTimeISO :: UTCTime -> String
formatTimeISO = formatTime' "%Y-%m-%dT%H:%M:%SZ"

-- | Parse time as %Y-%m-%dT%H:%M:%S%QZ or %Y-%m-%dT%H:%M:%S%Q%z.
parseTimeISO :: String -> Maybe UTCTime
parseTimeISO s =
  msum [parseTime' "%Y-%m-%dT%H:%M:%S%QZ" s, parseTime' "%Y-%m-%dT%H:%M:%S%Q%z" s]

-- | Formating time for mail header. RFC2822
formatTimeForMail :: UTCTime -> String
formatTimeForMail = formatTime' "%a, %d %b %Y %H:%M:%S %z"
----------------------------------------

secondsAfter :: (Integral a) => a -> UTCTime -> UTCTime
secondsAfter = addUTCTime . fromIntegral

secondsBefore :: (Integral a) => a -> UTCTime -> UTCTime
secondsBefore = secondsAfter . negate

minutesAfter :: (Integral a) => a -> UTCTime -> UTCTime
minutesAfter = addUTCTime . (60 *) . fromIntegral

minutesBefore :: (Integral a) => a -> UTCTime -> UTCTime
minutesBefore = minutesAfter . negate

daysAfter :: (Integral a) => a -> UTCTime -> UTCTime
daysAfter = minutesAfter . (60 * 24 *)

daysBefore :: (Integral a) => a -> UTCTime -> UTCTime
daysBefore = daysAfter . negate

monthsBefore :: (Integral a) => a -> UTCTime -> UTCTime
monthsBefore i = localTimeToUTC utc . f . utcToLocalTime utc
  where f t = t { localDay = addGregorianMonthsClip (fromIntegral $ -i) $ localDay t }

-- | Transform the time to the beginning of the current month.
beginingOfMonth :: UTCTime -> UTCTime
beginingOfMonth = localTimeToUTC utc . f . utcToLocalTime utc
  where
    f LocalTime {..} = LocalTime { localDay       = fromGregorian year month 1
                                 , localTimeOfDay = midnight
                                 }
      where (year, month, _) = toGregorian localDay

-- | Transform the time to the beginning of the current month with respect to
-- UTC only.
beginingOfMonthUTC :: UTCTime -> UTCTime
beginingOfMonthUTC t =
  let (year, month, _date) = toGregorian $ utctDay t
      monthFirstDay        = fromGregorian year month 1
  in  UTCTime { utctDay = monthFirstDay, utctDayTime = 0 }

beginningOfDay :: UTCTime -> UTCTime
beginningOfDay t = t { utctDayTime = 0 }

nextDayMidnight :: UTCTime -> UTCTime
nextDayMidnight = nextDayAtHour 0

nextDayAt :: Int -> Int -> UTCTime -> UTCTime
nextDayAt hours minutes time = UTCTime
  { utctDay     = 1 `addDays` utctDay time
  , utctDayTime = secondsToDiffTime . toInteger $ (3600 * hours) + (60 * minutes)
  }

nextDayAtHour :: Int -> UTCTime -> UTCTime
nextDayAtHour hours = nextDayAt hours 0

todayAtHour :: Int -> UTCTime -> UTCTime
todayAtHour hours time =
  time { utctDayTime = secondsToDiffTime . toInteger $ 3600 * hours }

beginningOfNextMonthAtHour :: Int -> UTCTime -> UTCTime
beginningOfNextMonthAtHour hours time = UTCTime
  { utctDay     = 1 `addGregorianMonthsClip` utctDay (beginingOfMonthUTC time)
  , utctDayTime = secondsToDiffTime . toInteger $ 3600 * hours
  }
