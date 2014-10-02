{-# OPTIONS_GHC -fno-warn-orphans #-}
module MinutesTime (
    MinutesTime
  , unixEpoch
  , getMinutesTime
  , showMinutesTime
  , minutesAfter
  , minutesBefore
  , showDateYMD
  , showMinutesTimeForAPI
  , formatMinutesTimeUTC
  , formatMinutesTimeSimple
  , parseMinutesTimeSimple
  , formatMinutesTimeISO
  , parseMinutesTimeUTC
  , parseMinutesTimeISO
  , parseMinutesTime
  , monthsBefore
  , daysBefore
  , daysAfter
  , beginingOfMonth
  ) where

import Control.Monad
import Data.Int
import Data.Time
import Data.Time.Clock.POSIX
import Database.PostgreSQL.PQTypes
import System.Locale
import qualified Data.Binary as B

type MinutesTime = UTCTime

instance B.Binary MinutesTime where
  put t = B.put (floor $ utcTimeToPOSIXSeconds t :: Int64)
  get = do
    n :: Int64 <- B.get
    return . posixSecondsToUTCTime . fromIntegral $ n

unixEpoch :: MinutesTime
unixEpoch = posixSecondsToUTCTime 0

getMinutesTime :: MonadDB m => m MinutesTime
getMinutesTime = do
  runSQL_ "SELECT now()"
  fetchOne unSingle

parseMinutesTime :: ParseTime t => String -> String -> Maybe t
parseMinutesTime = parseTime defaultTimeLocale

showMinutesTime :: FormatTime t => String -> t -> String
showMinutesTime = formatTime defaultTimeLocale

-- | Show time in %Y-%m-%d %H:%M:%S %Z format.
-- This change was requested by Upsales. Should not affect much.
showMinutesTimeForAPI :: MinutesTime -> String
showMinutesTimeForAPI = showMinutesTime "%Y-%m-%d %H:%M:%S %Z"

-------------

parseMinutesTimeUTC :: String -> Maybe MinutesTime
parseMinutesTimeUTC = parseMinutesTime "%Y-%m-%d %H:%M:%S%Q"

formatMinutesTimeUTC :: MinutesTime -> String
formatMinutesTimeUTC = showMinutesTime "%Y-%m-%d %H:%M:%S%Q"

-------------

parseMinutesTimeISO :: String -> Maybe MinutesTime
parseMinutesTimeISO s = msum [
    parseMinutesTime "%Y-%m-%dT%H:%M:%S%QZ" s
  , parseMinutesTime "%Y-%m-%dT%H:%M:%S%Q%z" s
  ]

formatMinutesTimeISO :: MinutesTime -> String
formatMinutesTimeISO = showMinutesTime "%Y-%m-%dT%H:%M:%S%QZ"

-------------

formatMinutesTimeSimple :: MinutesTime -> String
formatMinutesTimeSimple = showMinutesTime "%Y-%m-%d %H:%M"

parseMinutesTimeSimple :: String -> Maybe MinutesTime
parseMinutesTimeSimple = parseMinutesTime "%Y-%m-%d %H:%M"

-------------

-- | Show date as %Y-%m-%d.
showDateYMD :: MinutesTime -> String
showDateYMD = showMinutesTime "%Y-%m-%d"

-------------

minutesAfter :: Int -> MinutesTime -> MinutesTime
minutesAfter = addUTCTime . fromIntegral

minutesBefore :: Int -> MinutesTime -> MinutesTime
minutesBefore = minutesAfter . negate

-------------

daysAfter :: Int -> MinutesTime -> MinutesTime
daysAfter = minutesAfter . (60 * 24 *)

daysBefore :: Int -> MinutesTime -> MinutesTime
daysBefore = daysAfter . negate

-------------

monthsBefore :: Int -> MinutesTime -> MinutesTime
monthsBefore i = localTimeToUTC utc . f . utcToLocalTime utc
  where
    f t = t { localDay = addGregorianMonthsClip (fromIntegral $ -i) $ localDay t }

beginingOfMonth :: MinutesTime -> MinutesTime
beginingOfMonth = localTimeToUTC utc . f . utcToLocalTime utc
  where
    f LocalTime{..} = LocalTime {
        localDay = fromGregorian year month 1
      , localTimeOfDay = midnight
      }
      where
        (year, month, _) = toGregorian localDay
