module MinutesTime
       ( MinutesTime
       , getMinutesTime
       , asInt
       , fromSeconds
       , fromMinutes
       , minutesAfter
       , minutesBefore
       , parseMinutesTimeDMY
       , showDateDMY
       , showDateDMYYYY
       , showDateOnly
       , parseDateOnly
       , showDateYMD
       , showMinutesTimeForAPI
       , showMinutesTimeForFileName
       , toMinutes
       , toSeconds
       , toCalendarTime
       , toCalendarTimeInUTC
       , showAsMonth
       , showAsDate
       , formatMinutesTimeUTC
       , formatMinutesTimeSimple
       , formatMinutesTimeRealISO
       , formatMinutesTimeISO
       , formatMinutesTime
       , parseMinutesTimeUTC
       , parseMinutesTimeISO
       , parseMinutesTimeRealISO
       , parseMinutesTime
       , monthsBefore
       , daysBefore
       , daysAfter
       , beginingOfMonth
       , toClockTime
       , toUTCTime
       , mtMonth
       , mtYear
       , fromClockTime
       ) where

import Data.Time
import Data.Time.Clock.POSIX
import Data.Typeable
import System.Locale
import System.Time hiding (toClockTime, toUTCTime, toCalendarTime)
import qualified System.Time as System.Time (toUTCTime, toCalendarTime,toClockTime)
import Text.Printf
import System.IO.Unsafe
import Control.Monad
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Utils

-- | Time in seconds from 1970-01-01 00:00:00 in UTC coordinates
-- Same as POSIX seconds and what every other database uses as TIMESTAMP time type.
newtype MinutesTime = MinutesTime Int
    deriving (Eq, Ord, Typeable)

instance Show MinutesTime where
    show = formatMinutesTime "%Y-%m-%d, %H:%M:%S %Z"

getMinutesTime :: MonadDB m => m MinutesTime
getMinutesTime = do
  runSQL_ "SELECT now()"
  fetchOne unSingle

-- | Show time in %Y-%m-%d %H:%M:%S %Z format.
-- This change was requested by Upsales. Should not affect much.
showMinutesTimeForAPI :: MinutesTime -> String
showMinutesTimeForAPI mt = formatMinutesTime "%Y-%m-%d %H:%M:%S %Z" mt

-- | Show time for files creation
showMinutesTimeForFileName :: MinutesTime -> String
showMinutesTimeForFileName mt = formatMinutesTime "%Y-%m-%d-%H-%M" mt


-- | Show time in "%Y-%m-%d" format.
showDateOnly :: MinutesTime -> String
showDateOnly mt | toSeconds mt == 0 = ""
                | otherwise = formatMinutesTime "%Y-%m-%d" mt

formatMinutesTimeISO :: MinutesTime -> String
formatMinutesTimeISO = formatMinutesTime "%Y-%m-%d %H:%M:%S"

parseMinutesTimeISO :: String -> Maybe MinutesTime
parseMinutesTimeISO = parseMinutesTime "%Y-%m-%d %H:%M:%S %Z"

parseMinutesTimeRealISO :: String -> Maybe MinutesTime
parseMinutesTimeRealISO s = (parseMinutesTime "%Y-%m-%dT%H:%M:%S%QZ" s) `mplus` (parseMinutesTime "%Y-%m-%dT%H:%M:%S%Q%z" s)

parseMinutesTimeUTC :: String -> Maybe MinutesTime
parseMinutesTimeUTC = parseMinutesTime "%Y-%m-%d %H:%M:%S"

parseMinutesTime :: String -> String -> Maybe MinutesTime
parseMinutesTime format string = do
    time <- parseTime defaultTimeLocale format string
    return $ fromSeconds $ floor $ utcTimeToPOSIXSeconds time

parseDateOnly :: String -> Maybe MinutesTime
parseDateOnly = parseMinutesTime "%Y-%m-%d"

-- | Convert 'ClockTime' to 'MinutesTime'. Uses just seconds, picoseconds are ignored.
fromClockTime :: ClockTime -> MinutesTime
fromClockTime (TOD secs _picos) =  fromSeconds (fromIntegral secs)

-- | Convert 'MinutesTime' to 'ClockTime'.
toClockTime :: MinutesTime -> ClockTime
toClockTime mt = (TOD (fromIntegral $ toSeconds mt) 0)

-- | Convert 'MinutesTime' to 'CalendarTime' through 'System.Time.toUTCTime'.
toCalendarTimeInUTC :: MinutesTime -> CalendarTime
toCalendarTimeInUTC = System.Time.toUTCTime . toClockTime

-- | Convert 'MinutesTime' to 'UTCTime'.
toUTCTime :: MinutesTime -> UTCTime
toUTCTime (MinutesTime t) = posixSecondsToUTCTime (fromIntegral t)

-- | This is wrong on so many different levels, kill this function, use something sensible.
toCalendarTime :: MinutesTime -> CalendarTime
toCalendarTime mt = unsafePerformIO (System.Time.toCalendarTime (toClockTime mt))

-- | Convert minutes to proper 'MinutesTime'.
fromMinutes :: Int -> MinutesTime
fromMinutes m = MinutesTime (m*60)

-- | Extract the minutes component from 'MinutesTime'. Seconds are ignored.
toMinutes :: MinutesTime -> Int
toMinutes (MinutesTime s) = s `div` 60

-- | Convert seconds to proper 'MinutesTime'.
fromSeconds :: Int -> MinutesTime
fromSeconds s = MinutesTime s

-- | Get number of seconds from 'MinutesTime' since 1970.
toSeconds :: MinutesTime -> Int
toSeconds (MinutesTime s) = s

mtMonth :: MinutesTime -> String
mtMonth = formatCalendarTime defaultTimeLocale "%m" . toCalendarTime

mtYear :: MinutesTime -> String
mtYear = formatCalendarTime defaultTimeLocale "%Y" . toCalendarTime

-- | Format time according to Swedish rules of time formating.
formatMinutesTime :: String -> MinutesTime -> String
formatMinutesTime fmt mt = formatCalendarTime defaultTimeLocale fmt (toCalendarTime mt)

formatMinutesTimeUTC :: MinutesTime -> String
formatMinutesTimeUTC mt = formatCalendarTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (toCalendarTimeInUTC mt)

formatMinutesTimeSimple :: MinutesTime -> String
formatMinutesTimeSimple mt = formatCalendarTime defaultTimeLocale "%Y-%m-%d %H:%M" (toCalendarTimeInUTC mt)

formatMinutesTimeRealISO :: MinutesTime -> String
formatMinutesTimeRealISO mt = formatCalendarTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (toCalendarTimeInUTC mt)

-- | Parse format %d-%m-%Y.
parseMinutesTimeDMY :: String -> Maybe MinutesTime
parseMinutesTimeDMY = parseMinutesTime "%d-%m-%Y"

-- | Show date as %d-%m-%y.
showDateDMY :: MinutesTime -> String
showDateDMY = formatMinutesTime "%d-%m-%y"

showDateDMYYYY :: MinutesTime -> String
showDateDMYYYY = formatMinutesTime "%d-%m-%Y"


-- | Show date as %Y-%m-%d.
showDateYMD :: MinutesTime -> String
showDateYMD = formatMinutesTime "%Y-%m-%d"

-- | Use as:
--
-- > 5 `minutesAfter` midnight
minutesAfter :: Int -> MinutesTime -> MinutesTime
minutesAfter i (MinutesTime s) = MinutesTime (s + i*60)

minutesBefore :: Int -> MinutesTime -> MinutesTime
minutesBefore i (MinutesTime s) = MinutesTime (s - i * 60)

daysBefore :: Int -> MinutesTime -> MinutesTime
daysBefore i mt = minutesBefore (i * 60 * 24) mt

daysAfter :: Int -> MinutesTime -> MinutesTime
daysAfter i mt = minutesAfter (i * 60 * 24) mt

monthsBefore :: Int -> MinutesTime -> MinutesTime
monthsBefore i = fromClockTime . System.Time.toClockTime . move . toCalendarTimeInUTC
  where
    move (ct@CalendarTime { ctMonth = m, ctYear = y }) =
      ct { ctMonth = toEnum ((fromEnum m + y*12 - i) `mod` 12)
         , ctYear = (fromEnum m + y*12 - i) `div` 12
         }

beginingOfMonth :: MinutesTime -> MinutesTime
beginingOfMonth time = fromClockTime $ System.Time.toClockTime $ (toCalendarTimeInUTC time) { ctDay = 1, ctHour = 0, ctMin = 0, ctSec = 0 }

-- | Convert a date representation to integer. For date like
-- "2010-06-12" result will be 20100612. Useful in IntMap for
-- example.
asInt :: MinutesTime -> Int
asInt m = ctYear*10000 + (fromEnum ctMonth+1)*100 + ctDay
  where
    -- January counts as 0, so we need to add 1
    CalendarTime {ctYear,ctMonth,ctDay} = toCalendarTimeInUTC m

showAsDate :: Int -> String
showAsDate int = printf "%04d-%02d-%02d" (int `div` 10000) (int `div` 100 `mod` 100) (int `mod` 100)

showAsMonth :: Int -> String
showAsMonth int = printf "%04d-%02d" (int `div` 10000) (int `div` 100 `mod` 100)

instance PQFormat MinutesTime where
  pqFormat _ = pqFormat (undefined::UTCTime)

instance FromSQL MinutesTime where
  type PQBase MinutesTime = PGtimestamp
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just PGtimestamp{..}) = return . MinutesTime . fromIntegral $ pgTimestampEpoch

instance ToSQL MinutesTime where
  type PQDest MinutesTime = PGtimestamp
  toSQL mt = toSQL (toUTCTime mt)
