module MinutesTime
       ( MinutesTime
       , asInt
       , dateDiffInDays
       , formatSwedishMinutesTime
       , fromClockTime
       , fromMinutes
       , fromSeconds
       , fromUTCTime
       , getMinuteTimeDB
       , getMinutesTime
       , minutesAfter
       , monthsAfter
       , parseMinutesTimeDMY
       , showDateAbbrev
       , showDateDMY
       , showDateOnly
       , showDateYMD
       , showMinutesTimeForAPI
       , startOfMonth
       , swedishTimeLocale
       , toClockTime
       , toMinutes
       , toSeconds
       , toUTCTime
       ) where

import Data.Data
import Data.Time
import Happstack.Data
import Happstack.State
import System.IO.Unsafe
import System.Locale
import System.Time hiding (toClockTime, toUTCTime)
import qualified System.Time as System.Time (toUTCTime, toClockTime)

import DB.Derive

-- | Time in minutes from 1970-01-01 00:00 in UTC coordinates
newtype MinutesTime0 = MinutesTime0 Int
       deriving (Eq, Ord, Typeable)

-- | Time in seconds from 1970-01-01 00:00:00 in UTC coordinates
data MinutesTime1 = MinutesTime1
    { minutes :: Int
    , secs :: Int
    }
    deriving (Eq, Ord, Typeable)

-- | Time in seconds from 1970-01-01 00:00:00 in UTC coordinates
newtype MinutesTime = MinutesTime { _unMinutesTime :: Int }
    deriving (Eq, Ord, Typeable, Data)
$(newtypeDeriveConvertible ''MinutesTime)

instance Version MinutesTime0
$(deriveSerialize ''MinutesTime0)

$(deriveSerialize ''MinutesTime1)
instance Version MinutesTime1 where
   mode = extension 1 (Proxy :: Proxy MinutesTime0)

$(deriveSerialize ''MinutesTime)
instance Version MinutesTime where
   mode = extension 2 (Proxy :: Proxy MinutesTime1)

instance Migrate MinutesTime0 MinutesTime1 where
      migrate (MinutesTime0 m) = MinutesTime1 {minutes = m, secs = 0 }

instance Migrate MinutesTime1 MinutesTime where
      migrate (MinutesTime1 m s) = fromSeconds (m*60 + s)

instance Show MinutesTime where
    showsPrec _prec mt = (++) $ formatSwedishMinutesTime "%Y-%m-%d, %H:%M:%S" mt

-- | Show time in %Y-%m-%d %H:%M format. Warning: system needs to run in UTC time!
showMinutesTimeForAPI :: MinutesTime -> String
showMinutesTimeForAPI mt = formatSwedishMinutesTime "%Y-%m-%d %H:%M" mt

-- | Show time in "%Y-%m-%d" format.  Warning: system needs to run in UTC time!
showDateOnly :: MinutesTime -> String
showDateOnly mt | toSeconds mt == 0 = ""
                | otherwise = formatSwedishMinutesTime "%Y-%m-%d" mt

-- | Swedish time locale is like normal, but has Swedish month abbreviations.
swedishTimeLocale :: TimeLocale
swedishTimeLocale = defaultTimeLocale { months = [ ("januari","jan")
                                                 , ("februari", "feb")
                                                 , ("mars", "mar")
                                                 , ("april", "apr")
                                                 , ("maj", "maj")
                                                 , ("juni", "jun")
                                                 , ("juli", "jul")
                                                 , ("augusti", "aug")
                                                 , ("september", "sep")
                                                 , ("oktober", "okt")
                                                 , ("november", "nov")
                                                 , ("december", "dec")
                                                 ]
                                      , wDays = [ ("måndag", "mån")
                                                , ("tisdag", "tis")
                                                , ("onsdag", "ons")
                                                , ("torsdag", "tor")
                                                , ("fredag", "fre")
                                                , ("lördag", "lör")
                                                , ("söndag", "sön")
                                                ]
                                      }

-- | Show date abbreviated according to how far past that date we
-- are. Options are: %H:%M, %d %b and %Y-%m-%d.  See
-- 'formatCalendarTime' to understand the meaning.
showDateAbbrev :: MinutesTime -> MinutesTime -> String
showDateAbbrev current time
               | ctYear ct1 == ctYear ct && ctMonth ct1 == ctMonth ct && ctDay ct1 == ctDay ct =
                   formatSwedishMinutesTime "%H:%M" time
               | ctYear ct1 == ctYear ct =
                   formatSwedishMinutesTime "%d %b" time
               | otherwise =
                   formatSwedishMinutesTime "%Y-%m-%d" time
               where
                 ct1 = unsafePerformIO $ toCalendarTime $ toClockTime current
                 ct = unsafePerformIO $ toCalendarTime $ toClockTime time

-- | Get current time as 'MinutesTime'. Warning: server should work in UTC time.
getMinutesTime :: IO MinutesTime
getMinutesTime = (return . fromClockTime) =<< getClockTime

-- | Get event time as 'MinutesTime'. Warning: server should work in UTC time.
--
-- Avoid this function. Soon we will need virtual time, not time taken
-- globally. Simulation and unit testing requires time to be specified
-- explicitely.
-- 
-- FIXME: rename to 'getMinutesTimeDB'
getMinuteTimeDB :: AnyEv MinutesTime
getMinuteTimeDB = (return . fromClockTime) =<< getEventClockTime

-- | Convert 'ClockTime' to 'MinutesTime'. Uses just seconds, picoseconds are ignored.
fromClockTime :: ClockTime -> MinutesTime
fromClockTime (TOD secs _picos) =  fromSeconds (fromIntegral secs)

-- | Convert 'MinutesTime' to 'ClockTime'.
toClockTime :: MinutesTime -> ClockTime
toClockTime mt = (TOD (fromIntegral $ toSeconds mt) 0)

-- | Convert 'CalendarTime' to 'MinutesTime' through 'System.Time.toClockTime'.
fromUTCTime :: CalendarTime -> MinutesTime
fromUTCTime = fromClockTime . System.Time.toClockTime

-- | Convert 'MinutesTime' to 'CalendarTime' through 'System.Time.toUTCTime'.
toUTCTime :: MinutesTime -> CalendarTime
toUTCTime = System.Time.toUTCTime . toClockTime

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


-- | Format time according to Swedish rules of time formating.
--
-- This is probably as wrong as it gets. We use current time zone (of the server!) to show correct time.
--
-- FIXME: Fix all of this. We need proper user timezone handling, not such hacks as these.
formatSwedishMinutesTime :: String -> MinutesTime -> String
formatSwedishMinutesTime fmt mt = formatCalendarTime swedishTimeLocale fmt (unsafePerformIO $ toCalendarTime $ toClockTime mt)


-- | Parse format %d-%m-%Y.
parseMinutesTimeDMY :: String -> Maybe MinutesTime
parseMinutesTimeDMY s = do
    t <- parseTime defaultTimeLocale "%d-%m-%Y" s
    startOfTime <- parseTime defaultTimeLocale "%d-%m-%Y" "01-01-1970"
    let val = diffDays t startOfTime
    return $ fromMinutes (fromIntegral $ val *24*60)

-- | Show date as %d-%m-%y. As you see name lies.
showDateDMY :: MinutesTime -> String
showDateDMY = formatSwedishMinutesTime "%d-%m-%y"


-- | Show date as %Y-%m-%d.
showDateYMD :: MinutesTime -> String
showDateYMD = formatSwedishMinutesTime "%Y-%m-%d"

-- | Use as:
--
-- > 5 `minutesAfter` midnight
minutesAfter :: Int -> MinutesTime -> MinutesTime
minutesAfter i (MinutesTime s) = MinutesTime (s + i*60)

-- | Add a month. This adds full month, independed how many days there
-- are in current month. So of 2nd today we end up on 2nd next month.
monthsAfter :: Int -> MinutesTime -> MinutesTime
monthsAfter i t = fromClockTime $ addToClockTime (noTimeDiff {tdMonth = i})  (toClockTime t)

-- | Calculate start of month.
--
-- FIXME: This is wrong as is calculates date in UTC that may be about 2h off and get start of the month wrong.
startOfMonth :: MinutesTime -> MinutesTime
startOfMonth t = let
                   CalendarTime {ctDay,ctHour,ctMin,ctSec,ctPicosec} = toUTCTime t
                   diff = (noTimeDiff {tdDay= (-1)*ctDay+1,tdHour=(-1)*ctHour,tdMin=(-1)*ctMin,tdSec=(-1)*ctSec,tdPicosec=(-1)*ctPicosec})
                 in fromClockTime $ addToClockTime diff  (toClockTime t)


-- | Calcualte day difference between two dates. Rounds the difference
-- down. A day is 24h. First date must be earlier then second,
-- otherwise 0 is returned.
dateDiffInDays :: MinutesTime -> MinutesTime -> Int
dateDiffInDays ctime mtime
    | ctime > mtime = 0
    | otherwise = (toMinutes mtime - toMinutes ctime) `div` (60*24)

-- | Convert a date representation to integer. For date like
-- "2010-06-12" result will bee 20100612. Useful in IntMap for
-- example.
asInt :: MinutesTime -> Int
asInt m = ctYear*10000 + (fromEnum ctMonth+1)*100 + ctDay
  where
    -- January counts as 0, so we need to add 1
    CalendarTime {ctYear,ctMonth,ctDay} = toUTCTime m
