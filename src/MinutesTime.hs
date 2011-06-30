module MinutesTime
       ( MinutesTime
       , asInt
       , dateDiffInDays
       , fromClockTime
       , fromMinutes
       , fromSeconds
       , fromUTCTime
       , getMinuteTimeDB
       , getMinutesTime
       , minutesAfter
       , monthsAfter
       , parseMinutesTimeMDY
       , showDateAbbrev
       , showDateMDY
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

-- | Time in minutes from 1970-01-01 00:0 in UTC coordinates
newtype MinutesTime0 = MinutesTime0 Int
       deriving (Eq, Ord, Typeable)

-- | Time in minutes from 1970-01-01 00:0 in UTC coordinates
data MinutesTime = MinutesTime
    { minutes :: Int
    , secs :: Int
    }
    deriving (Eq, Ord, Typeable, Data, Read)

instance Version MinutesTime0
$(deriveSerialize ''MinutesTime0)

$(deriveSerialize ''MinutesTime)
instance Version (MinutesTime) where
   mode = extension 1 (Proxy :: Proxy MinutesTime0)

instance Migrate MinutesTime0 MinutesTime where
      migrate (MinutesTime0 m) = MinutesTime {minutes = m, secs = 0 }

instance Show MinutesTime where
    showsPrec _prec (MinutesTime mins secs) =
        let clocktime = TOD (fromIntegral $ mins*60 + secs) 0
            -- FIXME: use TimeZone of user
            calendartime = unsafePerformIO $ toCalendarTime clocktime
        in (++) $ formatCalendarTime defaultTimeLocale
               "%Y-%m-%d, %H:%M:%S" calendartime

-- | Show time in %Y-%m-%d %H:%M format. Warning: system needs to run in UTC time!
showMinutesTimeForAPI :: MinutesTime -> String
showMinutesTimeForAPI (MinutesTime mins secs) =
        let clocktime = TOD (fromIntegral $ mins*60 + secs) 0
            calendartime = unsafePerformIO $ toCalendarTime clocktime
        in formatCalendarTime defaultTimeLocale
               "%Y-%m-%d %H:%M" calendartime

-- | Show time in "%Y-%m-%d" format.  Warning: system needs to run in UTC time!
showDateOnly :: MinutesTime -> String
showDateOnly (MinutesTime 0 _) = ""
showDateOnly (MinutesTime mins _) =
        let clocktime = TOD (fromIntegral mins*60) 0
            -- FIXME: use TimeZone of user
            calendartime = unsafePerformIO $ toCalendarTime clocktime
        in formatCalendarTime defaultTimeLocale
               "%Y-%m-%d" calendartime

-- | Swedish time locale is like normal, but has Swedish month abbreviations.
swedishTimeLocale :: TimeLocale
swedishTimeLocale = defaultTimeLocale { months =
                                            [ ("jan","jan")
                                            , ("feb", "feb")
                                            , ("mar", "mar")
                                            , ("apr", "apr")
                                            , ("maj", "maj")
                                            , ("jun", "jun")
                                            , ("jul", "jul")
                                            , ("aug", "aug")
                                            , ("sep", "sep")
                                            , ("okt", "okt")
                                            , ("nov", "nov")
                                            , ("dec", "dec")
                                            ] }

-- | Show date abbreviated according to how far past that date we
-- are. Options are: %H:%M, %d %b and %Y-%m-%d.  See
-- 'formatCalendarTime' to understand the meaning.
showDateAbbrev :: MinutesTime -> MinutesTime -> String
showDateAbbrev (MinutesTime current _ ) (MinutesTime mins _)
               | ctYear ct1 == ctYear ct && ctMonth ct1 == ctMonth ct && ctDay ct1 == ctDay ct =
                   formatCalendarTime swedishTimeLocale "%H:%M" ct
               | ctYear ct1 == ctYear ct =
                   formatCalendarTime swedishTimeLocale "%d %b" ct
               | otherwise =
                   formatCalendarTime swedishTimeLocale "%Y-%m-%d" ct
               where
                 ct1 = unsafePerformIO $ toCalendarTime $ TOD (fromIntegral current*60) 0
                 ct = unsafePerformIO $ toCalendarTime $ TOD (fromIntegral mins*60) 0

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
fromMinutes m = MinutesTime m 0

-- | Extract the minutes component from 'MinutesTime'. Seconds are ignored.
toMinutes :: MinutesTime -> Int
toMinutes (MinutesTime m _) = m

-- | Convert seconds to proper 'MinutesTime'.
fromSeconds :: Int -> MinutesTime
fromSeconds s = MinutesTime (s `div` 60) (s `mod` 60)

-- | Get number of seconds from 'MinutesTime' since 1970.
toSeconds :: MinutesTime -> Int
toSeconds (MinutesTime m s) = m*60 + s


-- | Parse format %d-%m-%Y.
parseMinutesTimeMDY :: String -> Maybe MinutesTime
parseMinutesTimeMDY s = do
                      t <- parseTime defaultTimeLocale "%d-%m-%Y" s
                      startOfTime <- parseTime defaultTimeLocale "%d-%m-%Y" "01-01-1970"
                      let val = diffDays t startOfTime
                      return (MinutesTime (fromIntegral $ (val *24*60)) 0)

-- | Show date as %d-%m-%y. As you see name lies.
showDateMDY :: MinutesTime -> String
showDateMDY (MinutesTime mins _) =  let clocktime = TOD (fromIntegral mins*60) 0
                                        calendartime = unsafePerformIO $ toCalendarTime clocktime
                                    in formatCalendarTime defaultTimeLocale "%d-%m-%y" calendartime

-- | Show date as %Y-%m-%d.
showDateYMD :: MinutesTime -> String
showDateYMD (MinutesTime mins _) =  let clocktime = TOD (fromIntegral mins*60) 0
                                        calendartime = unsafePerformIO $ toCalendarTime clocktime
                                    in formatCalendarTime defaultTimeLocale "%Y-%m-%d" calendartime

-- | Use as:
--
-- > 5 `minutesAfter` midnight
minutesAfter :: Int -> MinutesTime -> MinutesTime
minutesAfter i (MinutesTime i' s) = MinutesTime (i + i') s

-- | Add a month. This adds full month, independed how many days there
-- are in current month. So of 2nd today we end up on 2nd next month.
monthsAfter :: Int -> MinutesTime -> MinutesTime
monthsAfter i t = fromClockTime $ addToClockTime (noTimeDiff {tdMonth = i})  (toClockTime t)

-- | Calculate start of month.
startOfMonth :: MinutesTime -> MinutesTime
startOfMonth t = let
                   CalendarTime {ctDay,ctHour,ctMin,ctSec,ctPicosec} = toUTCTime t
                   diff = (noTimeDiff {tdDay= (-1)*ctDay+1,tdHour=(-1)*ctHour,tdMin=(-1)*ctMin,tdSec=(-1)*ctSec,tdPicosec=(-1)*ctPicosec})
                 in fromClockTime $ addToClockTime diff  (toClockTime t)


-- | Calcualte day difference between two dates. Rounds the difference
-- down. A day is 24h. First date must be earile then second,
-- otherwise 0 is returned.
dateDiffInDays :: MinutesTime -> MinutesTime -> Int
dateDiffInDays (MinutesTime ctime _) (MinutesTime mtime _)
                       | ctime>mtime = 0
                       | otherwise = (mtime - ctime) `div` (60*24)

-- | Convert a date representation to integer. For date like
-- "2010-06-12" result will bee 20100612. Useful in IntMap for
-- example.
asInt :: MinutesTime -> Int
asInt m = ctYear*10000 + (fromEnum ctMonth+1)*100 + ctDay
  where
    -- January counts as 0, so we need to add 1
    CalendarTime {ctYear,ctMonth,ctDay} = toUTCTime m
