module MinutesTime
       ( MinutesTime
       , KontraTimeLocale (..)
       , asInt
       , fromSeconds
       , fromMinutes
       , getMinutesTime
       , minutesAfter
       , minutesBefore
       , parseMinutesTimeDMY
       , showDateAbbrev
       , showDateDMY
       , showDateOnly
       , parseDateOnly
       , showDateYMD
       , showMinutesTimeForAPI
       , showMinutesTimeForFileName
       , toMinutes
       , toSeconds
       , toUTCTime
       , showAsMonth
       , showAsDate
       , formatMinutesTimeUTC
       , formatMinutesTimeISO
       , formatMinutesTime
       , parseMinutesTimeUTC
       , parseMinutesTimeISO
       , monthsBefore
       , daysBefore
       , daysAfter
       , mtMonth
       , mtYear
       ) where

import Control.Monad.IO.Class
import Data.Char
import Data.Convertible
import Data.SafeCopy
import Data.Time
import Data.Time.Clock.POSIX
import Data.Typeable
import Database.HDBC
import System.Locale
import System.Time hiding (toClockTime, toUTCTime, toCalendarTime)
import qualified System.Time as System.Time (toUTCTime, toCalendarTime)
import Text.Printf
import System.IO.Unsafe

-- | Time in seconds from 1970-01-01 00:00:00 in UTC coordinates
-- Same as POSIX seconds and what every other database uses as TIMESTAMP time type.
newtype MinutesTime = MinutesTime Int
    deriving (Eq, Ord, Typeable)

$(deriveSafeCopy 0 'base ''MinutesTime)

instance Show MinutesTime where
    show = formatMinutesTime defaultKontraTimeLocale "%Y-%m-%d, %H:%M:%S %Z"

-- | Show time in %Y-%m-%d %H:%M:%S %Z format.
-- This change was requested by Upsales. Should not affect much.
showMinutesTimeForAPI :: MinutesTime -> String
showMinutesTimeForAPI mt = formatMinutesTime defaultKontraTimeLocale "%Y-%m-%d %H:%M:%S %Z" mt

-- | Show time for files creation
showMinutesTimeForFileName :: MinutesTime -> String
showMinutesTimeForFileName mt = formatMinutesTime defaultKontraTimeLocale "%Y-%m-%d-%H-%M" mt


-- | Show time in "%Y-%m-%d" format.
showDateOnly :: MinutesTime -> String
showDateOnly mt | toSeconds mt == 0 = ""
                | otherwise = formatMinutesTime defaultKontraTimeLocale "%Y-%m-%d" mt

formatMinutesTimeISO :: MinutesTime -> String
formatMinutesTimeISO = formatMinutesTime defaultKontraTimeLocale "%Y-%m-%d %H:%M:%S"

parseMinutesTimeISO :: String -> Maybe MinutesTime
parseMinutesTimeISO = parseMinutesTime "%Y-%m-%d %H:%M:%S %Z"

parseMinutesTimeUTC :: String -> Maybe MinutesTime
parseMinutesTimeUTC = parseMinutesTime "%Y-%m-%d %H:%M:%S"

parseMinutesTime :: String -> String -> Maybe MinutesTime
parseMinutesTime format string = do
    time <- parseTime defaultTimeLocale format string
    return $ fromSeconds $ floor $ utcTimeToPOSIXSeconds time

parseDateOnly :: String -> Maybe MinutesTime
parseDateOnly = parseMinutesTime "%Y-%m-%d"

{- |
    Use this to tell formatting functions the locale
    you would like to use when formatting dates and times.
    For example, if you say SwedishTimeLocale then october will be abbreviated
    to "okt", but if you say BritishTimeLocale then october will be
    abbreviated to "oct".
-}
data KontraTimeLocale = SwedishTimeLocale | BritishTimeLocale

defaultKontraTimeLocale :: KontraTimeLocale
defaultKontraTimeLocale = SwedishTimeLocale

getTimeLocale :: KontraTimeLocale -> TimeLocale
getTimeLocale SwedishTimeLocale = swedishTimeLocale
getTimeLocale BritishTimeLocale = britishTimeLocale

-- | British time locale is like normal one
britishTimeLocale :: TimeLocale
britishTimeLocale = defaultTimeLocale { months = map lowerCase $ months defaultTimeLocale
                                      , wDays = map lowerCase $ wDays defaultTimeLocale }
  where
    lowerCase :: (String, String) -> (String, String)
    lowerCase (a, b) = (map toLower a, map toLower b)

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
showDateAbbrev :: KontraTimeLocale -> MinutesTime -> MinutesTime -> String
showDateAbbrev locale current time
               | ctYear ct1 == ctYear ct && ctMonth ct1 == ctMonth ct && ctDay ct1 == ctDay ct =
                   formatMinutesTime locale "%H:%M %Z" time
               | ctYear ct1 == ctYear ct =
                   formatMinutesTime locale "%d %b" time
               | otherwise =
                   formatMinutesTime locale "%Y-%m-%d" time
               where
                 ct1 = toCalendarTime current
                 ct = toCalendarTime time

-- | Get current time as 'MinutesTime'. Warning: server should work in UTC time.
getMinutesTime :: MonadIO m => m MinutesTime
getMinutesTime = liftIO $ (return . fromClockTime) =<< getClockTime

-- | Convert 'ClockTime' to 'MinutesTime'. Uses just seconds, picoseconds are ignored.
fromClockTime :: ClockTime -> MinutesTime
fromClockTime (TOD secs _picos) =  fromSeconds (fromIntegral secs)

-- | Convert 'MinutesTime' to 'ClockTime'.
toClockTime :: MinutesTime -> ClockTime
toClockTime mt = (TOD (fromIntegral $ toSeconds mt) 0)

-- | Convert 'MinutesTime' to 'CalendarTime' through 'System.Time.toUTCTime'.
toUTCTime :: MinutesTime -> CalendarTime
toUTCTime = System.Time.toUTCTime . toClockTime

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
formatMinutesTime :: KontraTimeLocale -> String -> MinutesTime -> String
formatMinutesTime ktl fmt mt = formatCalendarTime (getTimeLocale ktl) fmt (toCalendarTime mt)

formatMinutesTimeUTC :: MinutesTime -> String
formatMinutesTimeUTC mt = formatCalendarTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (toUTCTime mt)


-- | Parse format %d-%m-%Y.
parseMinutesTimeDMY :: String -> Maybe MinutesTime
parseMinutesTimeDMY = parseMinutesTime "%d-%m-%Y"

-- | Show date as %d-%m-%y.
showDateDMY :: MinutesTime -> String
showDateDMY = formatMinutesTime defaultKontraTimeLocale "%d-%m-%y"


-- | Show date as %Y-%m-%d.
showDateYMD :: MinutesTime -> String
showDateYMD = formatMinutesTime defaultKontraTimeLocale "%Y-%m-%d"

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
monthsBefore i mt = daysBefore (i * 31) mt

-- | Convert a date representation to integer. For date like
-- "2010-06-12" result will be 20100612. Useful in IntMap for
-- example.
asInt :: MinutesTime -> Int
asInt m = ctYear*10000 + (fromEnum ctMonth+1)*100 + ctDay
  where
    -- January counts as 0, so we need to add 1
    CalendarTime {ctYear,ctMonth,ctDay} = toUTCTime m

showAsDate :: Int -> String
showAsDate int = printf "%04d-%02d-%02d" (int `div` 10000) (int `div` 100 `mod` 100) (int `mod` 100)

showAsMonth :: Int -> String
showAsMonth int = printf "%04d-%02d" (int `div` 10000) (int `div` 100 `mod` 100)

instance Convertible SqlValue MinutesTime where
  safeConvert = either Left (Right . fromClockTime) . safeConvert

instance Convertible MinutesTime SqlValue where
  safeConvert = safeConvert . toUTCTime
