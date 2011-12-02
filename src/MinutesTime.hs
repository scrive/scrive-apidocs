module MinutesTime
       ( MinutesTime
       , KontraTimeLocale (..)
       , asInt
       , fromSeconds
       , getMinuteTimeDB
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
       ) where

import Control.Monad.IO.Class
import Data.Char
import Data.Data 
import Data.Time
import Happstack.Data
import Happstack.State
import System.Locale
import System.Time hiding (toClockTime, toUTCTime)
import qualified System.Time as System.Time (toUTCTime)
import Text.Printf

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
-- Same as POSIX seconds and what every other database uses as TIMESTAMP time type.
newtype MinutesTime = MinutesTime Int
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
    showsPrec _prec mt = (++) $ formatMinutesTime defaultKontraTimeLocale "%Y-%m-%d, %H:%M:%S %Z" mt

-- | Show time in %Y-%m-%d %H:%M format.
showMinutesTimeForAPI :: MinutesTime -> String
showMinutesTimeForAPI mt = formatMinutesTime defaultKontraTimeLocale "%Y-%m-%d %H:%M" mt

-- | Show time for files creation
showMinutesTimeForFileName :: MinutesTime -> String
showMinutesTimeForFileName mt = formatMinutesTime defaultKontraTimeLocale "%Y-%m-%d-%H-%M" mt


-- | Show time in "%Y-%m-%d" format.
showDateOnly :: MinutesTime -> String
showDateOnly mt | toSeconds mt == 0 = ""
                | otherwise = formatMinutesTime defaultKontraTimeLocale "%Y-%m-%d" mt

parseDateOnly :: String -> Maybe MinutesTime 
parseDateOnly s =  do
    t <- parseTime defaultTimeLocale "%Y-%m-%d" s
    startOfTime <- parseTime defaultTimeLocale "%d-%m-%Y" "01-01-1970"
    let val = diffDays t startOfTime
    return $ fromMinutes (fromIntegral $ val *24*60)

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
                 ct1 = System.Time.toUTCTime $ toClockTime current
                 ct = System.Time.toUTCTime $ toClockTime time

-- | Get current time as 'MinutesTime'. Warning: server should work in UTC time.
getMinutesTime :: MonadIO m => m MinutesTime
getMinutesTime = liftIO $ (return . fromClockTime) =<< getClockTime

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
formatMinutesTime :: KontraTimeLocale -> String -> MinutesTime -> String
formatMinutesTime ktl fmt mt = formatCalendarTime (getTimeLocale ktl) fmt (System.Time.toUTCTime $ toClockTime mt)

-- | Parse format %d-%m-%Y.
parseMinutesTimeDMY :: String -> Maybe MinutesTime
parseMinutesTimeDMY s = do
    t <- parseTime defaultTimeLocale "%d-%m-%Y" s
    startOfTime <- parseTime defaultTimeLocale "%d-%m-%Y" "01-01-1970"
    let val = diffDays t startOfTime
    return $ fromMinutes (fromIntegral $ val *24*60)

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
