{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators, TypeSynonymInstances,
    GeneralizedNewtypeDeriving, StandaloneDeriving, NamedFieldPuns
    #-}


module MinutesTime where

import System.Time
import Happstack.Data
import Data.Data
import System.Locale
import System.IO.Unsafe

$(deriveAll [''Eq, ''Ord, ''Default]
  [d|

   -- | Time in minutes from 1970-01-01 00:00 in UTC coordinates
   newtype MinutesTime = MinutesTime Int

 |])

instance Show MinutesTime where
    showsPrec prec (MinutesTime mins) = 
        let clocktime = TOD (fromIntegral mins*60) 0
            -- FIXME: use TimeZone of user
            calendartime = unsafePerformIO $ toCalendarTime clocktime
        in (++) $ formatCalendarTime defaultTimeLocale 
               "%Y-%m-%d %H:%M" calendartime

instance Version MinutesTime
$(deriveSerialize ''MinutesTime)

getMinutesTime = do
  TOD secs picos <- getClockTime
  return (MinutesTime (fromIntegral $ (secs `div` 60)))

  
toUTCTime (MinutesTime time) = 
    System.Time.toUTCTime (TOD (fromIntegral time * 60) 0)