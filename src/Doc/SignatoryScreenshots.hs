module Doc.SignatoryScreenshots
  ( SignatoryScreenshots(..)
  , emptySignatoryScreenshots
  ) where

import Doc.Screenshot
import MinutesTime (MinutesTime,parseMinutesTimeRealISO, fromSeconds)
import Text.JSON.FromJSValue
import Data.Maybe
import qualified Data.ByteString as BS
import System.IO.Unsafe
import DB.Binary

referenceScreenshotBinary :: Screenshot
referenceScreenshotBinary = unsafePerformIO $ do
  jpg <- BS.readFile "frontend/app/reference_screenshot.jpg"
  return (Screenshot (Binary jpg))

referenceScreenshotTime :: MinutesTime
referenceScreenshotTime = unsafePerformIO $ do
  mt <- readFile "frontend/app/reference_screenshot_seconds.txt"
  seconds <- readIO mt
  return (fromSeconds seconds)

{-# NOINLINE referenceScreenshot #-}
referenceScreenshot :: (MinutesTime, Screenshot)
referenceScreenshot = (referenceScreenshotTime,referenceScreenshotBinary)

data SignatoryScreenshots = SignatoryScreenshots
  { first     :: Maybe (MinutesTime, Screenshot)
  , signing   :: Maybe (MinutesTime, Screenshot)
  , reference :: (MinutesTime, Screenshot)
  } deriving Show

emptySignatoryScreenshots :: SignatoryScreenshots
emptySignatoryScreenshots = SignatoryScreenshots Nothing Nothing referenceScreenshot

instance FromJSValue SignatoryScreenshots where
  fromJSValueM = do
    first <- fromJSValueField "first"
    signing <- fromJSValueField "signing"
    reference <- fromJSValueField "reference"
    return $ Just $ SignatoryScreenshots (withISO first) (withISO signing) (fromMaybe referenceScreenshot (withISO reference))
   where
    withISO Nothing = Nothing
    withISO (Just (m,v)) = case (parseMinutesTimeRealISO m) of
                               Nothing -> Nothing
                               Just m' -> Just (m',v)
