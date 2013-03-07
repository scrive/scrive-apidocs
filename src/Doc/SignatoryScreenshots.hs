module Doc.SignatoryScreenshots
  ( SignatoryScreenshots(..)
  , emptySignatoryScreenshots
  ) where

import Doc.Screenshot
import Doc.ReferenceScreenshot (referenceScreenshot)
import MinutesTime (MinutesTime,parseMinutesTimeRealISO)
import Text.JSON.FromJSValue

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
    return $ Just $ SignatoryScreenshots (withISO first) (withISO signing) referenceScreenshot
   where
    withISO Nothing = Nothing
    withISO (Just (m,v)) = case (parseMinutesTimeRealISO m) of
                               Nothing -> Nothing
                               Just m' -> Just (m',v)
