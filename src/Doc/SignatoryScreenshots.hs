module Doc.SignatoryScreenshots
  ( SignatoryScreenshots(..)
  , emptySignatoryScreenshots
  ) where

import Doc.Screenshot
import Doc.ReferenceScreenshot (referenceScreenshot)
import MinutesTime (MinutesTime,fromSeconds)
import Text.JSON.FromJSValue
import Utils.Tuples

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
    return $ Just $ SignatoryScreenshots (mapFst fromSeconds first) (mapFst fromSeconds signing) referenceScreenshot

