module Doc.SignatoryScreenshots
  ( T(..)
  , empty
  ) where

import qualified Doc.Screenshot as Screenshot
import Doc.ReferenceScreenshot (referenceScreenshot)
import MinutesTime (MinutesTime)


data T = T
  { first     :: Maybe (MinutesTime, Screenshot.T)
  , signing   :: Maybe (MinutesTime, Screenshot.T)
  , reference :: (MinutesTime, Screenshot.T)
  } deriving Show

empty :: T
empty = T Nothing Nothing referenceScreenshot