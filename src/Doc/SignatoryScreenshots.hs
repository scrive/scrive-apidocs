module Doc.SignatoryScreenshots
  ( T(..)
  ) where

import Data.Monoid (Monoid(..), First(..))
import qualified Doc.Screenshot as Screenshot
import MinutesTime (MinutesTime)


data T = T
  { first   :: Maybe (MinutesTime, Screenshot.T)
  , signing :: Maybe (MinutesTime, Screenshot.T)
  } deriving Show

instance Monoid T where
  mempty = T Nothing Nothing
  T a1 b1 `mappend` T a2 b2 = T (getFirst $ First a1 `mappend` First a2)
                                (getFirst $ First b1 `mappend` First b2)