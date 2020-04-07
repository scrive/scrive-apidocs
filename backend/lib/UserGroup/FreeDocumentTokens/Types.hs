module UserGroup.FreeDocumentTokens.Types (
    FreeDocumentTokens
  , freeDocumentTokensFromValues
  , noFreeDocumentTokens
  , numberOfValidTokens
  , validityOfTokens
  ) where

import Control.Monad.Time
import Data.Int
import Data.Time.Clock

import MinutesTime

data FreeDocumentTokens = FreeDocumentTokens
  { fdtCount        :: Int32
  , fdtValidity       :: UTCTime
  } deriving (Eq, Ord, Show)

noFreeDocumentTokens :: FreeDocumentTokens
noFreeDocumentTokens = FreeDocumentTokens { fdtCount = 0, fdtValidity = unixEpoch }

freeDocumentTokensFromValues :: Int32 -> UTCTime -> FreeDocumentTokens
freeDocumentTokensFromValues tc v = FreeDocumentTokens { fdtCount = tc, fdtValidity = v }

numberOfValidTokensAtTime :: FreeDocumentTokens -> UTCTime -> Int32
numberOfValidTokensAtTime fdts time =
  if fdtValidity fdts > time then fromIntegral (fdtCount fdts) else 0

numberOfValidTokens :: (MonadTime m) => FreeDocumentTokens -> m Int32
numberOfValidTokens fdts = numberOfValidTokensAtTime fdts <$> currentTime

validityOfTokens :: (MonadTime m) => FreeDocumentTokens -> m UTCTime
validityOfTokens fdts = max (fdtValidity fdts) <$> currentTime
