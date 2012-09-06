module Utils.Read where

import Control.Applicative
import Control.Monad
import Numeric

maybeReadM :: (Monad m, Read a, Functor m) =>  m (Maybe String) -> m (Maybe a)
maybeReadM c = join <$> fmap maybeRead <$> c

maybeRead::(Read a) => String -> Maybe a
maybeRead s = case reads s of
  [(v, "")] -> Just v
  _         -> Nothing

maybeReadIntM :: (Monad m, Functor m) => m (Maybe String) -> m (Maybe Int)
maybeReadIntM c = join <$> fmap maybeReadInt <$> c

maybeReadInt :: String -> Maybe Int
maybeReadInt s = case readDec s of
  [(v, "")] -> Just v
  _         -> Nothing

lookupAndRead :: (Read a, Eq k) => k -> [(k, String)] -> Maybe a
lookupAndRead k kvs = maybeRead =<< lookup k kvs

lookupAndReadString :: (Read a, Eq k) => k -> [(k, String)] -> Maybe a
lookupAndReadString k kvs = maybeRead =<< maybeRead =<< lookup k kvs

-- | Pad string with zeros at the beginning.
pad0 :: Int         -- ^ how long should be the number
     -> String      -- ^ the number as string
     -> String      -- ^ zero padded number
pad0 len str = take missing (repeat '0') ++ str
  where
    diff = len - length str
    missing = max 0 diff
