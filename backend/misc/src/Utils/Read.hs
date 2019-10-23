module Utils.Read where

lookupAndRead :: (Read a, Eq k) => k -> [(k, Text)] -> Maybe a
lookupAndRead k kvs = maybeRead =<< lookup k kvs

lookupAndReadString :: (Read a, Eq k) => k -> [(k, Text)] -> Maybe a
lookupAndReadString k kvs = maybeRead =<< maybeRead =<< lookup k kvs

-- | Pad string with zeros at the beginning.
pad0
  :: Int         -- ^ how long should be the number
  -> String      -- ^ the number as string
  -> String      -- ^ zero padded number
pad0 len str = take missing (repeat '0') <> str
  where
    diff    = len - length str
    missing = max 0 diff

