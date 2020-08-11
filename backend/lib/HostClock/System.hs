module HostClock.System
  ( getOffset
  , getFrequency
  ) where

import System.Process (readProcess)
import qualified Data.Text as T

-- | Get the current offset between our host clock and a number of reference time servers
getOffset :: [Text] -> IO Double
getOffset ntpservers = do
  a <- readProcess "ntpdate" (words "-q -p 1" <> (T.unpack <$> ntpservers)) ""
  case take 3 . reverse $ words (last $ lines a) of
    ["sec", offset, "offset"] -> do
      case offset of
        ""           -> fail $ "HostClock.System.getOffset: (empty output)"
        ('+' : rest) -> return (read $ T.pack rest)
        _            -> return (read $ T.pack offset)
    _ -> fail $ "HostClock.System.getOffset:  cannot parse " <> show a

-- | Get the frequency from the kernel phase-lock loop governing the clock.
getFrequency :: IO Double
getFrequency = do
  a <- readProcess "ntpdc" ["-c", "loopinfo oneline"] ""
  let err = fail $ "HostClock.System.getFrequency: cannot parse " ++ show a
  case drop 2 $ words a of
    "frequency" : fs : _ -> case reads fs of
      [(frequency, ",")] -> return $ frequency / 1e6
      _                  -> err
    _ -> err
