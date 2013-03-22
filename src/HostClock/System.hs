module HostClock.System
  ( defaultNtpServers,
    getOffset
  , getFrequency
  ) where

import System.Process (readProcess)

defaultNtpServers :: [String]
defaultNtpServers = [ show n ++ ".ubuntu.pool.ntp.org" | n <- [0..3] ]

-- | Get the current offset between our host clock and a number of reference time servers
getOffset :: [String] -> IO Double
getOffset ntpservers = do
  a <- readProcess "/usr/sbin/ntpdate" (words "-q -p 1" ++ ntpservers) ""
  case take 3 $ reverse $ words $ last $ lines a of
    ["sec", offset, "offset"] -> return (read offset)
    _                         -> fail $ "HostClock.System.getOffset:  cannot parse " ++ show a

-- | Get the frequency from the kernel phase-lock loop governing the clock.
getFrequency :: IO Double
getFrequency = do
  a <- readProcess "/usr/bin/ntpdc" ["-c", "loopinfo oneline"] ""
  let err = fail $ "HostClock.System.getFrequency: cannot parse " ++ show a
  case drop 2 $ words a of
    "frequency":fs:_ -> case reads fs of
                          [(frequency,",")] -> return $ frequency / 1e6
                          _ -> err
    _ -> err
