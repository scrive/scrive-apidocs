module Session.Constant (
    defaultSessionTimeoutSecs
  , maxSessionTimeoutSecs
  , minSessionTimeoutSecs
  , maxSessionExpirationDelaySecs
  )
where

-- Cookie session timeout in 12 hours by default
defaultSessionTimeoutSecs :: Num a => a
defaultSessionTimeoutSecs = 12 * 60 * 60

-- Maximum session timeout to be 30 days
maxSessionTimeoutSecs :: Num a => a
maxSessionTimeoutSecs = 30 * 24 * 60 * 60

-- Minimum session timeout to be 5 minutes
minSessionTimeoutSecs :: Num a => a
minSessionTimeoutSecs = 5 * 60

-- Extend session expiration for maximum of 2 hours
-- from current time when session is expiring.
maxSessionExpirationDelaySecs :: Num a => a
maxSessionExpirationDelaySecs = 2 * 60 * 60

