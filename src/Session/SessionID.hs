module Session.SessionID (
    SessionID
  , tempSessionID
  ) where

import Data.Int

import DB

newtype SessionID = SessionID Int64
  deriving (Eq, Ord)
$(newtypeDeriveConvertible ''SessionID)
$(newtypeDeriveUnderlyingReadShow ''SessionID)

tempSessionID :: SessionID
tempSessionID = SessionID 0
