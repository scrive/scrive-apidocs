{-# OPTIONS_GHC -fno-warn-orphans #-}

module Session.SessionID
  ( module Auth.Session.SessionID
  ) where

import Auth.Session.SessionID
import Log.Identifier

instance Identifier SessionID where
  idDefaultLabel = "session_id"
  idValue (SessionID k) = int64AsStringIdentifier k
