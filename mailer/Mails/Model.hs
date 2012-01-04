module Mails.Model (
    module Mails.Public
  , MarkEmailAsSent(..)
  , UpdateWithEvent(..)
  ) where

import Database.HDBC

import DB.Classes
import DB.Utils
import Mails.Data
import Mails.Public
import MinutesTime

data MarkEmailAsSent = MarkEmailAsSent MailID MinutesTime
instance DBUpdate MarkEmailAsSent Bool where
  dbUpdate (MarkEmailAsSent mid time) = wrapDB $ \conn -> do
    r <- run conn "UPDATE mails SET sent = ? WHERE id = ?"
      [toSql time, toSql mid]
    oneRowAffectedGuard r

data UpdateWithEvent = UpdateWithEvent MailID Event
instance DBUpdate UpdateWithEvent Bool where
  dbUpdate (UpdateWithEvent mid ev) = wrapDB $ \conn -> do
    r <- run conn "INSERT INTO mail_events (mail_id, event) VALUES (?, ?)"
      [toSql mid, toSql ev]
    oneRowAffectedGuard r
