{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module Mails.Public (
    module Mails.Data
  , CreateEmail(..)
  , AddContentToEmail(..)
  , GetEvents(..)
  , MarkEventAsRead(..)
  , DeleteEmail(..)
  ) where

import Control.Applicative
import Database.HDBC

import DB.Classes
import DB.Fetcher2
import DB.Types
import DB.Utils
import Mails.Data
import MinutesTime
import OurPrelude

data CreateEmail = CreateEmail MagicHash Address [Address]
instance DBUpdate CreateEmail MailID where
  dbUpdate (CreateEmail token sender to) = $(fromJust)
    <$> getOne "INSERT INTO mails (token, sender, receivers) VALUES (?, ?, ?) RETURNING id"
      [toSql token, toSql sender, toSql to]

data AddContentToEmail = AddContentToEmail MailID String String [Attachment] XSMTPAttrs
instance DBUpdate AddContentToEmail Bool where
  dbUpdate (AddContentToEmail mid title content attachments xsmtpapi) = wrapDB $ \conn -> do
    r <- run conn ("UPDATE mails SET"
      ++ "  title = ?"
      ++ ", content = ?"
      ++ ", attachments = ?"
      ++ ", x_smtp_attrs = ? WHERE id = ?") [
          toSql title
        , toSql content
        , toSql attachments
        , toSql xsmtpapi
        , toSql mid
        ]
    oneRowAffectedGuard r

data GetEvents = GetEvents
instance DBQuery GetEvents [(EventID, MailID, XSMTPAttrs, Event)] where
  dbQuery GetEvents = wrapDB $ \conn -> do
    st <- prepare conn $ "SELECT "
      ++ "  e.id"
      ++ ", e.mail_id"
      ++ ", m.x_smtp_attrs"
      ++ ", e.event"
      ++ " FROM mails m JOIN mail_events e ON (m.id = e.mail_id)"
      ++ " ORDER BY m.id DESC, e.id DESC"
    _ <- execute st []
    foldDB st fetchEvents []
    where
      fetchEvents acc eid mid attrs event = (eid, mid, attrs, event) : acc

data MarkEventAsRead = MarkEventAsRead EventID MinutesTime
instance DBUpdate MarkEventAsRead Bool where
  dbUpdate (MarkEventAsRead eid time) = wrapDB $ \conn -> do
    r <- run conn "UPDATE mail_events SET event_read = ? WHERE id = ?"
      [toSql time, toSql eid]
    oneRowAffectedGuard r

data DeleteEmail = DeleteEmail MailID
instance DBUpdate DeleteEmail Bool where
  dbUpdate (DeleteEmail mid) = wrapDB $ \conn -> do
    r <- run conn "DELETE FROM mails WHERE id = ?"
      [toSql mid]
    oneRowAffectedGuard r
