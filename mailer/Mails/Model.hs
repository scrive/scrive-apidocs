{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module Mails.Model (
    module Mails.Data
  , CreateEmail(..)
  , AddContentToEmail(..)
  , MarkEventAsRead(..)
  , DeleteEmail(..)
  , GetEvents(..)
  , GetIncomingEmails(..)
  , GetEmail(..)
  , MarkEmailAsSent(..)
  , UpdateWithEvent(..)
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

data GetIncomingEmails = GetIncomingEmails
instance DBQuery GetIncomingEmails [Mail] where
  dbQuery GetIncomingEmails = wrapDB $ \conn -> do
    st <- prepare conn $ selectMailsSQL ++ "WHERE title IS NOT NULL AND content IS NOT NULL AND sent IS NULL ORDER BY id DESC"
    _ <- execute st []
    fetchMails st

-- below handlers are for use within mailer only. I can't hide them properly
-- since mailer is not separated into another package yet so it has to be
-- here for now. do not use it though.

data GetEmail = GetEmail MailID MagicHash
instance DBQuery GetEmail (Maybe Mail) where
  dbQuery (GetEmail mid token) = wrapDB $ \conn -> do
    st <- prepare conn $ selectMailsSQL ++ "WHERE id = ? AND token = ?"
    _ <- execute st [toSql mid, toSql token]
    fetchMails st >>= oneObjectReturnedGuard

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

selectMailsSQL :: String
selectMailsSQL = "SELECT"
  ++ "  id"
  ++ ", token"
  ++ ", sender"
  ++ ", receivers"
  ++ ", title"
  ++ ", content"
  ++ ", attachments"
  ++ ", x_smtp_attrs"
  ++ " FROM mails"
  ++ " "

fetchMails :: Statement -> IO [Mail]
fetchMails st = foldDB st decoder []
  where
    -- Note: this function gets mails in reversed order, but all queries
    -- use ORDER BY DESC, so in the end everything is properly ordered.
    decoder acc id' token sender receivers title
      content attachments x_smtp_attrs = Mail {
          mailID = id'
        , mailToken = token
        , mailFrom = sender
        , mailTo = receivers
        , mailTitle = title
        , mailContent = content
        , mailAttachments = attachments
        , mailXSMTPAttrs = x_smtp_attrs
        } : acc
