{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module Mails.Model (
    module Mails.Data
  , CreateEmail(..)
  , AddContentToEmail(..)
  , MarkEventAsRead(..)
  , DeleteEmail(..)
  , DeleteMailsOlderThenDays(..)
  , GetUnreadEvents(..)
  , GetIncomingEmails(..)
  , GetEmails(..)
  , CreateServiceTest(..)
  , GetServiceTestEvents(..)
  , GetEmail(..)
  , ResendEmailsSentSince(..)
  , DeferEmail(..)
  , MarkEmailAsSent(..)
  , UpdateWithEvent(..)
  ) where

import Control.Monad
import Data.Monoid

import DB
import MagicHash
import Mails.Data
import Mails.Tables
import MinutesTime
import OurPrelude

data CreateEmail = CreateEmail MagicHash Address [Address] MinutesTime
instance MonadDB m => DBUpdate m CreateEmail MailID where
  update (CreateEmail token sender to to_be_sent) =
    $(fromJust) `liftM` insertEmail False token sender to to_be_sent

data AddContentToEmail = AddContentToEmail MailID String String [Attachment] XSMTPAttrs
instance MonadDB m => DBUpdate m AddContentToEmail Bool where
  update (AddContentToEmail mid title content attachments xsmtpapi) =
    kRun01 $ mkSQL UPDATE tableMails [
        sql "title" title
      , sql "content" content
      , sql "attachments" attachments
      , sql "x_smtp_attrs" xsmtpapi
      ] <> SQL "WHERE id = ?" [toSql mid]

data MarkEventAsRead = MarkEventAsRead EventID MinutesTime
instance MonadDB m => DBUpdate m MarkEventAsRead Bool where
  update (MarkEventAsRead eid time) =
    kRun01 $ mkSQL UPDATE tableMailEvents [sql "event_read" time]
      <> SQL "WHERE id = ?" [toSql eid]

data DeleteEmail = DeleteEmail MailID
instance MonadDB m => DBUpdate m DeleteEmail Bool where
  update (DeleteEmail mid) = do
    kPrepare "DELETE FROM mails WHERE id = ?"
    kExecute01 [toSql mid]

data DeleteMailsOlderThenDays = DeleteMailsOlderThenDays Integer
instance MonadDB m => DBUpdate m DeleteMailsOlderThenDays Integer where
  update (DeleteMailsOlderThenDays days) = do
    kPrepare $ "DELETE FROM mails where (now() > to_be_sent + interval '"++show days++" days')" -- Sorry but it did not work as param.
    kExecute []

data GetUnreadEvents = GetUnreadEvents
instance MonadDB m => DBQuery m GetUnreadEvents [(EventID, MailID, XSMTPAttrs, Event)] where
  query GetUnreadEvents = getUnreadEvents False

data GetIncomingEmails = GetIncomingEmails
instance MonadDB m => DBQuery m GetIncomingEmails [Mail] where
  query GetIncomingEmails = do
    _ <- kRun $ selectMailsSQL <> SQL "WHERE title IS NOT NULL AND content IS NOT NULL AND to_be_sent <= now() AND sent IS NULL ORDER BY id DESC" []
    fetchMails

data GetEmails = GetEmails
instance MonadDB m => DBQuery m GetEmails [Mail] where
  query GetEmails = do
    _ <- kRun $ selectMailsSQL <> SQL "WHERE title IS NOT NULL AND content IS NOT NULL ORDER BY to_be_sent" []
    fetchMails

-- below handlers are for use within mailer only. I can't hide them properly
-- since mailer is not separated into another package yet so it has to be
-- here for now. do not use it though.

data CreateServiceTest = CreateServiceTest MagicHash Address [Address] MinutesTime
instance MonadDB m => DBUpdate m CreateServiceTest MailID where
  update (CreateServiceTest token sender to to_be_sent) =
    $(fromJust) `liftM` insertEmail True token sender to to_be_sent

data GetServiceTestEvents = GetServiceTestEvents
instance MonadDB m => DBQuery m GetServiceTestEvents [(EventID, MailID, XSMTPAttrs, Event)] where
  query GetServiceTestEvents = getUnreadEvents True

data GetEmail = GetEmail MailID MagicHash
instance MonadDB m => DBQuery m GetEmail (Maybe Mail) where
  query (GetEmail mid token) = do
    _ <- kRun $ selectMailsSQL <> SQL "WHERE id = ? AND token = ?"
      [toSql mid, toSql token]
    fetchMails >>= oneObjectReturnedGuard

data ResendEmailsSentSince = ResendEmailsSentSince MinutesTime
instance MonadDB m => DBUpdate m ResendEmailsSentSince Integer where
  update (ResendEmailsSentSince time) =
    kRun $ mkSQL UPDATE tableMails [sql "sent" SqlNull]
      <> SQL "WHERE service_test = FALSE AND sent >= ?" [toSql time]

data DeferEmail = DeferEmail MailID MinutesTime
instance MonadDB m => DBUpdate m DeferEmail Bool where
  update (DeferEmail mid time) =
    kRun01 $ mkSQL UPDATE tableMails [
        sql "to_be_sent" time
      ] <> SQL "WHERE id = ?" [toSql mid]

data MarkEmailAsSent = MarkEmailAsSent MailID MinutesTime
instance MonadDB m => DBUpdate m MarkEmailAsSent Bool where
  update (MarkEmailAsSent mid time) = do
    kPrepare "UPDATE mails SET sent = ? WHERE id = ?"
    kExecute01 [toSql time, toSql mid]

data UpdateWithEvent = UpdateWithEvent MailID Event
instance MonadDB m => DBUpdate m UpdateWithEvent Bool where
  update (UpdateWithEvent mid ev) = do
    kPrepare "INSERT INTO mail_events (mail_id, event) VALUES (?, ?)"
    kExecute01 [toSql mid, toSql ev]

selectMailsSQL :: SQL
selectMailsSQL = SQL ("SELECT"
  ++ "  id"
  ++ ", token"
  ++ ", sender"
  ++ ", receivers"
  ++ ", title"
  ++ ", content"
  ++ ", attachments"
  ++ ", x_smtp_attrs"
  ++ ", service_test"
  ++ " FROM mails"
  ++ " ") []

insertEmail :: MonadDB m => Bool -> MagicHash -> Address -> [Address] -> MinutesTime -> DBEnv m (Maybe MailID)
insertEmail service_test token sender to to_be_sent =
  getOne $ mkSQL INSERT tableMails [
      sql "token" token
    , sql "sender" sender
    , sql "receivers" to
    , sql "to_be_sent" to_be_sent
    , sql "service_test" service_test
    ] <> SQL "RETURNING id" []

getUnreadEvents :: MonadDB m => Bool -> DBEnv m [(EventID, MailID, XSMTPAttrs, Event)]
getUnreadEvents service_test = do
  kPrepare $ "SELECT"
    ++ "  e.id"
    ++ ", e.mail_id"
    ++ ", m.x_smtp_attrs"
    ++ ", e.event"
    ++ " FROM mails m JOIN mail_events e ON (m.id = e.mail_id)"
    ++ " WHERE m.service_test = ? AND e.event_read IS NULL"
    ++ " ORDER BY m.id DESC, e.id DESC"
  _ <- kExecute [toSql service_test]
  foldDB fetchEvents []
  where
    fetchEvents acc eid mid attrs event = (eid, mid, attrs, event) : acc

fetchMails :: MonadDB m => DBEnv m [Mail]
fetchMails = foldDB decoder []
  where
    -- Note: this function gets mails in reversed order, but all queries
    -- use ORDER BY DESC, so in the end everything is properly ordered.
    decoder acc mid token sender receivers title content
     attachments x_smtp_attrs service_test = Mail {
          mailID = mid
        , mailToken = token
        , mailFrom = sender
        , mailTo = receivers
        , mailTitle = title
        , mailContent = content
        , mailAttachments = attachments
        , mailXSMTPAttrs = x_smtp_attrs
        , mailServiceTest = service_test
        } : acc
