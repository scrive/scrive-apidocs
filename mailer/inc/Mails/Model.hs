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
  , GetEmailsByRecipient(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Data.Int
import Data.Maybe hiding (fromJust)
import Data.Monoid.Utils
import qualified Data.Map as Map

import DB
import MagicHash
import Mails.Data
import MinutesTime
import OurPrelude

data CreateEmail = CreateEmail MagicHash Address [Address]
instance (MonadDB m, MonadThrow m) => DBUpdate m CreateEmail MailID where
  update (CreateEmail token sender to) =
    $(fromJust) `liftM` insertEmail False token sender to 0

data AddContentToEmail = AddContentToEmail MailID String (Maybe Address) String [Attachment] XSMTPAttrs
instance (MonadDB m, MonadThrow m) => DBUpdate m AddContentToEmail Bool where
  update (AddContentToEmail mid title reply_to content attachments xsmtpapi) = do
    result <- runQuery01 $ sqlUpdate "mails" $ do
      sqlSet "title" title
      sqlSet "reply_to" reply_to
      sqlSet "content" content
      sqlSet "x_smtp_attrs" xsmtpapi
      sqlWhereEq "id" mid
    when result $ do
      runQuery_ $ sqlDelete "mail_attachments" $
        sqlWhereEq "mail_id" mid
      when (not $ null attachments) $ do
        runQuery_ $ sqlInsert "mail_attachments" $ do
          sqlSet "mail_id" mid
          sqlSetList "name" $ attName <$> attachments
          sqlSetList "content" $ either (Just . Binary) (const Nothing) . attContent <$> attachments
          sqlSetList "file_id" $ either (const Nothing) (Just) . attContent <$> attachments
    return result

data MarkEventAsRead = MarkEventAsRead EventID UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m MarkEventAsRead Bool where
  update (MarkEventAsRead eid time) =
    runQuery01 $ sqlUpdate "mail_events" $ do
      sqlSet "event_read" time
      sqlWhereEq "id" eid

data DeleteEmail = DeleteEmail MailID
instance (MonadDB m, MonadThrow m) => DBUpdate m DeleteEmail Bool where
  update (DeleteEmail mid) = do
    runQuery01 $ sqlDelete "mails" $ do
      sqlWhereEq "id" mid

data DeleteMailsOlderThenDays = DeleteMailsOlderThenDays Int
instance (MonadDB m, MonadTime m) => DBUpdate m DeleteMailsOlderThenDays Int where
  update (DeleteMailsOlderThenDays days) = do
    past <- (days `daysBefore`) <$> currentTime
    runQuery . sqlDelete "mails" $ do
      sqlWhere $ "sent <=" <?> past

data GetUnreadEvents = GetUnreadEvents
instance MonadDB m => DBQuery m GetUnreadEvents [(EventID, MailID, XSMTPAttrs, Event)] where
  query GetUnreadEvents = getUnreadEvents False

selectMails :: MonadDB m => SqlSelect -> m [Mail]
selectMails query' = do
  runQuery_ $ "CREATE TEMP TABLE mails_tmp AS" <+> toSQLCommand query'
  runSQL_ "SELECT * FROM mails_tmp"
  mails <- fetchMany fetchMail

  runQuery_ . sqlSelect "mail_attachments" $ do
    sqlResult "mail_id"
    sqlResult "name"
    sqlResult "content"
    sqlResult "file_id"
    sqlOrderBy "id"
    sqlWhereExists $ sqlSelect "mails_tmp" $
      sqlWhere "mails_tmp.id = mail_attachments.mail_id"

  attachments <- fetchMailAttachments

  runSQL_ "DROP TABLE mails_tmp"

  return (flip map mails $ \mail -> mail { mailAttachments = Map.findWithDefault [] (mailID mail) attachments })


data GetIncomingEmails = GetIncomingEmails
instance (MonadDB m, MonadTime m) => DBQuery m GetIncomingEmails [Mail] where
  query GetIncomingEmails = do
    now <- currentTime
    selectMails $ sqlSelectMails $ do
      sqlWhere "title IS NOT NULL"
      sqlWhere "content IS NOT NULL"
      sqlWhere "sent IS NULL"
      sqlWhere $ "to_be_sent <=" <?> now

data GetEmails = GetEmails
instance MonadDB m => DBQuery m GetEmails [Mail] where
  query GetEmails = do
    selectMails $ sqlSelectMails $ do sqlWhere "title IS NOT NULL AND content IS NOT NULL"

data GetEmailsByRecipient = GetEmailsByRecipient String
instance MonadDB m => DBQuery m GetEmailsByRecipient [Mail] where
  query (GetEmailsByRecipient recipient) = do
    selectMails $ sqlSelectMails $ do
      sqlWhere "title IS NOT NULL"
      sqlWhere "content IS NOT NULL"
      -- receivers is yet another crappy json field in database
      -- change it into proper SQL column some later time
      sqlWhereILike "receivers" ("%\"" ++ recipient ++ "\"%")

-- below handlers are for use within mailer only. I can't hide them properly
-- since mailer is not separated into another package yet so it has to be
-- here for now. do not use it though.

data CreateServiceTest = CreateServiceTest MagicHash Address [Address]
instance (MonadDB m, MonadThrow m) => DBUpdate m CreateServiceTest MailID where
  update (CreateServiceTest token sender to) =
    $(fromJust) `liftM` insertEmail True token sender to 0

data GetServiceTestEvents = GetServiceTestEvents
instance MonadDB m => DBQuery m GetServiceTestEvents [(EventID, MailID, XSMTPAttrs, Event)] where
  query GetServiceTestEvents = getUnreadEvents True

data GetEmail = GetEmail MailID MagicHash
instance MonadDB m => DBQuery m GetEmail (Maybe Mail) where
  query (GetEmail mid token) = do
    mails <- selectMails . sqlSelectMails $ do
      sqlWhereEq "id" mid
      sqlWhereEq "token" token
    return $ listToMaybe mails

data ResendEmailsSentSince = ResendEmailsSentSince UTCTime
instance MonadDB m => DBUpdate m ResendEmailsSentSince Int where
  update (ResendEmailsSentSince time) =
    runQuery $ sqlUpdate "mails" $ do
      sqlSet "sent" (Nothing :: Maybe UTCTime)
      sqlWhereEq "service_test" False
      sqlWhere $ "sent >= " <?> time

data DeferEmail = DeferEmail MailID UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m DeferEmail Bool where
  update (DeferEmail mid time) =
    runQuery01 $ sqlUpdate "mails" $ do
      sqlSet "to_be_sent" time
      sqlSetCmd "attempt" "attempt + 1"
      sqlWhereEq "id" mid

data MarkEmailAsSent = MarkEmailAsSent MailID UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m MarkEmailAsSent Bool where
  update (MarkEmailAsSent mid time) = do
    runQuery01 . sqlUpdate "mails" $ do
      sqlSet "sent" time
      sqlWhereEq "id" mid

data UpdateWithEvent = UpdateWithEvent MailID Event
instance (MonadDB m, MonadThrow m) => DBUpdate m UpdateWithEvent Bool where
  update (UpdateWithEvent mid ev) = do
    runQuery01 . sqlInsert "mail_events" $ do
      sqlSet "mail_id" mid
      sqlSet "event" ev

sqlSelectMails :: State SqlSelect () -> SqlSelect
sqlSelectMails refine = sqlSelect "mails" $ do
  sqlResult "mails.id"
  sqlResult "mails.token"
  sqlResult "mails.sender"
  sqlResult "mails.receivers"
  sqlResult "mails.title"
  sqlResult "mails.content"
  sqlResult "mails.x_smtp_attrs"
  sqlResult "mails.service_test"
  sqlResult "mails.attempt"
  sqlResult "mails.reply_to"
  sqlOrderBy "service_test ASC"
  sqlOrderBy "id"
  refine

insertEmail :: (MonadDB m, MonadThrow m) => Bool -> MagicHash -> Address -> [Address] -> Int32 -> m (Maybe MailID)
insertEmail service_test token sender to attempt = do
  runQuery_ . sqlInsert "mails" $ do
    sqlSet "token" token
    sqlSet "sender" sender
    sqlSet "receivers" to
    sqlSet "to_be_sent" unixEpoch
    sqlSet "service_test" service_test
    sqlSet "attempt" attempt
    sqlResult "id"
  fetchMaybe runIdentity

getUnreadEvents :: MonadDB m => Bool -> m [(EventID, MailID, XSMTPAttrs, Event)]
getUnreadEvents service_test = do
  runQuery_ . sqlSelect "mails" $ do
    sqlResult "mail_events.id"
    sqlResult "mail_events.mail_id"
    sqlResult "mails.x_smtp_attrs"
    sqlResult "mail_events.event"
    sqlJoinOn "mail_events" "mails.id = mail_events.mail_id"
    sqlWhereEq "mails.service_test" service_test
    sqlWhere "mail_events.event_read IS NULL"
    sqlOrderBy "mails.id"
    sqlOrderBy "mail_events.id"
  fetchMany id

fetchMail :: (MailID, MagicHash, Address, [Address], String, String, XSMTPAttrs, Bool, Int32, Maybe Address) -> Mail
fetchMail (mid, token, sender, receivers, title, content, x_smtp_attrs, service_test, attempt, reply_to) = Mail {
  mailID = mid
, mailToken = token
, mailFrom = sender
, mailTo = receivers
, mailTitle = title
, mailContent = content
, mailAttachments = []
, mailXSMTPAttrs = x_smtp_attrs
, mailServiceTest = service_test
, mailAttempt = attempt
, mailReplyTo = reply_to
}

fetchMailAttachments :: MonadDB m => m (Map.Map MailID [Attachment])
fetchMailAttachments = foldrDB decoder Map.empty
  where
    decoder (mid, name, content, file_id) acc = return $ Map.insertWith' (++) mid [Attachment {
      attName = name
    , attContent = case (content, file_id) of
      (Just content', Nothing) -> Left $ unBinary content'
      (Nothing, Just file_id') -> Right file_id'
      _ -> error "fetchMailAttachments: content/file_id mismatch"
    }] acc
