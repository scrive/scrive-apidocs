module Mails.Model (
    module Mails.Data
  , mailerJobNotificationChannel
  , mailerJobSelectors
  , mailerJobFetcher
  , GetCurrentSenderType(..)
  , SwitchToSlaveSenderImmediately(..)
  , CollectServiceTestResultIn(..)
  , ScheduleServiceTest(..)
  , mailSelectors
  , mailFetcher
  , mailNotificationChannel
  , CreateEmail(..)
  , CreateServiceTest(..)
  , GetEmail(..)
  , GetEmailForRecipient(..)
  , GetEmailsForTest(..)
  , ResendEmailsSentAfterServiceTest(..)
  , CleanEmailsOlderThanDays(..)
  , UpdateWithEvent(..)
  , GetUnreadEvents(..)
  , GetServiceTestEvents(..)
  , MarkEventAsRead(..)
  ) where

import Control.Monad.Catch
import Data.Int
import Data.Maybe hiding (fromJust)
import Data.Time

import DB
import KontraPrelude
import MagicHash
import Mails.Data
import MinutesTime

mailerJobNotificationChannel :: Channel
mailerJobNotificationChannel = "mailer_job"

mailerJobSelectors :: [SQL]
mailerJobSelectors = ["id", "attempts"]

mailerJobFetcher :: (JobType, Int32) -> MailerJob
mailerJobFetcher (jtype, attempts) = MailerJob {
  mjType = jtype
, mjAttempts = attempts
}

data GetCurrentSenderType = GetCurrentSenderType
instance (MonadDB m, MonadThrow m) => DBQuery m GetCurrentSenderType SenderType where
  query GetCurrentSenderType = do
    runQuery01_ . sqlSelect "mailer_jobs" $ do
      sqlResult "finished_at IS NOT NULL"
      sqlWhereEq "id" CollectServiceTestResult
    fetchOne runIdentity >>= \case
      True  -> return MasterSender
      False -> return SlaveSender

data SwitchToSlaveSenderImmediately = SwitchToSlaveSenderImmediately
instance (MonadDB m, MonadThrow m) => DBUpdate m SwitchToSlaveSenderImmediately () where
  update SwitchToSlaveSenderImmediately = do
    -- Column 'finished_at' of the job is updated in a separate
    -- transaction after the job was processed and all the emails
    -- were rescheduled, so we need to update it earlier ourselves
    -- for the correct sender to be used for rescheduled emails.
    success <- runQuery01 . sqlUpdate "mailer_jobs" $ do
      sqlSet "finished_at" (Nothing :: Maybe UTCTime)
      sqlWhereEq "id" job
    when (not success) $ do
      $unexpectedErrorM $ show job <+> "doesn't exist"
    where
      job = CollectServiceTestResult

data CollectServiceTestResultIn = CollectServiceTestResultIn Interval
instance (MonadDB m, MonadTime m, MonadThrow m) => DBUpdate m CollectServiceTestResultIn () where
  update (CollectServiceTestResultIn int) = do
    now <- currentTime
    success <- runQuery01 . sqlUpdate "mailer_jobs" $ do
      sqlSetCmd "run_at" $ sqlParam now <+> "+" <?> int
      sqlWhereEq "id" job
    when (not success) $ do
      $unexpectedErrorM $ show job <+> "doesn't exist"
    where
      job = CollectServiceTestResult

data ScheduleServiceTest = ScheduleServiceTest
instance (MonadDB m, MonadThrow m) => DBUpdate m ScheduleServiceTest () where
  update ScheduleServiceTest = do
    success <- runQuery01 . sqlUpdate "mailer_jobs" $ do
      sqlSet "run_at" unixEpoch
      sqlWhereEq "id" job
    when (not success) $ do
      $unexpectedErrorM $ show job <+> "doesn't exist"
    where
      job = PerformServiceTest

----------------------------------------

mailNotificationChannel :: Channel
mailNotificationChannel = "mailer_mail"

mailSelectors :: [SQL]
mailSelectors = [
    "mails.id"
  , "mails.token"
  , "mails.sender"
  , "mails.receivers"
  , "mails.reply_to"
  , "mails.title"
  , "mails.content"
  , "ARRAY(SELECT (name, content, file_id)::mail_attachment FROM mail_attachments a WHERE a.mail_id = mails.id ORDER BY a.id)"
  , "mails.x_smtp_attrs"
  , "mails.service_test"
  , "mails.attempts"
  ]

mailFetcher :: (MailID, MagicHash, Address, [Address], Maybe Address, String, String, CompositeArray1 Attachment, XSMTPAttrs, Bool, Int32) -> Mail
mailFetcher (mid, token, from, to, reply_to, title, content, CompositeArray1 attachments, x_smtp_attrs, service_test, attempts) = Mail {
  mailID = mid
, mailToken = token
, mailFrom = from
, mailTo = to
, mailReplyTo = reply_to
, mailTitle = title
, mailContent = content
, mailAttachments = attachments
, mailXSMTPAttrs = x_smtp_attrs
, mailServiceTest = service_test
, mailAttempts = attempts
}

----------------------------------------

type EmailData = (MagicHash, Address, [Address], Maybe Address, String, String, [Attachment], XSMTPAttrs)

data CreateEmail = CreateEmail EmailData
instance (MonadDB m, MonadThrow m) => DBUpdate m CreateEmail MailID where
  update (CreateEmail mdata) = insertEmail False mdata

data CreateServiceTest = CreateServiceTest EmailData
instance (MonadDB m, MonadThrow m) => DBUpdate m CreateServiceTest MailID where
  update (CreateServiceTest mdata) = do
    runQuery_ . sqlDelete "mails" $ do
      sqlWhereEq "service_test" True
    insertEmail True mdata

data GetEmail = GetEmail MailID MagicHash
instance (MonadDB m, MonadThrow m) => DBQuery m GetEmail (Maybe Mail) where
  query (GetEmail mid token) = do
    runQuery01_ . sqlSelect "mails" $ do
      mapM_ sqlResult mailSelectors
      sqlWhereEq "id" mid
      sqlWhereEq "token" token
    fetchMaybe mailFetcher

data GetEmailForRecipient = GetEmailForRecipient String String UTCTime
instance (MonadDB m, MonadThrow m, MonadTime m) => DBQuery m GetEmailForRecipient (Maybe Mail) where
  query (GetEmailForRecipient recipient title startDate) = do
    runQuery01_ . sqlSelect "mails" $ do
      mapM_ sqlResult mailSelectors
      sqlWhereILike "title" ("%" ++ title ++ "%")
      sqlWhere $ "finished_at >=" <?> startDate
      -- receivers is yet another crappy json field in database
      -- change it into proper SQL column some later time
      sqlWhereILike "receivers" ("%\"" ++ recipient ++ "\"%")
    fetchMaybe mailFetcher

data GetEmailsForTest = GetEmailsForTest
instance MonadDB m => DBQuery m GetEmailsForTest [Mail] where
  query GetEmailsForTest = do
    runQuery_ . sqlSelect "mails" $ do
      mapM_ sqlResult mailSelectors
    fetchMany mailFetcher

data ResendEmailsSentAfterServiceTest = ResendEmailsSentAfterServiceTest
instance MonadDB m => DBUpdate m ResendEmailsSentAfterServiceTest Int where
  update ResendEmailsSentAfterServiceTest = do
    n <- runQuery . sqlUpdate "mails" $ do
      sqlSet "run_at" unixEpoch
      sqlWhereEq "service_test" False
      sqlWhere $ "finished_at >= (SELECT j.finished_at FROM mailer_jobs j WHERE j.id =" <?> PerformServiceTest <> ")"
    when (n > 0) $ do
      notify mailNotificationChannel ""
    return n

data CleanEmailsOlderThanDays = CleanEmailsOlderThanDays Int
instance (MonadDB m, MonadTime m) => DBUpdate m CleanEmailsOlderThanDays Int where
  update (CleanEmailsOlderThanDays days) = do
    past <- (days `daysBefore`) <$> currentTime
    runQuery . sqlDelete "mails" $ do
      sqlWhere $ "finished_at <=" <?> past

----------------------------------------

data UpdateWithEvent = UpdateWithEvent MailID Event
instance (MonadDB m, MonadThrow m) => DBUpdate m UpdateWithEvent Bool where
  update (UpdateWithEvent mid ev) = do
    runQuery01 . sqlInsert "mail_events" $ do
      sqlSet "mail_id" mid
      sqlSet "event" ev

data GetUnreadEvents = GetUnreadEvents
instance MonadDB m => DBQuery m GetUnreadEvents [(EventID, MailID, XSMTPAttrs, Event)] where
  query GetUnreadEvents = getUnreadEvents False

data GetServiceTestEvents = GetServiceTestEvents
instance MonadDB m => DBQuery m GetServiceTestEvents [(EventID, MailID, XSMTPAttrs, Event)] where
  query GetServiceTestEvents = getUnreadEvents True

data MarkEventAsRead = MarkEventAsRead EventID UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m MarkEventAsRead Bool where
  update (MarkEventAsRead eid time) =
    runQuery01 $ sqlUpdate "mail_events" $ do
      sqlSet "event_read" time
      sqlWhereEq "id" eid

----------------------------------------

insertEmail :: (MonadDB m, MonadThrow m) => Bool -> EmailData -> m MailID
insertEmail service_test (token, sender, to, reply_to, title, content, attachments, xsmtpapi) = do
  runQuery_ . sqlInsert "mails" $ do
    sqlSet "token" token
    sqlSet "sender" sender
    sqlSet "receivers" to
    sqlSet "reply_to" reply_to
    sqlSet "title" title
    sqlSet "content" content
    sqlSet "x_smtp_attrs" xsmtpapi
    sqlSet "run_at" unixEpoch
    sqlSet "service_test" service_test
    sqlResult "id"
  mid <- fetchOne runIdentity
  when (not $ null attachments) $ do
    runQuery_ $ sqlInsert "mail_attachments" $ do
      sqlSet "mail_id" mid
      sqlSetList "name" names
      sqlSetList "content" $ either (Just . Binary) (const Nothing) `map` contents
      sqlSetList "file_id" $ either (const Nothing) Just `map` contents
  notify mailNotificationChannel ""
  return mid
  where
    names = map attName attachments
    contents = map attContent attachments

getUnreadEvents :: MonadDB m => Bool -> m [(EventID, MailID, XSMTPAttrs, Event)]
getUnreadEvents service_test = do
  runQuery_ . sqlSelect "mails m" $ do
    sqlResult "e.id"
    sqlResult "e.mail_id"
    sqlResult "m.x_smtp_attrs"
    sqlResult "e.event"
    sqlJoinOn "mail_events e" "m.id = e.mail_id"
    sqlWhereEq "m.service_test" service_test
    sqlWhere "e.event_read IS NULL"
    sqlOrderBy "m.id"
    sqlOrderBy "e.id"
  fetchMany id
