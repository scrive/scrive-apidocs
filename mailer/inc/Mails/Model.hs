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
  , GetEmailsBySender(..)
  ) where

import Control.Monad
import Control.Applicative
import Data.Monoid
import Control.Monad.State.Lazy

import DB
import DB.SQL2
import MagicHash
import Mails.Data
import Mails.Tables
import MinutesTime
import OurPrelude
import qualified Data.Map as Map

data CreateEmail = CreateEmail MagicHash Address [Address] MinutesTime
instance MonadDB m => DBUpdate m CreateEmail MailID where
  update (CreateEmail token sender to to_be_sent) =
    $(fromJust) `liftM` insertEmail False token sender to to_be_sent

data AddContentToEmail = AddContentToEmail MailID String String [Attachment] XSMTPAttrs
instance MonadDB m => DBUpdate m AddContentToEmail Bool where
  update (AddContentToEmail mid title content attachments xsmtpapi) = do
    result <- kRun01 $ sqlUpdate "mails" $ do
        sqlSet "title" title
        sqlSet "content" content
        sqlSet "x_smtp_attrs" xsmtpapi
        sqlWhereEq "id" mid
    when result $ do
      kRun_ $ sqlDelete "mail_attachments" $
        sqlWhereEq "mail_id" mid
      when( not (null attachments)) $ do
        kRun_ $ sqlInsert "mail_attachments" $ do
          sqlSet "mail_id" mid
          sqlSetList "name" $ attName <$> attachments
          sqlSetList "content" $ (Binary . attContent) <$> attachments
    return result

data MarkEventAsRead = MarkEventAsRead EventID MinutesTime
instance MonadDB m => DBUpdate m MarkEventAsRead Bool where
  update (MarkEventAsRead eid time) =
    kRun01 $ sqlUpdate "mail_events" $ do
      sqlSet "event_read" time
      sqlWhereEq "id" eid

data DeleteEmail = DeleteEmail MailID
instance MonadDB m => DBUpdate m DeleteEmail Bool where
  update (DeleteEmail mid) = do
    kRun01 $ sqlDelete "mails" $ do
      sqlWhereEq "id" mid

data DeleteMailsOlderThenDays = DeleteMailsOlderThenDays Integer
instance MonadDB m => DBUpdate m DeleteMailsOlderThenDays Integer where
  update (DeleteMailsOlderThenDays days) = do
    kRun $ sqlDelete "mails" $ do
      sqlWhere $ "(now() > to_be_sent + interval" <+> raw (unsafeFromString ("'"++show days++" days'")) <+> ")" -- Sorry but it did not work as param.

data GetUnreadEvents = GetUnreadEvents
instance MonadDB m => DBQuery m GetUnreadEvents [(EventID, MailID, XSMTPAttrs, Event)] where
  query GetUnreadEvents = getUnreadEvents False

selectMails :: MonadDB m => SqlSelect -> DBEnv m [Mail]
selectMails query' = do
  kRun_ $ "CREATE TEMP TABLE mails_tmp AS" <+> toSQLCommand query'
  kRun_ $ ("SELECT * FROM mails_tmp" :: SQL)
  mails <- fetchMails

  kRun_ $ sqlSelect "mail_attachments" $ do
    sqlResult "mail_id"
    sqlResult "name"
    sqlResult "content"
    sqlOrderBy "id DESC"
    sqlWhereExists $ sqlSelect "mails_tmp" $
      sqlWhere "mails_tmp.id = mail_attachments.mail_id"

  attachments <- fetchMailAttachments

  kRun_ $ ("DROP TABLE mails_tmp" :: SQL)

  return (flip map mails $ \mail -> mail { mailAttachments = Map.findWithDefault [] (mailID mail) attachments })


data GetIncomingEmails = GetIncomingEmails
instance MonadDB m => DBQuery m GetIncomingEmails [Mail] where
  query GetIncomingEmails = do
    selectMails $ sqlSelectMails $ do
                              sqlWhere "title IS NOT NULL AND content IS NOT NULL AND to_be_sent <= now() AND sent IS NULL"

data GetEmails = GetEmails
instance MonadDB m => DBQuery m GetEmails [Mail] where
  query GetEmails = do
    selectMails $ sqlSelectMails $ do sqlWhere "title IS NOT NULL AND content IS NOT NULL"

data GetEmailsBySender = GetEmailsBySender String
instance MonadDB m => DBQuery m GetEmailsBySender [Mail] where
  query (GetEmailsBySender sender) = do
    selectMails $ sqlSelectMails $ do
      sqlWhere "title IS NOT NULL"
      sqlWhere "content IS NOT NULL"
      -- sender is yet another crappy json field in database
      -- change it into proper SQL column some later time
      sqlWhereILike "sender" ("%\"" ++ sender ++ "\"%")

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
    mails <- selectMails $ sqlSelectMails $ sqlWhereEq "id" mid >> sqlWhereEq "token" token
    oneObjectReturnedGuard mails

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
                    sqlOrderBy "service_test ASC"
                    sqlOrderBy "id DESC"
                    refine

insertEmail :: MonadDB m => Bool -> MagicHash -> Address -> [Address] -> MinutesTime -> DBEnv m (Maybe MailID)
insertEmail service_test token sender to to_be_sent =
  getOne $ sqlInsert "mails" $ do
      sqlSet "token" token
      sqlSet "sender" sender
      sqlSet "receivers" to
      sqlSet "to_be_sent" to_be_sent
      sqlSet "service_test" service_test
      sqlResult "id"

getUnreadEvents :: MonadDB m => Bool -> DBEnv m [(EventID, MailID, XSMTPAttrs, Event)]
getUnreadEvents service_test = do
  kRun_ $ sqlSelect "mails" $ do
    sqlResult "mail_events.id"
    sqlResult "mail_events.mail_id"
    sqlResult "mails.x_smtp_attrs"
    sqlResult "mail_events.event"
    sqlJoinOn "mail_events" "mails.id = mail_events.mail_id"
    sqlWhereEq "mails.service_test" service_test
    sqlWhere "mail_events.event_read IS NULL"
    sqlOrderBy "mails.id DESC"
    sqlOrderBy "mail_events.id DESC"
  foldDB fetchEvents []
  where
    fetchEvents acc eid mid attrs event = (eid, mid, attrs, event) : acc

fetchMails :: MonadDB m => DBEnv m [Mail]
fetchMails = foldDB decoder []
  where
    -- Note: this function gets mails in reversed order, but all queries
    -- use ORDER BY DESC, so in the end everything is properly ordered.
    decoder acc mid token sender receivers title content
     x_smtp_attrs service_test = Mail {
          mailID = mid
        , mailToken = token
        , mailFrom = sender
        , mailTo = receivers
        , mailTitle = title
        , mailContent = content
        , mailAttachments = []
        , mailXSMTPAttrs = x_smtp_attrs
        , mailServiceTest = service_test
        } : acc

fetchMailAttachments :: MonadDB m => DBEnv m (Map.Map MailID [Attachment])
fetchMailAttachments = foldDB decoder Map.empty
  where
    -- Note: this function gets mails in reversed order, but all queries
    -- use ORDER BY DESC, so in the end everything is properly ordered.
    decoder acc mid name content = Map.insertWith' (++) mid
            [Attachment { attName = name
                        , attContent = unBinary content
                        }] acc
