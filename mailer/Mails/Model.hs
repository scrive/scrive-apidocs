module Mails.Model (
    module Mails.Public
  , Mail(..)
  , GetIncomingEmails(..)
  , MarkEmailAsSent(..)
  , UpdateWithEvent(..)
  ) where

import Database.HDBC

import DB.Classes
import DB.Fetcher2
import DB.Utils
import Mails.Data
import Mails.Public
import MinutesTime

data Mail = Mail {
    mailID          :: MailID
  , mailFrom        :: Address
  , mailTo          :: [Address]
  , mailTitle       :: String
  , mailContent     :: String
  , mailAttachments :: [Attachment]
  , mailXSMTPAPI    :: String
  } deriving (Eq, Ord, Show)

data GetIncomingEmails = GetIncomingEmails
instance DBQuery GetIncomingEmails [Mail] where
  dbQuery GetIncomingEmails = wrapDB $ \conn -> do
    st <- prepare conn $ "SELECT "
      ++ "  id"
      ++ ", sender"
      ++ ", receivers"
      ++ ", title"
      ++ ", content"
      ++ ", attachments"
      ++ ", x_smtpapi"
      ++ " FROM mails WHERE"
      ++ "  title IS NOT NULL AND content IS NOT NULL AND sent IS NULL"
      ++ " ORDER BY id DESC"
    _ <- execute st []
    foldDB st fetchMails []
    where
      fetchMails acc id' sender receivers title content attachments x_smtpapi = Mail {
          mailID = id'
        , mailFrom = sender
        , mailTo = receivers
        , mailTitle = title
        , mailContent = content
        , mailAttachments = attachments
        , mailXSMTPAPI = x_smtpapi
        } : acc

data MarkEmailAsSent = MarkEmailAsSent MailID
instance DBUpdate MarkEmailAsSent Bool where
  dbUpdate (MarkEmailAsSent mid) = wrapDB $ \conn -> do
    t <- getMinutesTime
    r <- run conn "UPDATE mails SET sent = ? WHERE id = ?" [toSql t, toSql mid]
    oneRowAffectedGuard r

data UpdateWithEvent = UpdateWithEvent MailID Event
instance DBUpdate UpdateWithEvent Bool where
  dbUpdate (UpdateWithEvent _mid _ev) = undefined
