module Mails.Model (
    module Mails.Public
  , Mail(..)
  , GetIncomingEmails(..)
  , MarkEmailAsSent(..)
  ) where

import Database.HDBC

import DB.Classes
import DB.Utils
import Mails.Public
import Mails.SendGrid ()
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
    _ <- execute st []
    fetchMails st []
    where
      fetchMails st acc = fetchRow st >>= maybe (return acc) f
        where
          f [id', sender, receivers, title, content, attachments, x_smtpapi] =
            fetchMails st $ Mail {
                mailID = fromSql id'
              , mailFrom = fromSql sender
              , mailTo = fromSql receivers
              , mailTitle = fromSql title
              , mailContent = fromSql content
              , mailAttachments = fromSql attachments
              , mailXSMTPAPI = fromSql x_smtpapi
              } : acc
          f r = error $ "fetchMails: unexpected row: " ++ show r

data MarkEmailAsSent = MarkEmailAsSent MailID
instance DBUpdate MarkEmailAsSent Bool where
  dbUpdate (MarkEmailAsSent mid) = wrapDB $ \conn -> do
    t <- getMinutesTime
    r <- run conn "UPDATE mails SET sent = ? WHERE id = ?" [toSql t, toSql mid]
    oneRowAffectedGuard r
