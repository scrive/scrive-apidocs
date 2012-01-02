module Mails.Model (
    module Mails.Public
  , Mail(..)
  , GetIncomingEmails(..)
  , GetEmail(..)
  , MarkEmailAsSent(..)
  , UpdateWithEvent(..)
  ) where

import Database.HDBC

import DB.Classes
import DB.Fetcher2
import DB.Types
import DB.Utils
import Mails.Data
import Mails.Public
import MinutesTime

data Mail = Mail {
    mailID          :: MailID
  , mailToken       :: MagicHash
  , mailFrom        :: Address
  , mailTo          :: [Address]
  , mailTitle       :: String
  , mailContent     :: String
  , mailAttachments :: [Attachment]
  , mailXSMTPAttrs  :: XSMTPAttrs
  } deriving (Eq, Ord, Show)

data GetIncomingEmails = GetIncomingEmails
instance DBQuery GetIncomingEmails [Mail] where
  dbQuery GetIncomingEmails = wrapDB $ \conn -> do
    st <- prepare conn $ selectMailsSQL ++ "WHERE title IS NOT NULL AND content IS NOT NULL AND sent IS NULL ORDER BY id DESC"
    _ <- execute st []
    fetchMails st

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
    r <- run conn "UPDATE mails SET event = ?, event_read = NULL WHERE id = ?"
      [toSql ev, toSql mid]
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
