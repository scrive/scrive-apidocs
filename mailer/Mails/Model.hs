module Mails.Model (
    module Mails.Public
  , Mail(..)
  , GetIncomingEmails(..)
  , MarkEmailAsSent(..)
  ) where

import DB.Classes
import Mails.Public
import Mails.SendGrid ()

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
  dbQuery = undefined

data MarkEmailAsSent = MarkEmailAsSent MailID
instance DBUpdate MarkEmailAsSent Bool where
  dbUpdate = undefined
