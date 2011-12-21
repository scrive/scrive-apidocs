module Mails.Model where

import Data.Int
import qualified Data.ByteString.Lazy.Char8 as BSL

import DB.Classes
import DB.Derive
import Mails.Tables ()
import Mails.SendGrid ()

newtype MailID = MailID Int64
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''MailID)

data Mail = Mail {
    mailID      :: MailID
  , mailFrom    :: String
  , mailTo      :: [String]
  , mailContent :: BSL.ByteString
  } deriving (Eq, Ord, Show)

data GetIncomingEmails = GetIncomingEmails
instance DBQuery GetIncomingEmails [Mail] where
  dbQuery = undefined

data MarkEmailAsSent = MarkEmailAsSent MailID
instance DBUpdate MarkEmailAsSent Bool where
  dbUpdate = undefined
