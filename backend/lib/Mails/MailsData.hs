module Mails.MailsData (
    MailAddress(..)
  , Mail(..)
  , emptyMail
) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.Text as T

import File.FileID
import KontraPrelude
import Log.Identifier
import MessageData

data MailAddress = MailAddress {
    fullname    :: String
  , email       :: String
  } deriving (Eq, Ord, Show)

instance ToJSON MailAddress where
  toJSON MailAddress{..} = object [
      "name" .= fullname
    , "email" .= email
    ]

-- | Structure for holding mails. If from is not set mail will be send
-- as SkrivaPa admin (fromMails Config).
data Mail = Mail {
    to              :: [MailAddress]
  , originator      :: String -- Name of service sending email. Default is Scrive
  , originatorEmail :: String -- Adress of no reply email
  , replyTo     :: Maybe MailAddress
  , title       :: String
  , content     :: String
  , attachments :: [(String, Either BS.ByteString FileID)] -- list of attachments (name,content)
  , mailInfo    :: MessageData
  } deriving (Eq, Ord, Show)

instance ToJSON Mail where
  toJSON Mail{..} = object $ [
      "to" .= to
    , "originator" .= originator
    , "originator_email" .= originatorEmail
    , "reply_to" .= replyTo
    , "title" .= title
    , "content" .= content
    , "attachments" .= map attachmentToJson attachments
    ] ++ jsonizeMailInfo mailInfo
    where

jsonizeMailInfo :: MessageData -> [Pair]
jsonizeMailInfo (Invitation did slid) = [
    "type" .= ("invitation"::T.Text)
  , identifier_ did
  , identifier_ slid
  ]
jsonizeMailInfo (DocumentRelatedMail did) = [
    "type" .= ("document_related_mail"::T.Text)
  , identifier_ did
  ]
jsonizeMailInfo (SMSPinSendout slid) = [
    "type" .= ("sms_pin_sendout"::T.Text)
  , identifier_ slid
  ]
jsonizeMailInfo None = []

attachmentToJson :: (String, Either BS.ByteString FileID) -> Value
attachmentToJson (name, acontent) = object [
    "name" .= name
  , "storage_type" .= case acontent of
    Left _ -> "direct"
    Right fid -> object [identifier_ fid]
  ]

instance LogObject Mail where
  logObject Mail{..} = object $ [
      "to" .= to
    , "originator" .= originator
    , "originator_email" .= originatorEmail
    , "reply_to" .= replyTo
    , "subject" .= title
    , "content" .= content
    , "attachment_count" .= length attachments
    , "attachments" .= map attachmentToJson attachments
    ] ++ jsonizeMailInfo mailInfo

instance LogDefaultLabel Mail where
  logDefaultLabel _ = "mail"

emptyMail :: Mail
emptyMail = Mail {
    to              = []
  , originator      = "Scrive"
  , originatorEmail = "noreply@scrive.com"
  , replyTo         = Nothing
  , title           = []
  , content         = []
  , attachments     = []
  , mailInfo        = None
}
