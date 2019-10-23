module Mails.MailsData (
    MailAddress(..)
  , Mail(..)
  , emptyMail
  , KontraInfoForMail(..)
  , AddKontraInfoForMail(..)
) where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.Monoid as Monoid

import File.FileID
import Log.Identifier
import Mails.KontraInfoForMail

data MailAddress = MailAddress {
    fullname    :: Text
  , email       :: Text
  } deriving (Eq, Ord, Show)

instance ToJSON MailAddress where
  toJSON MailAddress {..} = object ["name" .= fullname, "email" .= email]
  toEncoding MailAddress {..} =
    pairs $ Monoid.mconcat ["name" .= fullname, "email" .= email]

-- | Structure for holding mails. If from is not set mail will be send
-- as Scrive admin (fromMails Config).
data Mail = Mail {
    to                :: [MailAddress]
  , originator        :: Text
    -- ^ Name of service sending email. Default is Scrive.
  , originatorEmail   :: Text
    -- ^ Adress of no reply email
  , replyTo           :: Maybe MailAddress
  , title             :: Text
  , content           :: Text
  , attachments       :: [( Text
                          , Either BS.ByteString FileID )]
    -- ^ List of attachments (name,content).
  , kontraInfoForMail :: Maybe KontraInfoForMail
    -- ^ Connection between this message and some entity in kontrakcja.
  } deriving (Eq, Ord, Show)


attachmentToJson :: (Text, Either BS.ByteString FileID) -> Value
attachmentToJson (name, acontent) = object
  [ "name" .= name
  , "storage_type" .= case acontent of
    Left  _   -> "direct"
    Right fid -> object [identifier fid]
  ]

instance Loggable Mail where
  logValue Mail {..} =
    object
      $  [ "attachment_count" .= length attachments
         , "attachments" .= map attachmentToJson attachments
         , "content" .= content
         , "from" .= originatorEmail
         , "originator" .= originator
         , "reply_to" .= fromMaybe Null (toJSON <$> replyTo)
         , "subject" .= title
         , "to" .= to
         ]
      ++ maybeToList (logPair_ <$> kontraInfoForMail)

  logDefaultLabel _ = "mail"

emptyMail :: Mail
emptyMail = Mail { to                = []
                 , originator        = "Scrive"
                 , originatorEmail   = "noreply@scrive.com"
                 , replyTo           = Nothing
                 , title             = ""
                 , content           = ""
                 , attachments       = []
                 , kontraInfoForMail = Nothing
                 }
