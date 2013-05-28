module Mails.MailsData where

import qualified Data.ByteString as BS
import File.FileID
import MessageData

data MailAddress = MailAddress {
    fullname    :: String
  , email       :: String
  } deriving (Eq, Ord, Show)

-- | Structure for holding mails. If from is not set mail will be send
-- as SkrivaPa admin (fromMails Config).
data Mail = Mail {
    to          :: [MailAddress]
  , title       :: String
  , content     :: String
  , attachments :: [(String, Either BS.ByteString FileID)] -- list of attachments (name,content)
  , mailInfo    :: MessageData
  } deriving (Eq, Ord, Show)

emptyMail :: Mail
emptyMail = Mail {
    to             = []
  , title          = []
  , content        = []
  , attachments    = []
  , mailInfo       = None
}
