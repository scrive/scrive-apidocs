module Mails.MailsData where

import qualified Data.ByteString as BS

import Doc.DocStateData
import Doc.SignatoryLinkID

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
  , attachments :: [(String, BS.ByteString)] -- list of attachments (name,content)
  , mailInfo    :: MailInfo
  } deriving (Eq, Ord, Show)

data MailInfo =
    Invitation DocumentID SignatoryLinkID
  | None
    deriving (Eq, Ord, Show, Read)

emptyMail :: Mail
emptyMail = Mail {
    to             = []
  , title          = []
  , content        = []
  , attachments    = []
  , mailInfo       = None
}
