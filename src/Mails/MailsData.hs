module Mails.MailsData where

import Data.Typeable
import Happstack.State
import qualified Data.ByteString.UTF8 as BS

import Doc.DocStateData
import API.Service.Model

data MailAddress = MailAddress {
    fullname    :: BS.ByteString
  , email       :: BS.ByteString
  } deriving (Eq, Ord, Show, Typeable)

-- | Structure for holding mails. If from is not set mail will be send
-- as SkrivaPa admin (fromMails Config).
data Mail = Mail {
    to          :: [MailAddress]
  , title       :: String
  , content     :: String
  , attachments :: [(String, BS.ByteString)] -- list of attachments (name,content)
  , from        :: Maybe ServiceID
  , mailInfo    :: MailInfo
  } deriving (Eq, Ord, Show, Typeable)

data Mail3 = Mail3 {
    to3          :: [MailAddress]
  , title3       :: BS.ByteString
  , content3     :: BS.ByteString
  , attachments3 :: [(BS.ByteString, BS.ByteString)] -- list of attachments (name,content)
  , from3        :: Maybe ServiceID
  , mailInfo3    :: MailInfo
  } deriving (Eq, Ord, Show, Typeable)

data MailInfo =
    Invitation DocumentID SignatoryLinkID
  | None
    deriving (Eq, Ord, Show, Read, Typeable)

emptyMail :: Mail
emptyMail = Mail {
    to             = []
  , title          = []
  , content        = []
  , attachments    = []
  , from           = Nothing
  , mailInfo       = None
}

-- old stuff, not needed anymore except for compatibility reasons
-- (will be dumped once we move ActionScheduler to PostgreSQL)

instance Version Mail3 where
    mode = extension 3 (Proxy :: Proxy ())

instance Version Mail where
    mode = extension 4 (Proxy :: Proxy Mail3)

instance Version MailInfo
instance Version MailAddress

instance Migrate () Mail3 where
  migrate () = error "Can't migrate to Mail"

instance Migrate Mail3 Mail where
  migrate (Mail3 
           { to3
           , title3
           , content3
           , attachments3
           , from3
           , mailInfo3
           }) = Mail
           { to = to3
           , title = BS.toString title3
           , content = BS.toString content3
           , attachments = map (\(a,b) -> (BS.toString a,b)) attachments3
           , from = from3
           , mailInfo = mailInfo3
           }


$(deriveSerializeFor [''Mail, ''Mail3, ''MailAddress, ''MailInfo])

