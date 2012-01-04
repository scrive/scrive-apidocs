module Mails.MailsData where

import Data.Typeable
import Happstack.State
import qualified Data.ByteString as BS

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

instance Version Mail where
    mode = extension 3 (Proxy :: Proxy ())

instance Version MailInfo
instance Version MailAddress

$(deriveSerializeFor [''Mail, ''MailAddress, ''MailInfo])

instance Migrate () Mail where
  migrate () = error "Can't migrate to Mail"
