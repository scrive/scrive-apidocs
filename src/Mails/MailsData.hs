{-# OPTIONS_GHC -Wall -Werror  #-}
module Mails.MailsData where

import Data.Typeable
import Happstack.State
import qualified Data.ByteString as BS

import User.UserState
import Doc.DocStateData

data SendGridEventType =
      Processed
    | Opened
    | Dropped String              -- ^ drop reason
    | Deferred String Int         -- ^ response, delivery attempt
    | Delivered String            -- ^ response from mta
    | Bounce String String String -- ^ status, reason, type
    | Other String                -- ^ type
      deriving (Eq, Ord, Show, Typeable)

-- | Structure for holding mails. If from is not set mail will be send
-- as SkrivaPa admin (fromMails Config).
data Mail = Mail {
      fullname    :: BS.ByteString
    , email       :: BS.ByteString
    , title       :: BS.ByteString
    , content     :: BS.ByteString
    , attachments :: [(BS.ByteString, BS.ByteString)] -- list of attachments (name,content)
    , from        :: Maybe User
    , mailInfo    :: MailInfo 
    } deriving (Eq, Ord, Show, Typeable)

data Mail0 = Mail0 {
      fullnameemails0 :: [(BS.ByteString, BS.ByteString)] -- Fullname, Email
    , title0          :: BS.ByteString
    , content0        :: BS.ByteString
    , attachments0    :: [(BS.ByteString, BS.ByteString)] -- list of attachments (name,content)
    , from0           :: Maybe User
    , mailInfo0       :: MailInfo 
    } deriving Typeable

data MailInfo = Invitation DocumentID SignatoryLinkID
              | None
                deriving (Eq, Ord, Show, Read, Typeable)

instance Version SendGridEventType

instance Version Mail0
instance Version Mail where
    mode = extension 1 (Proxy :: Proxy Mail0)

instance Version MailInfo

$(deriveSerializeFor [''SendGridEventType, ''Mail, ''Mail0, ''MailInfo])

instance Migrate Mail0 Mail where
    migrate Mail0 {
          fullnameemails0
        , title0
        , content0
        , attachments0
        , from0
        , mailInfo0
    } = Mail {
          fullname = case fullnameemails0 of
                          []          -> BS.empty
                          ((name,_):_) -> name
        , email = case fullnameemails0 of
                       []            -> BS.empty
                       ((_, mail):_) -> mail
        , title = title0
        , content = content0
        , attachments = attachments0
        , from = from0
        , mailInfo = mailInfo0
    }
