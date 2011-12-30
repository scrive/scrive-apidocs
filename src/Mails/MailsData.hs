module Mails.MailsData where

import Data.Typeable
import Happstack.State
import qualified Data.ByteString as BS

import Doc.DocStateData
import API.Service.Model

data SendGridEventType =
      Processed
    | Opened
    | Dropped String              -- ^ drop reason
    | Deferred String Int         -- ^ response, delivery attempt
    | Delivered String            -- ^ response from mta
    | Bounce String String String -- ^ status, reason, type
    | Other String                -- ^ type
      deriving (Eq, Ord, Show, Typeable)

data MailAddress = MailAddress
    { fullname    :: BS.ByteString
    , email       :: BS.ByteString
    } deriving (Eq, Ord, Show, Typeable)

-- | Structure for holding mails. If from is not set mail will be send
-- as SkrivaPa admin (fromMails Config).
data Mail = Mail
    { to          :: [MailAddress]
    , title       :: String
    , content     :: String
    , attachments :: [(String, BS.ByteString)] -- list of attachments (name,content)
    , from        :: Maybe ServiceID
    , mailInfo    :: MailInfo
    } deriving (Eq, Ord, Show, Typeable)

{-
data Mail2 = Mail2
    { to2          :: [MailAddress]
    , title2       :: BS.ByteString
    , content2     :: BS.ByteString
    , attachments2 :: [(BS.ByteString, BS.ByteString)] -- list of attachments (name,content)
    , from2        :: Maybe User
    , mailInfo2    :: MailInfo
    } deriving (Eq, Ord, Show, Typeable)

data Mail1 = Mail1
    { fullname1    :: BS.ByteString
    , email1       :: BS.ByteString
    , title1       :: BS.ByteString
    , content1     :: BS.ByteString
    , attachments1 :: [(BS.ByteString, BS.ByteString)] -- list of attachments (name,content)
    , from1        :: Maybe User
    , mailInfo1    :: MailInfo
    } deriving (Eq, Ord, Show, Typeable)

data Mail0 = Mail0 {
      fullnameemails0 :: [(BS.ByteString, BS.ByteString)] -- Fullname, Email
    , title0          :: BS.ByteString
    , content0        :: BS.ByteString
    , attachments0    :: [(BS.ByteString, BS.ByteString)] -- list of attachments (name,content)
    , from0           :: Maybe User
    , mailInfo0       :: MailInfo
    } deriving Typeable
-}

data MailInfo = Invitation DocumentID SignatoryLinkID
              | None
                deriving (Eq, Ord, Show, Read, Typeable)

instance Version SendGridEventType

{-instance Version Mail0
instance Version Mail1 where
    mode = extension 1 (Proxy :: Proxy Mail0)
instance Version Mail2 where
    mode = extension 2 (Proxy :: Proxy Mail1)
    -}
instance Version Mail where
    mode = extension 3 (Proxy :: Proxy ())

instance Version MailInfo
instance Version MailAddress

$(deriveSerializeFor [''SendGridEventType, ''Mail, {-''Mail2, ''Mail1, ''Mail0,-} ''MailAddress, ''MailInfo])

instance Migrate () Mail where
  migrate () = error "Can't migrate to Mail"

{-
instance Migrate Mail0 Mail1 where
    migrate Mail0 {
          fullnameemails0
        , title0
        , content0
        , attachments0
        , from0
        , mailInfo0
    } = Mail1 {
          fullname1 = case fullnameemails0 of
                          []          -> BS.empty
                          ((name,_):_) -> name
        , email1 = case fullnameemails0 of
                       []            -> BS.empty
                       ((_, mail):_) -> mail
        , title1 = title0
        , content1 = content0
        , attachments1 = attachments0
        , from1 = from0
        , mailInfo1 = mailInfo0
    }

instance Migrate Mail1 Mail2 where
        migrate (Mail1
                 { fullname1
                 , email1
                 , title1
                 , content1
                 , attachments1
                 , from1
                 , mailInfo1
                 }) = Mail2
                    { to2 = [MailAddress fullname1 email1]
                    , title2 = title1
                    , content2 = content1
                    , attachments2 = attachments1
                    , from2 = from1
                    , mailInfo2 = mailInfo1
                    }
instance Migrate Mail2 Mail where
        migrate (Mail2
                 { to2
                 , title2
                 , content2
                 , attachments2
                 , mailInfo2
                 }) = Mail
                    { to = to2
                    , title = title2
                    , content = content2
                    , attachments = attachments2
                    , from = Nothing
                    , mailInfo = mailInfo2
                    }
-}
