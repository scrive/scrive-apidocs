module MailingServerConf (
    MailingServerConf(..)
  , SenderConfig(..)
  , SMTPUser(..)
  , SMTPDedicatedUser(..)
  ) where

import Data.Word
import Utils.Default
import Mails.Data
import qualified Data.ByteString as BS
import Data.Unjson
import Control.Applicative
import Data.Maybe


data MailingServerConf = MailingServerConf {
    mscHttpBindAddress :: (Word32, Word16)
  , mscDBConfig        :: BS.ByteString
  , mscMasterSender    :: SenderConfig
  , mscSlaveSender     :: Maybe SenderConfig
  , mscAmazonConfig    :: Maybe (String, String, String)
  , testReceivers      :: [Address]
  } deriving (Read, Show)


unjsonMailingServerConf :: UnjsonDef MailingServerConf
unjsonMailingServerConf = objectOf $ pure MailingServerConf
  <*> (pure (,)
         <*> fieldDefBy "bind_ip" 0
            (fst . mscHttpBindAddress)
            "IP to listen on, defaults to 0.0.0.0"
            unjsonIPv4AsWord32
         <*> field' "bind_port"
            (snd . mscHttpBindAddress)
            "Port to listen on")
  <*> field' "database"
      mscDBConfig
      "Database connection string"
  <*> field "master_sender"
      mscMasterSender
      "Master sender"
  <*> fieldOpt "slave_sender"
      mscSlaveSender
      "Slave sender"
  <*> fieldOptBy "amazon"
      mscAmazonConfig
      "Amazon configuration"
      (objectOf $ pure (,,)
       <*> field' "bucket"
         (\(x,_,_) -> x)
         "In which bucket to store new files"
       <*> field' "access_key"
         (\(_,x,_) -> x)
         "Amazon access key"
       <*> field' "secret_key"
         (\(_,_,x) -> x)
         "Amazon secret key")
  <*> fieldDef "test_receivers" []
      testReceivers
      "Email addresses of people regarded as admins"

instance Unjson MailingServerConf where
  unjsonDef = unjsonMailingServerConf

data SenderConfig = SMTPSender {
    serviceName        :: String
  , smtpAddr           :: String
  , smtpUser           :: SMTPUser
  , smtpDedicatedUsers :: [SMTPDedicatedUser]
  }
  | LocalSender {
    localDirectory     :: FilePath
  , localOpenCommand   :: Maybe String
  }
  | NullSender
  deriving (Read, Show)

data SMTPUser = SMTPUser {
    smtpAccount  :: String
  , smtpPassword :: String
}  deriving (Read, Show)


-- SMTP user that is dedicated only to email where from address matched given address 
data SMTPDedicatedUser = SMTPDedicatedUser {
    smtpFromDedicatedAddress :: String
  , smtpDedicatedUser    :: SMTPUser
} deriving (Read, Show)

mkSenderConfig :: Maybe String
               -> Maybe String
               -> Maybe String
               -> Maybe String
               -> Maybe String
               -> Maybe String
               -> SenderConfig
mkSenderConfig serviceName' (Just smtpAddr') smtpUser' smtpPassword' _localDirectory' _localOpenCommand' =
  SMTPSender { serviceName = fromMaybe "" serviceName',
               smtpAddr = smtpAddr',
               smtpUser = fromMaybe "" smtpUser',
               smtpPassword = fromMaybe "" smtpPassword' }

mkSenderConfig _serviceName' _smtpAddr' _smtpUser' _smtpPassword' (Just localDirectory') localOpenCommand' =
  LocalSender { localDirectory = localDirectory',
                localOpenCommand = localOpenCommand' }
mkSenderConfig _ _ _ _ _ _ = NullSender

unjsonSenderConfig :: UnjsonDef SenderConfig
unjsonSenderConfig = objectOf $ pure mkSenderConfig
  <*> fieldOpt "name"
     (\s -> case s of
         SMTPSender v _ _ _ -> Just v
         _ -> Nothing)
     "Name of this sender service"
  <*> fieldOpt "smtp_addr"
     (\s -> case s of
         SMTPSender _ v _ _ -> Just v
         _ -> Nothing)
     "SMTP address to contact"
  <*> fieldOpt "username"
     (\s -> case s of
         SMTPSender _ _ v _ -> Just v
         _ -> Nothing)
      "Username for SMTP service"
  <*> fieldOpt "password"
     (\s -> case s of
         SMTPSender _ _ _ v -> Just v
         _ -> Nothing)
      "Password for SMTP service"
  <*> fieldOpt "dir"
     (\s -> case s of
         LocalSender v _ -> Just v
         _ -> Nothing)
      "Local directory to save 'eml' files to (ignored if SMTP)"
  <*> fieldOpt "open"
     (\s -> case s of
         LocalSender _ v -> v
         _ -> Nothing)
      "Local open command to open 'eml' files ('/usr/bin/open', 'gnome-open', 'kde-open') (ignored if SMTP)"

instance Unjson SenderConfig where
  unjsonDef = unjsonSenderConfig

-- SMTPSender {
--     serviceName = "SendGrid"
--   , smtpAddr = "smtps://smtp.sendgrid.net"
--   , smtpUser= "duzyrak@gmail.com"
--   , smtpPassword = "zimowisko"
-- }

instance HasDefaultValue MailingServerConf where
  defaultValue = MailingServerConf {
      mscHttpBindAddress = (0x7f000001, 6666)
    , mscDBConfig = "user='kontra' password='kontra' dbname='kontrakcja'"
    , mscMasterSender = LocalSender {
        localDirectory = "/tmp"
      , localOpenCommand = Nothing
    }
    , mscSlaveSender = Nothing
    , mscAmazonConfig = Nothing
    , testReceivers   = [Address { addrName = "test",   addrEmail = "your@email.scrive.com" }]
  }
