{-# LANGUAGE RecordWildCards, NoImplicitPrelude, TemplateHaskell #-}
module Sender (
    Sender(..)
  , createSender
  ) where

import Control.Monad.IO.Class
import Data.List hiding (head)
import System.Process
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU

import Crypto.RNG (CryptoRNG)
import MessengerServerConf
--import SMS.Model
import SMS.Data
import OurPrelude
import qualified Log (messengerServer)
import Data.Char
import Data.Hash.MD5
import qualified Codec.Text.IConv as IConv
import qualified Codec.Binary.Url as URL
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy.UTF8 as BSU
import Utils.IO

data Sender = Sender {
    senderName :: String
  , sendSMS   :: CryptoRNG m => ShortMessage -> m Bool
  }

instance Show Sender where
  show Sender{senderName} = senderName

instance Eq Sender where
  Sender name _ == Sender name' _ = name == name'

createSender :: SenderConfig -> Sender
createSender mc = case mc of
  SMSSender{..}   -> createExternalSender "GlobalMouth" smsSenderUser smsSenderPassword
  LocalSender{}  -> createLocalSender mc

createExternalSender :: String -> String -> String -> Sender
createExternalSender name user password = Sender { senderName = name, sendSMS = send }
  where
    send :: CryptoRNG m => ShortMessage -> m Bool
    send _sms@ShortMessage{..} = do
      liftIO $ do
        sendSMS2 (user,password) smOriginator smMSISDN smBody
        return True

sendSMS2 :: (String, String) -> String -> String -> String -> IO ()
sendSMS2 (user, password) originator msisdn body = do
  (code, stdout, stderr) <- readProcessWithExitCode' "curl" [url] BS.empty
  putStrLn $ show code
  putStr "STDOUT: "
  BSC.putStrLn stdout
  putStr "STDERR: "
  BSC.putStrLn stderr
  where
    latin_user = toLatin user
    latin_password = toLatin password
    latin_originator = toLatin originator
    latin_msisdn = toLatin msisdn
    latin_body = toLatin body
    hash = md5s . Str $ concat [
        latin_user
      , latin_body
      , latin_originator
      , latin_msisdn
      , md5s . Str $ latin_user ++ ":" ++ latin_password
      ]
    url = concat [
        "http://mcm.globalmouth.com:8080/api/mcm?"
      , "username=", urlEncode latin_user, "&"
      , "body=", urlEncode latin_body, "&"
      , "msisdn=", urlEncode latin_msisdn, "&"
      , "originator=", urlEncode latin_originator, "&"
      , "hash=", hash
      ]

    toLatin = BSC.unpack . IConv.convert "utf8" "latin1" . BSU.fromString
    urlEncode = URL.encode . map (fromIntegral . ord)

{-
createExternalSender :: String -> String -> (Mail -> [String]) -> Sender
createExternalSender name program createargs = Sender { senderName = name, sendMail = send }
  where
    send :: CryptoRNG m => Mail -> m Bool
    send mail@Mail{..} = do
      content <- assembleContent mail
      liftIO $ do
        (code, _, bsstderr) <- readProcessWithExitCode' program (createargs mail) content
        let receivers = intercalate ", " (map addrEmail mailTo)
        case code of
          ExitFailure retcode -> do
            Log.mailingServer $ "Error while sending email #" ++ show mailID ++ ", cannot execute " ++ program ++ " to send email (code " ++ show retcode ++ ") stderr: \n" ++ BSLU.toString bsstderr
            return False
          ExitSuccess -> do
            let subject = filter (not . (`elem` "\r\n")) mailTitle
            Log.mailingServer $ "Email #" ++ show mailID ++ " with subject '" ++ subject ++ "' sent correctly to: " ++ receivers
            Log.mailContent $ unlines [
                "Subject: " ++ subject
              , "To: " ++ intercalate ", " (map addrEmail mailTo)
              , "Attachments: " ++ show (length mailAttachments)
              , htmlToTxt mailContent
              ]
            return True
-}

{-
createSMTPSender :: SenderConfig -> Sender
createSMTPSender config = createExternalSender (serviceName config) "curl" createargs
  where
    mailRcpt addr = [
        "--mail-rcpt"
      , "<" ++ addrEmail addr ++ ">"
      ]
    createargs Mail{mailFrom, mailTo} =
      [ "-s", "-S"                   -- show no progress information but show error messages
      , "-k", "--ssl"                -- use SSL but do not fret over self-signed or outdated certifcate
      , "--user"
      , smtpUser config ++ ":" ++ smtpPassword config
      , smtpAddr config
      , "--mail-from", "<" ++ addrEmail mailFrom ++ ">"
      ] ++ concatMap mailRcpt mailTo
-}

createLocalSender :: SenderConfig -> Sender
createLocalSender config = Sender { senderName = "localSender", sendSMS = send }
  where
    send :: CryptoRNG m => ShortMessage -> m Bool
    send sms@ShortMessage{..} = do
      content <- return (show sms)
      let filename = localDirectory config ++ "/SMS-" ++ show smID ++ ".html"
      liftIO $ do
        BSL.writeFile filename (BSLU.fromString content)
        Log.messengerServer $ "SMS #" ++ show smID ++ " saved to file " ++ filename
        case localOpenCommand config of
          Nothing  -> return ()
          Just cmd -> do
            _ <- createProcess (proc cmd [filename]) {
                std_in  = Inherit
              , std_out = Inherit
              , std_err = Inherit
            }
            return ()
        return True

{-
    toLatin = BSC.unpack
            . IConv.convertFuzzy IConv.Transliterate "utf8" "latin1"
            . BSU.fromString
-}
