module Sender (
    Sender(..)
  , createSender
  ) where

import Control.Monad.Base
import Control.Monad.IO.Class
import Data.Char
import Data.Hash.MD5
import Data.List hiding (head)
import System.Exit
import System.Process
import Text.Regex.TDFA
import qualified Codec.Binary.Url as URL
import qualified Codec.Text.IConv as IConv
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.ByteString.Lazy.UTF8 as BSU

import Crypto.RNG (CryptoRNG)
import KontraPrelude
import Log
import MessengerServerConf
import SMS.Data
import Utils.IO
import Utils.Read

data Sender = Sender {
    senderName :: String
  , sendSMS    :: (CryptoRNG m, MonadIO m, MonadBase IO m, MonadLog m)
              => ShortMessage -> m Bool
  }

instance Show Sender where
  show Sender{senderName} = senderName

instance Eq Sender where
  Sender name _ == Sender name' _ = name == name'

createSender :: SenderConfig -> Sender
createSender mc = case mc of
  GlobalMouthSender{..}   -> createGlobalMouthSender gmSenderUser gmSenderPassword gmURL
  LocalSender{}  -> createLocalSender mc

createGlobalMouthSender :: String -> String -> String -> Sender
createGlobalMouthSender user password url = Sender {
  senderName = "GlobalMouth"
, sendSMS = \sms@ShortMessage{..} -> do
  logInfo_ $ show sms
  sendSMSHelper (user, password, url) smOriginator smMSISDN smBody (show smID)
}

sendSMSHelper :: (CryptoRNG m, MonadBase IO m, MonadIO m, MonadLog m) => (String, String, String) -> String -> String -> String -> String -> m Bool
sendSMSHelper (user, password, baseurl) originator msisdn body ref = do
  (code, stdout, stderr) <- readCurl [url] BS.empty
  case (code, maybeRead (takeWhile (not . isSpace) $ BSC.unpack stdout)) of
    (ExitSuccess, Just (httpcode :: Int)) | httpcode >= 200 && httpcode<300 ->
      return True
    _ -> do
      logInfo "sendSMSHelper failed" $ object [
          "code" .= show code
        , "message" .= BSLU.toString stdout
        , "stderr" .= BSLU.toString stderr
        , "number" .= msisdn
        ]
      return False
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
        baseurl
      , "?"
      , "username=", urlEncode latin_user, "&"
      , "body=", urlEncode latin_body, "&"
      , "msisdn=", urlEncode latin_msisdn, "&"
      , "originator=", urlEncode latin_originator, "&"
      , "hash=", hash, "&"
      , "dlr=true", "&"
      , "ref=", ref
      ]
    -- Seems we hit a bug in iconv under Mac. Using translitera mode there
    -- loves to produce empty strings for unknown reason. Using discard mode
    -- removes everything but ANSI, so it is not good either, but good enough
    -- for signing links to get through.
    toLatinTransliterate = BSC.unpack . IConv.convertFuzzy IConv.Transliterate "utf8" "latin1" . BSU.fromString
    toLatinDiscard = BSC.unpack . IConv.convertFuzzy IConv.Discard "utf8" "latin1" . BSU.fromString
    toLatin x = case toLatinTransliterate x of
                  "" -> toLatinDiscard x
                  z -> z
    urlEncode = URL.encode . map (fromIntegral . ord)

createLocalSender :: SenderConfig -> Sender
createLocalSender config = Sender { senderName = "localSender", sendSMS = send }
  where
    send :: (CryptoRNG m, MonadIO m, MonadBase IO m, MonadLog m) => ShortMessage -> m Bool
    send ShortMessage{..} = do
      let matchResult = (match (makeRegex ("https?://[a-zA-Z:0-9.-]+/[a-zA-Z_:/0-9#?-]+" :: String) :: Regex) (smBody :: String) :: MatchResult String)
      let withClickableLinks = mrBefore matchResult ++ "<a href=\"" ++ mrMatch matchResult ++ "\">" ++ mrMatch matchResult ++ "</a>" ++ mrAfter matchResult
      let content = "<html><head><title>SMS - " ++ show smID ++ " to " ++ smMSISDN ++ "</title></head><body>" ++
                    "ID: " ++ show smID ++ "<br>" ++
                    "Data: " ++ show smData ++ "<br>" ++
                    "<br>" ++
                    "Originator: " ++ smOriginator ++ "<br>" ++
                    "MSISDN: " ++ smMSISDN ++ "<br>" ++
                    "<br>" ++
                    withClickableLinks ++
                    "</body></html>"
      let filename = localDirectory config ++ "/SMS-" ++ show smID ++ ".html"
      liftBase $ BSL.writeFile filename (BSLU.fromString content)
      logInfo_ $ "SMS #" ++ show smID ++ " saved to file " ++ filename
      case localOpenCommand config of
        Nothing  -> return ()
        Just cmd -> do
          _ <- liftBase $ createProcess (proc cmd [filename]) {
              std_in  = Inherit
            , std_out = Inherit
            , std_err = Inherit
          }
          return ()
      return True
