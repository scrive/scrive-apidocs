module Sender (
    Sender(..)
  , createSender
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Hash.MD5
import Log
import Network.HTTP.Base (urlEncode)
import System.Exit
import System.Process
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.JSON
import Text.JSON.FromJSValue
import Text.JSON.Gen hiding (object)
import Text.Regex.TDFA
import qualified Codec.Text.IConv as IConv
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.ByteString.Lazy.UTF8 as BSU
import qualified Data.Text as T

import Crypto.RNG (CryptoRNG)
import DB
import KontraPrelude
import Log.Identifier
import Log.Utils
import MessengerServerConf
import SMS.Data
import SMS.Model
import Utils.IO

data Sender = Sender {
  sendSMS    :: forall m. (MonadDB m, MonadThrow m, CryptoRNG m, MonadIO m, MonadBase IO m, MonadLog m) => ShortMessage -> m Bool
}

createSender :: SendersConfig -> Sender
createSender (SendersConfig getConf) = Sender {
  sendSMS = \sms@ShortMessage{smProvider} -> sendSMSHelper (getConf smProvider) sms
}

clearMobileNumber :: String -> String
clearMobileNumber = filter (\c -> not (c `elem` (" -()."::String)))

sendSMSHelper :: (MonadDB m, MonadThrow m, CryptoRNG m, MonadBase IO m, MonadIO m, MonadLog m) => SenderConfig -> ShortMessage -> m Bool
sendSMSHelper GlobalMouthSender{..} sm@ShortMessage{..} = localData [identifier_ smID] $ do
  let clearmsisdn = clearMobileNumber smMSISDN
  logInfoSendSMS "GlobalMouth" sm
  let latin_user = toLatin gmSenderUser
      latin_password = toLatin gmSenderPassword
      latin_originator = toLatin smOriginator
      latin_msisdn = toLatin clearmsisdn
      latin_body = toLatin smBody
      hash = md5s . Str $ concat [
          latin_user
        , latin_body
        , latin_originator
        , latin_msisdn
        , md5s . Str $ latin_user ++ ":" ++ latin_password
        ]
      url = concat [
          gmURL
        , "?"
        , "username=", urlEncode latin_user, "&"
        , "body=", urlEncode latin_body, "&"
        , "msisdn=", urlEncode latin_msisdn, "&"
        , "originator=", urlEncode latin_originator, "&"
        , "hash=", hash, "&"
        , "dlr=true", "&"
        , "ref=", show smID
        ]
  (success, _) <- curlSMSSender [url] clearmsisdn
  return success

sendSMSHelper MbloxSender{..} sm@ShortMessage{..} = localData [identifier_ smID] $ do
  let clearmsisdn = clearMobileNumber smMSISDN
  logInfoSendSMS "Mblox" sm
  let smsDataJSON =  encode $ runJSONGen $ do
        value "from" smOriginator
        value "to" $ [clearmsisdn]
        value "body" smBody
        value "delivery_report" ("per_recipient" :: String)
  (success, resp) <- curlSMSSender [
      "-X" , "POST"
    , "-H" , "Authorization: Bearer " ++ mbToken
    , "-H" , "Content-Type: application/json"
    , "-d" , smsDataJSON
    , mbURL
    ] clearmsisdn
  case (success, decode resp) of
       (True, Ok jresp) -> case (runIdentity $ withJSValue jresp $ fromJSValueField "id") of
         Just mbloxID -> do
           res <- dbUpdate $ UpdateSMSWithMbloxID smID mbloxID
           return res
         Nothing -> do
           logAttention "Sendout with Mblox failed  - no id in response " $ object [
             "resp" .= show resp
             ]
           return False
       (True, Error err) -> do
         logAttention "Sendout with Mblox failed  - response is not a valid json " $ object [
             "resp" .= show resp
           , "err" .= err
           ]
         return False
       _ -> return False

sendSMSHelper TeliaCallGuideSender{..} sm@ShortMessage{..} = localData [identifier_ smID] $ do
  -- Telia CallGuide doesn't want leading +
  let msisdn = filter (/='+') smMSISDN
  let clearmsisdn = clearMobileNumber msisdn
  logInfoSendSMS "TeliaCallGuide" sm
  let userpass = tcgSenderUser ++ ":" ++ tcgSenderPassword
      url = concat [
          tcgSenderUrl
        , "?", "correlationId=", take 100 . show $ smID -- Telia Callguide has a 100 char limit on this
        , "&", "originatingAddress=", take 11 . urlEncode . toLatin $ smOriginator -- Telia Callguide has 11 alphanum char limit on this
        , "&", "destinationAddress=", urlEncode . toLatin $ clearmsisdn
        , "&", "userData=", urlEncode . toLatin $ smBody
        , "&", "statusReportFlags=1"
        ]
  (success, resp) <- curlSMSSender ["--user", userpass, url] clearmsisdn
  -- Example response from Telia CallGuide:
  -- (see Telia CallGuide SMS Interface Extended Interface Specification)
  -- (should be here https://drive.google.com/drive/folders/0B8akyOlg6VShRnJOYVI1N2FaNU0)
  --
  -- >   <SendResponse>
  -- >     <correlationId>987235723</correlationId>
  -- >     <messageId>172926119</messageId>
  -- >     <responseCode>0</responseCode>
  -- >     <responseMessage>Success</responseMessage>
  -- >     <temporaryError>false</temporaryError>
  -- >   </SendResponse>
  --
  -- Telia guarentees to return all these values, except for correlationId as it is only there if we supply one.
  -- We want to store "messageId" as that is all we get for delivery reports:
  let teliaid = case take 1 . drop 1 . dropWhile (not . tagOpenNameLit "messageId") . parseTags $ resp of
                     (TagText tid):_ -> tid
                     _ -> $unexpectedError "Could not parse Telia CallGuide response"
  res <- dbUpdate $ UpdateSMSWithTeliaID smID teliaid
  return (success && res)

sendSMSHelper LocalSender{..} ShortMessage{..} = localData [identifier_ smID] $ do
  let clearmsisdn = clearMobileNumber smMSISDN
  let matchResult = (match (makeRegex ("https?://[a-zA-Z:0-9.-]+/[a-zA-Z_:/0-9#?-]+" :: String) :: Regex) (smBody :: String) :: MatchResult String)
  let withClickableLinks = mrBefore matchResult ++ "<a href=\"" ++ mrMatch matchResult ++ "\">" ++ mrMatch matchResult ++ "</a>" ++ mrAfter matchResult
  let content = "<html><head><title>SMS - " ++ show smID ++ " to " ++ clearmsisdn ++ "</title></head><body>" ++
                "ID: " ++ show smID ++ "<br>" ++
                "Data: " ++ show smData ++ "<br>" ++
                "Provider: " ++ show smProvider ++ "<br>" ++
                "<br>" ++
                "Originator: " ++ smOriginator ++ "<br>" ++
                "MSISDN: " ++ clearmsisdn ++ "<br>" ++
                "<br>" ++
                withClickableLinks ++
                "</body></html>"
  let filename = localDirectory ++ "/SMS-" ++ show smID ++ ".html"
  liftBase $ BSL.writeFile filename (BSLU.fromString content)
  logInfo "SMS saved to file" $ object [
      "path" .= filename
    ]
  case localOpenCommand of
    Nothing  -> return ()
    Just cmd -> do
      _ <- liftBase $ createProcess (proc cmd [filename]) {
          std_in  = Inherit
        , std_out = Inherit
        , std_err = Inherit
      }
      return ()
  return True

curlSMSSender :: (CryptoRNG m, MonadBase IO m, MonadIO m, MonadLog m) => [String] -> String -> m (Bool, String)
curlSMSSender params msisdn = do
  logInfo_ $ T.pack $ show params
  (code, stdout, stderr) <- readCurl (params ++ ["--write-out","\n%{http_code}"]) BS.empty
  let (stdout_without_code,http_code) = case reverse . lines . BSC.unpack $ stdout of
        [] -> ("", Nothing)
        (lastline:otherlinesreversed) -> (unlines $ reverse $ otherlinesreversed, maybeRead lastline)
  case (code, http_code) of
    (ExitSuccess, Just (httpcode :: Int)) | httpcode >= 200 && httpcode<300 -> do
      logInfo "curlSMSSender success" $ object [
          "code" .= show code
        , "http_code" .= httpcode
        , "stdout" `equalsExternalBSL` stdout
        , "stderr" `equalsExternalBSL` stderr
        , "number" .= msisdn
        ]
      return (True, stdout_without_code)
    _ -> do
      logInfo "curlSMSSender failed" $ object [
          "code" .= show code
        , "http_code" .= http_code
        , "stdout" `equalsExternalBSL` stdout
        , "stderr" `equalsExternalBSL` stderr
        , "number" .= msisdn
        ]
      return (False, stdout_without_code)

toLatin :: String -> String
toLatin x = case toLatinTransliterate x of
              "" -> toLatinDiscard x
              z -> z
  where
    -- Seems we hit a bug in iconv under Mac. Using translitera mode there
    -- loves to produce empty strings for unknown reason. Using discard mode
    -- removes everything but ANSI, so it is not good either, but good enough
    -- for signing links to get through.
    toLatinTransliterate :: String -> String
    toLatinTransliterate = BSC.unpack . IConv.convertFuzzy IConv.Transliterate "utf8" "latin1" . BSU.fromString
    toLatinDiscard :: String -> String
    toLatinDiscard = BSC.unpack . IConv.convertFuzzy IConv.Discard "utf8" "latin1" . BSU.fromString

logInfoSendSMS :: MonadLog m => String -> ShortMessage -> m ()
logInfoSendSMS sender ShortMessage{..} = logInfo "Sending SMS" $ object [
      "sender"     .= sender
    , "provider"   .= show smProvider
    , "originator" .= smOriginator
    , "msisdn"     .= smMSISDN -- original/non-clean format
    , "body"       .= smBody
    , "data"       .= smData
    , "attempts"   .= smAttempts
    ]
