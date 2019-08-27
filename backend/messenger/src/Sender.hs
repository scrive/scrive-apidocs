module Sender (
    Sender(..)
  , createSender
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Crypto.RNG (CryptoRNG)
import Log
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
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import DB
import Log.Identifier
import Log.Utils
import MessengerServerConf
import SMS.Model
import SMS.Types
import Utils.IO

data Sender = Sender {
  sendSMS
    :: forall m. ( MonadDB m, MonadThrow m, CryptoRNG m, MonadIO m
                 , MonadBase IO m, MonadLog m )
    => ShortMessage -> m Bool
}

createSender :: SendersConfig -> Sender
createSender (SendersConfig getConf) = Sender {
  sendSMS =
      \sms@ShortMessage{smProvider} -> sendSMSHelper (getConf smProvider) sms
}

clearMobileNumber :: Text -> Text
clearMobileNumber = T.filter (\c -> not (c `elem` (" -()."::String)))

sendSMSHelper
  :: ( MonadDB m, MonadThrow m, CryptoRNG m
     , MonadBase IO m, MonadIO m, MonadLog m )
  => SenderConfig -> ShortMessage -> m Bool
sendSMSHelper MbloxSender{..} sm@ShortMessage{..} =
  localData [identifier smID] $ do
  let clearmsisdn = clearMobileNumber smMSISDN
      northAmericanNumber = "+1" `T.isPrefixOf` clearmsisdn
      -- Sending SMS to the USA cannot be done with AlphaNumeric sender
      -- instead you need to use a registered short-code.
      -- This is the short-code assigned to us via Mblox for the USA
      -- "+1" is actually for the whole of North America, but we've
      -- duct-taped it for now...
      -- See https://scrive.fogbugz.com/f/cases/2488/ for more information!
      originator = if northAmericanNumber then "18556644314" else smOriginator
      sm' = sm{smOriginator = originator}
  logInfoSendSMS "Mblox" sm'
  let smsDataJSON = encode $ runJSONGen $ do
        value "from" originator
        value "to" $ [clearmsisdn]
        value "body" smBody
        value "delivery_report" ("per_recipient" :: String)
  (success, resp) <- curlSMSSender [
      "-X" , "POST"
    , "-H" , "Authorization: Bearer " <> (T.pack mbToken)
    , "-H" , "Content-Type: application/json"
    , "-d" , T.pack smsDataJSON
    , T.pack mbURL
    ] clearmsisdn
  case (success, decode $ T.unpack resp) of
       (True, Ok jresp) -> case (runIdentity $
                                 withJSValue jresp $
                                 fromJSValueField "id") of
         Just mbloxID -> do
           logInfo "SMS sent through MBlox" $ object [ logPair_ sm'
                                                     , "mbloxid" .= show mbloxID
                                                     ]
           res <- dbUpdate $ UpdateSMSWithMbloxID smID mbloxID
           return res
         Nothing -> do
           logAttention "Sendout with Mblox failed  - \
                        \no id in response " $ object [
             "resp" .= show resp
             ]
           return False
       (True, Error err) -> do
         logAttention "Sendout with Mblox failed  - \
                      \response is not a valid json " $ object [
             "resp" .= show resp
           , "err" .= err
           ]
         return False
       _ -> return False

sendSMSHelper TeliaCallGuideSender{..} sm@ShortMessage{..} =
  localData [identifier smID] $ do
  -- Telia CallGuide doesn't want leading +
  let msisdn      = T.filter (/='+') smMSISDN
      clearmsisdn = clearMobileNumber msisdn
  logInfoSendSMS "TeliaCallGuide" sm
  let userpass          = T.pack $ tcgSenderUser <> ":" <> tcgSenderPassword
      url               = tcgSenderUrl
      urlEnc param      = ["--data-urlencode", param]
      userData          = urlEnc $ "userData=" <> toLatin smBody
      dstAddr           = urlEnc $ "destinationAddress=" <> toLatin clearmsisdn
      originator        = urlEnc $ "originatingAddress="
                          -- Telia Callguide has 11 alphanum char limit on this
                          <> (T.unpack $ T.take 11 $ toLatin smOriginator)
      correlationId     = urlEnc $ "correlationId="
                          -- Telia Callguide has a 100 char limit on this
                          <> (take 100 $ show smID)
      statusReportFlags = urlEnc "statusReportFlags=1"
      args :: [Text]  = concat [ ["--user", userpass]
                                 , [T.pack url]
                                 , userData
                                 , dstAddr
                                 , T.pack <$> originator
                                 , T.pack <$> correlationId
                                 , statusReportFlags
                                 ]
  (success, resp) <- curlSMSSender args clearmsisdn

  -- Example response from Telia CallGuide:
  -- (see Telia CallGuide SMS Interface Extended Interface Specification)
  -- (should be here:
  -- <https://drive.google.com/drive/folders/0B8akyOlg6VShRnJOYVI1N2FaNU0>)
  --
  -- >   <SendResponse>
  -- >     <correlationId>987235723</correlationId>
  -- >     <messageId>172926119</messageId>
  -- >     <responseCode>0</responseCode>
  -- >     <responseMessage>Success</responseMessage>
  -- >     <temporaryError>false</temporaryError>
  -- >   </SendResponse>
  --
  -- Telia guarantees to return all these values, except for
  -- correlationId as it is only there if we supply one.
  -- We want to store "messageId" as that is all we get for delivery reports:
  let teliaid =
        case take 1 . drop 1 .
             dropWhile (not . tagOpenNameLit "messageId") . parseTags $ resp
        of
          (TagText tid):_ -> tid
          _               -> unexpectedError
                             "Could not parse Telia CallGuide response"
  res <- dbUpdate $ UpdateSMSWithTeliaID smID teliaid
  return (success && res)

sendSMSHelper LocalSender{..} ShortMessage{..} = localData [identifier smID] $ do
  let clearmsisdn :: Text = clearMobileNumber smMSISDN

      regex :: String       = "https?://[a-zA-Z:0-9.-]+/[a-zA-Z_:/0-9#?-]+"
      matchResult :: MatchResult String =
        match (makeRegex regex :: Regex) (T.unpack smBody)

      withClickableLinks :: Text = T.pack $
        mrBefore matchResult <>
        "<a href=\"" <> mrMatch matchResult <> "\">" <>
        mrMatch matchResult <> "</a>" <>
        mrAfter matchResult

      content :: Text =
        "<html><head><title>SMS - " <> (showt smID) <>
        " to " <> clearmsisdn <> "</title></head><body>" <>
        "ID: " <> (showt smID) <> "<br>" <>
        "Provider: " <> (showt smProvider) <> "<br>" <>
        "<br>" <>
        "Originator: " <> smOriginator <> "<br>" <>
        "MSISDN: " <> clearmsisdn <> "<br>" <>
        "<br>" <>
        withClickableLinks <>
        "</body></html>"

      filename = localDirectory <> "/SMS-" <> show smID <> ".html"

  liftBase $ BSL.writeFile filename (BS.fromStrict $ TE.encodeUtf8 content)
  logInfo "SMS saved to file" $ object [
      "path" .= filename
    ]
  case localOpenCommand of
    Nothing  -> return ()
    Just cmd -> do
      void $ liftBase $ createProcess (proc cmd [filename]) {
          std_in  = Inherit
        , std_out = Inherit
        , std_err = Inherit
      }
      return ()
  return True

curlSMSSender :: (CryptoRNG m, MonadBase IO m, MonadIO m, MonadLog m)
              => [Text] -> Text -> m (Bool, Text)
curlSMSSender params msisdn = do
  logInfo_ $ showt params
  (code, stdout, stderr) <-
    readCurl ((T.unpack <$> params) <> ["--write-out","\n%{http_code}"]) BS.empty
  let (stdout_without_code,http_code) =
        case reverse . lines . BSC.unpack $ stdout of
        [] -> ("", Nothing)
        (lastline:otherlinesreversed) ->
          (unlines $ reverse $ otherlinesreversed, maybeRead $ T.pack lastline)
  case (code, http_code) of
    (ExitSuccess, Just (httpcode :: Int))
      | httpcode >= 200 && httpcode<300 -> do
      logInfo "curlSMSSender success" $ object [
          "code" .= show code
        , "http_code" .= httpcode
        , "stdout" `equalsExternalBSL` stdout
        , "stderr" `equalsExternalBSL` stderr
        , "number" .= msisdn
        ]
      return (True, T.pack stdout_without_code)
    _ -> do
      logInfo "curlSMSSender failed" $ object [
          "code" .= show code
        , "http_code" .= http_code
        , "stdout" `equalsExternalBSL` stdout
        , "stderr" `equalsExternalBSL` stderr
        , "number" .= msisdn
        ]
      return (False, T.pack stdout_without_code)

toLatin :: Text -> Text
toLatin x = case toLatinTransliterate x of
              "" -> toLatinDiscard x
              z -> z
  where
    -- Seems we hit a bug in iconv under Mac. Using translitera mode there
    -- loves to produce empty strings for unknown reason. Using discard mode
    -- removes everything but ANSI, so it is not good either, but good enough
    -- for signing links to get through.
    toLatinTransliterate :: Text -> Text
    toLatinTransliterate =
      TE.decodeUtf8 . BS.toStrict .
      IConv.convertFuzzy IConv.Transliterate "utf8" "latin1" .
      BS.fromStrict . TE.encodeUtf8
    toLatinDiscard :: Text -> Text
    toLatinDiscard =
      TE.decodeUtf8 . BS.toStrict .
      IConv.convertFuzzy IConv.Discard "utf8" "latin1" .
      BS.fromStrict . TE.encodeUtf8

logInfoSendSMS :: MonadLog m => Text -> ShortMessage -> m ()
logInfoSendSMS sender sm = logInfo "Sending SMS" $ object [
      "sender"     .= sender
    , logPair_ sm
    ]
