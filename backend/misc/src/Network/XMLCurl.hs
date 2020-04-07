module Network.XMLCurl (
    SSL(..)
  , CurlErrorHandler
  , noopErrorHandler
  , mkCertErrorHandler
  , mkDebugFunction
  , curlTransport
  , CurlAuth(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Trans.Control
import Data.ByteString.Lazy (ByteString)
import Foreign.C.String
import Log
import Network.Curl
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Log.Utils

-- TODO: in the future refactor curl bits, possibly create
-- MonadCurl class, stop using curl binary for mails, smses etc.

-- | Type of SSL connection to be used by curl.
data SSL = SecureSSL | InsecureSSL

type CurlErrorHandler = Curl -> CurlCode -> IO ByteString

-- | Dummy error handler, just throws an exception.
noopErrorHandler :: CurlErrorHandler
noopErrorHandler _ = curlError

-- | Throw an exception informing about curl error.
curlError :: MonadThrow m => CurlCode -> m a
curlError code = unexpectedError $ "Curl response code was" <+> showt code

-- | In case of certificate error, reconnect with
-- disabled peer verification and log the issue.
mkCertErrorHandler :: (MonadBaseControl IO m, MonadLog m) => m CurlErrorHandler
mkCertErrorHandler = liftBaseWith $ \runInBase -> return $ \curl code -> case code of
  CurlSSLCACert -> do
    void . runInBase $ logAttention_
      "CERTIFICATE VERIFICATION ERROR, falling back to insecure connection"
    (final_body, fetch_body) <- newIncoming
    setopts curl [CurlWriteFunction $ gatherOutput_ fetch_body, CurlSSLVerifyPeer False]
    perform curl >>= \case
      CurlOK -> final_body
      code'  -> curlError code'
  code' -> curlError code'

mkDebugFunction :: (MonadBaseControl IO m, MonadLog m) => m DebugFunction
mkDebugFunction = liftBaseWith $ \run -> do
  return $ \_curl debugInfo msg msgSize _userData -> case debugInfo of
    InfoText -> do
      -- Show text message.
      strMsg <- peekCStringLen (msg, fromIntegral msgSize)
      (void . run . curlDomain) . forM_ (lines strMsg) $ logInfo_ . T.pack
    _ -> F.forM_ (maybeShowInfo debugInfo) $ \strMsg -> do
      data_ <- BS.packCStringLen (msg, fromIntegral msgSize)
      void . run . curlDomain . logInfo strMsg $ object ["data" `equalsExternalBS` data_]
  where
    curlDomain = localDomain "curl"

    maybeShowInfo :: DebugInfo -> Maybe Text
    maybeShowInfo InfoText       = Nothing -- irrelevant
    maybeShowInfo InfoHeaderIn   = Just "Incoming header"
    maybeShowInfo InfoHeaderOut  = Just "Outgoing header"
    maybeShowInfo InfoDataIn     = Just "Incoming data"
    maybeShowInfo InfoDataOut    = Just "Outgoing data"
    -- Do not log SSL data.
    maybeShowInfo InfoSslDataIn  = Nothing
    maybeShowInfo InfoSslDataOut = Nothing

----------------------------------------

data CurlAuth
  = CurlAuthCert FilePath
  | CurlAuthCertKey FilePath FilePath -- user certificate and user private key for authentication to the server
  | CurlAuthBasic Text Text
  | CurlAuthNone

curlTransport
  :: SSL
  -> CurlAuth
  -> String
  -> CurlErrorHandler
  -> DebugFunction
  -> String
  -> [String]
  -> IO ByteString
curlTransport ssl curlAuth url on_failure debug_fun xml_request_bin additionalHeaders =
  do
    curl                     <- initialize
    (final_body, fetch_body) <- newIncoming
    case curlAuth of
      CurlAuthCert fp -> void . setopt curl $ CurlCAInfo fp
      CurlAuthCertKey fpcert fpkey ->
        void . setopts curl $ [CurlSSLCert fpcert, CurlSSLKey fpkey]
      _ -> return ()
    let authHeaders = case curlAuth of
          CurlAuthBasic un pwd ->
            [ "Authorization: Basic"
                <+> BSC8.unpack (B64.encode . T.encodeUtf8 $ un <> ":" <> pwd)
            ]
          _ -> []
    setopts
      curl
      [ CurlWriteFunction $ gatherOutput_ fetch_body
      , CurlNoSignal True
      , CurlTimeout 10
      , CurlURL url
      , CurlSSLVerifyPeer $ case ssl of
        SecureSSL   -> True
        InsecureSSL -> False
      , CurlHttpHeaders authHeaders
      , CurlHttpHeaders
      $  ["Content-Type: text/xml; charset=utf-8"]
      ++ authHeaders
      ++ additionalHeaders
      , CurlPost True
      , CurlPostFields [xml_request_bin]
      , CurlVerbose True
      , CurlDebugFunction debug_fun
      ]
    perform curl >>= \case
      CurlOK -> final_body
      code   -> on_failure curl code
