module Network.SOAP.Transport.Curl (
    SSL(..)
  , CurlErrorHandler
  , noopErrorHandler
  , mkCertErrorHandler
  , mkDebugFunction
  , curlTransport
  ) where

import Control.Monad.Catch
import Control.Monad.Trans.Control
import Data.ByteString.Lazy (ByteString)
import Foreign.C.String
import Log
import Network.Curl
import Network.SOAP.Transport
import Network.SOAP.Transport.HTTP
import Text.XML
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.Text.Lazy as TL

import KontraPrelude
import Log.Utils

-- TODO: in the future refactor curl bits, possibly create
-- MonadCurl class, stop using curl binary for mails, smses etc.

-- | Type of SSL connection to be used by curl.
data SSL = SecureSSL | InsecureSSL

type CurlErrorHandler = BodyP -> Curl -> CurlCode -> IO ByteString

-- | Dummy error handler, just throws an exception.
noopErrorHandler :: CurlErrorHandler
noopErrorHandler _ _ = curlError

-- | Throw an exception informing about curl error.
curlError :: MonadThrow m => CurlCode -> m a
curlError code = $unexpectedErrorM $ "Curl response code was" <+> show code


-- | In case of certificate error, reconnect with
-- disabled peer verification and log the issue.
mkCertErrorHandler :: (MonadBaseControl IO m, MonadLog m) => m CurlErrorHandler
mkCertErrorHandler = liftBaseWith $ \runInBase ->
  return $ \response_parser curl code ->
    case code of
      CurlSSLCACert -> do
        _ <- runInBase (logAttention_ "CERTIFICATE VERIFICATION ERROR, falling back to insecure connection")
        (final_body, fetch_body) <- newIncoming
        setopts curl
          [ CurlWriteFunction $ gatherOutput_ fetch_body
          , CurlSSLVerifyPeer False
          ]
        perform curl >>= \case
          CurlOK -> response_parser <$> final_body
          code'  -> curlError code'
      code' -> curlError code'

mkDebugFunction :: (MonadBaseControl IO m, MonadLog m) => m DebugFunction
mkDebugFunction = liftBaseWith $ \run -> do
  return $ \_curl debugInfo msg msgSize _userData -> case debugInfo of
    InfoText -> do
      -- Show text message.
      strMsg <- peekCStringLen (msg, fromIntegral msgSize)
      void . run . curlDomain $ forM_ (lines strMsg) logInfo_
    _ -> F.forM_ (maybeShowInfo debugInfo) $ \strMsg -> do
      data_ <- BS.packCStringLen (msg, fromIntegral msgSize)
      void . run . curlDomain . logInfo strMsg $ object [
          "data" `equalsExternalBS` data_
        ]
  where
    curlDomain = localDomain "curl"

    maybeShowInfo :: DebugInfo -> Maybe String
    maybeShowInfo InfoText       = Nothing -- irrelevant
    maybeShowInfo InfoHeaderIn   = Just "Incoming header"
    maybeShowInfo InfoHeaderOut  = Just "Outgoing header"
    maybeShowInfo InfoDataIn     = Just "Incoming data"
    maybeShowInfo InfoDataOut    = Just "Outgoing data"
    -- Do not log SSL data.
    maybeShowInfo InfoSslDataIn  = Nothing
    maybeShowInfo InfoSslDataOut = Nothing

----------------------------------------

curlTransport :: SSL
              -> Maybe FilePath
              -> String
              -> BodyP
              -> CurlErrorHandler
              -> DebugFunction
              -> Transport
curlTransport ssl mcacert url response_parser on_failure debug_fun soap_action soap_request = do
  curl <- initialize
  (final_body, fetch_body) <- newIncoming
  F.forM_ mcacert $ setopt curl . CurlCAInfo
  setopts curl [
      CurlWriteFunction $ gatherOutput_ fetch_body
    , CurlNoSignal True
    , CurlTimeout 2
    , CurlURL url
    , CurlSSLVerifyPeer $ case ssl of
        SecureSSL   -> True
        InsecureSSL -> False
    , CurlHttpHeaders [
        "Content-Type: text/xml; charset=utf-8"
      , "SOAPAction:" <+> soap_action
      ]
    , CurlPost True
    , CurlPostFields [TL.unpack $ renderText def soap_request]
    , CurlVerbose True
    , CurlDebugFunction debug_fun
    ]
  perform curl >>= \case
    CurlOK -> response_parser <$> final_body
    code   -> on_failure response_parser curl code
