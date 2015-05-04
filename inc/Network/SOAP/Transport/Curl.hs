module Network.SOAP.Transport.Curl (
    SSL(..)
  , CurlErrorHandler
  , noopErrorHandler
  , mkCertErrorHandler
  , curlTransport
  ) where

import Control.Monad.Catch
import Control.Monad.Trans.Control
import Data.ByteString.Lazy (ByteString)
import Network.Curl
import Network.SOAP.Transport
import Network.SOAP.Transport.HTTP
import Text.XML
import qualified Data.Text.Lazy as TL

import KontraPrelude
import Log
import Utils.Prelude

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

----------------------------------------

curlTransport :: SSL
              -> Maybe FilePath
              -> String
              -> BodyP
              -> CurlErrorHandler
              -> Transport
curlTransport ssl mcacert url response_parser on_failure soap_action soap_request = do
  curl <- initialize
  (final_body, fetch_body) <- newIncoming
  maybeM (setopt curl . CurlCAInfo) mcacert
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
    ]
  perform curl >>= \case
    CurlOK -> response_parser <$> final_body
    code   -> on_failure response_parser curl code
