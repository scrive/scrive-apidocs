module Network.SOAP.Transport.Curl where

import Control.Applicative
import Data.Maybe
import Data.Monoid.Space
import Network.Curl
import Network.SOAP.Transport
import Network.SOAP.Transport.HTTP
import Text.XML
import qualified Data.Text.Lazy as TL

import Utils.Prelude

data SSL = SecureSSL | InsecureSSL

curlTransport :: SSL
              -> Maybe FilePath
              -> String
              -> BodyP
              -> Transport
curlTransport ssl mcacert url body_p soap_action soap_request = do
  putStrLn $ "RAW: " ++ show raw_request
  h <- initialize
  (final_body, fetch_body) <- newIncoming
  maybeM (setopt h . CurlCAInfo) mcacert
  setopts h [
      CurlWriteFunction $ gatherOutput_ fetch_body
    , CurlNoSignal True
    , CurlTimeout 2
    , CurlURL url
    , CurlSSLVerifyPeer $ case ssl of
        SecureSSL   -> True
        InsecureSSL -> False
    , CurlHttpHeaders $ catMaybes [
        Just "Content-Type: text/xml; charset=utf-8"
      , ("SOAPAction:" <+>) <$> msoap_action
      ]
    , CurlPost True
    , CurlPostFields [raw_request]
    , CurlVerbose True
    ]
  code <- perform h
  case code of
    CurlOK -> body_p <$> final_body
    _      -> error $ "Curl response code was " ++ show code
  where
    msoap_action = if null soap_action then Nothing else Just soap_action
    raw_request = TL.unpack $ renderText def soap_request
