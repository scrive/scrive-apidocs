module Network.SOAP.Transport.Curl (
    curlTransport
  ) where

import Network.Curl
import Network.SOAP.Transport
import Text.XML
import qualified Data.Text.Lazy as TL

import qualified Network.XMLCurl as XC

curlTransport
  :: XC.SSL -> XC.CurlAuth -> String -> XC.CurlErrorHandler -> DebugFunction -> Transport
curlTransport ssl curlAuth url on_failure debug_fun soap_action soap_request =
  XC.curlTransport ssl
                   curlAuth
                   url
                   on_failure
                   debug_fun
                   (TL.unpack . renderText def $ soap_request)
                   ["SOAPAction:" <+> soap_action]
