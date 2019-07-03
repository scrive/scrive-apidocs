module EID.Nets.Call (
    netsCall
  , NetsSignParsingError(..)
  ) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Time
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Time
import Data.Typeable
import Log
import Text.XML
import Text.XML.Cursor
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Text as T

import EID.Nets.Config
import EID.Nets.SignID
import Kontra hiding (InternalError)
import Log.Identifier
import MinutesTime
import Network.XMLCurl
import Network.XMLCurl
  ( CurlAuth(..), SSL(..), mkCertErrorHandler, mkDebugFunction )

import Text.XML.Parser
import Text.XML.XMLDSig
import qualified Text.XML.Writer.Extended as X

netsCall
  :: ( X.ToXML rq, Loggable rs
     , MonadTime m, MonadIO m, MonadLog m, MonadBase IO m, MonadBaseControl IO m
     , MonadMask m)
  => NetsSignConfig
  -> rq                    -- request
  -> XMLParser rs
  -> String                -- tmp dir suffix
  -> m rs
netsCall NetsSignConfig{..} request response_parser tmpdirsuffix = do
  tsmID <- newTrustSignMessageUUID
  now <- currentTime
  let xmlrequest = X.toXML request
      rqname = rqName xmlrequest
  certErrorHandler <- mkCertErrorHandler
  debugFunction <- mkDebugFunction
  mSignedRq <- signXMLDSig netssignPrivKeyFile netssignCertFile tmpdirsuffix . wrapTrustSignMessage tsmID netssignMerchantID now . X.toXML $ request
  signedRq <- maybe internalError return mSignedRq
  let runCurl = liftIO $ curlTransport SecureSSL (CurlAuthCertKey netssignCertFile netssignPrivKeyFile) (T.unpack $ netssignAPIUrl) certErrorHandler debugFunction (BC8.unpack signedRq) []
  rsXML <- parseLBS_ def <$> runCurl
  logInfo ("Succesfully received Nets XML for " <> rqname <> " response") $ object [
      "xml" .= (renderText def $ rsXML)
    ]
  let rsCursor = fromDocument rsXML
  rs <- case runParser response_parser rsCursor of
    Just response -> return response
    Nothing -> throwM $ NetsSignParsingError $ "UnsuccessfulNetsSignXMLParse(" <> rqname <> " response):" <> T.pack (ppCursor rsCursor)
  logInfo ("Succesfully parsed Nets" <> rqname <> " response") $ logObject_ rs
  return rs

rqName :: X.XML -> T.Text
rqName xml = case X.render xml of
  [NodeElement el] -> nameLocalName . elementName $ el
  _ -> "SomeXMLElementName" -- fallback for unexpected XML, which does not have element as top node

data NetsSignParsingError = NetsSignParsingError T.Text deriving (Show, Typeable)
instance Exception NetsSignParsingError

wrapTrustSignMessage
  :: TrustSignMessageUUID
  -> T.Text
  -> UTCTime
  -> X.XML
  -> X.XML
wrapTrustSignMessage tsmID merchantID now xml = do
  X.elementA
    "TrustSignMessage"
    [ ("xmlns","http://www.bbs.no/tt/trustsign/2017/06/tsm#")
    , ("xmlns:xs","http://www.w3.org/2001/XMLSchema-instance")
    ] $ do
      X.element "MerchantID" merchantID
      X.element "Time" . T.pack . formatTimeISO $ now
      X.element "MessageID" . T.pack . show $ tsmID
      xml
