module Text.XML.XMLDSig (
    signXMLDSig
  ) where

import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson ((.=), object)
import Log
import System.Exit
import System.FilePath ((</>))
import System.Process.ByteString.Lazy (readProcessWithExitCode)
import Text.XML (renderText)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL hiding (length)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Text.XML as TX

import Log.Utils
import Text.XML.Writer.Extended hiding (content, many, node)
import Utils.Directory (withSystemTempDirectory')

signXMLDSig
  :: (MonadIO m, MonadMask m, ToXML xml, MonadBaseControl IO m, MonadLog m)
  => FilePath
  -> FilePath
  -> String
  -> xml
  -> m (Maybe BS.ByteString)
signXMLDSig user_privkey_file user_cert_file tmpdirsuffix xml = do
  signed_xml_bs <-
    withSystemTempDirectory' ("XMLDSig-" ++ tmpdirsuffix ++ "-") $ \tmppath -> do
      let xml_file        = tmppath </> "doc-x509.xml"
          signed_xml_file = tmppath </> "doc-signed-x509.xml"
          xmlFileContent =
            T.encodeUtf8 . TL.toStrict . renderText TX.def . wrapXMLDSigTmpl . toXML $ xml
      logInfo "Temp file write" $ object
        [ "bytes_written" .= (BS.length xmlFileContent)
        , "originator" .= ("signXMLDSig" :: TL.Text)
        ]

      liftIO $ BS.writeFile xml_file xmlFileContent

      let args =
            [ "--sign"
            , "--output"
            , signed_xml_file
            , "--privkey-pem"
            , user_privkey_file ++ "," ++ user_cert_file
            , xml_file
            ]
      (code, stdout, stderr) <- liftIO $ readProcessWithExitCode "xmlsec1" args BSL.empty
      case (code == ExitSuccess) of
        False -> do
          logAttention "XMLDSig signing failed" $ object
            [ "exit_code" .= show code
            , "stdout" `equalsExternalBSL` stdout
            , "stderr" `equalsExternalBSL` stderr
            ]
          return Nothing
        True -> (Just <$>) . liftIO . BS.readFile $ signed_xml_file
  whenJust
    signed_xml_bs
    (\bs -> logInfo "Temp file read" $ object
      ["bytes_read" .= (BS.length bs), "originator" .= ("signXMLDSig" :: TL.Text)]
    )
  return signed_xml_bs

wrapXMLDSigTmpl :: XML -> TX.Document
wrapXMLDSigTmpl xml_content =
  documentA "Signature" [("xmlns", "http://www.w3.org/2000/09/xmldsig#")] $ do
    element "SignedInfo" $ do
      elementA "CanonicalizationMethod"
               [("Algorithm", "http://www.w3.org/TR/2001/REC-xml-c14n-20010315")]
               ()
      elementA "SignatureMethod"
               [("Algorithm", "http://www.w3.org/2000/09/xmldsig#rsa-sha1")]
               ()
      elementA "Reference" [("URI", "#object")] $ do
        elementA "DigestMethod"
                 [("Algorithm", "http://www.w3.org/2000/09/xmldsig#sha1")]
                 ()
        element "DigestValue" ()
    element "SignatureValue" ()
    element "KeyInfo" $ do
      element "X509Data" $ do
        element "X509SubjectName"  ()
        element "X509IssuerSerial" ()
        element "X509Certificate"  ()
    elementA "Object" [("Id", "object")] xml_content
