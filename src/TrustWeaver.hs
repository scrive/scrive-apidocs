
-----------------------------------------------------------------------------
-- |
-- Module      :  TrustWeaver
-- Maintainer  :  
-- Stability   :  development
-- Portability :  portable
--
-- TrustWeaver interface 
--
-----------------------------------------------------------------------------

module TrustWeaver 
    (signDocument)
    where
import Codec.Binary.Base64
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS hiding (length, drop)
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length, drop)
import qualified Data.ByteString.Lazy as BSL
import Misc
import System.Cmd
import System.Directory
import System.Exit
import System.IO
import System.Process
import Control.Monad
import User (Context(..))
import Text.XML.HaXml.XmlContent.Parser 
import Text.XML.HaXml.XmlContent

data SOAP a = SOAP a
            deriving (Eq,Ord,Show,Read)

instance HTypeable (SOAP a) where
    toHType x = Defined "soap" [] []
instance (XmlContent a) => XmlContent (SOAP a) where
    toContents (SOAP a) =
        [CElem (Elem "Envelope" [mkAttr "xmlns" "http://schemas.xmlsoap.org/soap/envelope/"] 
                         [CElem (Elem "Body" [] 
                         (toContents a)) ()]) ()]
    parseContents = do
        { e <- elementWith skipNamespacePrefix ["Envelope"]
        ; interior e $ do
            { b <- elementWith skipNamespacePrefix ["Body"]
            ; interior b $ return SOAP `apply` parseContents -- (text `onFail` return "")
            }
        } `adjustErr` ("in <Envelope/Body>, "++)


data SignRequest = SignRequest BS.ByteString
            deriving (Eq,Ord,Show,Read)

instance HTypeable (SignRequest) where
    toHType x = Defined "SignRequest" [] []
instance XmlContent (SignRequest) where
    toContents (SignRequest pdfdata) =
        let base64data = encode (BS.unpack pdfdata) in
        [CElem (Elem "SignRequest" [mkAttr "xmlns" "http://www.trustweaver.com/tsswitch"] 
                         [ mkElemC "InputType" (toText "PDF")
                         , mkElemC "JobType" (toText "CADESA")
                         , mkElemC "OutputType" (toText "PDF")
                         , mkElemC "SenderTag" (toText "SE")
                         , mkElemC "ReceiverTag" (toText "SE")
                         , mkElemC "Document" (toText base64data)
                         ]) ()]
    parseContents = error "Please do not parse SignRequest"

data SignResult = SignResult BS.ByteString
            deriving (Eq,Ord,Show,Read)

instance HTypeable (SignResult) where
    toHType x = Defined "SignResult" [] []
instance XmlContent (SignResult) where
    toContents (SignResult pdfdata) = error "Do not do this"
    parseContents =  do
        { e <- elementWith skipNamespacePrefix ["SignResult"]
        ; base64 <- interior e $ do
            { result <- elementWith skipNamespacePrefix ["Result"]
            ; signedDocument <- elementWith skipNamespacePrefix ["SignedDocument"]
            ; details <- elementWith skipNamespacePrefix ["Details"]
            ; interior signedDocument $ (text `onFail` return "")
            }
        ; case decode base64 of
            Nothing -> fail "Cannot parse base64 encoded PDF signed document"
            Just value -> return (SignResult (BS.pack value))
        } `adjustErr` ("in <SignResponse>, "++)

skipNamespacePrefix fqname tomatch = 
    case break (==':') fqname of 
      (name,"") -> name == tomatch
      (_,':':name) -> name == tomatch

signSoapCallText pdfdata = showXml False (SOAP (SignRequest pdfdata))


signDocument :: Context -> BS.ByteString -> IO (Maybe BS.ByteString)
signDocument ctx pdfdata = do
  let Context{ctxtwsigncert,ctxtwsigncertpwd} = ctx

  let input = BSL.fromString (signSoapCallText pdfdata)
  let args = [ "-X", "POST", 
               "-k", "--silent", "--show-error",
               "--cert", ctxtwsigncert ++ ":" ++ ctxtwsigncertpwd,
               "--cacert", ctxtwsigncert,
               "--data-binary", "@-",
               "-H", "Content-Type: text/xml; charset=UTF-8",
               "-H", "Expect: 100-continue",
               "-H", "SOAPAction: \"http://www.trustweaver.com/tsswitch#Sign\"",
               "https://tseiod-dev.trustweaver.com/ts/svs.asmx"
             ]

  (code,stdout,stderr) <- readProcessWithExitCode' "./curl" args input

  when (code /= ExitSuccess) $ do
       putStrLn "Cannot execute ./curl for TrustWeaver"
       BSL.hPutStr System.IO.stdout stderr

  case readXml (BSL.toString stdout) of
    Right (SOAP (SignResult value)) -> return (Just value)
    Left errmsg -> do
      -- FIXME: should log it somewhere
      return Nothing
