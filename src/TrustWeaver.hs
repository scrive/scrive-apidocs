
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
        { e@(Elem _ [] _) <- element ["Envelope"]
        ; interior e $ do
            { b@(Elem _ [] _) <- element ["Body"]
            ; interior b $ return SOAP `apply` parseContents -- (text `onFail` return "")
            }
        } `adjustErr` ("in <Envelope/Body>, "++)


data SignRequest = SignRequest BS.ByteString

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
  
  let stdout1 = BS.concat (BSL.toChunks stdout)
  let begtag = BS.fromString "<SignedDocument>"
  let endtag = BS.fromString "</SignedDocument>"
  let (b,e) = BS.breakSubstring begtag stdout1
      d1 = BS.drop (BS.length begtag) e
      (d,_) = BS.breakSubstring endtag d1
  
  if BS.null d
     then return Nothing
     else return $ fmap BS.pack $ decode (BS.toString d)
