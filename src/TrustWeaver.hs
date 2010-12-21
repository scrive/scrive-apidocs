
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
 

signSoapCallText pdfdata = 
 let base64data = encode (BS.unpack pdfdata) in
 "<?xml version=\"1.0\"?>" ++
 "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\">" ++
 "  <s:Body xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\">" ++
 "  <SignRequest xmlns=\"http://www.trustweaver.com/tsswitch\">" ++
 "  <InputType>PDF</InputType>" ++
 "  <JobType>CADESA</JobType>" ++
 "  <OutputType>PDF</OutputType>" ++
 "  <Document>" ++ base64data ++ "</Document>" ++
 "  <SenderTag>SE</SenderTag>" ++
 "  <ReceiverTag>SE</ReceiverTag>" ++
 "  </SignRequest>" ++
 "  </s:Body>" ++
 "</s:Envelope>"


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

  print args

  (code,stdout,stderr) <- readProcessWithExitCode' "curl.exe" args input

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
