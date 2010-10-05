{-# LANGUAGE ForeignFunctionInterface, CPP #-}


module SendMail(Mail(),emptyMail,sendMail,title,content, fullnameemails,attachments) where
import Control.Monad(msum,liftM,mzero,guard,MonadPlus(..),when,forM_)
import Control.Monad.Reader (ask)
import Control.Monad.Trans(liftIO, MonadIO,lift)
import Happstack.Server hiding (simpleHTTP)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.State (update,query,getRandomR)
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Char8 as BSC

import qualified Data.Object.Json as Json
import Data.Maybe
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import HSX.XMLGenerator
import HSP
import Misc
import System.Process
import System.IO
import System.Directory
import Data.Array
import Data.Bits
import Data.Int
import Data.Char (chr,ord)
import qualified Codec.Binary.Base64 as Base64
import qualified Codec.Binary.QuotedPrintable as QuotedPrintable
import Data.List
import System.Log.Logger
import Happstack.Util.LogFormat
import Data.Time.Clock

-- from simple utf-8 to =?UTF-8?Q?zzzzzzz?=
-- FIXME: should do better job at checking if encoding should be applied or not
mailEncode :: BS.ByteString -> String
mailEncode source = 
    "=?UTF-8?Q?" ++ QuotedPrintable.encode (BS.unpack source) ++ "?="

mailEncode1 :: String -> String
mailEncode1 source = mailEncode (BS.fromString source)

type Email = BS.ByteString
type Fullname = BS.ByteString
data Mail =  Mail {
               fullnameemails::[(Fullname,Email)],
               title::BS.ByteString,
               content::BS.ByteString,      
               attachments::[(BS.ByteString,BS.ByteString)] -- list of attachments (name,content). 
             }
             
emptyMail = Mail { fullnameemails=[], title= BS.empty, content = BS.empty, attachments = []}    
    
sendMail::Mail->IO ()         
sendMail (Mail fullnameemails title content attachments) = do
  tm <- getCurrentTime
  let mailtos = map fmt fullnameemails 
      -- ("Gracjan Polak","gracjan@skrivapa.se") => "Gracjan Polak <gracjan@skrivapa.se>"
      fmt (fullname,email) = mailEncode fullname ++ " <" ++ BS.toString email ++ ">"
  logM "Kontrakcja.Mail" NOTICE $ formatTimeCombined tm ++ " " ++ concat (intersperse ", " mailtos)
#ifdef WINDOWS
  tmp <- getTemporaryDirectory
  let filename = tmp ++ "/Email-" ++ BS.toString (snd (head fullnameemails)) ++ ".eml"
  handle_in <- openBinaryFile filename WriteMode 
#else
  let rcpt = concatMap (\(_,x) -> ["--mail-rcpt", "<" ++ BS.toString x ++ ">"]) fullnameemails

  (Just handle_in,_,_,handle_process) <-
      -- createProcess (proc "sendmail" (["-i"] ++ mailtos)) { std_in = CreatePipe }
      createProcess (proc "./curl" ([ "--user"
                                    , "info@skrivapa.se:kontrakcja"
                                    , "smtp://smtp.gmail.com:587"
                                    , "-k", "--ssl" -- , "-v", "-v"
                                    , "--mail-from"
                                    , "<info@skrivapa.se>"
                                    ] ++ rcpt)) { std_in = CreatePipe}
#endif
  -- FIXME: add =?UTF8?B= everywhere it is needed here
  let boundary = "skrivapa-mail-12-337331046" 
  let header = 
          -- FIXME: encoded word should not be longer than 75 bytes including everything
          "Subject: " ++ mailEncode title ++ "\r\n" ++
          "To: " ++ concat (intersperse ", " mailtos) ++ "\r\n" ++
          "From: " ++ mailEncode1 "SkrivaPå" ++ " <info@skrivapa.se>\r\n" ++
          "MIME-Version: 1.0\r\n" ++
          "Content-Type: multipart/mixed; boundary=" ++ boundary ++ "\r\n" ++
          "\r\n"
  let header1 = "--" ++ boundary ++ "\r\n" ++
          "Content-type: text/html; charset=utf-8\r\n" ++
          "\r\n"
  let header2 = "\r\n--" ++ boundary ++ "\r\n" ++
          "Content-Disposition: inline; filename=\"" ++ BS.toString title ++".pdf\"\r\n" ++
          "Content-Type: application/pdf; name=\"" ++ BS.toString title ++ ".pdf\"\r\n" ++
          "Content-Transfer-Encoding: base64\r\n" ++
          "\r\n"
  let footer = "\r\n--" ++ boundary ++ "--\r\n.\r\n"
  BS.hPutStr handle_in (BS.fromString header)
  BS.hPutStr handle_in (BS.fromString header1)
  BS.hPutStr handle_in content
  when (not (BS.null attachmentcontent)) $ do
    BS.hPutStr handle_in (BS.fromString header2)
    BS.hPutStr handle_in (BSC.pack $ concat $ intersperse "\r\n" $ Base64.chop 72 $ Base64.encode $ BS.unpack attachmentcontent)
  BS.hPutStr handle_in (BS.fromString footer)
  
  hFlush handle_in
  hClose handle_in
#ifdef WINDOWS
  openDocument filename
#else
  waitForProcess handle_process
#endif
  return ()

