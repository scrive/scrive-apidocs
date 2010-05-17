{-# LANGUAGE ForeignFunctionInterface, CPP #-}


module SendMail where
import Control.Monad(msum,liftM,mzero,guard,MonadPlus(..))
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
import qualified Data.Object.Json as Json
import qualified Network.Curl as Curl
import Data.Maybe
import Happstack.Data.IxSet as IxSet
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import HSX.XMLGenerator
import HSP
import Misc
import System.Process
import System.IO
import System.Directory

sendMail :: BS.ByteString -- ^ full name
         -> BS.ByteString -- ^ email address
         -> BS.ByteString -- ^ title
         -> BS.ByteString -- ^ plaintext contents
         -- more arguments follow
         -> IO ()
#ifdef WINDOWS
sendMail fullname email title content = do
  let filename = "Email-" ++ BS.toString email ++ ".html"
  BS.writeFile filename content
  openDocument filename
#else
sendMail fullname email title content = do
  tmp <- getTemporaryDirectory
  let mailto = BS.toString fullname ++ " <" ++ BS.toString email ++ ">"
  (Just handle_in,_,_,handle_process) <-
      createProcess (proc "sendmail" ["-i", mailto ]) { std_in = CreatePipe }
  -- FIXME: encoding issues
  hPutStr handle_in ("Subject: " ++ BS.toString title ++
                        "\n\n" ++ BS.toString content)
  hClose handle_in
  waitForProcess handle_process
  return ()
  
#endif



