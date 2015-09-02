module Doc.Texts (
  runJavaTextExtract
) where

import Control.Monad.IO.Class
import Log
import Log.Utils (equalsExternalBSL)
import System.Exit
import Text.JSON hiding (Ok)
import Text.JSON.Convert (jsonToAeson)
import Text.JSON.FromJSValue
import Text.JSON.Gen
import Data.Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Text.JSON as J
import qualified Text.JSON.Pretty as J (pp_value)

import Kontra
import KontraPrelude
import Utils.Directory
import Utils.IO

{- |
Java scrivepdftools extract-text expect as input JSON in the following format:

{ "rects": [ { "rect": [0,0,1,1],   // rectangle to extract text from, normalized to 0,0-1,1
               "page": 1},          // page number to extract from, starting from 1
             { "rect": [0,0,0.2,0.2],
               "page": 7 }]}

as output it will add keys to the json on the input:
{ "rects": [ { "rect": [0,0,1,1],   // rectangle to extract text from, normalized to 0,0-1,1
               "page": 1,           // page number to extract from, starting from 1
               "lines": [ "first line of extracted text",
                          "second line of extracted text" ]
             { "rect": [0,0,0.2,0.2],
               // no lines here as document has less than 7 pages
               "page": 7 }]}

java tool tries to preserve lines of text that were give in
pdf. Whitespace is normalized: no whitespec at the beginning or the
end, single space between words, newlines, tabs changed to
spaces. Note that whitespace in PDF is not reliable as sometimes
letters are just spread out visually but do not contain whitespace
character between them. When matching you should probably just run the
words together to stay on the safe side.

-}
runJavaTextExtract :: (Monad m,Kontrakcja m) => JSValue -> BS.ByteString -> m (Either Text JSValue)
runJavaTextExtract json content = do
  withSystemTempDirectory' ("extract-texts-") $ \tmppath -> do
    let tmpin = tmppath ++ "/input.pdf"
    let specpath = tmppath ++ "/sealspec.json"

    let (rects :: Maybe JSValue) = fromJSValueField "rects" json
    let config = runJSONGen $ do
                   value "rects" rects

    liftIO $ BS.writeFile tmpin content
    liftIO $ BS.writeFile specpath (BS.fromString $ show $ J.pp_value (toJSValue config))
    (code, stdout, stderr) <- liftIO $ do
      readProcessWithExitCode' "java" ["-jar", "scrivepdftools/scrivepdftools.jar", "extract-texts", specpath, tmpin] (BSL.empty)
    case code of
      ExitSuccess -> do
          let (decoderesult :: Result JSValue) = decode $ BSL.toString stdout
          case decoderesult of
            J.Ok jsvalue -> do
              let (rectsresult :: Maybe JSValue) = fromJSValueField "rects" jsvalue
              let censoredresult = runJSONGen $ do
                       value "rects" rectsresult
              return $ Right censoredresult
            _ -> return $ Left "Backend did not return JSON"
      ExitFailure _ -> do
          logAttention "Extract texts failed" $ Log.object [
              "configuration" .= jsonToAeson json
            , "stderr" `equalsExternalBSL` stderr
            ]
          return $ Left "Extract texts failed on PDF"
