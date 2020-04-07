module ServerUtils.ServerUtils (
     handleParseCSV
   , handleSerializeImage
   , brandedImage
  ) where

import Control.Monad.Trans
import Data.List (isPrefixOf)
import Happstack.Server hiding (dir, simpleHTTP)
import Log
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.Exit
import System.FilePath ((</>), takeBaseName)
import System.Process.ByteString.Lazy (readProcessWithExitCode)
import Text.JSON
import Text.JSON.Gen hiding (object)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS (fromString)
import qualified Data.Text as T
import qualified Happstack.Server.Response as Web
import qualified Text.JSON.Gen as J

import Happstack.Fields
import Kontra
import Util.CSVUtil
import Util.MonadUtils

-- Read a csv file from POST, and returns a JSON with content
handleParseCSV :: Kontrakcja m => m JSValue
handleParseCSV = do
  input <- getDataFn' (lookInput "csv")
  case input of
    Just (Input contentspec (Just filename) _) -> do
      content <- case contentspec of
        Left  filepath -> liftIO $ BSL.readFile filepath
        Right content  -> return content
      let _title = BS.fromString (takeBaseName filename)
      case parseCSV content of
        Right (h : r) -> J.runJSONGenT $ do
          J.value "header" h
          J.value "rows" r
        _ -> runJSONGenT $ J.value "parseError" True
    _ -> runJSONGenT $ J.value "parseError" True

-- Read an image file from POST, and returns a its content encoded with Base64
-- extensions is comma-separated list of accepted extensions (returns 400 for other filetypes)
handleSerializeImage :: Kontrakcja m => m Response
handleSerializeImage = do
  fileinput                <- getDataFn' (lookInput "logo")
  acceptedExtensionsString <- guardJustM $ getField "extensions"
  let acceptedExtensions = T.splitOn "," acceptedExtensionsString
  case fileinput of
    Nothing -> badRequest' "Missing file"
    Just (Input _ Nothing _) -> badRequest' "Missing file"
    Just (Input contentspec (Just filename) _contentType) -> do
      let hasExtension ext = ("." <> ext) `T.isSuffixOf` T.toLower (T.pack filename)
      if any hasExtension acceptedExtensions
        then do
          content <- case contentspec of
            Left  filepath -> liftIO $ BS.readFile filepath
            Right content' -> return . BS.concat $ BSL.toChunks content'
          goodRequest . runJSONGen $ value "logo_base64" (showJSON $ B64.encode content)
        else badRequest' "Not image"
  where
    badRequest' s =
      return
        $ ( setHeader "Content-Type" "text/plain; charset=UTF-8"
          $ Web.toResponse (s :: String)
          ) { rsCode = 400
            }
    goodRequest js =
      return
        $ (setHeader "Content-Type" "text/plain; charset=UTF-8" . Web.toResponse $ encode
            js
          )
            { rsCode = 200
            }

-- Take any image and brand it by replacing black, non-transparent colors with the specified colour.
-- Expecting color and filename to be passed as parameters.
-- Color format should be #deadbe.
-- Filename is the basename of the file, brandedImage will find it in frontend/app/img/
brandedImage :: Kontrakcja m => m Response
brandedImage = do
  color <- fmap (T.take 12) . guardJustM $ getField "color"
  file  <- fmap (T.take 50) . guardJustM $ getField "file"
  img   <- brandImage file color
  ok . setHeaderBS "Cache-Control" "max-age=604800" $ toResponseBS "image/png" img

brandImage :: Kontrakcja m => Text -> Text -> m BSL.ByteString
brandImage file color = do
  cwd <- liftIO getCurrentDirectory
  let imgDir = cwd </> "frontend/app/img"
  fpath <- liftIO . makeAbsolute $ imgDir </> T.unpack file
  -- Make sure the normalized path is in the `imgDir` directory. This way nobody
  -- can exploit this functionality to do something malicious.
  unless (imgDir `isPrefixOf` fpath) $ do
    logAttention "Image file have to stay in image directory after path normalization"
      $ object
          [ "image-directory" .= imgDir
          , "image-relative-path" .= file
          , "image-normalized-path" .= fpath
          ]
    internalError
  (procResult, out, _) <- liftIO $ readProcessWithExitCode
    "convert"
    [ fpath
    , "-colorspace"
    , "Gray"
    , "-channel"
    , "RGB"
    , "+level-colors"
    , T.unpack color <> ",white"
    , "-"
    ]
    ""
  case procResult of
    ExitFailure msg -> do
      logInfo "Problem branding signview image" $ object ["error_code" .= msg]
      internalError
    ExitSuccess -> return out
