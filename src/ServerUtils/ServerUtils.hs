module ServerUtils.ServerUtils (
     handleSerializeImage
   , handleTextToImage
  ) where

--import Happstack.Server hiding (dir, simpleHTTP)
import Text.JSON
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS
import Control.Monad
import Data.Functor
import Kontra
import Happstack.Fields
import Util.MonadUtils
import Text.JSON.Gen
import System.Exit
import System.IO.Temp
import System.Process
import Happstack.Server hiding (dir, simpleHTTP)
import Control.Monad.Trans
import Log as Log
import Numeric
import Data.Maybe
import Utils.String

handleSerializeImage :: Kontrakcja m => m JSValue
handleSerializeImage = do
  logo <- guardJustM $ getFileField "logo"
  runJSONGenT $ value "logo_base64" $ showJSON $ B64.encode logo

handleTextToImage :: Kontrakcja m =>  m Response
handleTextToImage = do
    text <- fmap (take 50) $ guardJustM $ getField "text"
    (width::Int)  <- fst <$> (guardJustM $ join <$> fmap (listToMaybe . readDec) <$> getField "width")
    (height::Int) <- fst <$> (guardJustM $ join <$> fmap (listToMaybe . readDec) <$> getField "height")
    (font, fontSize) <- do
              mfont <- getField "font"
              return $ case mfont of
                        Just "JenniferLynne"    -> ("public/fonts/JenniferLynne.ttf",22)
                        Just "TalkingToTheMoon" -> ("public/fonts/TalkingToTheMoon.ttf",20)
                        _                       -> ("public/fonts/TheOnlyException.ttf",16)
    base64 <- isFieldSet "base64"
    transparent <- isFieldSet "transparent"
    left <- isFieldSet "left"
    mfcontent <- liftIO $ withSystemTempDirectory "text_to_image" $ \tmppath -> do
      let fpath = tmppath ++ "/text_to_image.png"
      (_,_,_, drawer) <-createProcess $  proc "convert" [  "-size",(show width ++ "x" ++ show height)
                                                    , "-background",if (transparent) then "transparent" else "white"
                                                    , "-pointsize", show (pointSize width height (length text) fontSize)
                                                    , "-gravity",if (left) then "West" else "Center"
                                                    , "-font", font
                                                    , "label:" ++ (if null text then " " else text)
                                                    ,  fpath]
      drawerexitcode <- waitForProcess drawer
      case drawerexitcode of
          ExitFailure msg -> do
            Log.debug $ "Problem text_to_image " ++ show msg
            return Nothing
          ExitSuccess -> (BSL.readFile fpath) >>= (return . Just)
    case mfcontent of
         Just fcontent -> if base64
                             then ok $ toResponseBS (BS.fromString "text/plain") $ BSL.fromChunks [BS.fromString "data:image/png;base64,", B64.encode $ concatChunks fcontent]
                             else ok $ toResponseBS (BS.fromString "img/png") $ fcontent
         Nothing -> internalError

-- Point scale - some heuristic for pointsize, based on size of image, type of font and lenght of text.
-- Result should be point size that will result in best fit. It is also expected that this value does not change much on text lenght change.
-- Magic font dependend value sugests what point size will fill in 30x30 squere.

pointSize :: Int -> Int  -> Int -> Int -> Int
pointSize width height textl fontSize =
    min ((height - 20)  * fontSize `div` 30) ((width `div` (textl + 1)) * fontSize `div` 11)

