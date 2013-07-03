module ScriveByMail.Control (
    handleMailAPI
  ) where

import Doc.DocStateData
import Kontra
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as BSL

import Utils.String
import ScriveByMail.Action
import Doc.Action

import Control.Applicative
import Happstack.Server hiding (simpleHTTP, host)

{- |

   The handler for the mail api. This is where emails get posted to.

-}
handleMailAPI :: Kontrakcja m => m String
handleMailAPI = do
  Input contentspec _ _ <- getDataFnM (lookInput "mail")
  content <- concatChunks <$> case contentspec of
    Left filepath -> liftIO $ BSL.readFile filepath
    Right content -> return content
  mresult <- doMailAPI content
  case mresult of
    Just doc -> do
        _ <- postDocumentPreparationChange doc False
        return $ show $ documentid doc
    Nothing -> return ""
