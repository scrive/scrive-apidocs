{-# LANGUAGE StrictData #-}
module Flow.DocumentStarting
  ( startDocument
  )
 where

import Control.Concurrent.MVar
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import Data.CaseInsensitive
import Data.Char
import GHC.Generics
import Happstack.Server
import Log.Class
import Servant.Server
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map

import Doc.API.V2.Calls.DocumentPostCalls
import Doc.DocumentID
import Flow.Error
import Flow.Server.Types
import KontraMonad
import User.Types.User

startDocument :: Account -> DocumentID -> AppM ()
startDocument account docId = do
  FlowContext {..} <- ask
  request          <- liftIO $ startDocumentRequest account
  response         <- lift $ handleWithKontra (kontraActions (user account) docId) request
  processResponse docId response

kontraActions :: User -> DocumentID -> (forall  m . Kontrakcja m => m Response)
kontraActions user docId = do
  modifyContext $ \ctx -> ctx & (#maybeUser ?~ user)
  docApiV2Start docId

startDocumentRequest :: Account -> IO Request
startDocumentRequest Account {..} = do
  rqBody       <- newMVar $ Body BS.empty
  rqInputsBody <- newMVar []

  let grouped = groupBy (\x y -> fst x == fst y) $ sortOn fst headers
  let headers' = fmap toHappstackHeader grouped
  pure $ Request { rqSecure      = True
                 , rqMethod      = POST
                 , rqPaths       = []
                 , rqUri         = ""
                 , rqQuery       = ""
                 , rqInputsQuery = []
                 , rqInputsBody
                 , rqCookies     = []
                 , rqVersion     = HttpVersion 1 1
                 , rqHeaders     = Map.fromList headers'
                 , rqBody
                 , rqPeer        = ("peer hostname", 12345)
                 }
  where
    toHappstackHeader :: [(CI ByteString, ByteString)] -> (ByteString, HeaderPair)
    toHappstackHeader l = (BSC8.map toLower orig, HeaderPair orig values)
      where
        orig   = original keys
        keys   = fst $ head l
        values = fmap snd l

data KontraResponse = KontraResponse
  { documentId :: DocumentID
  , error      :: Maybe Value
  } deriving (Eq, Generic, Show)

instance ToJSON KontraResponse where
  toEncoding = genericToEncoding aesonOptions

toFlowError :: Int -> KontraResponse -> FlowError
toFlowError statusCode details =
  flowError statusCode "Document could not be started" "Document could not be started"
    $ Just details

processResponse
  :: (MonadError ServerError m, MonadLog m) => DocumentID -> Response -> m ()
processResponse docId = \case
  Response {..} -> case rsCode of
    _ | rsCode < 300 -> pure ()
    _ | rsCode < 500 -> throwError . makeJSONError $ toFlowError rsCode details
    _                -> do
      logInfo_ $ "Internal server error when starting document" <> showt rsBody
      throwInternalServerError "Could not start document"
    where details = KontraResponse docId (decode rsBody)

  SendFile{} -> do
    logInfo_ "Expected Happstack Response, got SendFile instead."
    throwInternalServerError "Could not start document"
