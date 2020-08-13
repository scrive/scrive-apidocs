{-# LANGUAGE StrictData #-}
module Flow.DocumentStarting
  ( startDocument
  , emptyKontrakcjaRequestFromAccount
  )
 where

import Control.Concurrent.MVar
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import GHC.Generics
import Happstack.Server
import Log.Class
import Servant.Server
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
  request          <- liftIO $ emptyKontrakcjaRequestFromAccount account
  response         <- lift $ handleWithKontra (kontraActions (user account) docId) request
  processResponse docId response

kontraActions :: User -> DocumentID -> (forall  m . Kontrakcja m => m Response)
kontraActions user docId = do
  modifyContext $ \ctx -> ctx & (#maybeUser ?~ user)
  docApiV2Start docId

emptyKontrakcjaRequestFromAccount :: Account -> IO Request
emptyKontrakcjaRequestFromAccount Account {..} = do
  rqBody       <- newMVar $ Body BS.empty
  rqInputsBody <- newMVar []

  pure $ Request { rqSecure      = True
                 , rqMethod      = POST
                 , rqPaths       = []
                 , rqUri         = ""
                 , rqQuery       = ""
                 , rqInputsQuery = []
                 , rqInputsBody
                 , rqCookies     = []
                 , rqVersion     = HttpVersion 1 1
                 , rqHeaders     = Map.empty
                 , rqBody
                 , rqPeer        = ("peer hostname", 12345)
                 }

data KontraResponse = KontraResponse
  { documentId :: DocumentID
  , error      :: Maybe Value
  } deriving (Eq, Generic, Show)

instance ToJSON KontraResponse where
  toEncoding = genericToEncoding aesonOptions

processResponse
  :: (MonadError ServerError m, MonadLog m) => DocumentID -> Response -> m ()
processResponse docId = \case
  Response {..} -> if
    | rsCode < 300 -> pure ()
    | rsCode < 500 -> throwDocumentCouldNotBeStarted rsCode $ Just details
    | otherwise -> do
      logInfo_ $ "Internal server error when starting document" <> showt rsBody
      throwInternalServerError "Could not start document"
    where details = KontraResponse docId (decode rsBody)

  SendFile{} -> do
    logInfo_ "Expected Happstack Response, got SendFile instead."
    throwInternalServerError "Could not start document"
