module Callback.Consumer
  ( callbackConsumer
  ) where

import Control.Monad.Base
import Data.Aeson
import Data.Int
import Data.Ix
import Database.PostgreSQL.Consumers.Config
import Log
import Network.HTTP.Client
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Callback.Model
import Callback.Types
import DB
import Log.Identifier
import Log.Utils

data Callback = Callback
  { id         :: CallbackID
  , url        :: T.Text
  , payload    :: JSONB BSL.ByteString
  , attempts   :: Int32
  , authMethod :: AuthMethod
  , mNext      :: Maybe CallbackID
  }

callbackConsumer
  :: (MonadBase IO m, MonadLog m, MonadDB db)
  => (forall r . db r -> m r)
  -> Manager
  -> Int
  -> ConsumerConfig m CallbackID Callback
callbackConsumer runDB manager maxRunningJobs = ConsumerConfig
  { ccJobsTable           = "callbacks"
  , ccConsumersTable      = "callback_consumers"
  , ccJobSelectors        = ["id", "url", "payload", "attempts", "auth_method", "next"]
  , ccJobFetcher = \(id, url, payload, attempts, authMethod, mNext) -> Callback { .. }
  , ccJobIndex            = id
  , ccNotificationChannel = Just callbackNotificationChannel
  , ccNotificationTimeout = 60 * 1000000 -- 1 minute
  , ccMaxRunningJobs      = maxRunningJobs
  , ccProcessJob          = processJob
  , ccOnException         = const onFailure
  }
  where
    processJob callback@Callback {..} = localData [identifier id] $ do
      resp <- liftBase $ dispatch url payload authMethod
      if not $ isSuccess resp
        then do
          logInfo "Callback failed" $ object
            [ "response_code" .= responseCode resp
            , "response_body" `equalsExternalBSL` responseBody resp
            ]
          Failed <$> onFailure callback
        else do
          logInfo "Callback succeeded" $ object ["response_code" .= responseCode resp]
          whenJust mNext $ \next -> do
            logInfo "Scheduling next callback" $ object ["next_callback_id" .= show next]
            runDB $ scheduleExistingCallback next
          pure $ Ok Remove

    onFailure Callback {..} = case attempts of
      1 -> return . RerunAfter $ iminutes 5
      2 -> return . RerunAfter $ iminutes 10
      3 -> return . RerunAfter $ iminutes 30
      4 -> return . RerunAfter $ ihours 1
      5 -> return . RerunAfter $ ihours 2
      6 -> return . RerunAfter $ ihours 4
      7 -> return . RerunAfter $ ihours 4
      8 -> return . RerunAfter $ ihours 4
      9 -> return . RerunAfter $ ihours 8
      _ -> do
        logInfo "10th call attempt failed, discarding" $ object [identifier id]
        return Remove

    dispatch
      :: T.Text -> JSONB BSL.ByteString -> AuthMethod -> IO (Response BSL.ByteString)
    dispatch url (JSONB payload) authMethod = do
      initReq <- parseRequest $ T.unpack url
      let
        reqHook = case authMethod of
          NoAuth -> identity
          BasicAuth user password ->
            applyBasicAuth (T.encodeUtf8 user) (T.encodeUtf8 password)
        req = reqHook $ initReq
          { method         = methodPost
          , requestBody    = RequestBodyLBS payload
          , requestHeaders = ("Content-Type", "application/json")
                               : requestHeaders initReq
          }
      httpLbs req manager

    responseCode :: Response a -> Int
    responseCode = statusCode . responseStatus

    isSuccess :: Response a -> Bool
    isSuccess = inRange (200, 299) . responseCode
