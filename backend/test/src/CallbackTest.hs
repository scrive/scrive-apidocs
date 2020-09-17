{-# LANGUAGE DeriveAnyClass #-}
module CallbackTest (callbackTests) where

import Control.Concurrent.Async.Lifted
import Control.Concurrent.Lifted
import Control.Monad.Base
import Data.Aeson
import Database.PostgreSQL.Consumers
import GHC.Generics (Generic)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Optics.View
import Test.Framework
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import Callback.Consumer
import Callback.Model
import Callback.Types
import DB
import TestingUtil
import TestKontra

callbackTests :: TestEnvSt -> Test
callbackTests env = testGroup
  "Callback"
  [ testThat "Execution of a single callback works"           env testSingleCallback
  , testThat "Execution of multiple dependent callbacks work" env testDependentCallbacks
  ]

testSingleCallback :: TestEnv ()
testSingleCallback = do
  let payload = Payload "hi" 1337
  receivedPayloads <- withTestCallbackProcessor $ do
    scheduleNewCallback receiverAddress NoAuth payload
  assertEqual "Payload received successfully" [payload] receivedPayloads

testDependentCallbacks :: TestEnv ()
testDependentCallbacks = do
  let payloads =
        [ Payload "hello"  1
        , Payload "there"  2
        , Payload "friend" 3
        , Payload "how"    4
        , Payload "are"    5
        , Payload "you"    6
        ]
  receivedPayloads <- withTestCallbackProcessor $ do
    scheduleDependentCallbacks $ map (receiverAddress, NoAuth, ) payloads
  assertEqual "Payloads received successfully" payloads receivedPayloads

----------------------------------------

data Payload = Payload T.Text Int
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

receiverPort :: Int
receiverPort = 31846

receiverAddress :: T.Text
receiverAddress = "http://localhost:" <> showt receiverPort

-- | Start the warp server that receives callbacks, then the callback processor
-- so that it can send callbacks to the server, which collects their payloads.
withTestCallbackProcessor :: TestEnv r -> TestEnv [Payload]
withTestCallbackProcessor action = do
  mv                    <- newMVar []
  reqManager            <- liftBase newTlsManager
  ConnectionSource pool <- gview #connSource
  withAsync (liftBase $ runReceiver mv) $ \_ -> do
    -- Process multiple callbacks in parallel to make sure we're not messing up
    -- processing of dependent callbacks, which should be processed one by one.
    let callbacks = callbackConsumer runDB reqManager 10
    finalize (runConsumer callbacks pool) $ do
      void action
      -- Gotta commit so that dispatcher sees db updates
      commit
      -- Idle signal doesn't work here, because we want to potentially process
      -- multiple callbacks here, so just pause for a second.
      threadDelay 1000000
    reverse <$> readMVar mv
  where
    runDB m = do
      r <- m
      -- Gotta commit so that dispatcher sees db updates
      commit
      pure r

    runReceiver mv = run receiverPort $ \req respondWith -> do
      payload <-
        fromMaybe (unexpectedError "invalid payload")
        .   decode @Payload
        .   BSL.fromStrict
        <$> getRequestBodyChunk req
      modifyMVar_ mv $ \payloads -> return $! payload : payloads
      respondWith $ responseLBS status200 [] BSL.empty
