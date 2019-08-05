{-# LANGUAGE ExtendedDefaultRules #-}
module Handlers (
    router
  , handlers
  ) where

import Control.Monad.Reader
import Crypto.RNG
import Happstack.Server hiding (dir, path)
import Happstack.StaticRouting
import Log
import System.Directory

import DB
import DB.PostgreSQL
import Happstack.Server.ReqHandler
import Mblox
import Messenger
import TeliaCallGuide

router
  :: CryptoRNGState
  -> TrackedConnectionSource
  -> Messenger Response
  -> LogT (ReqHandlerT IO) Response
router rng (ConnectionSource pool) routes = withPostgreSQL pool $ do
  runMessenger rng routes

handlers :: Route (Messenger Response)
handlers = choice [
    hGet showHelloMessage
  , dir "sms" $ dir "telia"       $ hPost $
                                    withDecodedBody_ handleTeliaCallGuideEvents
  , dir "sms" $ dir "mblox"       $ hPost handleMbloxEvents
  ]
  where
    hGet = path GET id
    hPost = path POST id

showHelloMessage :: Messenger Response
showHelloMessage = ok $ toResponse "Messenger says hello!"

-- Some providers send a valid POST request that should be decoded for
-- further processing Mblox events have JSON in body but not as
-- parameter, and should not be decoded.
withDecodedBody_ :: Messenger Response -> Messenger Response
withDecodedBody_ action = do
  tempDir <- liftIO getTemporaryDirectory
  withDecodedBody (bodyPolicy tempDir) action
  where
    quota = 65536
    bodyPolicy tempDir = defaultBodyPolicy tempDir quota quota quota
