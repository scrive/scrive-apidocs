{-# LANGUAGE ExtendedDefaultRules #-}
module Handlers (
    router
  , handlers
  ) where

import Control.Monad.Reader
import Happstack.Server hiding (dir, path)
import Happstack.StaticRouting
import Log
import System.Directory

import Crypto.RNG
import DB
import DB.PostgreSQL
import GlobalMouth
import Happstack.Server.ReqHandler
import KontraPrelude
import Messenger
import TeliaCallGuide

router :: CryptoRNGState -> ConnectionSource -> Messenger Response -> ReqHandlerT (LogT IO) Response
router rng cs routes = withPostgreSQL cs $ do
  tempDir <- liftIO getTemporaryDirectory
  withDecodedBody (bodyPolicy tempDir) $ do
    runMessenger rng routes
  where
    quota = 65536
    bodyPolicy tempDir = defaultBodyPolicy tempDir quota quota quota

handlers :: Route (Messenger Response)
handlers = choice [
    hGet showHelloMessage
  , dir "sms" $ dir "globalmouth" $ hGet handleGlobalMouthEvents
  , dir "sms" $ dir "telia"       $ hGet handleTeliaCallGuideEvents
  ]
  where
    hGet = path GET id

showHelloMessage :: Messenger Response
showHelloMessage = ok $ toResponse "Messenger says hello!"
