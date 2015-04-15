{-# LANGUAGE ExtendedDefaultRules #-}
module Handlers (
    router
  , handlers
  ) where

import Control.Monad.Reader
import Happstack.Server hiding (dir, path)
import Happstack.StaticRouting
import System.Directory

import Crypto.RNG
import DB
import DB.PostgreSQL
import GlobalMouth
import Happstack.Server.ReqHandler
import KontraPrelude
import Messenger
import qualified Log

router :: CryptoRNGState -> ConnectionSource -> Messenger Response -> ReqHandlerT (Log.LogT IO) Response
router rng cs routes = withPostgreSQL cs $ do
  tempDir <- liftIO getTemporaryDirectory
  withDecodedBody (bodyPolicy tempDir) $ do
    runMessenger rng routes
  where
    quota = 65536
    bodyPolicy tempDir = defaultBodyPolicy tempDir quota quota quota

handlers :: Route (Messenger Response)
handlers = choice [
    dir "sms" $ dir "globalmouth" $ hGet handleGlobalMouthEvents
  ]
  where
    hGet = path GET id
