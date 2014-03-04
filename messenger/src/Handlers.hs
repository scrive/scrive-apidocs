{-# LANGUAGE ExtendedDefaultRules #-}
module Handlers (
    router
  , handlers
  ) where

import Control.Monad.Reader
import Happstack.Server hiding (dir, path)
import Happstack.StaticRouting
import System.Directory

import DB
import DB.PostgreSQL
import Crypto.RNG
import GlobalMouth
import Messenger

router :: CryptoRNGState -> ConnectionSource -> Messenger Response -> ServerPartT IO Response
router rng cs routes = withPostgreSQL cs $ do
  let quota = 65536
  temp <- liftIO getTemporaryDirectory
  decodeBody $ defaultBodyPolicy temp quota quota quota
  res <- runMessenger rng $ routes `mplus` notFound (toResponse "Nothing is here.")
  return res

handlers :: Route (Messenger Response)
handlers = choice [
    dir "sms" $ dir "globalmouth" $ hGet handleGlobalMouthEvents
  ]
  where
    hGet = path GET id
