{-# LANGUAGE ExtendedDefaultRules #-}
module Handlers (
    router
  , handlers
  ) where

import Control.Monad.Reader
import Happstack.Server hiding (dir, path)
import Happstack.StaticRouting
import System.Directory

import DB.PostgreSQL
import Crypto.RNG
import GlobalMouth
import MessengerServerConf
import Messenger

router :: CryptoRNGState -> MessengerServerConf -> Messenger Response -> ServerPartT IO Response
router rng conf routes = withPostgreSQL (mscDBConfig conf) $ do
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
