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
import MailGun
import MailingServerConf
import Mailer
import SendGrid

router :: CryptoRNGState -> MailingServerConf -> Mailer Response -> ServerPartT IO Response
router rng conf routes = withPostgreSQL (mscDBConfig conf) $ do
  let quota = 65536
  temp <- liftIO getTemporaryDirectory
  decodeBody $ defaultBodyPolicy temp quota quota quota
  res <- runMailer rng $ routes `mplus` notFound (toResponse "Nothing is here.")
  return res

handlers :: Route (Mailer Response)
handlers = choice [
    dir "mail" $ dir "sendgrid" $ hPost handleSendGridEvents
  , dir "mail" $ dir "mailgun" $ hPost handleMailGunEvents
  ]
  where
    hPost = path POST id
