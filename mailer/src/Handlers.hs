{-# LANGUAGE ExtendedDefaultRules #-}
module Handlers (
    router
  , handlers
  ) where

import Control.Monad.Reader
import Happstack.Server hiding (dir, path)
import Happstack.StaticRouting

import DB.PostgreSQL
import Crypto.RNG
import MailGun
import MailingServerConf
import Mailer
import SendGrid
import System.Directory

router :: CryptoRNGState -> MailingServerConf -> Mailer Response -> ServerPartT IO Response
router rng conf routes = withPostgreSQL (mscDBConfig conf) $
  runMailer rng $ routes `mplus` notFound (toResponse "Nothing is here.")

handlers :: Route (Mailer Response)
handlers = choice [
    dir "mail" $ dir "sendgrid" $ hPost $ withDecodedBody handleSendGridEvents
  , dir "mail" $ dir "sendgrid" $ dir "v3" $ hPost handleSendGridEventsV3
  , dir "mail" $ dir "mailgun" $ hPost $ withDecodedBody handleMailGunEvents
  ]
  where
    hPost = path POST id


withDecodedBody :: Mailer Response -> Mailer Response
withDecodedBody mr = do
  let quota = 65536
  temp <- liftIO getTemporaryDirectory
  decodeBody $ defaultBodyPolicy temp quota quota quota
  mr