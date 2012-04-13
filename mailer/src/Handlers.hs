module Handlers (
    router
  , handlers
  ) where

import Control.Monad.Reader
import Database.HDBC
import DB.Classes (mkDBEnv)
import Database.HDBC.PostgreSQL
import Happstack.Server hiding (dir, path)
import Happstack.StaticRouting
import System.Directory

import Crypto.RNG (CryptoRNGState)
import MailGun
import MailingServerConf
import Mailer
import SendGrid

router :: CryptoRNGState -> MailingServerConf -> Mailer Response -> ServerPartT IO Response
router rng conf routes = do
  let quota = 65536
  temp <- liftIO getTemporaryDirectory
  decodeBody $ defaultBodyPolicy temp quota quota quota
  conn <- liftIO $ connectPostgreSQL $ mscDBConfig conf
  dbenv <- mkDBEnv conn rng
  res <- runMailer dbenv $ routes `mplus` notFound (toResponse "Nothing is here.")
  liftIO $ disconnect conn
  return res

handlers :: Route (Mailer Response)
handlers = choice [
    dir "mail" $ dir "sendgrid" $ hPost handleSendGridEvents
  , dir "mail" $ dir "mailgun" $ hPost handleMailGunEvents
  ]
  where
    hPost = path POST id
