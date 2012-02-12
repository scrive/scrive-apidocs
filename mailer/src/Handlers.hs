module Handlers (
    router
  , handlers
  ) where

import Control.Monad.Reader
import Database.HDBC
import DB.Classes (mkDBEnv)
import Database.HDBC.PostgreSQL
import Happstack.Server hiding (dir, path, decodeBody)
import Happstack.StaticRouting
import Happstack.Server.Internal.MessageWrap (bodyInput)
import System.Directory

import Crypto.RNG (CryptoRNGState)
import MailGun
import MailingServerConf
import Mailer
import SendGrid
import qualified Log

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
  where
    decodeBody bp = do
      rq <- askRq
      (_, me) <- bodyInput bp rq
      case me of
        Nothing -> return ()
        -- workaround invalid happstack behavior: mailgun sends multipart/form-data
        -- POST requests, happstack treats ending boundary as if it was next data
        -- part, tries to parse it and obviously gets nothing as it's the end of
        -- request, so it assumes that request was invalid and immediately returns
        -- an error. I filed a bug about it:
        -- https://code.google.com/p/happstack/issues/detail?id=169
        -- it will be fixed in happstack-server 6.6.1, so when it's released, we
        -- will upgrade and get rid of this piece of code.
        Just "Expected content-disposition: form-data but got Nothing" -> return ()
        Just e -> do
          Log.mailingServer $ "Error while parsing request body: " ++ e
          escape $ requestEntityTooLarge $ toResponse e

handlers :: Route (Mailer Response)
handlers = choice [
    dir "mail" $ dir "sendgrid" $ hPost handleSendGridEvents
  , dir "mail" $ dir "mailgun" $ hPost handleMailGunEvents
  ]
  where
    hPost = path POST id
