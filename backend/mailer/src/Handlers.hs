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
import Mailer
import MailGun
import MailingServerConf
import MailJet
import SendGrid
import SocketLabs

router
  :: CryptoRNGState
  -> TrackedConnectionSource
  -> Mailer Response
  -> LogT (ReqHandlerT IO) Response
router rng (ConnectionSource pool) routes = withPostgreSQL pool $ runMailer rng routes

handlers :: MailingServerConf -> Route (Mailer Response)
handlers conf = choice
  [ hGet showHelloMessage
  , dir "mail" $ dir "sendgrid" $ hPost $ handleSendGridEvents
  , dir "mail" $ dir "mailgun" $ hPost $ withDecodedBody_ handleMailGunEvents
  , dir "mail" $ dir "socketlabs" $ hPost $ withDecodedBody_ $ handleSocketLabsEvents conf
  , dir "mail" $ dir "mailjet" $ hPost $ handleMailJetEvents
  ]
  where
    hGet  = path GET id
    hPost = path POST id

showHelloMessage :: Mailer Response
showHelloMessage = ok $ toResponse "Mailer says hello!"

-- All providers except SendGrid send a valid POST request that should be decoded for further processing
-- SendGrid events have JSON in body but not as parameter, and should not be decoded.
withDecodedBody_ :: Mailer Response -> Mailer Response
withDecodedBody_ action = do
  tempDir <- liftIO getTemporaryDirectory
  withDecodedBody (bodyPolicy tempDir) action
  where
    quota = 31000000
    bodyPolicy tempDir = defaultBodyPolicy tempDir quota quota quota
