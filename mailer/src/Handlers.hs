{-# LANGUAGE ExtendedDefaultRules #-}
module Handlers (
    router
  , handlers
  ) where

import Control.Monad.Reader
import Data.ByteString.UTF8  as BS
import Data.Functor
import Happstack.Server hiding (dir, path)
import Happstack.StaticRouting
import Log
import System.Directory

import Crypto.RNG
import DB
import DB.PostgreSQL
import Happstack.Server.ReqHandler
import KontraPrelude
import Mailer
import MailGun
import SendGrid

router :: CryptoRNGState -> ConnectionSource -> Mailer Response -> ReqHandlerT (LogT IO) Response
router rng cs routes = withPostgreSQL cs $
  runMailer rng routes

handlers :: Route (Mailer Response)
handlers = choice [
    hGet showHelloMessage
  , dir "mail" $ dir "sendgrid" $ hPost $ handleSendGridEvents
  , dir "mail" $ dir "mailgun" $ hPost $ withDecodedBody_ handleMailGunEvents
  ]
  where
    hGet = path GET id
    hPost = path POST id

showHelloMessage :: Mailer Response
showHelloMessage = ok $ toResponse "Mailer says hello!"

handleSendGridEvents :: Mailer Response
handleSendGridEvents = do
  ct <- getHeader "Content-Type" <$> askRq
  if ("x-www-form-urlencoded" `isInfixOf` (fromMaybe "" $ BS.toString <$> ct))
     then withDecodedBody_ handleSendGridEventsV1
     else handleSendGridEventsV3

withDecodedBody_ :: Mailer Response -> Mailer Response
withDecodedBody_ action = do
  tempDir <- liftIO getTemporaryDirectory
  withDecodedBody (bodyPolicy tempDir) action
  where
    quota = 65536
    bodyPolicy tempDir = defaultBodyPolicy tempDir quota quota quota
