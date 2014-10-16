{-# LANGUAGE ExtendedDefaultRules #-}
module Handlers (
    router
  , handlers
  ) where

import Control.Monad.Reader
import Data.ByteString.UTF8  as BS
import Data.Functor
import Data.List
import Data.Maybe
import Happstack.Server hiding (dir, path)
import Happstack.StaticRouting
import System.Directory

import Crypto.RNG
import DB
import DB.PostgreSQL
import Mailer
import MailGun
import SendGrid

router :: CryptoRNGState -> ConnectionSource -> Mailer Response -> ServerPartT IO Response
router rng cs routes = withPostgreSQL cs $
  runMailer rng routes

handlers :: Route (Mailer Response)
handlers = choice [
    dir "mail" $ dir "sendgrid" $ hPost $ handleSendGridEvents
  , dir "mail" $ dir "mailgun" $ hPost $ withDecodedBody handleMailGunEvents
  ]
  where
    hPost = path POST id

handleSendGridEvents :: Mailer Response
handleSendGridEvents = do
  ct <- getHeader "Content-Type" <$> askRq
  if ("x-www-form-urlencoded" `isInfixOf` (fromMaybe "" $ BS.toString <$> ct))
     then withDecodedBody handleSendGridEventsV1
     else handleSendGridEventsV3

withDecodedBody :: Mailer Response -> Mailer Response
withDecodedBody mr = do
  let quota = 65536
  temp <- liftIO getTemporaryDirectory
  decodeBody $ defaultBodyPolicy temp quota quota quota
  mr
