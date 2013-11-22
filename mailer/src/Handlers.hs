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
import Data.List
import Data.Functor
import Data.ByteString.UTF8  as BS
import Data.Maybe

router :: CryptoRNGState -> MailingServerConf -> Mailer Response -> ServerPartT IO Response
router rng conf routes = withPostgreSQL (mscDBConfig conf) $
  runMailer rng $ routes `mplus` notFound (toResponse "Nothing is here.")

handlers :: Route (Mailer Response)
handlers = choice [
    dir "mail" $ dir "sendgrid" $ hPost $ withDecodedBody handleSendGridEvents
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