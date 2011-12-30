module Handlers (
    router
  , handlers
  ) where

import Control.Monad.Reader
import Database.HDBC
import Database.HDBC.PostgreSQL
import Happstack.Server hiding (dir, path)
import Happstack.StaticRouting

import MailingServerConf
import Mailer

router :: MailingServerConf -> Mailer Response -> ServerPartT IO Response
router conf routes = do
  conn <- liftIO $ connectPostgreSQL $ mscDBConfig conf
  res <- runMailer conn $ routes `mplus` notFound (toResponse "Nothing is here.")
  liftIO $ disconnect conn
  return res

handlers :: Route (Mailer Response)
handlers = choice [
    dir "sendgrid" $ hPost handleSendgridEvents
  ]
  where
    hPost = path POST id

handleSendgridEvents :: Mailer Response
handleSendgridEvents = ok $ toResponse "Thanks"
