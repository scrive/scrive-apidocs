module Dispatcher (
    dispatcher
  ) where

import Control.Concurrent
import Control.Monad
import Database.HDBC.PostgreSQL
import qualified Control.Exception as E

import DB.Classes
import Sender
import Mails.Model
import qualified AppLogger as Log (mailingServer)

dispatcher :: Sender -> String -> IO ()
dispatcher sender dbconf = withPostgreSQL dbconf send
  where
    send conn = do
      res <- E.try $ ioRunDB conn $ do
        mails <- dbQuery GetIncomingEmails
        forM_ mails $ \mail -> do
          success <- sendMail sender mail
          if success
            then do
              res <- dbUpdate $ MarkEmailAsSent $ mailID mail
              when (not res) $
                error $ "Marking email #" ++ show (mailID mail) ++ " as sent failed"
            else error "Sending email failed"
      case res of
        Right () -> threadDelay second
        Left (e::E.SomeException) -> do
          Log.mailingServer $ "Exception '" ++ show e ++ "' thrown while sending emails, sleeping for 10 minutes"
          threadDelay $ 10 * 60 * second
      send conn
    second = 1000000
