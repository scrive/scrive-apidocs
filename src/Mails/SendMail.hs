{-# LANGUAGE CPP, RecordWildCards #-}
module Mails.SendMail
    ( Mail(..)
    , emptyMail
    , MailAddress(..)
    , MailInfo(..)
    , scheduleEmailSendout'
    , scheduleEmailSendout
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import System.Random (randomIO)
import qualified Data.ByteString.UTF8 as BSU

import API.Service.Model
import DB.Classes
import InputValidation
import Mails.MailsConfig
import Mails.MailsData
import Mails.Model hiding (Mail)
import MinutesTime
import Util.MonadUtils
import qualified Log
import qualified Mails.Model as M

-- Needed only for FROM address
import User.Region
import Util.SignatoryLinkUtils
import Doc.Transitory
import Doc.DocStateData

scheduleEmailSendout :: DBMonad m => MailsConfig -> Mail -> m ()
scheduleEmailSendout mc = runDB . scheduleEmailSendout' mc

scheduleEmailSendout' :: MailsConfig -> Mail -> DB ()
scheduleEmailSendout' MailsConfig{..} mail@Mail{..} = do
  if unsendable to
    then Log.error $ "Email " ++ show mail ++ " is unsendable, discarding."
    else do
      fromAddr <- do
        addr <- join . fmap (servicemailfromaddress . servicesettings)
          <$> liftMM (dbQuery . GetService) (return from)
        case addr of
          Nothing -> do
            niceAddress <- fromNiceAddress mailInfo ourInfoEmailNiceName
            return Address {addrName = niceAddress, addrEmail = ourInfoEmail }
          Just address -> return Address { addrName = "", addrEmail = BSU.toString address }
      token <- liftIO randomIO
      now <- getMinutesTime
      mid <- dbUpdate $ CreateEmail token fromAddr (map toAddress to) now
      let xsmtpapi = XSMTPAttrs [("mailinfo", show mailInfo)]
      _ <- dbUpdate $ AddContentToEmail mid title (wrapHTML content) (map toAttachment attachments) xsmtpapi
      return ()
  where
    toAddress MailAddress{..} = Address {
        addrName  = BSU.toString fullname
      , addrEmail = BSU.toString email
    }
    toAttachment (name, cont) = M.Attachment {
        attName = name
      , attContent = cont
    }

    -- Mail is unsendable if there is no to adress provided
    unsendable = any (not . valid) . map email
      where
        valid x = case asValidEmail $ BSU.toString x of
          Good _ -> True
          _ -> False

wrapHTML :: String -> String
wrapHTML body = concat [
    "<html>"
  , "<head>"
  , "<meta http-equiv='content-type' content='text/html; charset=utf-8'/>"
  , "</head>"
  , "<body style='background-color: #f5f5f5;'>"
  , body
  , "</body>"
  , "</html>"
  ]

-- Prototyped. This is why texts are here. But the propper way to do
-- that is not to add some extra info in Mail data structure
-- Propper way is to hold as abstract data there.
fromNiceAddress :: MailInfo -> String -> DB String
fromNiceAddress (None) servicename = return servicename
fromNiceAddress (Invitation did _) servicename = do
  mdoc <- doc_query' $ GetDocumentByDocumentID did
  case mdoc of
    Nothing -> return $ servicename
    Just doc -> case (documentregion doc, BSU.toString $ getAuthorName doc) of
      (_,         []) -> return $ servicename
      (REGION_SE, an) -> return $ an ++ " genom " ++ servicename
      (REGION_GB, an) -> return $ an ++ " through " ++ servicename
