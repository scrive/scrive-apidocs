{-# LANGUAGE RecordWildCards #-}
module Mails.SendMail
    ( Mail(..)
    , emptyMail
    , MailAddress(..)
    , MessageData(..)
    , scheduleEmailSendout
    , scheduleEmailSendoutWithDocumentAuthorSender
    , kontramail
    , kontramaillocal
    ) where

import DB
import Crypto.RNG
import InputValidation
import Mails.MailsConfig
import Mails.MailsData
import Mails.Model hiding (Mail)
import qualified Log
import qualified Mails.Model as M
import qualified Text.StringTemplates.Templates as T
import qualified Text.StringTemplates.Fields as F
import Templates
import Control.Logic
import Data.Char
import MessageData
import MinutesTime

-- Needed only for FROM address
import User.Lang
import Util.SignatoryLinkUtils
import Doc.Model
import Doc.DocumentID
import Data.Maybe
import BrandedDomain.BrandedDomain


scheduleEmailSendout :: (CryptoRNG m, MonadDB m, Log.MonadLog m) => MailsConfig -> Mail -> m ()
scheduleEmailSendout c m =  scheduleEmailSendout' (originator m) c m

scheduleEmailSendoutWithDocumentAuthorSender :: (CryptoRNG m, MonadDB m, Log.MonadLog m, T.TemplatesMonad m) => DocumentID  -> MailsConfig -> Mail -> m ()
scheduleEmailSendoutWithDocumentAuthorSender did c m = do
  doc <- dbQuery $ GetDocumentByDocumentID did
  name <- case (getAuthorName doc) of
      ("") -> return $ (originator m)
      (an) -> renderLocalTemplate doc "_mailInvitationFromPart" $ do
                F.value "authorname" an
                F.value "originator" (originator m)
  scheduleEmailSendout' name c m

scheduleEmailSendout' :: (CryptoRNG m, MonadDB m, Log.MonadLog m) => String -> MailsConfig -> Mail ->  m ()
scheduleEmailSendout' authorname  MailsConfig{..} mail@Mail{..} = do
  Log.mixlog_ $ "Sending mail with originator " ++ show originator
  if unsendable to
    then Log.attention_ $ "Email " ++ show mail ++ " is unsendable, discarding."
    else do
      fromAddr <- return Address {addrName = authorname, addrEmail = ourInfoEmail }
      token <- random
      now <- getMinutesTime
      mid <- dbUpdate $ CreateEmail token fromAddr (map toAddress to) now
      let xsmtpapi = XSMTPAttrs [("mailinfo", show mailInfo)]
      _ <- dbUpdate $ AddContentToEmail mid title (wrapHTML content) (map toAttachment attachments) xsmtpapi
      return ()
  where
    toAddress MailAddress{..} = Address {
        addrName  = fullname
      , addrEmail = email
    }
    toAttachment (name, cont) = M.Attachment {
        attName = name
      , attContent = cont
    }

    -- Mail is unsendable if there is no to adress provided
    unsendable = any (not . valid) . map email
      where
        valid x = case asValidEmail x of
          Good _ -> True
          _ -> False

wrapHTML :: String -> String
wrapHTML body = concat [
    "<!DOCTYPE html PUBLIC \"-//W4C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
  , "<html>"
  , "<head>"
  , "<meta http-equiv='content-type' content='text/html; charset=utf-8'/>"
  , "</head>"
  , "<body style='padding:0px;margin:0px;'>"
  , body
  , "</body>"
  , "</html>"
  ]

kontramaillocal :: (HasLang a, T.TemplatesMonad m) => MailsConfig -> Maybe BrandedDomain -> a -> String -> F.Fields m () -> m Mail
kontramaillocal mc mbd = kontramailHelper mc mbd . renderLocalTemplate

kontramail :: T.TemplatesMonad m  => MailsConfig -> Maybe BrandedDomain  -> String -> F.Fields m () -> m Mail
kontramail mc mbd = kontramailHelper mc mbd T.renderTemplate

kontramailHelper :: T.TemplatesMonad m => MailsConfig -> Maybe BrandedDomain -> (String -> F.Fields m () -> m String) -> String -> F.Fields m () -> m Mail
kontramailHelper mc mbd renderFunc tname fields = do
    wholemail <- renderFunc tname fields
    let (title,content) = span (/= '\n') $ dropWhile (isControl ||^ isSpace) wholemail
    return $ emptyMail {
                         originator = fromMaybe (ourInfoEmailNiceName mc) (fmap bdemailoriginator mbd)
                       , title   = title
                       , content = content
                       }
