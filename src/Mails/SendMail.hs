{-# LANGUAGE RecordWildCards #-}
module Mails.SendMail
    ( Mail(..)
    , emptyMail
    , MailAddress(..)
    , MessageData(..)
    , scheduleEmailSendout
    , scheduleEmailSendoutWithAuthorSender
    , scheduleEmailSendoutWithAuthorSenderThroughService
    , kontramail
    , kontramaillocal
    ) where

import Control.Monad.Catch
import Data.Char
import Data.String.Utils
import Log
import qualified Text.StringTemplates.Fields as F
import qualified Text.StringTemplates.Templates as T

import BrandedDomain.BrandedDomain
import Branding.Adler32
import Control.Logic
import Crypto.RNG
import DB
import Doc.DocumentID
import Doc.Model
import InputValidation
import KontraPrelude
import Mails.MailsConfig
import Mails.MailsData
import Mails.Model hiding (Mail)
import MessageData
import Templates
import Theme.Model
import User.Lang
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.String
import qualified Mails.Model as M

scheduleEmailSendout :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m) => MailsConfig -> Mail -> m ()
scheduleEmailSendout c m = scheduleEmailSendoutHelper (originator m) c m

-- Sending mail with from address like 'Mariusz throught Scrive'
scheduleEmailSendoutWithAuthorSenderThroughService :: (CryptoRNG m, MonadDB m, MonadLog m, T.TemplatesMonad m, MonadThrow m) => DocumentID  -> MailsConfig -> Mail -> m ()
scheduleEmailSendoutWithAuthorSenderThroughService did c m = do
  doc <- dbQuery $ GetDocumentByDocumentID did
  name <- case (maybe "" getFullName $ getAuthorSigLink doc) of
      ("") -> return $ (originator m)
      (an) -> renderLocalTemplate doc "_mailInvitationFromPart" $ do
                F.value "authorname" an
                F.value "originator" (originator m)
  scheduleEmailSendoutHelper name c m

-- Sending mail with from address like 'Mariusz'
scheduleEmailSendoutWithAuthorSender :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m, T.TemplatesMonad m) => DocumentID  -> MailsConfig -> Mail -> m ()
scheduleEmailSendoutWithAuthorSender did c m = do
  doc <- dbQuery $ GetDocumentByDocumentID did
  let names = [getFullName <$> getAuthorSigLink doc, getCompanyName <$> getAuthorSigLink doc]
  name <- case firstNonEmpty (fromMaybe "" <$> names) of
      ("") -> return $ (originator m)
      (an) -> return an
  scheduleEmailSendoutHelper name c m

scheduleEmailSendoutHelper :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m) => String -> MailsConfig -> Mail ->  m ()
scheduleEmailSendoutHelper authorname  MailsConfig{..} mail@Mail{..} = do
  logInfo_ $ "Sending mail with originator " ++ show originator
  if unsendable to
    then logAttention_ $ "Email " ++ show mail ++ " is unsendable, discarding."
    else do
      fromAddr <- return Address {addrName = authorname, addrEmail = originatorEmail }
      token <- random
      let xsmtpapi = XSMTPAttrs [("mailinfo", show mailInfo)]
      void . dbUpdate $ CreateEmail (token, fromAddr, map toAddress to, fmap toAddress replyTo, title, wrapHTML content, map toAttachment attachments, xsmtpapi)
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

kontramaillocal :: (HasLang a, T.TemplatesMonad m) => BrandedDomain -> Theme -> a -> String -> F.Fields m () -> m Mail
kontramaillocal bd theme = kontramailHelper  bd theme . renderLocalTemplate

kontramail :: T.TemplatesMonad m  => BrandedDomain -> Theme -> String -> F.Fields m () -> m Mail
kontramail bd theme = kontramailHelper bd theme T.renderTemplate

kontramailHelper :: T.TemplatesMonad m => BrandedDomain -> Theme ->  (String -> F.Fields m () -> m String) -> String -> F.Fields m () -> m Mail
kontramailHelper bd theme renderFunc tname fields = do

    wholemail <- renderFunc tname fields
    let (title,content) = span (/= '\n') $ dropWhile (isControl ||^ isSpace) wholemail
    return $ emptyMail {
                         originator = bdEmailOriginator bd
                       , originatorEmail = strip $ bdNoreplyEmail bd
                       , title   = title
                       , content = content
                       , attachments = [("logo-"++ (imageAdler32 $ themeLogo  theme) ++ ".png", Left $ unBinary $ themeLogo theme)]
                       }
