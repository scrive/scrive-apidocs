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

import Control.Applicative
import Control.Monad.Catch
import Data.Char
import Data.Maybe
import Data.String.Utils
import qualified Text.StringTemplates.Fields as F
import qualified Text.StringTemplates.Templates as T

import BrandedDomain.BrandedDomain
import Branding.MD5
import Control.Logic
import Crypto.RNG
import DB
import Doc.DocumentID
import Doc.Model
import InputValidation
import Mails.MailsConfig
import Mails.MailsData
import Mails.Model hiding (Mail)
import MessageData
import MinutesTime
import Templates
import Theme.Model
import User.Lang
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.String
import qualified Log
import qualified Mails.Model as M

scheduleEmailSendout :: (CryptoRNG m, MonadDB m, MonadThrow m, Log.MonadLog m) => MailsConfig -> Mail -> m ()
scheduleEmailSendout c m =  scheduleEmailSendout' (originator m) c m

-- Sending mail with from address like 'Mariusz throught Scrive'
scheduleEmailSendoutWithAuthorSenderThroughService :: (CryptoRNG m, MonadDB m, Log.MonadLog m, T.TemplatesMonad m, MonadThrow m) => DocumentID  -> MailsConfig -> Mail -> m ()
scheduleEmailSendoutWithAuthorSenderThroughService did c m = do
  doc <- dbQuery $ GetDocumentByDocumentID did
  name <- case (maybe "" getFullName $ getAuthorSigLink doc) of
      ("") -> return $ (originator m)
      (an) -> renderLocalTemplate doc "_mailInvitationFromPart" $ do
                F.value "authorname" an
                F.value "originator" (originator m)
  scheduleEmailSendout' name c m

-- Sending mail with from address like 'Mariusz'
scheduleEmailSendoutWithAuthorSender :: (CryptoRNG m, MonadDB m, MonadThrow m, Log.MonadLog m, T.TemplatesMonad m) => DocumentID  -> MailsConfig -> Mail -> m ()
scheduleEmailSendoutWithAuthorSender did c m = do
  doc <- dbQuery $ GetDocumentByDocumentID did
  let names = [getFullName <$> getAuthorSigLink doc, getCompanyName <$> getAuthorSigLink doc]
  name <- case firstNonEmpty (fromMaybe "" <$> names) of
      ("") -> return $ (originator m)
      (an) -> return an
  scheduleEmailSendout' name c m

scheduleEmailSendout' :: (CryptoRNG m, MonadDB m, MonadThrow m, Log.MonadLog m) => String -> MailsConfig -> Mail ->  m ()
scheduleEmailSendout' authorname  MailsConfig{..} mail@Mail{..} = do
  Log.mixlog_ $ "Sending mail with originator " ++ show originator
  if unsendable to
    then Log.attention_ $ "Email " ++ show mail ++ " is unsendable, discarding."
    else do
      fromAddr <- return Address {addrName = authorname, addrEmail = originatorEmail }
      token <- random
      now <- currentTime
      mid <- dbUpdate $ CreateEmail token fromAddr (map toAddress to) now
      let xsmtpapi = XSMTPAttrs [("mailinfo", show mailInfo)]
      _ <- dbUpdate $ AddContentToEmail mid title (fmap toAddress replyTo) (wrapHTML content) (map toAttachment attachments) xsmtpapi
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
                       , attachments = [("logo-"++ (imageMD5 $ themeLogo  theme) ++ ".png", Left $ unBinary $ themeLogo theme)]
                       }
