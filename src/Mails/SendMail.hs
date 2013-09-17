{-# LANGUAGE CPP, RecordWildCards #-}
module Mails.SendMail
    ( Mail(..)
    , emptyMail
    , MailAddress(..)
    , MessageData(..)
    , scheduleEmailSendout
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

-- Needed only for FROM address
import User.Lang
import Util.SignatoryLinkUtils
import Doc.Model
import Doc.DocStateData
import Data.Maybe
import BrandedDomains

scheduleEmailSendout :: (CryptoRNG m, MonadDB m) => MailsConfig -> Mail -> m ()
scheduleEmailSendout MailsConfig{..} mail@Mail{..} = do
  Log.error $ "Sending mail with originator" ++ show originator
  if unsendable to
    then Log.error $ "Email " ++ show mail ++ " is unsendable, discarding."
    else do
      fromAddr <- do
        niceAddress <- fromNiceAddress mailInfo $ fromMaybe ourInfoEmailNiceName originator
        return Address {addrName = niceAddress, addrEmail = ourInfoEmail }
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

-- Prototyped. This is why texts are here. But the proper way to do
-- that is not to add some extra info in Mail data structure
-- Proper way is to hold as abstract data there.
fromNiceAddress :: MonadDB m => MessageData -> String -> m String
fromNiceAddress (None) servicename = return servicename
fromNiceAddress (Invitation did _) servicename = do
  mdoc <- dbQuery $ GetDocumentByDocumentID did
  case mdoc of
    Nothing -> return $ servicename
    Just doc -> case (documentlang doc, getAuthorName doc) of
      (_,         []) -> return $ servicename
      (LANG_SV, an) -> return $ an ++ " genom " ++ servicename
      (LANG_EN, an) -> return $ an ++ " through " ++ servicename

kontramaillocal :: (HasLang a, T.TemplatesMonad m) => Maybe BrandedDomain -> a -> String -> F.Fields m () -> m Mail
kontramaillocal mbd = kontramailHelper mbd . renderLocalTemplate

kontramail :: T.TemplatesMonad m  => Maybe BrandedDomain  -> String -> F.Fields m () -> m Mail
kontramail mbd = kontramailHelper mbd T.renderTemplate

kontramailHelper :: T.TemplatesMonad m => Maybe BrandedDomain -> (String -> F.Fields m () -> m String) -> String -> F.Fields m () -> m Mail
kontramailHelper mbd renderFunc tname fields = do
    wholemail <- renderFunc tname fields
    let (title,content) = span (/= '\n') $ dropWhile (isControl ||^ isSpace) wholemail
    return $ emptyMail {
                         originator = fmap bdemailoriginator mbd
                       , title   = title
                       , content = content
                       }
