{-# LANGUAGE RecordWildCards #-}

module Mails.SendMail
    ( Mail(..)
    , emptyMail
    , MailAddress(..)
    , scheduleEmailSendout
    , scheduleEmailSendoutWithAuthorSender
    , scheduleEmailSendoutWithAuthorSenderThroughService
    , kontramail
    , kontramaillocal
    ) where

import Control.Monad.Catch
import Crypto.RNG
import Data.Char
import Log
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F
import qualified Text.StringTemplates.Templates as T

import BrandedDomain.BrandedDomain
import Branding.Adler32
import DB
import Doc.DocumentID
import Doc.Model
import InputValidation
import Log.Identifier
import Mails.MailsData
import Mails.Model hiding (Mail)
import Templates
import Theme.Model
import User.Lang
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.String
import qualified Mails.Model as M

scheduleEmailSendout :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m) => Mail -> m ()
scheduleEmailSendout m = scheduleEmailSendoutHelper (originator m) m

-- Sending mail with from address like 'Mariusz throught Scrive'
scheduleEmailSendoutWithAuthorSenderThroughService
  :: (CryptoRNG m, MonadDB m, MonadLog m, T.TemplatesMonad m, MonadThrow m)
  => DocumentID
  -> Mail
  -> m ()
scheduleEmailSendoutWithAuthorSenderThroughService did m = do
  doc  <- dbQuery $ GetDocumentByDocumentID did
  name <- case maybe "" getFullName $ getAuthorSigLink doc of
    "" -> return (originator m)
    an -> renderLocalTemplate doc "_mailInvitationFromPart" $ do
      F.value "authorname" an
      F.value "originator" (originator m)
  scheduleEmailSendoutHelper name m

-- Sending mail with from address like 'Mariusz'
scheduleEmailSendoutWithAuthorSender
  :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m, T.TemplatesMonad m)
  => DocumentID
  -> Mail
  -> m ()
scheduleEmailSendoutWithAuthorSender did m = do
  doc <- dbQuery $ GetDocumentByDocumentID did
  let names =
        [getFullName <$> getAuthorSigLink doc, getCompanyName <$> getAuthorSigLink doc]
  name <- case firstNonEmpty (fromMaybe "" <$> names) of
    "" -> return (originator m)
    an -> return an
  scheduleEmailSendoutHelper name m

scheduleEmailSendoutHelper
  :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m) => Text -> Mail -> m ()
scheduleEmailSendoutHelper authorname mail@Mail {..} = do
  logInfo "Sending mail" $ object ["originator" .= originator]
  if unsendable to
    then logAttention "Email is unsendable, discarding" $ logObject_ mail
    else do
      let fromAddr = Address { addrName = authorname, addrEmail = originatorEmail }
      token  <- random
      mailid <- dbUpdate $ CreateEmail
        ( token
        , fromAddr
        , map toAddress to
        , fmap toAddress replyTo
        , title
        , wrapHTML content
        , map toAttachment attachments
        )
      case kontraInfoForMail of
        Just kifm -> void . dbUpdate $ AddKontraInfoForMail mailid kifm
        Nothing   -> return ()
  where
    toAddress MailAddress {..} = Address { addrName = fullname, addrEmail = email }
    toAttachment (name, cont) = M.Attachment { attName = name, attContent = cont }

    -- Mail is unsendable if there is no to adress provided
    unsendable = any ((not . valid) . email)
      where
        valid x = case asValidEmail x of
          Good _ -> True
          _      -> False

wrapHTML :: Text -> Text
wrapHTML body = T.concat
  [ "<!DOCTYPE html PUBLIC \"-//W4C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
  , "<html>"
  , "<head>"
  , "<meta http-equiv='content-type' content='text/html; charset=utf-8'/>"
  , "</head>"
  , "<body style='padding:0px;margin:0px;'>"
  , body
  , "</body>"
  , "</html>"
  ]

kontramaillocal
  :: (HasLang a, T.TemplatesMonad m)
  => Text
  -> BrandedDomain
  -> Theme
  -> a
  -> Text
  -> F.Fields m ()
  -> m Mail
kontramaillocal noreplyAddress bd theme =
  kontramailHelper noreplyAddress bd theme . renderLocalTemplate

kontramail
  :: T.TemplatesMonad m
  => Text
  -> BrandedDomain
  -> Theme
  -> Text
  -> F.Fields m ()
  -> m Mail
kontramail noreplyAddress bd theme =
  kontramailHelper noreplyAddress bd theme renderTextTemplate

kontramailHelper
  :: T.TemplatesMonad m
  => Text
  -> BrandedDomain
  -> Theme
  -> (Text -> F.Fields m () -> m Text)
  -> Text
  -> F.Fields m ()
  -> m Mail
kontramailHelper noreplyAddress bd theme renderFunc tname fields = do
  wholemail <- renderFunc tname fields
  let (title, content) =
        case T.splitOn "\r\n" $ T.dropWhile (isControl || isSpace) wholemail of
          [] -> unexpectedError "Couldnt separate email content from title"
          (title' : contentChunks) ->
            (unescapeHTML title', T.intercalate "\r\n" contentChunks)
  return $ emptyMail
    { originator      = bd ^. #emailOriginator
    , originatorEmail = noreplyAddress
    , title           = title
    , content         = content
    , attachments     = [ ( "logo-" <> imageAdler32 (themeLogo theme) <> ".png"
                          , Left $ themeLogo theme
                          )
                        ]
    }
