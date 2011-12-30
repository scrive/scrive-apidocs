{-# LANGUAGE CPP, RecordWildCards #-}
module Mails.SendMail
    ( Mail(..)
    , emptyMail
    , unsendable
    , MailAddress(..)
    , MailInfo(..)
    , scheduleEmailSendout'
    , scheduleEmailSendout
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Text.JSON.Fields
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Text.JSON as J

import API.Service.Model
import DB.Classes
import InputValidation
import Mails.MailsConfig
import Mails.MailsData
import Util.MonadUtils
import Mails.Public as M

-- Needed only for FROM address
import User.Region
import Util.SignatoryLinkUtils
import Doc.Transitory
import Doc.DocStateData

scheduleEmailSendout :: DBMonad m => MailsConfig -> Mail -> m ()
scheduleEmailSendout mc = runDB . scheduleEmailSendout' mc

scheduleEmailSendout' :: MailsConfig -> Mail -> DB ()
scheduleEmailSendout' MailsConfig{..} Mail{..} = do
  fromAddr <- do
    addr <- join . fmap (servicemailfromaddress . servicesettings)
      <$> liftMM (dbQuery . GetService) (return from)
    case addr of
      Nothing -> do
        niceAddress <- fromNiceAddress mailInfo ourInfoEmailNiceName
        return Address {addrName = niceAddress, addrEmail = ourInfoEmail }
      Just address -> return Address { addrName = "", addrEmail = BSU.toString address }
  mid <- dbUpdate $ CreateEmail fromAddr (map toAddress to)
  xsmtpapi <- liftIO $ json $ field "unique_args" $ do
    field "mailinfo" $ show mailInfo
    field "id" $ show mid
  _ <- dbUpdate $ AddContentToEmail mid (BSU.toString title) (wrapHTML content) (map toAttachment attachments) (J.encode xsmtpapi)
  return ()
  where
    toAddress MailAddress{..} = Address {
        addrName  = BSU.toString fullname
      , addrEmail = BSU.toString email
    }
    toAttachment (name, cont) = M.Attachment {
        attName = BSU.toString name
      , attContent = cont
    }

wrapHTML :: BS.ByteString -> String
wrapHTML body = concat [
    "<html>"
  , "<head>"
  , "<meta http-equiv='content-type' content='text/html; charset=utf-8'/>"
  , "</head>"
  , "<body style='background-color: #f5f5f5;'>"
  , BSU.toString body
  , "</body>"
  , "</html>"
  ]

emptyMail :: Mail
emptyMail = Mail
    { to             = []
    , title          = BS.empty
    , content        = BS.empty
    , attachments    = []
    , from           = Nothing
    , mailInfo       = None
}

-- Mail is unsendable if there is no to adress provided
unsendable :: Mail -> Bool
unsendable mail = any (not . valid) (email <$> to mail)
  where
    valid x = case asValidEmail $ BSU.toString x of
                Good _ -> True
                _ -> False

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
