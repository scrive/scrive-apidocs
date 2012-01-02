{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module Mails.Public (
    module Mails.Data
  , CreateEmail(..)
  , AddContentToEmail(..)
  , GetEvents(..)
  , MarkEventAsRead(..)
  , DeleteEmail(..)
  ) where

import Control.Applicative
import Database.HDBC

import DB.Classes
import DB.Types
import DB.Utils
import Mails.Data
import OurPrelude

data CreateEmail = CreateEmail MagicHash Address [Address]
instance DBUpdate CreateEmail MailID where
  dbUpdate (CreateEmail token sender to) = $(fromJust)
    <$> getOne "INSERT INTO mails (token, sender, receivers) VALUES (?, ?, ?) RETURNING id"
      [toSql token, toSql sender, toSql to]

data AddContentToEmail = AddContentToEmail MailID String String [Attachment] XSMTPAttrs
instance DBUpdate AddContentToEmail Bool where
  dbUpdate (AddContentToEmail mid title content attachments xsmtpapi) = wrapDB $ \conn -> do
    r <- run conn ("UPDATE mails SET"
      ++ "  title = ?"
      ++ ", content = ?"
      ++ ", attachments = ?"
      ++ ", x_smtp_attrs = ? WHERE id = ?") [
          toSql title
        , toSql content
        , toSql attachments
        , toSql xsmtpapi
        , toSql mid
        ]
    oneRowAffectedGuard r

data GetEvents = GetEvents
instance DBQuery GetEvents [(MailID, Event)] where
  dbQuery GetEvents = undefined

data MarkEventAsRead = MarkEventAsRead MailID
instance DBUpdate MarkEventAsRead Bool where
  dbUpdate (MarkEventAsRead _mid) = undefined

data DeleteEmail = DeleteEmail MailID
instance DBUpdate DeleteEmail Bool where
  dbUpdate (DeleteEmail _mid) = undefined
