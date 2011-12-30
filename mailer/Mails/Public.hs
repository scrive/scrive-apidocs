{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module Mails.Public (
    module Mails.Data
  , CreateEmail(..)
  , AddContentToEmail(..)
  , GetSendgridEvents(..)
  , DeleteEmail(..)
  ) where

import Control.Applicative
import Database.HDBC

import DB.Classes
import DB.Utils
import Mails.Data
import OurPrelude

data CreateEmail = CreateEmail Address [Address]
instance DBUpdate CreateEmail MailID where
  dbUpdate (CreateEmail sender to) = $(fromJust)
    <$> getOne "INSERT INTO mails (sender, receivers) VALUES (?, ?) RETURNING id"
      [toSql sender, toSql to]

data AddContentToEmail = AddContentToEmail MailID String String [Attachment] String
instance DBUpdate AddContentToEmail Bool where
  dbUpdate (AddContentToEmail mid title content attachments xsmtpapi) = wrapDB $ \conn -> do
    r <- run conn ("UPDATE mails SET"
      ++ "  title = ?"
      ++ ", content = ?"
      ++ ", attachments = ?"
      ++ ", x_smtpapi = ? WHERE id = ?") [
          toSql title
        , toSql content
        , toSql attachments
        , toSql xsmtpapi
        , toSql mid
        ]
    oneRowAffectedGuard r

data GetSendgridEvents = GetSendgridEvents
instance DBQuery GetSendgridEvents [(MailID, SendGridEvent)] where
  dbQuery GetSendgridEvents = undefined

data DeleteEmail = DeleteEmail MailID
instance DBUpdate DeleteEmail Bool where
  dbUpdate (DeleteEmail _mid) = undefined
