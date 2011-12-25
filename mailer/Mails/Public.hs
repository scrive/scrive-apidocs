module Mails.Public where

import qualified Data.ByteString.Lazy.Char8 as BSL

import DB.Classes
import Mails.Model

data CreateEmail = CreateEmail [String]
instance DBUpdate CreateEmail MailID where
  dbUpdate = undefined

data AddContentToEmail = AddContentToEmail MailID BSL.ByteString
instance DBUpdate AddContentToEmail Bool where
  dbUpdate = undefined
