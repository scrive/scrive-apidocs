module MailContext.Internal where

import Data.Time

import BrandedDomain.BrandedDomain
import KontraPrelude
import User.Model

data MailContext = MailContext {
  _mctxlang                 :: !Lang
, _mctxcurrentBrandedDomain :: !BrandedDomain
, _mctxtime                 :: !UTCTime
, _mctxmailNoreplyAddress   :: !String
} deriving Show
