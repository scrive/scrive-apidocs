module MailContext.Internal where

import Data.Time

import BrandedDomain.BrandedDomain
import User.Model

data MailContext = MailContext {
  _mctxlang                 :: !Lang
, _mctxcurrentBrandedDomain :: !BrandedDomain
, _mctxtime                 :: !UTCTime
, _mctxmailNoreplyAddress   :: !String
} deriving Show
