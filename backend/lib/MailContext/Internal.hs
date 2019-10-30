module MailContext.Internal where

import Data.Time

import BrandedDomain.BrandedDomain
import User.Model

data MailContext = MailContext
  { mctxLang                 :: !Lang
  , mctxCurrentBrandedDomain :: !BrandedDomain
  , mctxTime                 :: !UTCTime
  , mctxMailNoreplyAddress   :: !Text
  } deriving Show
