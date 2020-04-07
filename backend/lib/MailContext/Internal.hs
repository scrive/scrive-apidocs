{-# LANGUAGE TemplateHaskell #-}
module MailContext.Internal where

import Data.Time
import Optics.TH

import BrandedDomain.BrandedDomain
import User.Model

data MailContext = MailContext
  { lang                 :: Lang
  , brandedDomain :: BrandedDomain
  , time                 :: UTCTime
  , mailNoreplyAddress   :: Text
  } deriving Show

makeFieldLabelsWith noPrefixFieldLabels ''MailContext
