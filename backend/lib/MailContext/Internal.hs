{-# LANGUAGE TemplateHaskell #-}
module MailContext.Internal where

import Data.Time
import Optics.TH

import BrandedDomain.BrandedDomain
import User.Model

data MailContext = MailContext
  { mctxLang                 :: !Lang
  , mctxCurrentBrandedDomain :: !BrandedDomain
  , mctxTime                 :: !UTCTime
  , mctxMailNoreplyAddress   :: !Text
  } deriving Show

makeFieldLabelsWith noPrefixFieldLabels ''MailContext
