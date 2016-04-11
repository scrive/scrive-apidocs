{-# LANGUAGE ExtendedDefaultRules #-}
module Branding.Cache (
     LessCache
   , LessCacheKey(..)
  ) where

import Data.Hashable
import GHC.Generics
import qualified Data.ByteString.Lazy.UTF8 as BSL

import BrandedDomain.BrandedDomainID
import KontraPrelude
import MemCache
import Theme.ThemeID

type LessCache = MemCache LessCacheKey BSL.ByteString

data LessCacheKey =
    DomainBranding BrandedDomainID String
  | SignviewBranding ThemeID String
  | ServiceBranding ThemeID String
  | LoginBranding ThemeID String
  | ScriveBranding String
  deriving (Eq, Ord, Generic)

instance Hashable LessCacheKey
