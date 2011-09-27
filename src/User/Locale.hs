module User.Locale (
    Locale
  , mkLocale
  , mkLocaleFromRegion
  , getRegion
  , getLang
  , HasLocale (..)
  , targetedLocales
  ) where

import Misc
import User.Lang
import User.Region

data Locale = Locale Region Lang
  deriving (Bounded, Show, Read, Ord, Eq)

mkLocale :: Region -> Lang -> Locale
mkLocale region _locale = mkLocaleFromRegion region --implemented like this to restrict the regions and langs allowed

mkLocaleFromRegion :: Region -> Locale
mkLocaleFromRegion region = Locale region (defaultRegionLang region)

class HasLocale a where
  getLocale :: a -> Locale

instance HasLocale Locale where
  getLocale = id

getRegion :: HasLocale a => a -> Region
getRegion x =
  let Locale region _lang = getLocale x in
  region            

getLang :: HasLocale a => a -> Lang
getLang x =
  let Locale _region lang = getLocale x in
  lang

targetedLocales :: [Locale]
targetedLocales = map mkLocaleFromRegion allValues
