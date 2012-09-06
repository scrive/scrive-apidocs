module User.Lang (
    Lang(..)
  , codeFromLang
  , langFromCode
  , timeLocaleForLang
  ) where

import Data.List
import DB.Derive
import MinutesTime
import Utils.Enum

data Lang = LANG_SE --according to IANA this really should be LANG_SV - em
          | LANG_EN
  deriving (Bounded, Enum, Show, Read, Ord, Eq)
$(enumDeriveConvertible ''Lang)

codeFromLang :: Lang -> String
codeFromLang LANG_SE = "sv"
codeFromLang LANG_EN = "en"

langFromCode :: String -> Maybe Lang
langFromCode s = find ((== s) . codeFromLang) allValues

timeLocaleForLang :: Lang -> KontraTimeLocale
timeLocaleForLang LANG_SE = SwedishTimeLocale
timeLocaleForLang LANG_EN = BritishTimeLocale
