module User.Lang (
    Lang(..)
  , HasLang(..)
  , codeFromLang
  , langFromCode
  , timeLocaleForLang
  , langFromHTTPHeader
  , allLangs
  ) where

import Data.List
import Data.Maybe
import Control.Monad
import DB.Derive
import MinutesTime
import Utils.Enum
import Utils.List
import Utils.Read
import Utils.Default

import Happstack.Server

data Lang = LANG_SV
          | LANG_EN
  deriving (Bounded, Enum, Show, Read, Ord, Eq)

instance HasDefaultValue Lang where
  defaultValue = LANG_SV

$(enumDeriveConvertible ''Lang)

instance FromReqURI Lang where
  fromReqURI = maybeRead

codeFromLang :: Lang -> String
codeFromLang LANG_SV = "sv"
codeFromLang LANG_EN = "en"

langFromCode :: String -> Maybe Lang
langFromCode s = find ((== s) . codeFromLang) allValues

timeLocaleForLang :: Lang -> KontraTimeLocale
timeLocaleForLang LANG_SV = SwedishTimeLocale
timeLocaleForLang LANG_EN = BritishTimeLocale

langFromHTTPHeader :: String -> Lang
langFromHTTPHeader s = fromMaybe LANG_EN $ msum $ map findLang (splitOver "," s)
  where
    findLang str = find ((`isInfixOf` str) . codeFromLang) allValues

allLangs :: [Lang]
allLangs = allValues

class HasLang a where
  getLang :: a -> Lang

instance HasLang Lang where
  getLang x = x
