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

import Happstack.Server

data Lang = LANG_SE --according to IANA this really should be LANG_SV - em
          | LANG_EN
  deriving (Bounded, Enum, Show, Read, Ord, Eq)
$(enumDeriveConvertible ''Lang)

instance FromReqURI Lang where
  fromReqURI = maybeRead

codeFromLang :: Lang -> String
codeFromLang LANG_SE = "sv"
codeFromLang LANG_EN = "en"

langFromCode :: String -> Maybe Lang
langFromCode s = find ((== s) . codeFromLang) allValues

timeLocaleForLang :: Lang -> KontraTimeLocale
timeLocaleForLang LANG_SE = SwedishTimeLocale
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
