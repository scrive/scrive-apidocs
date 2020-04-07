
module User.Lang (
    Lang(..)
  , HasLang(..)
  , codeFromLang
  , defaultLang
  , langFromCode
  , langFromHTTPHeader
  , allLangs
  ) where

import Data.Int
import Database.PostgreSQL.PQTypes
import Happstack.Server
import qualified Control.Exception.Lifted as E
import qualified Data.Text as T

import Utils.Enum

data Lang = LANG_SV
          | LANG_EN
          | LANG_DE
          | LANG_FR
          | LANG_IT
          | LANG_ES
          | LANG_PT
          | LANG_NL
          | LANG_DA
          | LANG_NO
          | LANG_GR
          | LANG_FI
          | LANG_IS
          | LANG_ET
          | LANG_LV
          | LANG_LT
          | LANG_CS
          | LANG_PL
          | LANG_HU
  deriving (Bounded, Enum, Show, Read, Ord, Eq)

defaultLang :: Lang
defaultLang = LANG_SV

instance FromReqURI Lang where
  fromReqURI = maybeRead . T.pack

instance PQFormat Lang where
  pqFormat = pqFormat @Int16

instance FromSQL Lang where
  type PQBase Lang = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1  -> return LANG_SV
      2  -> return LANG_EN
      3  -> return LANG_DE
      4  -> return LANG_FR
      5  -> return LANG_IT
      6  -> return LANG_ES
      7  -> return LANG_PT
      8  -> return LANG_NL
      9  -> return LANG_DA
      10 -> return LANG_NO
      11 -> return LANG_GR
      12 -> return LANG_FI
      13 -> return LANG_IS
      14 -> return LANG_ET
      15 -> return LANG_LV
      16 -> return LANG_LT
      17 -> return LANG_CS
      18 -> return LANG_PL
      19 -> return LANG_HU
      _  -> E.throwIO $ RangeError { reRange = [(1, 19)], reValue = n }

instance ToSQL Lang where
  type PQDest Lang = PQDest Int16
  toSQL LANG_SV = toSQL (1 :: Int16)
  toSQL LANG_EN = toSQL (2 :: Int16)
  toSQL LANG_DE = toSQL (3 :: Int16)
  toSQL LANG_FR = toSQL (4 :: Int16)
  toSQL LANG_IT = toSQL (5 :: Int16)
  toSQL LANG_ES = toSQL (6 :: Int16)
  toSQL LANG_PT = toSQL (7 :: Int16)
  toSQL LANG_NL = toSQL (8 :: Int16)
  toSQL LANG_DA = toSQL (9 :: Int16)
  toSQL LANG_NO = toSQL (10 :: Int16)
  toSQL LANG_GR = toSQL (11 :: Int16)
  toSQL LANG_FI = toSQL (12 :: Int16)
  toSQL LANG_IS = toSQL (13 :: Int16)
  toSQL LANG_ET = toSQL (14 :: Int16)
  toSQL LANG_LV = toSQL (15 :: Int16)
  toSQL LANG_LT = toSQL (16 :: Int16)
  toSQL LANG_CS = toSQL (17 :: Int16)
  toSQL LANG_PL = toSQL (18 :: Int16)
  toSQL LANG_HU = toSQL (19 :: Int16)


codeFromLang :: Lang -> Text
codeFromLang LANG_SV = "sv"
codeFromLang LANG_EN = "en"
codeFromLang LANG_DE = "de"
codeFromLang LANG_FR = "fr"
codeFromLang LANG_IT = "it"
codeFromLang LANG_ES = "es"
codeFromLang LANG_PT = "pt"
codeFromLang LANG_NL = "nl"
codeFromLang LANG_DA = "da"
codeFromLang LANG_NO = "no"
codeFromLang LANG_GR = "el"
codeFromLang LANG_FI = "fi"
codeFromLang LANG_IS = "is"
codeFromLang LANG_ET = "et"
codeFromLang LANG_LV = "lv"
codeFromLang LANG_LT = "lt"
codeFromLang LANG_CS = "cs"
codeFromLang LANG_PL = "pl"
codeFromLang LANG_HU = "hu"

langFromCode :: Text -> Maybe Lang
langFromCode s = find ((== s) . codeFromLang) allValues

langFromHTTPHeader :: Text -> Lang
langFromHTTPHeader s = fromMaybe LANG_EN . msum $ map findLang (T.splitOn "," s)
  where findLang str = find ((`T.isInfixOf` str) . codeFromLang) allValues

allLangs :: [Lang]
allLangs = allValues

class HasLang a where
  getLang :: a -> Lang

instance HasLang Lang where
  getLang x = x
