module User.Lang (
    Lang(..)
  , HasLang(..)
  , codeFromLang
  , langFromCode
  , langFromHTTPHeader
  , allLangs
  ) where

import Data.Int
import Data.List
import Data.Maybe
import Control.Monad
import Utils.Enum
import Utils.List
import Utils.Read
import Utils.Default
import qualified Control.Exception as E

import Database.PostgreSQL.PQTypes
import Happstack.Server

data Lang = LANG_SV
          | LANG_EN
  deriving (Bounded, Enum, Show, Read, Ord, Eq)

instance HasDefaultValue Lang where
  defaultValue = LANG_SV

instance FromReqURI Lang where
  fromReqURI = maybeRead

instance PQFormat Lang where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL Lang where
  type PQBase Lang = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return LANG_SV
      2 -> return LANG_EN
      _ -> E.throwIO $ RangeError {
        reRange = [(1, 2)]
      , reValue = n
      }

instance ToSQL Lang where
  type PQDest Lang = PQDest Int16
  toSQL LANG_SV = toSQL (1::Int16)
  toSQL LANG_EN = toSQL (2::Int16)

codeFromLang :: Lang -> String
codeFromLang LANG_SV = "sv"
codeFromLang LANG_EN = "en"

langFromCode :: String -> Maybe Lang
langFromCode s = find ((== s) . codeFromLang) allValues

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
