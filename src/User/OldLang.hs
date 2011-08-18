module User.OldLang
    ( Lang (..)
    , langDir
    , langFromHTTPHeader

) where

import Data.Data
import Happstack.Data hiding (defaultValue)
import Misc
import Data.Maybe
import Data.Foldable hiding (find)
import Data.List
import DB.Derive

data Lang =    LANG_SE
            |  LANG_EN
    deriving (Bounded, Enum, Show, Read, Ord, Eq, Typeable)
$(enumDeriveConvertible ''Lang)

instance Version Lang



langDir :: Lang -> String
langDir LANG_EN = "texts/en"
langDir LANG_SE = "texts/se"

langHTTPValue :: Lang -> String
langHTTPValue LANG_SE = "se"
langHTTPValue LANG_EN = "en"

langFromHTTPHeader :: String -> Lang
langFromHTTPHeader s = fromMaybe defaultValue $ msum $ map langCode (splitOver "," s)
    where
        langCode str = find ((`isInfixOf` str) . langHTTPValue) allValues
$(deriveSerializeFor [ ''Lang  ])


