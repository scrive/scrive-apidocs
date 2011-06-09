{-# OPTIONS_GHC -Wall -fno-warn-orphans -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror #-}
module User.Lang
    ( Lang (..)
    , langDir

) where

import Data.Data
import Happstack.Data



data Lang =    LANG_SE 
            |  LANG_EN 
    deriving (Bounded, Enum, Show, Read, Ord, Eq, Typeable)

instance Version Lang 



langDir :: Lang -> String
langDir LANG_EN = "texts/en"
langDir LANG_SE = "texts/se"


$(deriveSerializeFor [ ''Lang  ])


