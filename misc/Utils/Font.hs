module Utils.Font (isValidFont) where

import Data.String.Utils
import Text.Regex.TDFA

-- Our font's don't have " in their definition
isValidFont :: String -> Bool
isValidFont c =
      c' /= (""::String)
   && (not $  '\n' `elem` c)
   && (length c' < 50)
   && (length c' > 2)
   && c' =~ ("([A-z|0-9| |-]+,)*([A-z|0-9| |-]+)"::String) == c'
  where
    c' = strip c
