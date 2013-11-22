module Utils.Color (ensureHexRGB) where

import Numeric
import Text.Regex.TDFA

-- converts strings like "rgb(1,2,3)" to "#aabbcc" notation
-- returns Nothing for colors in other notation (or other garbage input)
ensureHexRGB :: String -> Maybe String
ensureHexRGB c =
    case getAllTextSubmatches $ c =~ rgbNotationRegex of
      [c', r, g, b] | c == c' -> Just $ "#" ++ dec2HEX r ++ dec2HEX g ++ dec2HEX b
      _ -> Nothing
  where rgbNotationRegex = "rgb\\(([0-9]+),([0-9]+),([0-9]+)\\)" :: String
        dec2HEX s = pad0 $ showHex (read s :: Int) ""
        pad0 [x] = ['0', x]
        pad0 s = s
