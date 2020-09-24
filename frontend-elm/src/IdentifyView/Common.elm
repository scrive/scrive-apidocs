module IdentifyView.Common exposing (..)

-- | Mask the trailing characters of a personal number string:
-- `maskedPersonalNumber 4 "841005-3371" = 841005-****`. Empty personal numbers
-- are mapped to "Empty" for some reason.
maskedPersonalNumber : Int -> String -> String
maskedPersonalNumber n personalNumber =
  case personalNumber of
    "" -> "Empty"  -- wth?
    _ -> String.concat <| String.dropRight n personalNumber :: List.repeat n "*"
