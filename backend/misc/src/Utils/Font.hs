module Utils.Font (isValidFont) where


isValidFont :: String -> Bool
isValidFont font = case font of
  "\"arial black\",sans-serif" -> True
  "\"arial narrow\",sans-serif" -> True
  "\"comic sans ms\",sans-serif" -> True
  "\"courier new\",monospace" -> True
  "\"Source Sans Pro\", \"Helvetica Neue\", Arial, sans-serif" -> True
  "garamond,serif"     -> True
  "georgia,serif"      -> True
  "\"times new roman\",serif" -> True
  "tahoma,sans-serif"  -> True
  "\"trebuchet ms\",sans-serif" -> True
  "verdana,sans-serif" -> True
  "arial,helvetica,sans-serif" -> True
  "helvetica,sans-serif" -> True
  _                    -> False

