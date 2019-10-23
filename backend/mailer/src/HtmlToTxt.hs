{-# LANGUAGE RecordWildCards #-}
module HtmlToTxt (
    htmlToTxt
  ) where

import Control.Arrow
import Data.Char
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Entity

unescapeHTML :: String -> String
unescapeHTML [] = []
unescapeHTML ('&' : xs) =
  let (b, a) = break (== ';') xs
  in  case lookupEntity b of
        Just c | listToMaybe a == Just ';' -> c ++ unescapeHTML (tail a)
        _ -> '&' : unescapeHTML xs
unescapeHTML (x : xs) = x : unescapeHTML xs

-- | Convert e-mail from html to txt
htmlToTxt :: String -> String
htmlToTxt =
  dropWhile isSpace
    . unescapeHTML
    . removeTripleNL
    . toText
    . removeWSAfterNL
    . reduceWS
    . replaceLinks
    . concatTexts
    . filterUnneeded
    . lowerTags
    . parseTags
  where
    toText = concat . foldr (\t ts -> f t : ts) []
      where
        f (TagText s  ) = s
        f (TagOpen t _) = fromTag t
        f (TagClose t ) = fromTag t
        f _             = ""

        fromTag t = case t of
          "b"      -> "*"
          "br"     -> "\r\n"
          "i"      -> "_"
          "p"      -> "\r\n"
          "tr"     -> "\r\n"
          "strong" -> "*"
          _        -> ""

    replaceLinks [] = []
    replaceLinks (t@(TagOpen "a" attrs) : ts) = case "href" `lookup` attrs of
      Just link ->
        TagText "[ " : linktext ++ [TagText (": " ++ link ++ " ]")] ++ replaceLinks
          (drop 1 rest)
        where (linktext, rest) = break (== (TagClose "a")) ts
      Nothing -> t : replaceLinks ts
    replaceLinks (t : ts) = t : replaceLinks ts

    removeTripleNL [] = []
    removeTripleNL ('\r' : '\n' : '\r' : '\n' : '\r' : '\n' : l) =
      removeTripleNL ('\r' : '\n' : '\r' : '\n' : l)
    removeTripleNL (a : l) = a : removeTripleNL l

    removeWSAfterNL = foldr f []
      where
        f t ts@((TagText s) : ts') = case t of
          TagOpen el _ -> g el
          TagClose el  -> g el
          _            -> t : ts
          where
            g el = if el `elem` ["p", "br", "tr"]
              then t : TagText (dropWhile isSpace s) : ts'
              else t : ts
        f t ts = t : ts

    reduceWS = map f
      where
        f (TagText s) = TagText $ omitWS s
        f t           = t

        omitWS [] = []
        omitWS (x : xs) =
          if isSpace x then ' ' : omitWS (dropWhile isSpace xs) else x : omitWS xs

    concatTexts = foldr f []
      where
        f (TagText s) ((TagText s') : ts) = TagText (s ++ s') : ts
        f t           ts                  = t : ts

    filterUnneeded = filter f
      where
        f (TagText s  ) = not $ all isSpace s
        f (TagOpen t _) = t `elem` ["a", "b", "br", "i", "p", "strong", "tr"]
        f (TagClose t ) = t `elem` ["a", "b", "i", "p", "strong", "tr"]
        f _             = False

    lowerTags = map f
      where
        f (TagOpen t attrs) = TagOpen (lowerCase t) $ map (first lowerCase) attrs
        f (TagClose t     ) = TagClose (lowerCase t)
        f t                 = t

    lowerCase = map toLower
