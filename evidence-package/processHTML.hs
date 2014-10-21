{-# LANGUAGE OverloadedStrings #-}
import Data.Char (ord)
import Data.List
import Data.Text (pack)
import Network.Mime (defaultMimeLookup)
import System.Environment
import System.FilePath.Posix
import System.IO
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Entity
import Text.HTML.TagSoup.Match
import Text.StringLike (toString, fromString)
import qualified Data.ByteString.Base64 as Base64 (encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap as IntMap

customRenderOptions = renderOptions { optRawTag = (\a -> toString a == "style")
                                    } --, optEscape = fromString . escapeHTML' . toString }
-- FIXME this is only because things like ä were not being escaped but now it
-- also escapes '.' and ','. So I need a sensible list :/
--escapeHTML' :: String -> String
--escapeHTML' = concatMap $ \x -> IntMap.findWithDefault [x] (ord x) mp
--    where mp = IntMap.fromList [(ord b, "&"++a++";") | (a,[b]) <- htmlEntities]

main :: IO ()
main = do
    args <- getArgs
    -- FIXME Proper args parsing and usage
    let filep = head args
        cssp  = head $ drop 1 args
    file <- readFile filep
    -- Process the tags with our magical tag soup processor!
    let processedtags = processor $ canonicalizeTags $ parseTags file
    -- Embed images in base64
    embedded <- embedImagesAsBase64 filep processedtags
    -- Embed provided style file
    styled   <- embedStyleSheet cssp embedded
    -- TODO Update last updated timestamp? or let Bash do this well?
    -- We're done!
    putStr $ renderTagsOptions customRenderOptions styled

processor :: [Tag String] -> [Tag String]
processor = foldr (.) id $ reverse
    -- Add a `!DOCTYPE` tag at the start if it is missing for W3C happiness
    -- `canonicalizeTags`, amongst other things, makes DOCTYPE ALL CAPS, so we
    -- can match it :)
    -- FIXME use prefix matching instead of this
    [ (\ts -> if tagOpenNameLit "!DOCTYPE" (head $ canonicalizeTags ts)
              then ts
              else (TagOpen "!DOCTYPE html" []) : ts)
    -- Make sure all <img> tags have an "alt" attribute for W3C happy!
    , map (\t -> if tagOpenNameLit "img" t
                 then let (TagOpen s attrs) = t
                      in  if anyAttrNameLit "alt" attrs
                          then t
                          else (TagOpen s (("alt"," ") : attrs))
                 else t
          )
    -- Remove all inline styles
    , map (removeAttribute "style")
    -- Remove all `class` and `name` attributes, we don't use them
    , map (removeAttribute "class")
    -- W3C says "name" is deprecated, "id" works fine
    , map (changeAttribute "name" "id")
    -- Google double wraps stuff with <span> tags, get rid of all of them!
    , filter (\t -> not $ (tagOpenNameLit "span" t) || (tagCloseLit "span" t))
    -- Google adds annoying `cellspacing` and `cellpadding`, bye bye!
    , map (removeAttribute "cellspacing")
    , map (removeAttribute "cellpadding")
    -- TODO fix Google URLs: prefix, params & slash and colon
    -- Remove <div>s containing comments
    , removeCommentDivs
    -- Remove <sup> references to comments
    -- i.e. Remove everything between <a>..</a> where the "href" attribute
    -- starts with "#cmnt", empty <sup> tags will be cleaned later.
    , dropBetween (tagOpen
                    ((==) "a")
                    (anyAttr (\(a,v) -> a == "href" && isPrefixOf "#cmnt" v))
                  )
                  (tagCloseLit "a")
    -- Remove all empty tags, our actions create a few...
    -- except a few tags which we want to keep otherwise it messes things up!
    , removeEmptyTagsWhere (not . tagOpen (\s -> s == "meta"
                                              || s == "td"
                                              || s == "tr"
                                          -- Empty <a> used as internal anchors
                                              || s == "a"
                                          ) (const True))
    ]

removeCommentDivs :: [Tag String] -> [Tag String]
removeCommentDivs (div : p : a : ts) |
    tagOpenNameLit "div" div && tagOpenNameLit "p" p
    && tagOpenLit "a" (anyAttr (\(a,v) -> a == "href" && isPrefixOf "#cmnt_ref" v)) a
    = removeCommentDivs $ drop 1 $ dropWhile (not . tagCloseLit "div") ts
removeCommentDivs (t:ts) = t : removeCommentDivs ts
removeCommentDivs [] = []

-- | Given a `FilePath` of where the root for any sources in the tag list lies,
-- we make a reasonable guess at getting the source files for any <img> tags.
-- We try to read the source file, encode it in `base64` and replace the "src"
-- attribute on the <img> tag.
embedImagesAsBase64 :: FilePath -> [Tag String] -> IO [Tag String]
embedImagesAsBase64 fp tags = mapM changeImgSrcAttrs tags
  where changeImgSrcAttrs (TagOpen t@"img" attrs) = do
            newAttrs <- (mapM changeSrcAttr attrs)
            return $ TagOpen t newAttrs
        changeImgSrcAttrs t = return t
        changeSrcAttr (a@"src", src) = do
            src' <- embed src
            return  (a, src')
        changeSrcAttr a = return a
        embed imgFile = do
            base64 <- fromPath (dropFileName fp ++ [pathSeparator] ++ imgFile)
            return $ B.unpack base64

-- | Remove everything in between existing <style>...</style> (assuming there
-- is only one) and replaces it with whatever is given in `FilePath`, with
-- newlines removed
embedStyleSheet :: FilePath -> [Tag String] -> IO [Tag String]
embedStyleSheet fp tags = do
    css <- readFile fp
    let css' = filter (\c -> not $ c == '\n' || c == '\r') css
    let newTags = replaceBetweenOnce (tagOpen (== "style") (const True))
                                 (tagClose (== "style"))
                                 (TagText css')
                                 tags
    return newTags

-------------------------------------------------------------------------------
-- Some useful utilities
-------------------------------------------------------------------------------

-- | Remove all "empty" tags that match the predicate; an empty tag is a
-- `TagOpen` followed by a `TagClose` having identical name values.
-- Does this recursively until the length of the list doesn't change.
removeEmptyTagsWhere :: (Tag String -> Bool) -> [Tag String] -> [Tag String]
removeEmptyTagsWhere f ts = if length ts == length removed
                               then removed
                               else removeEmptyTagsWhere f removed
    where removed = removeWhere f ts
          removeWhere f (a:b:ts) = if f a && (tagOpen (\s -> tagCloseLit s b || tagText ((==) "") b) (const True)) a
                                      then removeWhere f ts
                                      else a : removeWhere f (b:ts)
          removeWhere _ ts = ts

--closeTagImmediately :: String -> [Tag String] -> [Tag String]
--closeTagImmediately _ [] = []
--closeTagImmediately s (t:t1:ts) | tagOpenNameLit s t =
--    if tagCloseNameLit s t1
--    then t : t1 : closeTagImmediately s ts
--    else t : (TagClose s) : t1 : closeTagImmediately s ts
--closeTagImmediately s (t:[]) | tagOpenNameLit s t = t : (TagClose s) : []
--closeTagImmediately s (t:ts) | otherwise = t : closeTagImmediately s ts

removeAttribute :: String -> Tag String -> Tag String
removeAttribute s (TagOpen t attrs) = TagOpen t (filter ((/=) s . fst) attrs)
removeAttribute _ t = t

changeAttribute :: String -> String -> Tag String -> Tag String
changeAttribute from to (TagOpen t attrs) | anyAttrName ((==) from) attrs =
    TagOpen t (map (\(a,v) -> if a == from then (to,v) else (a,v)) attrs)
changeAttribute _ _ t = t

-- | Recursively drops everything in between the two predicates, including the
-- matching items.
dropBetween :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
dropBetween _ _ [] = []
dropBetween test1 test2 (x:xs) | test1 x = dropBetween test1 test2 $ drop 1 $ dropWhile (not . test2) xs
dropBetween test1 test2 (x:xs) | otherwise = x : dropBetween test1 test2 xs

-- | Inserts the given item after the first item matching the first predicate,
-- removing everything till the second predicate.
replaceBetweenOnce :: (a -> Bool) -> (a -> Bool) -> a -> [a] -> [a]
replaceBetweenOnce test1 test2 _ [] = []
replaceBetweenOnce test1 test2 i (x:xs) | test1 x = x : i : dropWhile (not . test2) xs
replaceBetweenOnce test1 test2 i (x:xs) | otherwise = x : replaceBetweenOnce test1 test2 i xs

-- Adapted from
-- https://github.com/cgag/data-uri
type DataURI = B.ByteString
-- | Convert a file into a Data URI.
-- Uses `defaultMimeLookup`.
-- Does not try to catch any IO errors.
fromPath :: FilePath -> IO DataURI
fromPath path = do
  let mime = defaultMimeLookup $ pack path
  file <- B.readFile path
  let encoded = Base64.encode file
  return $ "data:" `B.append` mime `B.append` ";base64," `B.append` encoded
