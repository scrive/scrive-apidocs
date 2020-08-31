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
    let filep = head args
        cssp  = head $ drop 1 args
    file <- readFile filep
    -- Process the tags with our magical tag soup processor!
    let processedtags = processor $ canonicalizeTags $ parseTags file
    -- Embed images in base64
    embedded <- embedImagesAsBase64 filep processedtags
    -- Embed provided style file
    styled   <- embedStyleSheet cssp embedded
    -- We're done!
    putStr $ renderTagsOptions customRenderOptions styled

processor :: [Tag String] -> [Tag String]
processor = foldr (.) id $ reverse
    -- Add a `!DOCTYPE` tag at the start if it is missing for W3C happiness
    -- `canonicalizeTags`, amongst other things, makes DOCTYPE ALL CAPS, so we
    -- can match it :)
    [ (\ts -> if tagOpenNameLit "!DOCTYPE" (head $ canonicalizeTags ts)
              then ts
              else (TagOpen "!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\"" []) : ts)
    -- Make sure all <img> tags have an "alt" attribute for W3C happy!
    , map (\t -> if tagOpenNameLit "img" t
                 then let (TagOpen s attrs) = t
                      in  if anyAttrNameLit "alt" attrs
                          then t
                          else (TagOpen s (("alt"," ") : attrs))
                 else t
          )
    -- Remove empty attributes as TagSoup doesn't render them properly
    , map removeEmptyAttributes
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
    -- Add proper class names to ToC <a>'s so we can indent them
    , addToCClassNames
    -- Makes first row of all tables into table header
    , makeFirstTableRowIntoHeader
    -- Do some magic stuff with <td> which only contain "N/A" text
    , addNATableClass
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
    -- Close "meta" and "img" tags for xml parser to be happy in kontrakcja-tests
    , closeTagImmediately "meta"
    , closeTagImmediately "img"
    -- Remove all empty tags, our actions create a few...
    -- except a few tags which we want to keep otherwise it messes things up!
    , removeEmptyTagsWhere (not . tagOpen (\s -> s == "meta"
                                              || s == "img"
                                              || s == "td"
                                              || s == "th"
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

-- | Add class to table rows with "N/A" in text, remove the text.
-- Converts things of the form:
--     <td><p>N/A</p></td>
-- to:
--     <td class="cell-na"><p></p></td>
addNATableClass :: [Tag String] -> [Tag String]
addNATableClass [] = []
addNATableClass (td : p : na : p' : xs)
    |  tagOpenNameLit "td" td
    && tagOpenNameLit "p"  p
    && tagText ((==) "N/A") na
    && tagClose ((==) "p") p'
    = (addClass "cell-na" td : na : addNATableClass xs)
addNATableClass (x:xs) = x : addNATableClass xs

-- | Change all <td> within the first <tr> after a <tbody> to <th>
makeFirstTableRowIntoHeader :: [Tag String] -> [Tag String]
makeFirstTableRowIntoHeader [] = []
makeFirstTableRowIntoHeader (tbody : tr : ts)
    |  tagOpenNameLit "tbody" tbody
    && tagOpenNameLit "tr" tr
    = tbody : tr : (map changeTDtoTH notTRclose) ++ (makeFirstTableRowIntoHeader rest)
  where
    (notTRclose, rest) = span (\t -> not $ tagCloseNameLit "tr" t) ts
makeFirstTableRowIntoHeader (x:xs) = x : makeFirstTableRowIntoHeader xs

changeTDtoTH :: Tag String -> Tag String
changeTDtoTH (TagOpen "td" attrs) = TagOpen "th" attrs
changeTDtoTH (TagClose "td") = TagClose "th"
changeTDtoTH t = t

-- | Add a 'toc-x' class to the <p> in something of the form:
--     <p><a href="#h.foo>2.1 FooBar</a></p>
-- where `x` is based on the numbering to best guess the level.
addToCClassNames :: [Tag String] -> [Tag String]
addToCClassNames (p : a : txt : xs) =
  if tagOpenLit "p" (const True) p
  && tagOpenLit "a" (anyAttr (\(a,v) -> a == "href" && isPrefixOf "#h." v)) a
  && isTagText txt
  then let num = head $ words $ fromTagText txt
           dots = length . filter ((==) '.')
           tocLevel s = if dots s == 1
                           then if length (dropWhile ((/=) '.') s) == 1
                                   then "1"
                                   else "2"
                           else show $ dots s + 1
           tocClass = "toc-" ++ tocLevel num
       in (addClass tocClass p : a : txt : addToCClassNames xs)
  else p : addToCClassNames (a : txt : xs)
addToCClassNames x = x

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

-- | Add the given string to the value of a "class" attribute on any `TagOpen`,
-- return any other tags unchanged.
-- Adds a "class" attribute if it doesn't already exist.
addClass :: String -> Tag String -> Tag String
addClass c (TagOpen t attrs) | any isClass attrs = TagOpen t attrs'
  where isClass = (==) "class" . fst
        Just (cA,cV) = find isClass attrs
        classAttr = (cA, cV ++ " " ++ c)
        attrs' = classAttr : filter (not . isClass) attrs
addClass c (TagOpen t attrs) | otherwise = TagOpen t (insert ("class",c) attrs)
addClass _ t = t

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

closeTagImmediately :: String -> [Tag String] -> [Tag String]
closeTagImmediately _ [] = []
closeTagImmediately s (t:t1:ts) | tagOpenNameLit s t =
    if tagCloseNameLit s t1
    then t : t1 : closeTagImmediately s ts
    else t : (TagClose s) : t1 : closeTagImmediately s ts
closeTagImmediately s (t:[]) | tagOpenNameLit s t = t : (TagClose s) : []
closeTagImmediately s (t:ts) | otherwise = t : closeTagImmediately s ts

removeAttribute :: String -> Tag String -> Tag String
removeAttribute s (TagOpen t attrs) = TagOpen t (filter ((/=) s . fst) attrs)
removeAttribute _ t = t

changeAttribute :: String -> String -> Tag String -> Tag String
changeAttribute from to (TagOpen t attrs) | anyAttrName ((==) from) attrs =
    TagOpen t (map (\(a,v) -> if a == from then (to,v) else (a,v)) attrs)
changeAttribute _ _ t = t

removeEmptyAttributes :: Tag String -> Tag String
removeEmptyAttributes (TagOpen t attrs) = TagOpen t (filter (\(a,v) -> not . null $ v) attrs)
removeEmptyAttributes x = x

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
