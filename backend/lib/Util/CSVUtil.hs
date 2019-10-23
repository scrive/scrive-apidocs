module Util.CSVUtil ( parseCSV
                    , CSV(..)
                    , renderCSV
                    ) where

import Codec.Text.IConv
import Control.Monad.Exception.Asynchronous (Exceptional(..))
import Data.ByteString.Lazy (toStrict)
import Data.Char
import Data.Either
import Data.Ord
import Data.Spreadsheet as SS
import Happstack.Server (ToMessage(..), setHeader, setHeaderBS)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL8
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

{- |
    Parses a csv file's contents.  It tries to guess the char encoding and the delimiters.
-}
parseCSV :: BSL8.ByteString -> Either String [[String]]
parseCSV csvcontents =
  let parseresult = splitCSVContents $ decodeByteString csvcontents
  in  case (exception parseresult, result parseresult) of
        (Just msg, _res) -> Left $ "Parse error : " ++ (show msg)
        (Nothing, res) ->
          Right $ fixSize $ map dropTrailingEmptyCells $ filter (not . isEmptyRow) res
  where
    dropTrailingEmptyCells = reverse . dropWhile isEmptyCell . reverse
    isEmptyRow             = all isEmptyCell
    isEmptyCell            = null . dropWhile isSpace . reverse . dropWhile isSpace
    fixSize s = fixSize' (maximum $ map length s) s
    fixSize' l (s : ss) = (s ++ (replicate (l - length s) "")) : fixSize' l ss
    fixSize' _ _        = []
{- |
    This splits up the csv contents, it makes an effort to guess separators
-}
splitCSVContents :: String -> Exceptional String [[String]]
splitCSVContents x = SS.fromString guessedQuoteMark guessedSeparator (x ++ "\n")
  where
    guessedQuoteMark = '"'
    guessedSeparator = maximumBy (comparing charOccurenceCount) alternativeSeparators
    charOccurenceCount c = length $ filter (== c) x
    alternativeSeparators = [',', '~', ':', ';', '|', '#']

{- |
    Excel especially will chuck out data in funky char encodings
    so we're going to look to see if some alternative ones "work better"
    than UTF-8.  Otherwise we'll use UTF-8.  The problem is determining
    which "works better" because they will normally all decode without an error,
    it's just it'll be a load of rubbish for a human.
-}
decodeByteString :: BSL8.ByteString -> String
decodeByteString bs =
  guessBest
    .  map (BS.toString . toStrict)
    .  lefts
    $  map (\enc -> convertStrictly enc "UTF-8" bs) alternativeEncodings
    ++ [Left bs]
  where
    {- |
        I picked these because these seem to be what Excel 2007 is outputting on my Windows machine if you choose to Save As ...
        CSV (Comma delimited) -> ISO8859-1
        CSV (MS-DOS) -> CP437
        CSV (Macintosh) -> MAC
        The MAC encoding seemed to cover the files Viktor sent me from his Mac too.
    -}
    alternativeEncodings = ["ISO8859-1", "CP437", "MAC"]
    {- |
        Guesses the best string by looking at it, there's not much else you can do really.
        This goes for the one with the most nordic chars in.  This also goes for things
        earlier in the list over those later in the list, because of the way maximumBy works.
    -}
    guessBest :: [String] -> String
    guessBest = maximumBy nordicCharCountOrdering
    nordicCharCountOrdering :: String -> String -> Ordering
    nordicCharCountOrdering a b = compare (nordicCharCount a) (nordicCharCount b)
    nordicCharCount = length . filter (\c -> c `elem` ("äÄöÖåÅ" :: String))


-- | Render a BSL8.ByteString representation of CSV. Uses \',\' as
-- spearator and \'\"\' as quote.
renderCSV :: [[String]] -> BSL.ByteString
renderCSV content =
  bom
    `BSL.append` (BSL.fromStrict $ Text.encodeUtf16LE $ Text.pack $ SS.toString '"'
                                                                                ','
                                                                                content
                 )
  where bom = BSL.pack [255, 254]


data CSV = CSV
         { csvHeader   :: [String]
         , csvContent  :: [[String]]
         , csvFilename :: String
         }

instance ToMessage CSV where
  toMessage csv = renderCSV (csvHeader csv : csvContent csv)
  toContentType _ = BS.fromString "text/csv; charset=UTF-16"
  toResponse csv =
    (if not (null (csvFilename csv))
        then setHeader "Content-Disposition" ("attachment;filename=" ++ csvFilename csv)
        else id
      )
      $ setHeaderBS (BS.fromString "Content-Type") (toContentType csv)
      $ toResponse (toMessage csv)
