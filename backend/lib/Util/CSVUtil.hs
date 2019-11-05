module Util.CSVUtil ( parseCSV
                    , CSV(..)
                    , renderCSV
                    ) where

import Control.Monad.Exception.Asynchronous (Exceptional(..))
import Data.ByteString.Lazy (toStrict)
import Data.Char
import Data.Ord
import Data.Spreadsheet as SS
import Happstack.Server (ToMessage(..), setHeader, setHeaderBS)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL8
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

{- |
    Parses a csv file's contents.  It tries to guess the delimiters.
-}
parseCSV :: BSL8.ByteString -> Either String [[String]]
parseCSV csvcontents =
  let parseresult = splitCSVContents $ BS.toString $ toStrict csvcontents
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
        else identity
      )
      $ setHeaderBS (BS.fromString "Content-Type") (toContentType csv)
      $ toResponse (toMessage csv)
