{-# OPTIONS_GHC -Wall #-}
module Doc.CSVUtils (
      CSVProblem(..)
    , CleanCSVData(..)
    , cleanCSVContents
    )
where

import Control.Applicative
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import Data.Char
import Data.Maybe
import Data.List

import InputValidation
import Kontra
import Templates.Templates 


data CSVProblem = CSVProblem 
                  { problemrowindex :: Maybe Int
                  , problemcolindex :: Maybe Int
                  , problemdescription :: KontrakcjaTemplates -> IO FlashMessage
                  , problemvalue :: Maybe BS.ByteString
                  }

data CleanCSVData = CleanCSVData 
                    { csvheader :: Maybe [BS.ByteString]
                    , csvbody :: [[BS.ByteString]]
                    }

{- |
    Cleans up csv contents. You get a list of all the problems alongside all the data
    that's been scrubbed up as much as possible.
-}
cleanCSVContents :: [[BS.ByteString]] -> ([CSVProblem], CleanCSVData)
cleanCSVContents contents = 
  let rows = filter (\x -> not (null x)) contents
      (mheader, _) = lookForHeader . zipWith cleanRow [0..] $ take 1 rows
      bodyrows = if (isJust mheader) then drop 1 rows else rows
      rowresults = zipWith cleanRow [0..] bodyrows
      rowproblems = concat . map fst $ rowresults
      body = map snd $ rowresults
      problems = case (mheader, rowresults) of
        (Nothing, []) -> mkProblemForAll flashMessageNoDataInCSV : rowproblems
        (Just _, []) -> mkProblemForAll flashMessageNoDataInCSVApartFromHeader : rowproblems
        _ -> rowproblems in
  (problems, CleanCSVData { csvheader = mheader, csvbody = body }) 
  where 
    {- |
        This looks a possible header and separates it out.  The oh so sophisticated rule this uses at the moment
        is that if the first line is invalid use it as a header!
    -}
    lookForHeader :: [([CSVProblem], [BS.ByteString])] -> (Maybe [BS.ByteString], [([CSVProblem], [BS.ByteString])])
    lookForHeader (((p:_), vals):xs) = (Just vals, xs)
    lookForHeader xs = (Nothing, xs) 
    {- |
        This cleans a single row of data.  It checks both the size of the row
        (meaning how many cols it has), and the values of the fields.
    -}
    cleanRow :: Int -> [BS.ByteString] -> ([CSVProblem], [BS.ByteString])
    cleanRow row xs = 
      let fieldresults = zipWith4 cleanField (repeat row) [0..] fieldValidators xs
          fieldproblems = map (fromJust . fst) . filter (isJust . fst) $ fieldresults
          rowsizeproblems = validateRowSize row 3 5 xs
          values = map snd fieldresults in
      (rowsizeproblems ++ fieldproblems, values)
    {- |
        We want the row to have a sensible number of columns
    -}
    validateRowSize :: Int -> Int -> Int -> [BS.ByteString] -> [CSVProblem]
    validateRowSize row mincols maxcols xs =
      case (length xs) of
        l | (l<mincols) -> [mkProblemForRow row $ flashMessageRowLessThanMinColCount mincols]
          | (l>maxcols) -> [mkProblemForRow row $ flashMessageRowGreaterThanMaxColCount maxcols]
        _ -> []
    {- |
        This cleans up a single field.  It is given the validator to use (which are mostly things from the
        InputValidation module).
    -}
    cleanField :: Int -> Int -> (String -> Result BS.ByteString) -> BS.ByteString -> (Maybe CSVProblem, BS.ByteString)
    cleanField row col f x =
      let raw = BS.toString x
          failedval = BS.fromString $ minimalScrub raw in
      case (f raw) of
        Empty -> (Nothing, BS.empty)
        Good clean -> (Nothing, clean)
        Bad msg -> (Just $ mkProblemForField row col msg failedval, failedval)
      where
        minimalScrub = filter (\c -> isAlphaNum c || isPunctuation c || isSymbol c || c==' ') . stripWhitespace  
        stripWhitespace = stripLeadingWhitespace . stripTrailingWhitespace
        stripLeadingWhitespace = dropWhile isSpace
        stripTrailingWhitespace = reverse . stripLeadingWhitespace . reverse
    {- |
        All the validators that we're going to use to check the field values.
    -}
    fieldValidators :: [String -> Result BS.ByteString]
    fieldValidators = 
      [ badIfEmpty flashMessageFirstNameIsRequired . asValidName
      , badIfEmpty flashMessageSecondNameIsRequired . asValidName
      , badIfEmpty flashMessageEmailIsRequired . asValidEmail
      , asValidCompanyName
      , asValidCompanyNumber
      ]
      where
        badIfEmpty :: (KontrakcjaTemplates -> IO FlashMessage) -> Result BS.ByteString -> Result BS.ByteString
        badIfEmpty msg res = 
          case res of
            Empty -> Bad msg
            x -> x
    {- |
        Handy for constructing problems.
    -}
    mkProblemForAll msg = CSVProblem 
                      { problemrowindex = Nothing
                      , problemcolindex = Nothing
                      , problemdescription = msg
                      , problemvalue = Nothing
                      }
    mkProblemForRow row msg = CSVProblem
                        { problemrowindex = Just row
                        , problemcolindex = Nothing
                        , problemdescription = msg
                        , problemvalue = Nothing
                        }
    mkProblemForField row col msg raw = CSVProblem
                    { problemrowindex = Just row
                    , problemcolindex = Just col
                    , problemdescription = msg
                    , problemvalue = Just raw
                    }

flashMessageNoDataInCSV :: KontrakcjaTemplates -> IO FlashMessage
flashMessageNoDataInCSV templates = 
  toFlashMsg OperationFailed <$> renderTemplate templates "flashMessageNoDataInCSV" ()

flashMessageNoDataInCSVApartFromHeader :: KontrakcjaTemplates -> IO FlashMessage
flashMessageNoDataInCSVApartFromHeader templates = 
  toFlashMsg OperationFailed <$> renderTemplate templates "flashMessageNoDataInCSVApartFromHeader" ()

flashMessageRowLessThanMinColCount :: Int -> KontrakcjaTemplates -> IO FlashMessage
flashMessageRowLessThanMinColCount mincols templates = 
  toFlashMsg OperationFailed <$> 
    (renderTemplate templates "flashMessageRowLessThanMinColCount" $ field "mincols" mincols)

flashMessageRowGreaterThanMaxColCount :: Int -> KontrakcjaTemplates -> IO FlashMessage
flashMessageRowGreaterThanMaxColCount maxcols templates = 
  toFlashMsg OperationFailed <$> 
    (renderTemplate templates "flashMessageRowGreaterThanMaxColCount" $ field "maxcols" maxcols)

flashMessageFirstNameIsRequired :: KontrakcjaTemplates -> IO FlashMessage
flashMessageFirstNameIsRequired templates = 
  toFlashMsg OperationFailed <$> renderTemplate templates "flashMessageFirstNameIsRequired" ()

flashMessageSecondNameIsRequired :: KontrakcjaTemplates -> IO FlashMessage
flashMessageSecondNameIsRequired templates = 
  toFlashMsg OperationFailed <$> renderTemplate templates "flashMessageSecondNameIsRequired" ()

flashMessageEmailIsRequired :: KontrakcjaTemplates -> IO FlashMessage
flashMessageEmailIsRequired templates = 
  toFlashMsg OperationFailed <$> renderTemplate templates "flashMessageEmailIsRequired" ()
