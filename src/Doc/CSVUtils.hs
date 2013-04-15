module Doc.CSVUtils (
      CSVProblem(..)
    , CleanCSVData(..)
    , cleanCSVContents
    , getCSVCustomFields
    , csvProblemToDescription
    , problemRow
    , problemCell
    )
where

import Control.Monad
import Data.Char
import Data.Maybe
import Data.List

import Control.Logic
import Doc.DocStateData
import Doc.DocUtils
import InputValidation
import Text.StringTemplates.Templates
import Util.SignatoryLinkUtils
import qualified Text.StringTemplates.Fields as F

data CSVProblem = NumberNotValid       Int Int    | -- Row Cell
                  EmailNotValid        Int Int    | -- Row Cell
                  SecondNameNotValid   Int Int    | -- Row Cell
                  FirstNameNotValid    Int Int    | -- Row Cell
                  ValueNotValid        Int Int    | -- Row Cell
                  RowMoreThenMaxCol    Int Int    | -- Row Limit
                  RowLessThenMinCol    Int Int    | -- Row Limit
                  NoDataExceptHeader              |
                  NoData

data CleanCSVData = CleanCSVData
                    { csvheader :: Maybe [String]
                    , csvbody :: [[String]]
                    }

{- |
    Looks up all the custom fields for the csv upload and returns their labels.
-}
getCSVCustomFields :: SignatoryLink -> Either String [String]
getCSVCustomFields sl =
      case isAuthor sl of
        True -> Left $ "getCSVCustomFields: signatory link #" ++ show (signatorylinkid sl) ++ " cannot be both csvupload and author"
        False ->
          Right $ map sfValue . filter isFieldCustom . signatoryfields . signatorydetails $ sl

{- |
    Cleans up csv contents. You get a list of all the problems alongside all the data
    that's been scrubbed up as much as possible.
-}
cleanCSVContents :: Bool -> Int -> [[String]] -> ([CSVProblem], CleanCSVData)
cleanCSVContents eleg customfieldcount contents =
  let mincols = (if eleg then 5 else 3) :: Int
      maxcols = 6 + customfieldcount
      cleanData = zipWith (cleanRow mincols maxcols) [0..]
      mheader = lookForHeader . cleanData $ take 1 contents
      bodyrows = if isJust mheader then drop 1 contents else contents
      rowresults = cleanData bodyrows
      rowproblems = concatMap fst rowresults
      body = map snd rowresults
      problems = case (mheader, rowresults) of
        (Nothing, []) -> NoData : rowproblems
        (Just _, []) ->  NoDataExceptHeader : rowproblems
        _ -> rowproblems in
  (problems, CleanCSVData { csvheader = mheader, csvbody = body })
  where
    {- |
        This looks a possible header and separates it out.  The oh so sophisticated rule this uses at the moment
        is that if the first line is invalid use it as a header!
    -}
    lookForHeader :: [([CSVProblem], [String])] -> Maybe [String]
    lookForHeader ((_:_, vals):_) = Just vals
    lookForHeader _ = Nothing
    {- |
        This cleans a single row of data.  It checks both the size of the row
        (meaning how many cols it has), and the values of the fields.
    -}
    cleanRow :: Int -> Int -> Int -> [String] -> ([CSVProblem], [String])
    cleanRow mincols maxcols row xs =
      let fieldresults = zipWith4 cleanField (repeat row) [0..] fieldValidators xs
          fieldproblems = map (fromJust . fst) . filter (isJust . fst) $ fieldresults
          rowsizeproblems = validateRowSize row mincols maxcols xs
          values = map snd fieldresults in
      (rowsizeproblems ++ fieldproblems, fitToMaxSize values)
      where fitToMaxSize vals = take maxcols $ vals ++ repeat ""
    {- |
        We want the row to have a sensible number of columns
    -}
    validateRowSize :: Int -> Int -> Int -> [String] -> [CSVProblem]
    validateRowSize row mincols maxcols xs =
      case length xs of
        l | l<mincols -> [RowLessThenMinCol row mincols]
          | l>maxcols -> [RowMoreThenMaxCol row maxcols]
        _ -> []
    {- |
        This cleans up a single field.  It is given the validator to use (which are mostly things from the
        InputValidation module).
    -}
    cleanField :: Int -> Int -> (String -> Either (Int -> Int -> CSVProblem) String) -> String -> (Maybe CSVProblem, String)
    cleanField row col f raw =
      let failedval = minimalScrub raw in
      case f raw of
        Right v -> (Nothing, v)
        Left pd -> (Just $ pd row col, failedval)
      where
        minimalScrub = filter (\c -> isAlphaNum c || isPunctuation c || isSymbol c || c==' ') . stripWhitespace
        stripWhitespace = stripLeadingWhitespace . stripTrailingWhitespace
        stripLeadingWhitespace = dropWhile isSpace
        stripTrailingWhitespace = reverse . stripLeadingWhitespace . reverse
    {- |
        All the validators that we're going to use to check the field values.
    -}
    fieldValidators :: [String -> Either (Int -> Int -> CSVProblem) String]
    fieldValidators = map validate $
      [ (checkIfEmpty >=>  asValidName,  FirstNameNotValid )
      , (checkIfEmpty >=>  asValidName,  SecondNameNotValid )
      , (checkIfEmpty >=>  asValidEmail, EmailNotValid )
      , (                  asValidCompanyName, ValueNotValid)
      , ((checkIfEmpty <| eleg |> return), ValueNotValid)
       ] ++ repeat (asValidFieldValue, ValueNotValid)

    validate :: (String -> Result a, Int -> Int -> CSVProblem) -> String -> Either (Int -> Int -> CSVProblem) String
    validate (v,pd) s =  case v s of
                           Bad -> Left pd
                           _ -> Right s

csvProblemToDescription :: (TemplatesMonad m) => CSVProblem -> m String
csvProblemToDescription p = case p of
   (NumberNotValid _ _ )     ->  renderTemplate_ "flashMessageValueNotValid"
   (EmailNotValid  _ _ )     ->  renderTemplate_ "flashMessageValueNotValid"
   (SecondNameNotValid _ _ ) ->  renderTemplate_ "flashMessageValueNotValid"
   (FirstNameNotValid _ _  ) ->  renderTemplate_ "flashMessageValueNotValid"
   (ValueNotValid  _ _ )     ->  renderTemplate_ "flashMessageValueNotValid"
   (RowMoreThenMaxCol _ l)   ->  renderTemplate "flashMessageRowGreaterThanMaxColCount" (F.value "maxcols" l)
   (RowLessThenMinCol _ l)   ->  renderTemplate "flashMessageRowLessThanMinColCount" (F.value "mincols" l)
   (NoDataExceptHeader)      ->  renderTemplate_ "flashMessageNoDataInCSVApartFromHeader"
   (NoData)                  ->  renderTemplate_ "flashMessageNoDataInCSV"

problemRow :: CSVProblem -> Maybe Int
problemRow p = case p of
   (NumberNotValid r _ )     ->  Just r
   (EmailNotValid  r _ )     ->  Just r
   (SecondNameNotValid r _ ) ->  Just r
   (FirstNameNotValid r _  ) ->  Just r
   (ValueNotValid  r _ )     ->  Just r
   (RowMoreThenMaxCol r _)   ->  Just r
   (RowLessThenMinCol r _)   ->  Just r
   (NoDataExceptHeader)      ->  Nothing
   (NoData)                  ->  Nothing

problemCell :: CSVProblem -> Maybe Int
problemCell p = case p of
   (NumberNotValid _ c)     ->  Just c
   (EmailNotValid  _ c)     ->  Just c
   (SecondNameNotValid _ c) ->  Just c
   (FirstNameNotValid _ c) ->  Just c
   (ValueNotValid  _ c)     ->  Just c
   (RowMoreThenMaxCol _ _)   ->  Nothing
   (RowLessThenMinCol _ _)   ->  Nothing
   (NoDataExceptHeader)      ->  Nothing
   (NoData)                  ->  Nothing