module Doc.CSVUtils (
      CSVProblem(..)
    , CleanCSVData(..)
    , cleanCSVContents
    , getCSVCustomFields
    )
where

import Control.Applicative
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import Data.Char
import Data.Maybe
import Data.List

import Doc.DocStateData
import Doc.DocUtils
import FlashMessage
import InputValidation
import Templates.Templates


data CSVProblem = CSVProblem
                  { problemrowindex :: Maybe Int
                  , problemcolindex :: Maybe Int
                  , problemdescription :: ValidationMessage
                  , problemvalue :: Maybe BS.ByteString
                  }

data CleanCSVData = CleanCSVData
                    { csvheader :: Maybe [BS.ByteString]
                    , csvbody :: [[BS.ByteString]]
                    }

{- |
    Looks up all the custom fields for the csv upload and returns their labels.
-}
getCSVCustomFields :: Document -> Either String [BS.ByteString]
getCSVCustomFields doc@Document{ documentsignatorylinks } =
  case fmap csvsignatoryindex $ documentcsvupload doc of
    Nothing -> Right []
    Just csvsignatoryindex ->
      case checkCSVSigIndex documentsignatorylinks csvsignatoryindex of
        Left msg -> Left msg
        Right _ ->
          Right $ map sfValue . filter isFieldCustom . signatoryfields . signatorydetails $ documentsignatorylinks !! csvsignatoryindex

{- |
    Cleans up csv contents. You get a list of all the problems alongside all the data
    that's been scrubbed up as much as possible.
-}
cleanCSVContents :: [IdentificationType] -> Int -> [[BS.ByteString]] -> ([CSVProblem], CleanCSVData)
cleanCSVContents allowedidtypes customfieldcount contents =
  let mincols = (if isEleg then 5 else 3) :: Int
      maxcols = 5 + customfieldcount
      cleanData = zipWith (cleanRow mincols maxcols) [0..]
      mheader = lookForHeader . cleanData $ take 1 contents
      bodyrows = if isJust mheader then drop 1 contents else contents
      rowresults = cleanData bodyrows
      rowproblems = concatMap fst rowresults
      body = map snd rowresults
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
    lookForHeader :: [([CSVProblem], [BS.ByteString])] -> Maybe [BS.ByteString]
    lookForHeader ((_:_, vals):_) = Just vals
    lookForHeader _ = Nothing
    {- |
        This cleans a single row of data.  It checks both the size of the row
        (meaning how many cols it has), and the values of the fields.
    -}
    cleanRow :: Int -> Int -> Int -> [BS.ByteString] -> ([CSVProblem], [BS.ByteString])
    cleanRow mincols maxcols row xs =
      let fieldresults = zipWith4 cleanField (repeat row) [0..] fieldValidators xs
          fieldproblems = map (fromJust . fst) . filter (isJust . fst) $ fieldresults
          rowsizeproblems = validateRowSize row mincols maxcols xs
          values = map snd fieldresults in
      (rowsizeproblems ++ fieldproblems, fitToMaxSize values)
      where fitToMaxSize vals = take maxcols $ vals ++ repeat BS.empty
    {- |
        We want the row to have a sensible number of columns
    -}
    validateRowSize :: Int -> Int -> Int -> [BS.ByteString] -> [CSVProblem]
    validateRowSize row mincols maxcols xs =
      case length xs of
        l | l<mincols -> [mkProblemForRow row $ flashMessageRowLessThanMinColCount mincols]
          | l>maxcols -> [mkProblemForRow row $ flashMessageRowGreaterThanMaxColCount maxcols]
        _ -> []
    {- |
        This cleans up a single field.  It is given the validator to use (which are mostly things from the
        InputValidation module).
    -}
    cleanField :: Int -> Int -> (String -> Result BS.ByteString) -> BS.ByteString -> (Maybe CSVProblem, BS.ByteString)
    cleanField row col f x =
      let raw = BS.toString x
          failedval = BS.fromString $ minimalScrub raw in
      case f raw of
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
      , numberValidator
      ] ++ repeat asValidFieldValue
      where
        numberValidator :: String -> Result BS.ByteString
        numberValidator
          | isEleg = badIfEmpty flashMessageNumberIsRequired . asValidCompanyNumber
          | otherwise = asValidCompanyNumber
        badIfEmpty :: ValidationMessage -> Result BS.ByteString -> Result BS.ByteString
        badIfEmpty msg res =
          case res of
            Empty -> Bad msg
            x -> x
    isEleg = isJust $ find (== ELegitimationIdentification) allowedidtypes
    {- |
        Handy for constructing problems.
    -}
    mkProblemForAll :: ValidationMessage -> CSVProblem
    mkProblemForAll msg = CSVProblem
                      { problemrowindex = Nothing
                      , problemcolindex = Nothing
                      , problemdescription = msg
                      , problemvalue = Nothing
                      }
    mkProblemForRow :: Int -> ValidationMessage -> CSVProblem
    mkProblemForRow row msg = CSVProblem
                        { problemrowindex = Just row
                        , problemcolindex = Nothing
                        , problemdescription = msg
                        , problemvalue = Nothing
                        }
    mkProblemForField :: Int -> Int -> ValidationMessage -> BS.ByteString -> CSVProblem
    mkProblemForField row col msg raw = CSVProblem
                    { problemrowindex = Just row
                    , problemcolindex = Just col
                    , problemdescription = msg
                    , problemvalue = Just raw
                    }

flashMessageNoDataInCSV :: TemplatesMonad m => m FlashMessage
flashMessageNoDataInCSV =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageNoDataInCSV" ()

flashMessageNoDataInCSVApartFromHeader :: TemplatesMonad m => m FlashMessage
flashMessageNoDataInCSVApartFromHeader =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageNoDataInCSVApartFromHeader" ()

flashMessageRowLessThanMinColCount :: TemplatesMonad m => Int -> m FlashMessage
flashMessageRowLessThanMinColCount mincols =
  toFlashMsg OperationFailed <$>
    renderTemplateFM "flashMessageRowLessThanMinColCount" (field "mincols" mincols)

flashMessageRowGreaterThanMaxColCount :: TemplatesMonad m => Int -> m FlashMessage
flashMessageRowGreaterThanMaxColCount maxcols =
  toFlashMsg OperationFailed <$>
    renderTemplateFM "flashMessageRowGreaterThanMaxColCount" (field "maxcols" maxcols)

flashMessageFirstNameIsRequired :: TemplatesMonad m => m FlashMessage
flashMessageFirstNameIsRequired =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageFirstNameIsRequired" ()

flashMessageSecondNameIsRequired :: TemplatesMonad m => m FlashMessage
flashMessageSecondNameIsRequired =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageSecondNameIsRequired" ()

flashMessageEmailIsRequired :: TemplatesMonad m => m FlashMessage
flashMessageEmailIsRequired =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageEmailIsRequired" ()

flashMessageNumberIsRequired :: TemplatesMonad m => m FlashMessage
flashMessageNumberIsRequired =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageNumberIsRequired" ()
