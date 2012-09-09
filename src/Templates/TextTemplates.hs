-----------------------------------------------------------------------------
-- |
-- Module      :  Templates.TextTemplates
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Parsing and updating internationalisation files.
-- Simple csv files located in texts/ directory.
-- First column is name. First line is a header-schema for columns.
-- If header matches lang we use it as translation, but it is ignored.
-----------------------------------------------------------------------------
module Templates.TextTemplates
    ( getTextTemplates
    , getAllTextTemplates
    , getTextTemplatesMTime
    , updateCSV
    ) where

import Control.Logic
import Utils.Directory
import Utils.Enum
import Utils.List
import Utils.Prelude
import Utils.Read
import Utils.Tuples
import Templates.TemplatesFiles

import Control.Monad
import Data.Functor
import Data.List
import Data.Ord
import Data.Char
import Data.Monoid
import System.Directory
import System.Time
import Data.Map ((!))
import qualified Data.Map as Map
import Data.CSV
import User.Lang
import User.Region
import Text.ParserCombinators.Parsec
import System.IO
import Data.String.Utils (replace)


-- | Reading CSV files Part of this code is done in wrong way. This is
-- a relict from times .po files times and different langs were in
-- different files.
getTextTemplatesMTime :: IO ClockTime
getTextTemplatesMTime = getRecursiveMTime textsDirectory

-- | We recursively read all csv files from texts directory. This
-- function will also merge by selecting the closest default text for those that are missing.
getTextTemplates:: Region -> Lang -> IO [(String,String)]
getTextTemplates region lang = do
    texts <- getAllTextTemplates
    let sameLang = map (\r -> (r, lang)) . filter (/= region) $ allValues
        sameRegion = map (\l -> (region, l)) . filter (/= lang) $ allValues
        unionFoldF a k = unionBy (\x y -> fst x == fst y) a (texts ! k)
        alternatives = sameLang ++ sameRegion
    return $ foldl unionFoldF (texts ! (region, lang)) alternatives

-- | All texts maped by the region and language.  Should be used only for tests
-- since it does not do a merge with a default.
getAllTextTemplates:: IO (Map.Map (Region, Lang) [(String,String)])
getAllTextTemplates = getTextTemplatesFrom textsDirectory    

getTextTemplatesFrom:: String -> IO (Map.Map (Region, Lang) [(String,String)])
getTextTemplatesFrom path =
 if ("." `isSuffixOf`path)
  then return emptyTextTemplatesMap
  else do
     isDir <- doesDirectoryExist path
     if not isDir
      then getTextTemplatesFromFile path
      else do
          files <- getDirectoryContents path
          texts <- forM files $ \fn -> getTextTemplatesFrom $  path ++ "/" ++fn
          return $ Map.unionsWith (++) texts


-- | Main parsing of csv file.  We get the schema from first line, so
-- we can skip columns with comments and match column with language.
-- And we do the reading line by line skiping lines starting with !
-- (control structure) and ***** (unussed translation).
getTextTemplatesFromFile:: String -> IO (Map.Map (Region, Lang) [(String,String)])
getTextTemplatesFromFile path =
 if (not $ ".csv" `isSuffixOf` path)
  then return emptyTextTemplatesMap
  else do
    csv <- basicCSVParser path
    case csv of
       schemeString:csv' -> return $ foldl (addLine $ getScheme schemeString) emptyTextTemplatesMap csv'
       _ -> error $ "CSV parsing error in" ++path ++ "  \n Empty file"
 where
   getScheme s  = drop 1 (map maybeRead s)
   addLine::[Maybe (Region, Lang)] -> (Map.Map (Region, Lang) [(String,String)]) -> [String] -> Map.Map (Region, Lang) [(String,String)]
   addLine scheme m (n:rest) = if ( "!" `isPrefixOf` n || notvalidString `isPrefixOf` n)
                       then m
                       else Map.unionWith (++) m $ Map.fromList $
                                    smartZip scheme $ for rest $ \v ->
                                        if (all (isSpace ||^ isControl) v && (length v < 6))
                                           then []
                                           else [(n,replace "\n" " " v)]
   addLine _ m _ = m


-- | Updating CSV. We look at CSV file and texts that should land
-- there (from templates).  We leave matching ones, put new ones
-- inside and mark with *** unussed ones.  There is some sorting
-- involved to make this work
updateCSV :: IO ()
updateCSV = do
    ttls <- getTextsFromTemplates
    let grouped = groupTTLs ttls Map.empty
    let fname = textsDirectory ++ "/" ++ "everything.csv"
    csv <- basicCSVParser fname
    let names = concat $ map snd $ Map.toList $ Map.map (map name) grouped
    let updatedTexts = updateTexts csv names
    writeFile fname (genCsvFile $ sortCSV $ updatedTexts)
    return ()
        
updateTexts :: [[String]] -> [String] -> [[String]]
updateTexts ((h:hs):r) names
    | validName h `elem` names || "!" `isPrefixOf` h =
        ((validName h) : hs) : (updateTexts r $ delete (validName h) names)
    | otherwise = ((unvalidName h) : hs) : (updateTexts r names)
updateTexts ([]:r) names = updateTexts r names
updateTexts [] names = map (:[]) names


-- | Finding texts.  We have template files in templates directory.
-- Every construction of form $_....()$ there is interpreted as
-- translation system redirection.  We sometimes do update of out text
-- csv files based on current templates.


{- Finding texts templates-}

-- | Structure that that we put text if we find it
data TemplateTextLocation = TemplateTextLocation
    { location    :: [(String,String)] -- file/template name where text was found
    , name        :: String
    } deriving (Show,Eq)

initialTTLocation :: String -> String -> String -> TemplateTextLocation
initialTTLocation file template ttname = TemplateTextLocation { name = ttname
                                                              , location = [(file,template)]
                                                              }


-- | Get all texts from templates and put them in TemplateTextLocation structure.
getTextsFromTemplates :: IO [TemplateTextLocation]
getTextsFromTemplates = joinTexts <$> concat <$> mapM getTextsFromTemplateFile templateFiles



-- | Get text from one template file.
getTextsFromTemplateFile :: String -> IO [TemplateTextLocation]
getTextsFromTemplateFile filename = do
    templates <- getTemplates $ templateFilePath filename
    let texts = mapSnd retriveTexts templates
    return $ map (uncurry $ initialTTLocation filename) $ join $ map propagateFst texts


-- Looking for texts in form $_...()$ in String
retriveTexts :: String -> [String]
retriveTexts ('$':('_': s)) =
                  let tSplit ('(':(')':('$':s'))) xs = ('_':xs) : (retriveTexts s')
                      tSplit (x:s') xs = tSplit s' (xs ++ [x])
                      tSplit [] _ = error "THIS should never happend | Not closed template"
                  in  tSplit s []

retriveTexts ('$':rest) =
    case dropWhile (isAlphaNum) rest of
      ('(':s) ->  (embTexts $ takeWhile (/= '$') s) ++ (retriveTexts $ dropWhile (/= '$') s)
      s -> retriveTexts s

retriveTexts (_:s) = retriveTexts s
retriveTexts _ = []

embTexts :: String -> [String]
embTexts ('_':s) = ('_':(takeWhile isAlphaNum s)) : (embTexts $ dropWhile isAlphaNum s)
embTexts (_:s) = embTexts s
embTexts _ = []

-- | After we found texts in different template files we need to merge them so we can have one TemplateTextLocation per text
joinTexts :: [TemplateTextLocation] -> [TemplateTextLocation]
joinTexts = map locationMerge . groupBy (\ttl1 ttl2 -> name ttl1 == name ttl2) . sortBy (comparing name)

locationMerge :: [TemplateTextLocation] -> TemplateTextLocation
locationMerge (t : ts) = TemplateTextLocation {name = name t, location = (concat $ map location $ t:ts)}
locationMerge [] = error "No locations provided"


{- Selecting csv file for text -}

-- | If text was found in many template files it lands in commons.csv,
--   else it lands in template-file-name.csv.
groupTTLs :: [TemplateTextLocation] -> Map.Map String [TemplateTextLocation] -> Map.Map String [TemplateTextLocation]
groupTTLs (t:tl) m = groupTTLs tl $ Map.insertWith (++) (finalLocation t) [t] m
groupTTLs [] m = m

finalLocation :: TemplateTextLocation -> String
finalLocation ttl 
    | 2 > (length $ nub $ map fst $ location ttl) =
        templateFileNameToCSV $ head $ map fst $ location ttl
    | otherwise = commonFileName

{- Utils -}

--How we mark names of unussed text
notvalidString :: String
notvalidString = "******"

--Name of file where `global` texts land
commonFileName :: String
commonFileName = templateFileNameToCSV "common"

-- Directory for csv files
textsDirectory :: String
textsDirectory = "texts"

templateFileNameToCSV :: String -> String
templateFileNameToCSV s = takeWhile (/= '.') s ++ ".csv"

validName :: String -> String
validName = dropWhile (== '*')

unvalidName :: String -> String
unvalidName s | notvalidString `isPrefixOf` s = s
              | otherwise = notvalidString ++ s
                    
-- Sorting when inserting to csv file. 
sortCSV :: [[String]] -> [[String]]
sortCSV = sortBy textCsvSort

textCsvSort :: [String] -> [String] -> Ordering
textCsvSort (s1:_) (s2:_) = compare (validName s1) (validName s2)
textCsvSort s1 s2 = compare s1 s2

-- Empty text templates map is used to ensure that we will not return map with
-- one of regions or languages missing even if not translation was provided for
-- it.
emptyTextTemplatesMap:: (Monoid a) => Map.Map (Region, Lang) a
emptyTextTemplatesMap = Map.fromList $ for allValues $ \s -> (s,mempty)

basicCSVParser :: String -> IO [[String]]
basicCSVParser path =
    withFile path ReadMode $ \h -> do
    hSetEncoding h utf8
    content <- hGetContents h
    case parse csvFile path content of
        Right csv -> return csv
        Left s -> error $ "CSV parse error in " ++ path ++ ": " ++ show s
