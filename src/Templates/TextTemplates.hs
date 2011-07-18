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
-- If header matches lang we use it as translation, ale it is ignored
-----------------------------------------------------------------------------
module Templates.TextTemplates
    (  getTextTemplates
      , getAllTextTemplates
      , getTextTemplatesMTime
      , updateCSV
    ) where

import Misc
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
import Text.ParserCombinators.Parsec
import System.IO


{- | Reading CSV files
     Part of this code is done in wrong way. This is a relict from times .po files times and different langs were in different files.
-}
getTextTemplatesMTime :: IO ClockTime
getTextTemplatesMTime = getRecursiveMTime textsDirectory


-- | We recursively read all csv files from texts directory
--   This function will also merge selected lang with default lang
getTextTemplates:: Lang -> IO [(String,String)]
getTextTemplates _lang = do
    texts <- getAllTextTemplates 
    return $ unionBy (\x y -> fst x == fst y) (texts ! defaultValue) (texts ! defaultValue)

-- | All texts maped by the language
--   Should be used only for tests since it does not do a mergewith default language
getAllTextTemplates:: IO (Map.Map Lang [(String,String)])
getAllTextTemplates = getTextTemplatesFrom textsDirectory    

getTextTemplatesFrom:: String -> IO (Map.Map Lang [(String,String)])
getTextTemplatesFrom path =
 if ("." `isSuffixOf`path)
  then return emptyLangsMap
  else do
     isDir <- doesDirectoryExist  path
     if not isDir
      then getTextTemplatesFromFile path
      else do
          files <- getDirectoryContents  path
          texts <- forM files $ \fn -> getTextTemplatesFrom $  path ++ "/" ++fn
          return $ Map.unionsWith (++) texts


-- | Main parsing of csv file
--   We get the schema from first line, so we can skipp columns with comments and match column with language
--   And we do the reading line by line skiping lines starting with ! (control structure) and ***** (unussed translation)
getTextTemplatesFromFile:: String -> IO (Map.Map Lang [(String,String)])
getTextTemplatesFromFile path =
 if (not $ ".csv" `isSuffixOf` path)
  then return emptyLangsMap
  else do
    csv <- basicCSVParser path
    case csv of
       schemeString:csv' -> return $ foldl (addLine $ getScheme schemeString) emptyLangsMap csv'
       _ -> error $ "CSV parsing error in" ++path ++ "  \n Empty file"
 where
   getScheme s  = drop 1 (map maybeRead s)
   addLine::[Maybe Lang] -> (Map.Map Lang [(String,String)]) -> [String] -> Map.Map Lang [(String,String)]
   addLine scheme m (n:rest) = if ( "!" `isPrefixOf` n || notvalidString `isPrefixOf` n)
                       then m
                       else Map.unionWith (++) m $ Map.fromList $
                                    smartZip scheme $ for rest $ \v ->
                                        if (all (isSpace ||^ isControl) v)
                                           then []
                                           else [(n,v)]
   addLine _ m _ = m


{-| Updataing CSV
    We look at csv file and texts that should land there (from templates)
    We leave matching ones, put new ones inside and mark with *** unussed ones.
    There is some sorting involved to make this work
-}

   
updateCSV :: IO ()
updateCSV = do
    ttls <- getTextsFromTemplates
    let grouped = groupTTLs ttls Map.empty
    forM_ (Map.toList $ Map.map (map name) grouped) $ \(fn, names) -> do
        let fname = textsDirectory ++ "/" ++ (templateFileNameToCSV fn)
        csv <- basicCSVParser fname
        writeFile fname (genCsvFile $ sortCSV $ updateTexts csv names)
        
updateTexts::[[String]] -> [String] -> [[String]]
updateTexts ((h:hs):r) names = if (validName h `elem` names || "!" `isPrefixOf` h)
                              then ((validName h) : hs) : (updateTexts r $ delete (validName h) names)
                              else ((unvalidName h) : hs) : (updateTexts r names)
updateTexts ([]:r) names = updateTexts r names
updateTexts [] names = map (:[]) names


{- | Finding texts
     We have template files in templates directory.
     Every construction of form $_....()$ there is interpreted as translation system redirection.
     We sometimes do update of out text csv files based on current templates.
-}

{- Finding texts templates-}

-- | Structure that that we put text if we find it
data TemplateTextLocation = TemplateTextLocation {
                                             location:: [(String,String)] -- file/templatename where text was found
                                           , name:: String
                                    } deriving (Show,Eq)

initialTTLocation::String -> String -> String -> TemplateTextLocation
initialTTLocation file template ttname = TemplateTextLocation {name=ttname,location=[(file,template)]}


-- | Get all texts from templates and put them in TemplateTextLocation structure.
getTextsFromTemplates::IO [TemplateTextLocation]
getTextsFromTemplates = joinTexts <$> concat <$> mapM getTextsFromTemplateFile templateFiles



-- | Get text from one template file
getTextsFromTemplateFile:: String -> IO [TemplateTextLocation]
getTextsFromTemplateFile filename = do
    templates <- getTemplates $ templateFilePath filename
    let texts = mapSnd retriveTexts templates
    return $ map (uncurry $ initialTTLocation filename) $ join $ map propagateFst texts


-- Looking for texts in form $_...()$ in String
retriveTexts::String -> [String]
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

embTexts::String -> [String]
embTexts ('_':s) = ('_':(takeWhile isAlphaNum s)) : (embTexts $ dropWhile isAlphaNum s)
embTexts (_:s) = embTexts s
embTexts _ = []

-- | After we found texts in different template files we need to merge them so we can have one TemplateTextLocation per text
joinTexts::[TemplateTextLocation] -> [TemplateTextLocation]
joinTexts = map locationMerge . groupBy (\ttl1 ttl2 -> name ttl1 == name ttl2) . sortBy (comparing name)

locationMerge :: [TemplateTextLocation] -> TemplateTextLocation
locationMerge (t : ts) = TemplateTextLocation {name = name t, location = (concat $ map location $ t:ts)}
locationMerge [] = error "No locations provided"


{- Selecting csv file for text -}

-- | If text was found in many template files it lands in commons.csv,
--   else it lands in template-file-name.csv.
groupTTLs:: [TemplateTextLocation] -> Map.Map String [TemplateTextLocation] -> Map.Map String [TemplateTextLocation]
groupTTLs (t:tl) m = groupTTLs tl $ Map.insertWith (++) (finalLocation t) [t] m
groupTTLs [] m = m

finalLocation:: TemplateTextLocation -> String
finalLocation ttl =  if (2 > (length $ nub $ map fst $ location ttl))
                        then templateFileNameToCSV $ head $ map fst $ location ttl
                        else commonFileName

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
templateFileNameToCSV s =  (takeWhile (/= '.') s) ++ ".csv"

validName:: String -> String
validName = dropWhile (== '*')

unvalidName::String -> String
unvalidName s = if (notvalidString `isPrefixOf` s)
                    then s
                    else (notvalidString ++ s)
                    
-- Sorting when inserting to csv file. 
sortCSV :: [[String]] -> [[String]]
sortCSV = sortBy textCsvSort

textCsvSort::[String] -> [String] -> Ordering
textCsvSort (s1:_) (s2:_) = compare (validName s1) (validName s2)
textCsvSort s1 s2 = compare s1 s2

-- Empty langs map is used to ensure that we will not return map with one of languages missing even if not translation was provided for it.
emptyLangsMap:: (Monoid a) => Map.Map Lang a
emptyLangsMap = Map.fromList $  for allValues $ \s -> (s,mempty)

basicCSVParser :: String -> IO [[String]]
basicCSVParser path = do
    h <- openFile path ReadMode
    hSetEncoding h utf8
    content <- hGetContents h
    res <- case (parse csvFile "(unknown)" content) of
     Right csv -> return csv
     Left s -> error $ "CSV parsing error in" ++path ++ "  \n" ++ show s
    hClose h
    return res
