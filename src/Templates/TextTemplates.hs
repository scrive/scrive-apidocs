-----------------------------------------------------------------------------
-- |
-- Module      :  Templates.TextTemplates
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Generating files for internationalisation
-----------------------------------------------------------------------------
module Templates.TextTemplates
    (   getRecursiveMTime
      , getTextTemplatesFrom
      , getAllTextTemplates
      , updateCSV
      , getTextTemplates
      , getTextTemplatesMTime
      , notvalidString
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


notvalidString :: String
notvalidString = "******"

commonFileName :: String
commonFileName = templateFileNameToCSV "common"

textsDirectory :: String
textsDirectory = "texts"


templateFileNameToCSV :: String -> String
templateFileNameToCSV s =  (takeWhile (/= '.') s) ++ ".csv"


data TemplateTextLocation = TemplateTextLocation {
                                             location:: [(String,String)] -- file/templatename
                                           , name:: String
                                    } deriving (Show,Eq)

initialTTLocation::String -> String -> String -> TemplateTextLocation
initialTTLocation file template ttname = TemplateTextLocation {name=ttname,location=[(file,template)]}


groupTTLs:: [TemplateTextLocation] -> Map.Map String [TemplateTextLocation] -> Map.Map String [TemplateTextLocation]
groupTTLs (t:tl) m = groupTTLs tl $ Map.insertWith (++) (finalLocation t) [t] m
groupTTLs [] m = m


getTextsFromTemplates::IO [TemplateTextLocation]
getTextsFromTemplates = do
    textTemplatesUnjoined <- concat <$> mapM getTextsFromTemplateFile templateFiles
    return $ joinTexts $ textTemplatesUnjoined

getTextsFromTemplateFile:: String -> IO [TemplateTextLocation]
getTextsFromTemplateFile filename = do
    templates <- getTemplates $ templateFilePath filename
    let texts = mapSnd retriveTexts templates
    return $ map (uncurry $ initialTTLocation filename) $ join $ map propagateFst texts

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


joinTexts::[TemplateTextLocation] -> [TemplateTextLocation]
joinTexts = map locationMerge . groupBy (\ttl1 ttl2 -> name ttl1 == name ttl2) . sortBy (comparing name)

locationMerge :: [TemplateTextLocation] -> TemplateTextLocation
locationMerge (t : ts) = TemplateTextLocation {name = name t, location = (concat $ map location $ t:ts)}
locationMerge [] = error "No locations provided"


finalLocation:: TemplateTextLocation -> String
finalLocation ttl =  if (2 > (length $ nub $ map fst $ location ttl))
                        then templateFileNameToCSV $ head $ map fst $ location ttl
                        else commonFileName 

getRecursiveMTime :: [Char] -> IO ClockTime
getRecursiveMTime "."  = return $ TOD 0 0
getRecursiveMTime ".." = return $ TOD 0 0
getRecursiveMTime dir = do
     isDir <- doesDirectoryExist dir
     if not isDir
      then getRecursiveMTime dir
      else do
          files <- getDirectoryContents dir
          mts <- forM files $ \fn -> getModificationTime $ dir ++ "/" ++fn
          mt <- getModificationTime dir
          return $ maximum $ mt:mts

getTextTemplatesMTime :: IO ClockTime
getTextTemplatesMTime = getRecursiveMTime textsDirectory

getTextTemplates:: Lang -> IO [(String,String)]
getTextTemplates lang = do
    texts <- getAllTextTemplates
    return $ unionBy (\x y -> fst x == fst y) (texts ! lang) (texts ! defaultValue)

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


emptyLangsMap:: (Monoid a) => Map.Map Lang a
emptyLangsMap = Map.fromList $  for allValues $ \s -> (s,mempty)

getTextTemplatesFromFile:: String -> IO (Map.Map Lang [(String,String)])
getTextTemplatesFromFile path =
 if (not $ ".csv" `isSuffixOf` path)
  then return emptyLangsMap
  else do
    eCsv <- parseFromFile csvFile path
    case eCsv of
       Right (schemeString:csv) -> return $ foldl (addLine $ getScheme schemeString) emptyLangsMap csv
       Right _ -> error $ "CSV parsing error in" ++path ++ "  \n Empty file"
       Left s -> error $ "CSV parsing error in" ++path ++ "  \n" ++ show s
       
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
   
updateCSV :: IO ()
updateCSV = do
    ttls <- getTextsFromTemplates
    let grouped = groupTTLs ttls Map.empty
    forM_ (Map.toList $ Map.map (map name) grouped) $ \(fn, names) -> do
        let fname = textsDirectory ++ "/" ++ (templateFileNameToCSV fn)
        eCsv <- parseFromFile csvFile fname
        case eCsv of
          Right csv -> writeFile fname (genCsvFile $ sortCSV $ updateTexts csv names)
          Left s -> error $ "CSV parsing error in" ++fname ++ "  \n" ++ show s
        
updateTexts::[[String]] -> [String] -> [[String]]
updateTexts ((h:hs):r) names = if (validName h `elem` names || "!" `isPrefixOf` h)
                              then ((validName h) : hs) : (updateTexts r $ delete (validName h) names)
                              else ((unvalidName h) : hs) : (updateTexts r names)
updateTexts ([]:r) names = updateTexts r names
updateTexts [] names = map (:[]) names

validName:: String -> String
validName = dropWhile (== '*')

unvalidName::String -> String
unvalidName s = if (notvalidString `isPrefixOf` s)
                    then s
                    else (notvalidString ++ s)
                    
sortCSV :: [[String]] -> [[String]]
sortCSV = sortBy textCsvSort

textCsvSort::[String] -> [String] -> Ordering
textCsvSort (s1:_) (s2:_) = compare (validName s1) (validName s2)
textCsvSort s1 s2 = compare s1 s2

