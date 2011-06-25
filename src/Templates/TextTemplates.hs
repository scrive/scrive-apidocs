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
      , getTextTemplatesFromDir
      , generatePOTFiles
      , potDirectory
    ) where

import Misc
import Templates.TemplatesFiles

import Control.Monad
import Data.Functor
import Data.List
import Data.Ord
import Data.Char
import System.Directory
import System.IO
import System.Time
import Text.I18n.Po hiding (putStrLn, getL10n)
import qualified Data.Map as Map
import qualified Text.I18n.Po (getL10n)


commonFileName :: Bool -> String
commonFileName makeTemplates = templateFileToPOT makeTemplates "common"

potDirectory :: String
potDirectory = "pot"

templateFileToPOT :: Bool -> String -> String
templateFileToPOT True  s = (takeWhile (/= '.') s) ++ ".pot"
templateFileToPOT False s = (takeWhile (/= '.') s) ++ ".po"


data TemplateTextLocation = TemplateTextLocation {
                                             location:: [(String,String)] -- file/templatename
                                           , name:: String
                                    } deriving (Show,Eq)
                                    
initialTTLocation::String -> String -> String -> TemplateTextLocation
initialTTLocation file template ttname = TemplateTextLocation {name=ttname,location=[(file,template)]}

generatePOTFiles:: Bool -> IO ()
generatePOTFiles makeTemplates = do
    ttls <- getTextsFromTemplates
    let grouped = groupTTLs makeTemplates ttls Map.empty
    dirExists <- doesDirectoryExist potDirectory
    when dirExists $ removeDirectoryRecursive potDirectory
    createDirectory potDirectory
    sequence_ $ Map.elems $ Map.mapWithKey writePOTFile grouped
    

writePOTFile:: String -> [TemplateTextLocation] -> IO ()
writePOTFile s ttls = do
    let dirname = reverse $ drop 1 $ dropWhile (/= '/') (reverse s)         
    createDirectoryIfMissing True (potDirectory ++ "/"++dirname)        
    h <- openFile (potDirectory ++ "/" ++ s) WriteMode
    forM_ ttls $ \ttl -> do 
        _ <- forM (location ttl) $ \(l,t)->  hPutStr h $ "#: " ++ l ++ ":" ++ t ++ "\n"
        hPutStr h $ "msgid \""++ name ttl ++"\"\n"
        hPutStr h $ "msgstr \"\"\n\n"
    hClose h
    
groupTTLs:: Bool -> [TemplateTextLocation] -> Map.Map String [TemplateTextLocation] -> Map.Map String [TemplateTextLocation]
groupTTLs makeTemplates (t:tl) m = groupTTLs makeTemplates  tl $ Map.insertWith (++) (finalLocation makeTemplates t) [t] m
groupTTLs _ [] m = m


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


finalLocation:: Bool -> TemplateTextLocation -> String
finalLocation makeTemplates  ttl =  if (2 > (length $ nub $ map fst $ location ttl))
                        then templateFileToPOT makeTemplates  $ head $ map fst $ location ttl
                        else commonFileName makeTemplates 

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
  

getTextTemplatesFromDir:: String -> IO [(String,[(String,String)])]        
getTextTemplatesFromDir "."  = return []
getTextTemplatesFromDir ".." = return []
getTextTemplatesFromDir dir = do
    l10 <- getL10n dir
    let  dumbMerge v = case v of 
                    (Msgid s,a:_) -> (s,a)  
                    (Msgid s,_) -> (s,"")  
    let base = Map.toList $ Map.map (map dumbMerge) $ Map.mapKeys (\(Locale s) -> s) $ Map.map ((concatMap Map.toList) . Map.elems) l10
    return $ mapSnd  (filter (not . null . fst)) base
    
getL10n::String -> IO L10n    
getL10n dir = case ("." `isSuffixOf` dir) of
 True -> return Map.empty                   
 False -> do
     files <- getDirectoryContents dir
     mts <- forM files $ \fn -> do
         isDir <- doesDirectoryExist $ dir ++ "/" ++fn 
         if not isDir 
          then return Map.empty
          else do 
             m <- getL10n $ dir ++ "/" ++fn 
             return $ Map.mapKeys (\(Locale s) -> Locale $ fn ++ "/" ++ s) m
     (mt,_) <- Text.I18n.Po.getL10n $ dir 
     return $ Map.unions $ mt:mts
    
        
