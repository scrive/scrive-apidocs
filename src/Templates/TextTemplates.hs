{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
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
    (   generatePOTFiles
      , getTextTemplates
      , getTextTemplatesMTime
    ) where

import Text.StringTemplate 
import Text.StringTemplate.Base
import Text.StringTemplate.Classes
import System.IO
import Control.Monad
import Data.Maybe
import Data.List
import Data.Char
import Data.Functor
import Text.Html (stringToHtmlString)
import System.Directory
import System.Time
import Templates.TemplatesFiles
import Misc
import qualified Data.Map as Map
import System.IO
import System.Directory
import Text.I18n.Po hiding (putStrLn)

translationDirs::[String]
translationDirs = ["texts/en","texts/se"]

commonFileName :: String
commonFileName = "common.pot"

templateFileToPOT :: String -> String
templateFileToPOT s = (takeWhile (/= '.') s) ++ ".pot"

data TemplateTextLocation = TemplateTextLocation {
                                             location:: [(String,String)] -- file/templatename
                                           , name:: String
                                    }

initialTTLocation::String -> String -> String -> TemplateTextLocation
initialTTLocation file template ttname = TemplateTextLocation {name=ttname,location=[(file,template)]}


generatePOTFiles:: IO ()
generatePOTFiles = do
    putStrLn "Digging started"
    ttls <- getTextsFromTemplates
    let grouped = groupTTLs  ttls Map.empty
    putStrLn "Cleaning pot dir ... "
    dirExists <- doesDirectoryExist "pot"
    when dirExists $ removeDirectoryRecursive "pot"
    createDirectory "pot"
    putStrLn "Pot Dirs recreated"

    sequence_ $ Map.elems $ Map.mapWithKey writePOTFile grouped
    

writePOTFile:: String -> [TemplateTextLocation] -> IO ()
writePOTFile s ttls = do
    let dirname = reverse $ drop 1 $ dropWhile (/= '/') (reverse s)         
    putStrLn $ "Creating extra dirs if needed " ++ dirname
    createDirectoryIfMissing True ("pot/"++dirname)        
    putStrLn $ "Creating file " ++ ("pot/" ++ s) 
    h <- openFile ("pot/" ++ s) WriteMode
    putStrLn $ "Writing content ... "
    forM_ ttls $ \ttl -> do 
        forM (location ttl) $ \(l,t)->  hPutStr h $ "#: " ++ l ++ ":" ++ t ++ "\n"
        hPutStr h $ "msgid \""++ name ttl ++"\"\n"
        hPutStr h $ "msgstr \"\"\n\n"
    hClose h
    putStrLn $ "Done"    
    
groupTTLs:: [TemplateTextLocation] -> Map.Map String [TemplateTextLocation] -> Map.Map String [TemplateTextLocation]
groupTTLs (t:tl) m = groupTTLs tl $ Map.insertWith (++) (finalLocation t) [t] m
groupTTLs []  m = m


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
                      tSplit [] xs = error "THIS should never happend | Not closed template"
                  in  tSplit s []
retriveTexts (_ : s) = retriveTexts s
retriveTexts _ = []


joinTexts::[TemplateTextLocation] -> [TemplateTextLocation] 
joinTexts = map locationMerge . groupBy (\ttl1 ttl2 -> name ttl1 == name ttl2) 

locationMerge (t : ts) = TemplateTextLocation {name = name t, location = (concat $ map location $ t:ts)}
locationMerge [] = error "No locations provided"


finalLocation::TemplateTextLocation -> String
finalLocation ttl =  if (2 > (length $ nub $ map fst $ location ttl))
                        then templateFileToPOT $ head $ map fst $ location ttl
                        else commonFileName


getTextTemplates:: IO [(String,String)]
getTextTemplates = do
    templates <- join <$> (sequence $ map getTextTemplatesFromDir translationDirs)
    putStrLn $ show $ templates
    return templates

getTextTemplatesMTime :: IO ClockTime
getTextTemplatesMTime =  do
    mtime <- maximum <$> (sequence $ map getRecursiveMTime translationDirs)
    return mtime

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
  

getTextTemplatesFromDir:: String -> IO [(String,String)] 
getTextTemplatesFromDir "."  = return []
getTextTemplatesFromDir ".." = return []
getTextTemplatesFromDir dir = do
    l10 <- fst <$> getL10n dir
    let msgmap = concatMap Map.toList $ concatMap Map.elems (Map.elems l10)
    return $ for msgmap $ \v -> case v of 
                                  (Msgid s,a:_) -> (s,a)  
                                  (Msgid s,_) -> (s,"")   
        