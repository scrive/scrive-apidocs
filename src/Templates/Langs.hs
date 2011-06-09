{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Templates.Langs
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Different language versions basic module
-----------------------------------------------------------------------------
module Templates.Langs
    ( 
          getTextTemplates
        , getTextTemplatesMTime
        , getTranslationStats
        , TranslationStats(..)
        , Lang(..)
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
import Templates.TextTemplates
import Data.Map (empty,insert, Map)


data Lang =    LANG_SE 
            |  LANG_EN 
    deriving (Bounded, Enum, Show, Ord, Eq)


langDir :: Lang -> String
langDir LANG_EN = "texts/en"
langDir LANG_SE = "texts/se"


translationDirs::[String]
translationDirs = map langDir allValues

getTextTemplates:: Lang -> IO [(String,String)]
getTextTemplates lang = do
    selectedTrans <- concatMap snd <$> (getTextTemplatesFromDir $ langDir lang)
    defaultTrans  <- concatMap snd <$> (getTextTemplatesFromDir $ langDir defaultValue)
    return $ unionBy (\x y -> fst x == fst y) selectedTrans defaultTrans
                    

getTextTemplatesMTime :: IO ClockTime
getTextTemplatesMTime = maximum <$> (sequence $ map getRecursiveMTime translationDirs)

generatePOTFiles:: IO ()
generatePOTFiles = Templates.TextTemplates.generatePOTFiles True


getTranslationStats:: IO [(Lang,TranslationStats)]
getTranslationStats = do
    Templates.TextTemplates.generatePOTFiles False
    tts <- getTextTemplatesFromDir "pot"
    Templates.TextTemplates.generatePOTFiles True
    mapM (makeTranslationStats tts) allValues 
   
makeTranslationStats :: [(String,[(String,String)])] -> Lang -> IO (Lang,TranslationStats)
makeTranslationStats tts lang = do
     lng <- getTextTemplatesFromDir $ langDir lang
     let stats = translationStats tts lng
     return (lang,stats)

translationStats :: [(String,[(String,String)])] -> [(String,[(String,String)])] -> TranslationStats
translationStats tts lng = 
   let
    missing = (map fst tts) \\ (map fst lng)
    extra = (map fst lng) \\ (map fst tts)
    notSynch = filter (\s -> not $ similarTranslations (fileTranslations s tts) (fileTranslations s lng)) $ (map fst tts) 
   in TranslationStats {
     missingFiles = missing
   , extraFiles = extra
   , notSynchronisedFiles  = (notSynch \\ missing)  \\ extra
   , emptyTranslations = let 
                          empt = mapSnd (filter (\(a,b) ->  null b)) lng 
                          joined = map (\(k,l) -> map (\(a,_) -> (k,a)) l) empt
                        in concat joined
}
    
similarTranslations:: [(String,String)] -> [(String,String)] -> Bool
similarTranslations as bs =  (map fst bs) \\ (map fst as) == (map fst as) \\ (map fst bs)  
                            
fileTranslations ::String -> [(String,[(String,String)])] ->  [(String,String)]
fileTranslations fn = concat . (map snd) . filter (((==) fn) . fst)

data TranslationStats = TranslationStats {
        missingFiles :: [String]
      , extraFiles :: [String]
      , notSynchronisedFiles :: [String]
      , emptyTranslations :: [(String,String)]
 } deriving (Show)

