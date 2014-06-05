module Transifex.Synch (push, merge, diff, fix, fetchLocal)where


import Data.CSV (csvFile)
import Data.Char (isSpace, isControl)
import Data.List (isSuffixOf)
import System.IO
import Data.Map (Map)
import qualified Data.Map as M
import Text.ParserCombinators.Parsec (parse)
import Text.JSON.Gen
import Text.JSON
import Text.JSON.Pretty
import Text.PrettyPrint.HughesPJ
import Data.List
import System.Process
import Transifex.Utils
import Control.Monad
import System.Environment
import System.Time
import System.Locale
import Data.Time.Calendar

apiURL :: String
apiURL = "http://www.transifex.com/api/2/"

project :: String
project = "kontrakcja"

resourceslug :: TranslationResource -> String
resourceslug Texts = "textsjson"
resourceslug Events = "eventsjson"
resourceslug Questionnaire = "questionnairejson"
resourceslug Signview = "signviewjson"

fetch :: String -> String -> String -> TranslationResource -> IO [(String,String)]
fetch user password lang resource = do
  mjson <- readProcess "curl" ["--compressed", "--user", user++":" ++ password, "-s" ,"-X", "GET" , apiURL ++ "project/" ++ project ++ "/resource/" ++ resourceslug resource ++ "/translation/"++lang++"/strings/"] ""
  case decode mjson of
     Ok js -> return $ sort $ textsFromStringJSON $ js
     _ -> error $ "Can't parse response from Transifex: " ++ mjson


fetchLocal  :: String -> TranslationResource -> IO [(String,String)]
fetchLocal lang resource = do
  mjson <- readFile $ translationFile lang resource
  case decode mjson of
     Ok js -> return $ sort $ textsFromJSON $ js
     _ -> error $ "Can't parse response from Transifex: " ++ mjson

makeBackup  :: String -> String ->  String -> TranslationResource -> IO ()
makeBackup user password lang resource = do
  s <- fetch user password lang resource
  timePart <- getClockTime >>= toCalendarTime >>= return . formatCalendarTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"
  withFile ("backup_" ++ lang ++ "_" ++ show resource ++ "_" ++ timePart ++ ".json") WriteMode $ \h -> do
    hSetEncoding h utf8
    hPutStr h (encodeTranslationJSON $ textsToJSON $ sort s)
    hClose h

push :: String -> String ->  String -> TranslationResource -> IO ()
push user password lang resource = do
  let url = if (sourceLang == lang)
              then  apiURL ++ "project/" ++ project ++ "/resource/" ++ resourceslug resource ++ "/content/"
              else  apiURL ++ "project/" ++ project ++ "/resource/" ++ resourceslug resource ++ "/translation/"++lang ++ "/"
  makeBackup user password lang resource
  resp  <- readProcess "curl" ["--user", user++":" ++ password,"-s", "-X", "PUT" ,"-F", "file=@" ++ translationFile lang resource, url] ""
  case (parsePushResponse resp) of
    Nothing -> putStrLn $ "Push of lang " ++ lang++ " resource "++ show resource ++ " failed. Error message " ++ resp
    Just (r,u,a) -> putStrLn $ "Push of lang " ++ lang ++ " resource "++ show resource ++ " done. " ++ show r ++ " removed, " ++ show u ++ " updated and " ++ show a ++ " added."

merge :: String -> String -> String -> TranslationResource -> IO ()
merge user password lang resource = do
  external <- fetch user password lang resource
  local <- fetchLocal lang resource
  let changes = compareTranslations external local
  if (length changes == 0)
    then putStrLn "Up to date."
    else do
      putStrLn $ "Found " ++ show (length changes) ++ " changes on Transifex server. Do you want to: overwrite local file[o], merge changes on-by-one[m], show diff[d] or skip[q]?"
      c <- getOneOfChars ['o','m','d','q']
      putStrLn $ ""
      case c of
        'o' -> do
                  putStrLn "Overwriting local file."
                  withFile (translationFile lang resource) WriteMode $ \h -> do
                    hSetEncoding h utf8
                    hPutStr h (encodeTranslationJSON $ textsToJSON $ sort external)
                    hClose h
                    putStrLn $ "Done."
        'm' -> do
                  putStrLn "Listing all changes for merge."
                  newtexts <- foldM askAndApply external changes
                  putStrLn $ ""
                  putStrLn $ "Done with changes. Saving to file"
                  withFile (translationFile lang resource) WriteMode $ \h -> do
                    hSetEncoding h utf8
                    hPutStr h (encodeTranslationJSON $ textsToJSON $ sort newtexts)
                    hClose h
                    putStrLn $ "Done."
        'd' -> do
                  putStrLn "Listing all changes. +++ means something was added on your local machine. --- means it is removed on your local machine."
                  mapM_  (putStrLn . show) changes
        'q' -> putStrLn "Done."




getOneOfChars :: [Char] -> IO Char
getOneOfChars l = do
  c <- getChar
  if (c `elem` l)
     then return c
     else getOneOfChars l

askAndApply ::  [(String,String)] -> Change -> IO [(String,String)]
askAndApply l ch = do
  putStrLn $ ""
  case ch of
    (Remove n v)       -> putStrLn $ "Text " ++ n ++ " in transifex but not locally. Did you intended to remove it [y/n]? "
    (Add n v)          -> putStrLn $ "Text " ++ n ++ " available locally but not in transifex. Do you want to keep it [y/n]? "
    (Change n _ _)    ->  putStrLn $ "Text " ++ n ++ " changed .\n Do you want to use local version [y/n]? "
  c <- getOneOfChars ['y','n']
  case c of
    'y' -> return $ applyChange ch l
    'n' -> return l

diff :: String -> String -> String -> TranslationResource ->  IO ()
diff user password lang resource = do
  external <- fetch user password lang resource
  local <- fetchLocal lang resource
  mapM_  (putStrLn . show) $ compareTranslations external local


fix :: IO ()
fix = do
  forM_ allLangs $ \l -> do
    mapM (fix' l) allResources
  putStrLn "Done."
  where fix' lang resource = do
          mjson <- readFile $ translationFile lang resource
          local <- case decode mjson of
            Ok js -> return $ textsFromJSON $ js
            _ -> error $ "Can't read translation for lang " ++ lang ++ " resource "++ show resource ++ "."
          if (local == sort local)
             then putStrLn $ "No fix is needed for language " ++ lang ++" resource "++ show resource ++"."
             else withFile (translationFile lang resource) WriteMode $ \h -> do
                          hSetEncoding h utf8
                          hPutStr h (encodeTranslationJSON $ textsToJSON $ sort local)
                          hClose h
                          putStrLn $ "Language "++ lang ++ " resource "++ show resource ++" fixed."

move :: String -> TranslationResource -> TranslationResource -> IO ()
move s sres tres = do
  forM_ allLangs $ \lang -> do
     msjson <- readFile $ translationFile lang sres
     mtjson <- readFile $ translationFile lang tres
     (stexts,ttexts) <- case (decode msjson, decode mtjson) of
                       (Ok sjs,Ok tjs) -> return (textsFromJSON sjs, textsFromJSON tjs)
                       _ -> error "Can read one of the source files"
     case find (\(s',_) -> s == s') stexts of
       Nothing -> putStrLn $ "Template " ++ s ++ " not found in basic file."
       Just mtch -> do
         withFile (translationFile lang sres) WriteMode $ \h -> do
                          hSetEncoding h utf8
                          hPutStr h (encodeTranslationJSON $ textsToJSON $ filter (\(s',_) -> s /= s') stexts)
                          hClose h
                          putStrLn $ "Text " ++ s ++ " removed from " ++ show sres

         withFile (translationFile lang tres) WriteMode $ \h -> do
                          hSetEncoding h utf8
                          hPutStr h (encodeTranslationJSON $ textsToJSON $ sort $ mtch:ttexts)
                          hClose h
                          putStrLn $ "Text " ++ s ++ " added to " ++ show tres


main :: IO ()
main = main' =<< getArgs


main' :: [String] -> IO ()
main' ("fix":_) = fix
main' ("diff":(user:(password:(lang:(res:_)))))  = case (readResource res) of
                                                     Just res' -> diff user password lang res'
                                                     _ -> error "Invalid parameters. Resource name is invalid"
main' ("diff":_)   = error "Invalid parameters. Usage: transifex.sh diff user password lang resource"
main' ("diff-lang":(user:(password:(lang:_))))  = mapM_ (diff user password lang) allResources
main' ("diff-lang":_)   = error "Invalid parameters. Usage: transifex.sh diff-lang user password lang"
main' ("diff-all":(user:(password:_)))  =  forM_ allLangs $ \lang -> putStrLn ("Diff for language " ++ lang) >> mapM_ (diff user password lang) allResources
main' ("diff-all":_)   = error "Invalid parameters. Usage: transifex.sh diff-all user password"

main' ("merge":(user:(password:(lang:(res:_))))) = case (readResource res) of
                                                     Just res' -> merge user password lang res'
                                                     _ -> error "Invalid parameters. Resource name is invalid"
main' ("merge":_)  = error "Invalid parameters. Usage: transifex.sh merge user password lang resource"
main' ("push":(user:(password:(lang:(res:_)))))  = case (readResource res) of
                                                     Just res' -> push user password lang res'
                                                     _ -> error "Invalid parameters. Resource name is invalid"
main' ("push":_)   = error "Invalid parameters. Usage: transifex.sh push user password lang resource"
main' ("push-lang":(user:(password:("en":_)))) = mapM_ (push user password "en") allResources
main' ("push-lang":(user:(password:(lang:_)))) =  error "Invalid parameters. Usage: transifex.sh push-lang user password lang is allowed only with en lang"
main' ("push-lang":_)   = error "Invalid parameters. Usage: transifex.sh push-lang user password lang"
main' ("merge-lang":(user:(password:(lang:_)))) = mapM_ (merge user password lang) allResources
main' ("merge-lang":_)   = error "Invalid parameters. Usage: transifex.sh merge-lang user password lang"

main' ("merge-all":(user:(password:_))) =  forM_ allLangs $ \lang -> putStrLn ("Merging language " ++ lang) >>  mapM_ (merge user password lang) allResources
main' ("merge-all":_)   = error "Invalid parameters. Usage: transifex.sh merge-all user password"

main' ("move":(source:(sres:(tres:_)))) = case (readResource sres, readResource tres) of
                                                     (Just sres',Just tres') -> move source sres' tres'
                                                     _ -> error "Invalid parameters. One of resources is invalid"
main' ("move":_)   = error "Invalid parameters. Usage: transifex.sh move templates_name source_resource target_resource"

main' _ = error "Invalid command. Valid commands are fix, diff, merge and push."
