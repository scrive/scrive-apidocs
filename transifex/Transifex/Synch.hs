module Transifex.Synch (push, merge, diff, fix, fetchLocal, main') where

import Control.Monad
import Data.List
import System.IO
import System.Locale
import System.Process
import System.Time
import Text.JSON

import Transifex.Utils

apiURL :: String
apiURL = "https://www.transifex.com/api/2/"

project :: String
project = "kontrakcja"

resourceslug :: TranslationResource -> String
resourceslug Texts    = "textsjson"
resourceslug Events   = "eventsjson"
resourceslug Signview = "signviewjson"

fetch :: String -> String -> String -> TranslationResource -> IO [(String, String)]
fetch user password lang resource = do
  let stringsURL =
        apiURL
          ++ "project/"
          ++ project
          ++ "/resource/"
          ++ resourceslug resource
          ++ "/translation/"
          ++ lang
          ++ "/strings/"
  mjson <- readProcess
    "curl"
    ["--compressed", "--user", user ++ ":" ++ password, "-s", "-X", "GET", stringsURL]
    ""
  case decode mjson of
    Ok js -> return $ sort $ textsFromStringJSON (lang == "en") js
             -- English strings don't have to be reviewed, since this is our
             -- source language
    _     -> error $ "Can't parse response from Transifex: " ++ mjson


fetchLocal :: String -> TranslationResource -> IO [(String, String)]
fetchLocal lang resource = do
  mjson <- readFile $ translationFile lang resource
  case decode mjson of
    Ok js -> return $ sort $ textsFromJSON $ js
    _     -> error $ "Can't parse response from Transifex: " ++ mjson

makeBackup :: String -> String -> String -> TranslationResource -> IO ()
makeBackup user password lang resource = do
  s        <- fetch user password lang resource
  timePart <- getClockTime >>= toCalendarTime >>= return . formatCalendarTime
    defaultTimeLocale
    "%Y-%m-%dT%H:%M:%S"
  let backupFile =
        ("backup_" ++ lang ++ "_" ++ show resource ++ "_" ++ timePart ++ ".json")
  withFile backupFile WriteMode $ \h -> do
    hSetEncoding h utf8
    hPutStr h (encodeTranslationJSON $ textsToJSON $ sort s)
    hClose h

push :: String -> String -> String -> TranslationResource -> IO ()
push user password lang resource = do
  let url = if (sourceLang == lang)
        then
          (  apiURL
          ++ "project/"
          ++ project
          ++ "/resource/"
          ++ resourceslug resource
          ++ "/content/"
          )
        else
          (  apiURL
          ++ "project/"
          ++ project
          ++ "/resource/"
          ++ resourceslug resource
          ++ "/translation/"
          ++ lang
          ++ "/"
          )
  makeBackup user password lang resource
  resp <- readProcess
    "curl"
    [ "--user"
    , user ++ ":" ++ password
    , "-s"
    , "-X"
    , "PUT"
    , "-F"
    , "file=@" ++ translationFile lang resource
    , url
    ]
    ""
  case (parsePushResponse resp) of
    Nothing ->
      putStrLn
        $  "Push of lang "
        ++ lang
        ++ " resource "
        ++ show resource
        ++ " failed. Error message "
        ++ resp
    Just (r, u, a) ->
      putStrLn
        $  "Push of lang "
        ++ lang
        ++ " resource "
        ++ show resource
        ++ " done. "
        ++ show r
        ++ " removed, "
        ++ show u
        ++ " updated and "
        ++ show a
        ++ " added."

merge :: String -> String -> String -> TranslationResource -> IO ()
merge user password lang resource = do
  external <- fetch user password lang resource
  local    <- fetchLocal lang resource
  let changes = compareTranslations external local
  if (length changes == 0)
    then putStrLn "Up to date."
    else do
      putStrLn
        $  "Found "
        ++ show (length changes)
        ++ " changes on the Transifex server. Overwriting local file."
      withFile (translationFile lang resource) WriteMode $ \h -> do
        hSetEncoding h utf8
        hPutStr h (encodeTranslationJSON $ textsToJSON $ sort external)
        hClose h
        putStrLn $ "Done."

diff :: String -> String -> String -> TranslationResource -> IO ()
diff user password lang resource = do
  external <- fetch user password lang resource
  local    <- fetchLocal lang resource
  mapM_ (putStrLn . show) $ compareTranslations external local


fix :: IO ()
fix = do
  forM_ allLangs $ \l -> do
    mapM (fix' l) allResources
  putStrLn "Done."
  where
    fix' lang resource = do
      mjson <- readFile $ translationFile lang resource
      local <- case decode mjson of
        Ok js -> return $ textsFromJSON $ js
        _ ->
          error
            $  "Can't read translation for lang "
            ++ lang
            ++ " resource "
            ++ show resource
            ++ "."
      if (local == sort local)
        then
          putStrLn
          $  "No fix is needed for language "
          ++ lang
          ++ " resource "
          ++ show resource
          ++ "."
        else withFile (translationFile lang resource) WriteMode $ \h -> do
          hSetEncoding h utf8
          hPutStr h (encodeTranslationJSON $ textsToJSON $ sort local)
          hClose h
          putStrLn $ "Language " ++ lang ++ " resource " ++ show resource ++ " fixed."

move :: String -> TranslationResource -> TranslationResource -> IO ()
move s sres tres = do
  forM_ allLangs $ \lang -> do
    msjson           <- readFile $ translationFile lang sres
    mtjson           <- readFile $ translationFile lang tres
    (stexts, ttexts) <- case (decode msjson, decode mtjson) of
      (Ok sjs, Ok tjs) -> return (textsFromJSON sjs, textsFromJSON tjs)
      _                -> error "Can read one of the source files"
    case find (\(s', _) -> s == s') stexts of
      Nothing   -> putStrLn $ "Template " ++ s ++ " not found in basic file."
      Just mtch -> do
        withFile (translationFile lang sres) WriteMode $ \h -> do
          hSetEncoding h utf8
          hPutStr
            h
            (encodeTranslationJSON $ textsToJSON $ filter (\(s', _) -> s /= s') stexts)
          hClose h
          putStrLn $ "Text " ++ s ++ " removed from " ++ show sres

        withFile (translationFile lang tres) WriteMode $ \h -> do
          hSetEncoding h utf8
          hPutStr h (encodeTranslationJSON $ textsToJSON $ sort $ mtch : ttexts)
          hClose h
          putStrLn $ "Text " ++ s ++ " added to " ++ show tres


main' :: [String] -> IO ()
main' ("fix"  : _) = fix

main' ("diff" : (user : (password : (lang : (res : _))))) = case (readResource res) of
  Just res' -> diff user password lang res'
  _         -> error "Invalid parameters. Resource name is invalid"

main' ("diff" : _) =
  error "Invalid parameters. Usage: transifex diff user password lang resource"

main' ("diff-lang" : (user : (password : (lang : _)))) =
  mapM_ (diff user password lang) allResources

main' ("diff-lang" : _) =
  error "Invalid parameters. Usage: transifex diff-lang user password lang"

main' ("diff-all" : (user : (password : _))) = forM_ allLangs $ \lang -> do
  putStrLn ("Diff for language " ++ lang)
  mapM_ (diff user password lang) allResources

main' ("diff-all" : _) =
  error "Invalid parameters. Usage: transifex diff-all user password"

main' ("merge" : (user : (password : (lang : (res : _))))) = case (readResource res) of
  Just res' -> merge user password lang res'
  _         -> error "Invalid parameters. Resource name is invalid"

main' ("merge" : _) =
  error $ "Invalid parameters. " ++ "Usage: transifex merge user password lang resource"

main' ("push" : (user : (password : (lang : (res : _))))) = case (readResource res) of
  Just res' -> push user password lang res'
  _         -> error "Invalid parameters. Resource name is invalid"

main' ("push" : _) =
  error $ "Invalid parameters. " ++ "Usage: transifex push user password lang resource"

main' ("push-lang" : (user : (password : ("en" : _)))) =
  mapM_ (push user password "en") allResources

main' ("push-lang" : (_user : (_password : (_lang : _)))) =
  error
    $  "Invalid parameters. "
    ++ "Usage: transifex push-lang user password lang "
    ++ "is allowed only with en lang"

main' ("push-lang" : _) =
  error "Invalid parameters. Usage: transifex push-lang user password lang"

main' ("merge-lang" : (user : (password : (lang : _)))) =
  mapM_ (merge user password lang) allResources

main' ("merge-lang" : _) =
  error "Invalid parameters. Usage: transifex merge-lang user password lang"

main' ("merge-all" : (user : (password : _))) = forM_ allTargetLangs $ \lang -> do
  putStrLn ("Merging language " ++ lang)
  mapM_ (merge user password lang) allResources

main' ("merge-all" : _) =
  error "Invalid parameters. Usage: transifex merge-all user password"

main' ("move" : (source : (sres : (tres : _)))) =
  case (readResource sres, readResource tres) of
    (Just sres', Just tres') -> move source sres' tres'
    _ -> error $ "Invalid parameters. " ++ "One of resources is invalid"

main' ("move" : _) =
  error
    $  "Invalid parameters. "
    ++ "Usage: transifex move templates_name "
    ++ "source_resource target_resource"

main' [] = putStr usageMsg

main' _  = error "Invalid command. Valid commands are fix, diff, merge and push."

usageMsg :: String
usageMsg = unlines
  [ "Transifex integration tool."
  , ""
  , "Usage:"
  , ""
  , pad "transifex fix" ++ "-- Sort local .json files with translations."
  , pad "transifex push-lang user password lang"
    ++ "-- Push local language to Transifex. "
  , indent $ "It will overwrite the external version. " ++ "'lang' must be 'en'."
  , pad "transifex diff-lang user password lang"
    ++ "-- Show a diff between the external version and your local one."
  , indent "'resource' is taken to be '*'."
  , pad "transifex diff-all user password"
    ++ "-- Show a diff between the external version and your local one."
  , indent "'resource' and 'language' are taken to be '*'."
  , pad "transifex merge-lang user password lang"
    ++ "-- Merge the external version with your local one. "
  , indent "'resource' is taken to be '*'."
  , pad "transifex merge-all user password"
    ++ "-- Merge the external version with your local one. "
  , indent "'resource' and 'language' are taken to be '*'."
  , ""
  , "Low-level commands:"
  , ""
  , pad "transifex push user password lang resource"
    ++ "-- Push local language to Transifex."
  , indent "It will overwrite the external version."
  , pad "transifex diff user password lang resource"
    ++ "-- Show a diff between the external version and your local one."
  , pad "transifex merge user password lang resource"
    ++ "-- Merge the external version with your local one."
  , ""
  , "Available languages : " ++ intercalate " " allLangs
  , "Available resources : texts | events | signview"
  ]
  where
    indentWidth = 44
    pad s = s ++ replicate (indentWidth - length s) ' '
    indent s = replicate (indentWidth + 3) ' ' ++ s
