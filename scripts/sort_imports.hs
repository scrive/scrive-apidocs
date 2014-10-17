{-# LANGUAGE BangPatterns, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.Set (Set)
import Data.Text (Text)
import System.Directory
import System.Environment
import System.Exit
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

(<&&>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<&&>) = liftA2 (&&)
infixr 3 <&&>

(<||>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<||>) = liftA2 (||)
infixr 2 <||>

----------------------------------------

-- | Transform the haskell source file by lexicographically sorting
-- all its imports and splitting them into two groups, foreign and
-- local ones.
convert :: Set Text -> Text -> Text
convert modules source = T.unlines . concat $ [
    header
  , fimports
  , fqimports
  , separator_if has_foreign_imports
  , limports
  , lqimports
  , separator_if has_local_imports
  , dropWhile (is import_ <||> T.null) rest
  ]
  where
    (header, rest) = break (is import_) . T.lines $ source
    (qimports, imports) = (icase_sort *** icase_sort)
      . partition (is qualified_import_)
      . filter (is import_) $ rest

    (limports, fimports) = partition (is_local import_) imports
    (lqimports, fqimports) = partition (is_local qualified_import_) qimports

    has_foreign_imports = not $ null fimports && null fqimports
    has_local_imports = not $ null limports && null lqimports

    separator_if p = if p then [T.empty] else []

    import_ = "import "
    qualified_import_ = "import qualified "
    is import_type = (import_type `T.isPrefixOf`)

    icase_sort = map snd
      . sortBy (compare `on` fst)
      . map (T.toLower &&& id)

    is_local import_type = flip S.member modules
      . T.takeWhile (isAlphaNum <||> (== '.'))
      . T.drop (T.length import_type)

-- | Recursively traverse the directory and pass all
-- haskell source files into accumulating function.
foldThroughHsFiles :: FilePath -> (acc -> FilePath -> IO acc) -> acc -> IO acc
foldThroughHsFiles basepath f iacc = do
  paths <- filter ((/= ".") <&&> (/= "..")) <$> getDirectoryContents basepath
  foldM run iacc paths
  where
    run acc path = do
      is_dir  <- doesDirectoryExist fullpath
      is_file <- doesFileExist fullpath
      case (is_file && ".hs" `isSuffixOf` path, is_dir) of
        (True, False) -> f acc fullpath
        (False, True) -> foldThroughHsFiles fullpath f acc
        _             -> return acc
      where
        fullpath = basepath ++ "/" ++ path

-- | Collect modules and file paths from given directories.
inspectDirectories :: [FilePath] -> IO (Set Text, [FilePath])
inspectDirectories dirs = foldM (\acc dir -> do
  putStrLn $ "Inspecting " ++ dir ++ "..."
  foldThroughHsFiles dir (\(!modules, !files) file -> do
    let modul = map slash_to_dot
              . drop (length dir + 1)  -- remove base directory (+ slash)
              . take (length file - 3) -- remove .hs at the end
              $ file
    putStrLn $ "Found " ++ file ++ " (" ++ modul ++ ")."
    return (S.insert (T.pack modul) modules, file : files)
    ) acc
  ) (S.empty, []) $ map remove_last_slash dirs
  where
    slash_to_dot '/' = '.'
    slash_to_dot c = c

    remove_last_slash [] = []
    remove_last_slash ['/'] = []
    remove_last_slash (c:cs) = c : remove_last_slash cs

-- | Sort import lists in files at given locations.
sortImports :: String -> [FilePath] -> IO ()
sortImports suffix dirs = do
  (modules, files) <- inspectDirectories dirs
  -- transform files at collected locations
  forM_ files $ \file -> do
    putStr $ "Sorting imports in " ++ file ++ "..."
    T.readFile file
      >>= return . convert modules
      >>= T.writeFile (file ++ suffix)
    putStrLn " done."

-- | Check whether import lists in
-- files at given locations are sorted.
checkConsistency :: [FilePath] -> IO ()
checkConsistency dirs = do
  (modules, files) <- inspectDirectories dirs
  forM_ files $ \file -> do
    putStr $ "Checking whether imports in " ++ file ++ " are sorted..."
    source <- T.readFile file
    if source == convert modules source
      then putStrLn " yes."
      else putStrLn " no." >> exitFailure

----------------------------------------

main :: IO ()
main = do
  (args, dirs) <- partition is_config_option <$> getArgs
  if null dirs
    then do
      prog <- getProgName
      putStrLn $ "Usage: " ++ prog ++ " [--check] [--suffix=SUFFIX] <directories>"
    else case get_options args of
      (False, suffix) -> sortImports suffix dirs
      (True, _) -> onException (checkConsistency dirs) $ do
        putStrLn $ "Run scripts/sort_imports.sh without --check option to fix the problem."
  where
    is_config_option = ("--" `isPrefixOf`)
    opt_suffix = "--suffix="
    opt_check  = "--check"

    get_options args = (check, suffix)
      where
        check = case find (== opt_check) args of
          Just _  -> True
          Nothing -> False
        suffix = case find (opt_suffix `isPrefixOf`) args of
          Just opt -> drop (length opt_suffix) opt
          Nothing  -> ""
