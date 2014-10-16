{-# LANGUAGE BangPatterns, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import System.Directory
import System.Environment
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

-- | Transform the haskell source file at a given location by
-- lexicographically sorting all its imports and splitting them
-- into two groups, foreign and local ones. If the suffix is empty,
-- the original file is overwritten. Otherwise a copy is made.
convert :: S.Set T.Text -> String -> FilePath -> IO ()
convert modules new_path_suffix path = do
  (header, rest) <- break (is import_) . T.lines <$> T.readFile path
  let (qimports, imports) = (icase_sort *** icase_sort)
        . partition (is qualified_import_)
        . filter (is import_) $ rest
      (limports, fimports) = partition (is_local import_) imports
      (lqimports, fqimports) = partition (is_local qualified_import_) qimports
      no_foreign_imports = null fimports && null fqimports
      no_local_imports = null limports && null lqimports
  T.writeFile new_path . T.unlines . concat $ [
      header
    , fimports
    , fqimports
    , separator_if $ not no_foreign_imports
    , limports
    , lqimports
    , separator_if $ not no_local_imports
    , dropWhile (is import_ <||> T.null) rest
    ]
  where
    new_path = path ++ new_path_suffix

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

sortImports :: String -> [FilePath] -> IO ()
sortImports suffix paths = do
  -- collect modules and file paths from supplied directories
  (modules, files) <- foldM (\acc dir -> do
    putStrLn $ "Inspecting '" ++ dir ++ "'..."
    foldThroughHsFiles dir (\(!modules, !files) file -> do
      let modul = map slash_to_dot
                . drop (length dir + 1)
                . take (length file - 3) $ file
      putStrLn $ "Found " ++ file ++ " (" ++ modul ++ ")."

      return (S.insert (T.pack modul) modules, file : files)
      ) acc
    ) (S.empty, []) $ map remove_last_slash paths
  -- transform files at collected locations
  forM_ files $ \file -> do
    putStr $ "Sorting imports in " ++ file ++ "..."
    convert modules suffix file
    putStrLn " done."
  where
    slash_to_dot '/' = '.'
    slash_to_dot c = c

    remove_last_slash [] = []
    remove_last_slash ['/'] = []
    remove_last_slash (c:cs) = c : remove_last_slash cs

----------------------------------------

main :: IO ()
main = do
  args <- getArgs
  if null args
    then do
      prog <- getProgName
      putStrLn $ "Usage: " ++ prog ++ " [--suffix=SUFFIX] <directories>"
    else do
      let suffix_option = "--suffix="
          (suffix, paths) = if suffix_option `isPrefixOf` head args
            then (drop (length suffix_option) $ head args, tail args)
            else ("", args)
      sortImports suffix paths
