{-# LANGUAGE BangPatterns, LambdaCase, OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Char
import Data.Functor
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import System.Directory
import System.Environment
import System.Exit
import System.IO (hPutStrLn, stderr)
import qualified Data.Attoparsec.Text as P
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

(<&&>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<&&>) = liftA2 (&&)
infixr 3 <&&>

(<||>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<||>) = liftA2 (||)
infixr 2 <||>

parenthesize :: [T.Text] -> T.Text
parenthesize ss = "(" <> T.intercalate ", " ss <> ")"

----------------------------------------

data Symbols
  = NoSymbols
  | Explicit ![T.Text]
  | Hiding ![T.Text]
  deriving (Eq, Ord, Show)

parseSymbols :: P.Parser Symbols
parseSymbols = P.choice [
    Hiding   <$> (P.string "hiding" *> P.skipSpace *> parseSymbolList)
  , Explicit <$> parseSymbolList
  , return NoSymbols
  ]
  where
    parseSymbolList = do
      _ <- P.char '('
      P.skipSpace
      symbols <- (`P.sepBy` P.char ',') $ do
        P.skipSpace
        symbol <- P.choice [
            do -- operator
              _ <- P.char '('
              op <- P.takeWhile1 (/= ')')
              _ <- P.char ')'
              return $ "(" <> op <> ")"
          , P.takeWhile1 ((not . isSpace) <&&> (/= '(') <&&> (/= ')') <&&> (/= ','))
          ]
        P.skipSpace
        ctors <- P.option "" (parenthesize <$> parseSymbolList)
        P.skipSpace
        return $ symbol <> ctors
      P.skipSpace
      _ <- P.char ')'
      P.skipSpace
      return $ sort symbols

data Import = Import {
    imQualified      :: !Bool
  , imModule         :: !T.Text
  , imCaselessModule :: !T.Text
  , imAlias          :: !(Maybe T.Text)
  , imPackage        :: !(Maybe T.Text)
  , imSymbols        :: !Symbols
  } deriving (Eq, Show)

compareImport :: Bool -> Import -> Import -> Ordering
compareImport ignore_qualified a b = mconcat [
    if ignore_qualified
    then mempty
    else imQualified a `compare` imQualified b
  , imPackage a        `compare` imPackage b
  , imCaselessModule a `compare` imCaselessModule b
  , imAlias a          `compare` imAlias b
  , imSymbols a        `compare` imSymbols b
  ]

parseImport :: P.Parser Import
parseImport = do
  _ <- P.string "import"
  P.skipSpace
  is_qualified <- P.option False (P.string "qualified" *> P.skipSpace $> True)
  package      <- P.option Nothing
                  (Just <$> (P.char '"' *> parsePkgId <* P.char '"')
                    <* P.skipSpace)
  module_ <- P.takeWhile1 $ (not . isSpace) <&&> (/= '(')
  P.skipSpace
  alias <- P.option Nothing (Just <$> parseAlias)
  P.skipSpace
  symbols <- parseSymbols
  return Import {
      imQualified      = is_qualified
    , imModule         = module_
    , imCaselessModule = T.toCaseFold module_
    , imAlias          = alias
    , imPackage        = package
    , imSymbols        = symbols
    }
  where
    parsePkgId :: P.Parser T.Text
    parsePkgId = P.takeWhile1 $ (not . isSpace) <&&> (/= '"')

    parseAlias :: P.Parser T.Text
    parseAlias = do
      _ <- P.string "as"
      P.skipSpace
      alias <- P.takeWhile1 $ (not . isSpace) <&&> (/= '(')
      P.skipSpace
      return alias

showImport :: Style -> Import -> T.Text
showImport Style{..} Import{..} = T.concat [
    import_module
  , case aliasAlignment of
      Just n | isJust imAlias -> T.replicate (max 0 $ n - 1 - T.length import_module) " "
      _ -> ""
  , maybe "" (" as " <>) imAlias
  , case imSymbols of
      NoSymbols   -> ""
      Explicit ss -> " " <> parenthesize ss
      Hiding ss   -> " hiding " <> parenthesize ss
  ]
  where
    import_module = T.concat [
        "import"
      , if imQualified
        then qualified_
        else if alignUnqualified
             then T.replicate (T.length qualified_) " "
             else ""
      , " "
      , case imPackage of
          Nothing  -> ""
          Just pkg -> "\"" <> pkg <> "\" "
      , imModule
      ]

    qualified_ = " qualified"

----------------------------------------

data ImportGrouping = NoGrouping | ExternalInternal | InternalExternal
  deriving Show

data Style = Style {
    alignUnqualified :: !Bool
  , aliasAlignment   :: !(Maybe Int)
  , importGrouping   :: !ImportGrouping
  } deriving Show

----------------------------------------

-- | Transform Haskell source by lexicographically sorting all its imports
-- according to the given style.
convert :: Style -> Set Text -> Text -> Text
convert style@Style{..} modules source = T.unlines . concat $ [
    reverse . dropWhile T.null $ reverse header
  , separator_if True
  , map (showImport style) first_import_group
  , separator_if $ not $ null first_import_group
  , map (showImport style) second_import_group
  , separator_if $ not $ null second_import_group
  , rest
  ]
  where
    (header, body) = break is_import . T.lines $ source
    (import_section, rest) = break (not . (T.null <||> is_import <||> is_indented)) body

    imports = case P.parseOnly (many parseImport) (T.unlines import_section) of
      Right imps -> sortBy (compareImport alignUnqualified) imps
      Left  msg  -> error msg

    (first_import_group, second_import_group) = case importGrouping of
      NoGrouping       -> (imports, [])
      ExternalInternal -> partition (not . (`S.member` modules) . imModule) imports
      InternalExternal -> partition (      (`S.member` modules) . imModule) imports

    is_import   = T.isPrefixOf "import "
    is_indented = T.isPrefixOf " "

    separator_if p = if p then [T.empty] else []

-- | Recursively traverse the directory and pass all haskell source files into
-- accumulating function.
foldThroughHsFiles :: FilePath -> (acc -> FilePath -> IO acc) -> acc -> IO acc
foldThroughHsFiles basepath f iacc = do
  paths <- filter ((/= ".") <&&> (/= "..")) <$> getDirectoryContents basepath
  foldM run iacc paths
  where
    run acc path = do
      is_dir  <- doesDirectoryExist fullpath
      is_file <- doesFileExist fullpath
      case (is_file && (".hs" `isSuffixOf` path || ".lhs" `isSuffixOf` path), is_dir) of
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
    let module_ = map slash_to_dot
                . drop (length dir + 1)  -- remove base directory (+ slash)
                . drop_extension
                $ file
    putStrLn $ "Found " ++ file ++ " (" ++ module_ ++ ")."
    return (S.insert (T.pack module_) modules, file : files)
    ) acc
  ) (S.empty, []) $ map remove_last_slash dirs
  where
    drop_extension = reverse . drop 1 . dropWhile (/= '.') . reverse

    slash_to_dot '/' = '.'
    slash_to_dot c = c

    remove_last_slash [] = []
    remove_last_slash ['/'] = []
    remove_last_slash (c:cs) = c : remove_last_slash cs

-- | Sort import lists in files at given locations.
sortImports :: Style -> String -> [FilePath] -> IO ()
sortImports style suffix dirs = do
  (modules, files) <- inspectDirectories dirs
  forM_ files $ \file -> do
    putStr $ "Sorting imports in " ++ file ++ "..."
    T.readFile file
      >>= rewriteFile (file ++ suffix) . convert style modules
    putStrLn " done."

-- | Write a file, but only if it would have new content, thus
-- preserving the modification date.
rewriteFile :: FilePath -> T.Text -> IO ()
rewriteFile fileName newContent = do
  fileExists <- doesFileExist fileName
  if fileExists then do
    existingContent <- T.readFile fileName
    unless (existingContent == newContent) $
      T.writeFile fileName newContent
  else T.writeFile fileName newContent

-- | Check whether import lists in files at given locations are sorted.
checkConsistency :: Style -> [FilePath] -> IO ()
checkConsistency style dirs = do
  (modules, files) <- inspectDirectories dirs
  forM_ files $ \file -> do
    putStr $ "Checking whether imports in " ++ file ++ " are sorted..."
    source <- T.readFile file
    if source == convert style modules source
      then putStrLn " yes."
      else do
        putStrLn " no."
        hPutStrLn stderr $ "Imports in " ++ file ++ " are not sorted"
        exitFailure

----------------------------------------

main :: IO ()
main = do
  (args, dirs) <- partition is_config_option <$> getArgs
  if null dirs
    then do
      prog <- getProgName
      putStrLn $ "Usage: " ++ prog ++ " [--check] [--suffix=SUFFIX] [--align-unqualified] [--alias-alignment=N] [--import-grouping=none|external-external|internal-external] <directories>"
    else do
      let (check, style, suffix) = get_options args
      putStrLn $ "Using " ++ show style
      if not check
        then sortImports style suffix dirs
        else onException (checkConsistency style dirs) $ do
          putStrLn $ "Run SortImports.sh without --check to fix the problem."
  where
    is_config_option = ("--" `isPrefixOf`)

    opt_check  = "--check"
    opt_suffix = "--suffix="

    opt_align_unqualified = "--align-unqualified"
    opt_alias_alignment   = "--alias-alignment="
    opt_import_grouping   = "--import-grouping="

    -- TODO: Use something more sensible like cmdargs.
    get_options args = (check, style, suffix)
      where
        check = case find (== opt_check) args of
          Just _  -> True
          Nothing -> False

        suffix = case find (opt_suffix `isPrefixOf`) args of
          Just opt -> drop (length opt_suffix) opt
          Nothing  -> ""

        style = Style align_unqualified alias_alignment import_grouping
          where
            align_unqualified = case find (== opt_align_unqualified) args of
              Just _  -> True
              Nothing -> False

            alias_alignment = case find (opt_alias_alignment `isPrefixOf`) args of
              Just opt -> Just . read $ drop (length opt_alias_alignment) opt
              Nothing  -> Nothing

            import_grouping = case find (opt_import_grouping `isPrefixOf`) args of
              Just opt -> case drop (length opt_import_grouping) opt of
                "none"              -> NoGrouping
                "external-internal" -> ExternalInternal
                "internal-external" -> InternalExternal
                _                   -> error "invalid import-grouping"
              Nothing -> ExternalInternal
