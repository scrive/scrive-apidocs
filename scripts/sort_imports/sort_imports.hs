{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wall          #-}
module Main (main) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Char
import Data.Functor
import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO (hPutStrLn, stderr)
import qualified Data.Attoparsec.Text as P
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

import AppDir (setupAppPaths)

max_line_length :: Int
max_line_length = 80

(<&&>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<&&>) = liftA2 (&&)
infixr 3 <&&>

(<||>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<||>) = liftA2 (||)
infixr 2 <||>

parenthesize :: [Text] -> Text
parenthesize ss = "(" <> T.intercalate ", " ss <> ")"

----------------------------------------

data ImpSpec
  = EmptyImpSpec
  | Explicit ![Text]
  | Hiding ![Text]
  deriving (Eq, Ord, Show)

parseImpSpec :: P.Parser ImpSpec
parseImpSpec = P.choice
  [ Hiding <$> (P.string "hiding" *> P.skipSpace *> parseEntityList)
  , Explicit <$> parseEntityList
  , return EmptyImpSpec
  ]
  where
    parseEntityList = do
      void $ P.char '('
      P.skipSpace
      entities <- (`P.sepBy` P.char ',') $ do
        P.skipSpace
        entity <- P.choice
          [ do -- operator
            void $ P.char '('
            op <- P.takeWhile1 (/= ')')
            void $ P.char ')'
            return $ "(" <> op <> ")"
          , P.takeWhile1 ((not . isSpace) <&&> (/= '(') <&&> (/= ')') <&&> (/= ','))
          ]
        P.skipSpace
        ctors <- P.option "" (parenthesize <$> parseEntityList)
        P.skipSpace
        return $ entity <> ctors
      P.skipSpace
      void $ P.char ')'
      P.skipSpace
      return $ sort entities

data Import = Import {
    imQualified      :: !Bool
  , imModule         :: !Text
  , imCaselessModule :: !Text
  , imAlias          :: !(Maybe Text)
  , imPackage        :: !(Maybe Text)
  , imImpSpec        :: !ImpSpec
  } deriving (Eq, Show)

compareImport :: Bool -> Import -> Import -> Ordering
compareImport ignore_qualified a b = mconcat
  [ if ignore_qualified then mempty else imQualified a `compare` imQualified b
  , imPackage a `compare` imPackage b
  , imCaselessModule a `compare` imCaselessModule b
  , imAlias a `compare` imAlias b
  , imImpSpec a `compare` imImpSpec b
  ]

parseImport :: P.Parser Import
parseImport = do
  void $ P.string "import"
  P.skipSpace
  is_qualified <- P.option False (P.string "qualified" *> P.skipSpace $> True)
  package <- P.option Nothing
                      (Just <$> (P.char '"' *> parsePkgId <* P.char '"') <* P.skipSpace)
  module_ <- P.takeWhile1 $ (not . isSpace) <&&> (/= '(')
  P.skipSpace
  alias <- P.option Nothing (Just <$> parseAlias)
  P.skipSpace
  impspec <- parseImpSpec
  return Import { imQualified      = is_qualified
                , imModule         = module_
                , imCaselessModule = T.toCaseFold module_
                , imAlias          = alias
                , imPackage        = package
                , imImpSpec        = impspec
                }
  where
    parsePkgId :: P.Parser Text
    parsePkgId = P.takeWhile1 $ (not . isSpace) <&&> (/= '"')

    parseAlias :: P.Parser Text
    parseAlias = do
      void $ P.string "as"
      P.skipSpace
      alias <- P.takeWhile1 $ (not . isSpace) <&&> (/= '(')
      P.skipSpace
      return alias

showImport :: Style -> Import -> Text
showImport Style {..} Import {..} = withImpSpec import_module
  where
    import_module = T.concat
      [ "import"
      , if imQualified
        then qualified_
        else if alignUnqualified then T.replicate (T.length qualified_) " " else ""
      , " "
      , case imPackage of
        Nothing  -> ""
        Just pkg -> "\"" <> pkg <> "\" "
      , imModule
      , case aliasAlignment of
        Just n | isJust imAlias ->
          T.replicate (max 0 $ n - 1 - T.length import_module) " "
        _ -> ""
      , maybe "" (" as " <>) imAlias
      ]

    qualified_ = " qualified"

    withImpSpec imp = if T.length oneline <= max_line_length then oneline else multiline

      where
        oneline = imp <> case imImpSpec of
          EmptyImpSpec -> ""
          Explicit es  -> " " <> parenthesize es
          Hiding   es  -> " hiding " <> parenthesize es

        multiline = imp <> case imImpSpec of
          EmptyImpSpec -> ""
          Explicit es  -> "\n" <> parenthesizeMultiLine es
          Hiding   es  -> " hiding\n" <> parenthesizeMultiLine es

        parenthesizeMultiLine :: [Text] -> Text
        parenthesizeMultiLine []       = "  ()"
        parenthesizeMultiLine entities = T.unlines $ unfoldr go entities'
          where
            entities' =
              (\l -> "( " <> head l : tail l)
                . (\l -> init l ++ [last l <> " )"])
                . (\l -> head l : map (", " <>) (tail l))
                $ entities

            buildLine [] _ = ("", [])
            buildLine (e : es) curlen
              | curlen < T.length e
              = ("", e : es)
              | otherwise
              = let (line', es') = buildLine es (curlen - T.length e)
                in  (e <> line', es')

            go [] = Nothing
            go (e : es) =
              -- Handle the case when 'e' is longer than max_line_length.
              let e0          = "  " <> e
                  (line, es') = buildLine es (max_line_length - T.length e0)
              in  Just (e0 <> line, es')

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
convert style@Style {..} modules source =
  T.unlines
    . concat
    $ [ reverse . dropWhile T.null $ reverse header
      , separator_if True
      , map (showImport style) first_import_group
      , separator_if $ not $ null first_import_group
      , map (showImport style) second_import_group
      , separator_if $ ((not . null $ second_import_group) && (not . null $ rest))
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
      InternalExternal -> partition ((`S.member` modules) . imModule) imports

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
      case (is_file && any (`isSuffixOf` path) extensions, is_dir) of
        (True , False) -> f acc fullpath
        (False, True ) -> foldThroughHsFiles fullpath f acc
        _              -> return acc
      where
        extensions = [".hs", ".hsc", ".lhs"]

        fullpath   = basepath ++ "/" ++ path

-- | Collect modules and file paths from given directories.
inspectDirectories :: [FilePath] -> IO (Set Text, [FilePath])
inspectDirectories dirs =
  foldM
      (\acc dir -> do
        putStrLn $ "Inspecting " ++ dir ++ "..."
        foldThroughHsFiles
          dir
          (\(!modules, !files) file -> do
            let module_ =
                  map slash_to_dot
                    . drop (length dir + 1)  -- remove base directory (+ slash)
                    . dropExtension
                    $ file
            putStrLn $ "Found " ++ file ++ " (" ++ module_ ++ ")."
            return (maybeInsert (T.pack module_) modules, file : files)
          )
          acc
      )
      (S.empty, [])
    $ map remove_last_slash dirs
  where
    maybeInsert mdl mdlSet =
      if mdl `elem` mdlWhitelist then mdlSet else S.insert mdl mdlSet
    -- We have a custom Prelude, but it should be considered
    -- non-local.
    mdlWhitelist = ["Prelude"]

    slash_to_dot '/' = '.'
    slash_to_dot c   = c

    remove_last_slash []       = []
    remove_last_slash ['/'   ] = []
    remove_last_slash (c : cs) = c : remove_last_slash cs

-- | Sort import lists in files at given locations.
sortImports :: Style -> String -> [FilePath] -> IO ()
sortImports style suffix dirs = do
  (modules, files) <- inspectDirectories dirs
  forM_ files $ \file -> do
    putStr $ "Sorting imports in " ++ file ++ "..."
    T.readFile file >>= rewriteFile (file ++ suffix) . convert style modules
    putStrLn " done."

-- | Write a file, but only if it would have new content, thus
-- preserving the modification date.
rewriteFile :: FilePath -> Text -> IO ()
rewriteFile fileName newContent = do
  fileExists <- doesFileExist fileName
  if fileExists
    then do
      existingContent <- T.readFile fileName
      unless (existingContent == newContent) $ T.writeFile fileName newContent
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
  _            <- setupAppPaths

  (args, dirs) <- partition is_config_option <$> getArgs
  if null dirs
    then do
      prog <- getProgName
      putStrLn
        $ "Usage: "
        ++ prog
        ++ " [--check] [--suffix=SUFFIX] [--align-unqualified]\
           \ [--alias-alignment=N]\
           \ [--import-grouping=none|external-internal|internal-external]\
           \ <directories>"
    else do
      let (check, style, suffix) = get_options args
      putStrLn $ "Using " ++ show style
      if not check
        then sortImports style suffix dirs
        else onException (checkConsistency style dirs) $ do
          putStrLn $ "Run SortImports.sh without --check to fix the problem."
  where
    is_config_option      = ("--" `isPrefixOf`)

    opt_check             = "--check"
    opt_suffix            = "--suffix="

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
