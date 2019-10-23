{-# LANGUAGE NoImplicitPrelude #-}
module Main where

-- TODO:
-- regex currently parses valid tokens like 'localization2' as 'localization',
-- which results in a false-positive.

import Control.Monad (forM_)
import Data.Foldable (foldlM)
import Data.List (intersperse, isPrefixOf, isSuffixOf, unfoldr)
import Data.Maybe
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import System.Directory
import System.Exit
import System.IO
import Text.Regex.TDFA
import qualified Data.Map as Map

import AppDir (setupAppPaths)
import ScriptsPrelude

------------------------------
-- utils
splitEithers :: [Either a b] -> ([a], [b])
splitEithers [] = ([], [])
splitEithers (e : es) =
  let (xs, ys) = splitEithers es
  in  case e of
        Left  x -> (x : xs, ys)
        Right y -> (xs, y : ys)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

-- Union of two maps or an error for some key that's present in both
-- maps (and its values),
disjointUnion
  :: Ord k => (k -> a -> a -> x) -> Map.Map k a -> Map.Map k a -> Either x (Map.Map k a)
disjointUnion f m1 m2 = case Map.minViewWithKey intersection of
  Just ((k, v), _) -> Left $ f k v $ m2 Map.! k
  Nothing          -> Right $ m1 `Map.union` m2
  where intersection = m1 `Map.intersection` m2

disjointUnions :: Ord k => (k -> a -> a -> x) -> [Map.Map k a] -> Either x (Map.Map k a)
disjointUnions f = foldlM (disjointUnion f) Map.empty
------------------------------
-- Localizations

-- string from js code, e.g. "$_email()$"
type Template = String

--  | Localization is a tree, where leaves are templates and branches are
-- [nested] dictionaries from string names to localizations.
--
-- E.g. this code:
-- @
-- var foo = { bar: 'baz', quux: {corge: 'grault'}};
-- @
--
-- corresponds to the following value:
--
-- @
-- Object "foo" (Map.fromList [("bar", Value "baz"), ("quux", Object
-- (Map.fromList [("corge", Value "grault")]))])
-- @
data Localization = Value Template
                  | Object (Map.Map String Localization)

-- | Recursively remove internal nodes that have no leaves.
pruneLocalization :: Localization -> Localization
pruneLocalization l@(Value  _) = l
pruneLocalization (  Object m) = Object $ Map.fromList $ filter
  (\(_, v) -> nonEmptyElem v)
  prunnedElems
  where
    elems        = Map.toList m
    prunnedElems = map (\(k, v) -> (k, pruneLocalization v)) elems
    nonEmptyElem (Value  _ ) = True
    nonEmptyElem (Object m') = not $ Map.null m'

instance Show Localization where
  show = aux 0
    where
      aux _ (Value t) = "\"" ++ t ++ "\""
      aux n (Object m) =
        let foo (k, l) = concat (replicate n " ") ++ k ++ ": " ++ aux (n + 4) l
        in  unlines $ "" : (map foo $ Map.assocs m)

nullLocalization :: Localization -> Bool
nullLocalization (Value  _) = False
nullLocalization (Object m) = all nullLocalization $ Map.elems m

-- | Intersect two localizations.
--
-- This assumes that the the two localizations have the same shapes
-- (of internal branches) but one of them could miss some leaves must
-- be called on localizations with Object ctor.
intersectLocalizations :: Localization -> Localization -> Localization
intersectLocalizations (Object m1) (Object m2) = Object $ Map.fromList items'
  where
    items  = Map.toList m1
    items' = catMaybes $ map aux items

    aux x@(k, Value _     ) = if k `Map.member` m2 then Just x else Nothing

    aux (  k, m@(Object _)) = let m' = m2 Map.! k in Just (k, intersectLocalizations m m')

intersectLocalizations _ _ = error "intersectLocalizations: impossible"

------------------------------
-- | Parsing Localization from JavaScript.
propsToLocalization :: String -> JSObjectPropertyList -> Either String Localization
propsToLocalization path props = do
  subProps <- mapM aux propsList
  Object <$> disjointUnions merger subProps
  where
    propsList =
      let toList JSLNil             = []
          toList (JSLOne a        ) = [a]
          toList (JSLCons l _ann a) = a : toList l
      in  case props of
            JSCTLComma l _ann -> toList l
            JSCTLNone l       -> toList l

    aux (JSPropertyNameandValue (JSPropertyIdent _ann varName) _ann' [JSStringLiteral _ann'' template])
      = return $ Map.singleton varName $ Value template
    aux (JSPropertyNameandValue (JSPropertyIdent _ann varName) _ann' [JSObjectLiteral _ann'' props' _ann'''])
      = do
        children <- propsToLocalization (path ++ "." ++ varName) props'
        return $ Map.singleton varName children
    aux (JSPropertyNameandValue (JSPropertyIdent _ann varName) _ann' [JSFunctionExpression _ _ _ _ _ _])
      = return $ Map.singleton varName $ Value "<function>"
    aux (JSPropertyNameandValue (JSPropertyIdent _ann varName) _ann' [JSArrayLiteral _ _ _])
      = return $ Map.singleton varName $ Value "<array>"
    aux _ = return $ Map.empty

    merger k x y = concat
      [ "Multiple values (in file 'javascript-langs.st') for key '"
      , path
      , "."
      , k
      , "': "
      , show x
      , ", and "
      , show y
      ]

localizationsFromFile :: FilePath -> IO Localization
localizationsFromFile path = do
  template <- readStFile path
  let Right jsast               = parse template path
      JSAstProgram stmts _annot = jsast
      JSAssignStatement _expr _op obj _semi = head stmts
      JSObjectLiteral _annot' props _annot'' = obj
  case propsToLocalization "localization" props of
    Left  e            -> hPutStrLn stderr e >> exitFailure
    Right localization -> return $ Object $ Map.singleton "localization" localization
  where
    readStFile fname =
      unlines . tail . filter (not . ("#" `isPrefixOf`)) . lines <$> readFile fname

------------------------------
-- Localization calls.

-- | Final element of localization call.
--
-- E.g. X in 'localization.foo.bar.X'.
data LocalizationCallFinalElem = Attribute String  -- X = baz
                               | FunCall String    -- X = baz(
                               | DictAccess String -- X = baz[

-- | Localization call is a lvalue usage of values from localization objects,
-- so if there's something like this in a file.js:
-- @
-- box.html(localization.foo.bar);
-- @
-- we extract from that (34 is a line number of ^^ line):
-- @
-- LocalizationCall ["localization", "foo"] (Attribute "bar") "file.js" 34
-- @
data LocalizationCall =
  LocalizationCall [String] LocalizationCallFinalElem FilePath Int

-- | Parsing.
localizationCallFromString :: String -> FilePath -> Int -> LocalizationCall
localizationCallFromString s = LocalizationCall path finalElem
  where
    aux ("", ""     ) = Nothing
    aux (x , ""     ) = Just (x, "")
    aux (x , '.' : y) = Just (x, y)
    aux _             = Nothing

    path'      = unfoldr (aux . break (== '.')) s
    path       = init path'
    finalElem' = last path'
    finalElem  = case last finalElem' of
      '[' -> DictAccess $ init finalElem'
      '(' -> FunCall $ init finalElem'
      _   -> Attribute finalElem'


-- | Remove one leaf (matching localization call) from a localization.
--
-- Localization must be an Object, and localization call cannot be empty.
--
-- Possible errors:
--  * localization call points to non-existing value in a localization.
--  * localization call points to a whole [sub]dictionary of the localization
--  * dict-like access to simple values (e.g. localization call
--    'foo.bar' for localization '{foo: "quuz"}')
removeLocalizationCallFromLocalization
  :: LocalizationCall -> Localization -> Either String Localization
removeLocalizationCallFromLocalization (LocalizationCall path finalElem filePath' lineNumber) loc
  = aux path loc
  where
    myError s =
      Left
        $  filePath'
        ++ ":"
        ++ show lineNumber
        ++ ": "
        ++ s
        ++ " (from call: '"
        ++ wholePathFormatted
        ++ "')"
    wholePathFormatted =
      format
        $  path
        ++ [ case finalElem of
               DictAccess x -> x ++ "["
               FunCall    x -> x ++ "("
               Attribute  x -> x
           ]
    format = concat . intersperse "."

    auxFinalElemFunCall m functionName = case Map.lookup functionName m of
      Nothing ->
        myError
          $  "Detected function/method like access to non-existing key '"
          ++ functionName
          ++ "'"
      Just (Object _) ->
        myError
          $  "Detected function/method like access to a sub-dict '"
          ++ functionName
          ++ "'"
      Just (Value "<function>") -> Right $ Object $ Map.delete functionName m
      Just (Value "<array>") ->
        myError
          $  "Detected function/method like access to an array '"
          ++ functionName
          ++ "'"
      Just (Value _) ->
        myError
          $  "Detected function/method like access to a string '"
          ++ functionName
          ++ "'"

    auxFinalElemDictAccess m dictName = case Map.lookup dictName m of
      Nothing ->
        myError
          $  "Detected dict/array like access to non-existing key '"
          ++ dictName
          ++ "'"
      Just (Object _) ->
        myError
          $  "Detected dict access ("
          ++ dictName
          ++ "), probably using dynamic keys. "
      Just (Value "<function>") ->
        myError $ "Detected dict/array like access to a function '" ++ dictName ++ "'"
      Just (Value "<array>") -> Right $ Object $ Map.delete dictName m
      Just (Value _) ->
        myError $ "Detected dict/array like access to a string '" ++ dictName ++ "'"

    auxFinalElemAttribute m attr = case Map.lookup attr m of
      Nothing         -> myError $ "Detected access to non-existing key '" ++ attr ++ "'"
      Just (Object _) -> myError $ "Detected access to a whole sub-dict '" ++ attr
      Just (Value "<function>") ->
        myError $ "Detected attribute access to a function '" ++ attr ++ "'"
      Just (Value "<array>") ->
        myError $ "Detected attribute access to an array '" ++ attr ++ "'"
      Just (Value _) -> Right $ Object $ Map.delete attr m

    auxEmptyChildrenFunCall m v node = case v of
      "<function>" -> -- "foo.quux(" "{foo: function() {}}"
        myError $ "Detected function call on a function '" ++ format path ++ "'"
      "<array>" -> -- "foo.quux(" "{foo: []}"
        myError $ "Detected function call on an array '" ++ format path ++ "'"
      _ -> -- "foo.quux(" "{foo: 'bar'}", string method call
        return $ Object $ Map.delete node m

    auxEmptyChildrenDictAccess v = case v of
      "<function>" -> -- "foo.quux[" "{foo: function() {}}"
        myError
          $  "Detected attribute access "
          ++ "(followed by a key retrieval)"
          ++ "to a function '"
          ++ format path
          ++ "'"
      "<array>" -> -- "foo.quux[" "{foo: []}"
        myError
          $  "Detected attribute access "
          ++ "(followed by a key retrieval) "
          ++ "to an array '"
          ++ format path
          ++ "'"
      _ -> -- "foo.quux[" "{foo: 'bar'}"
        myError
          $  "Detected attribute access "
          ++ "(followed by a key retrieval) "
          ++ "to a string '"
          ++ format path
          ++ "'"

    auxEmptyChildrenAttribute v = case v of
      "<function>" -> -- "foo.quux" "{foo: function() {}}"
        myError $ "Detected attribute access to a function '" ++ format path ++ "'"
      "<array>" -> -- "foo.quux" "{foo: []}"
        myError $ "Detected attribute access to an array '" ++ format path ++ "'"
      _ -> -- "foo.quux" "{foo: 'bar'}"
        myError $ "Detected attribute access to a string '" ++ format path ++ "'"

    aux _  (Value  _) = error "Trying to remove from Value Localization"
    aux [] (Object m) = case finalElem of
      FunCall    functionName -> auxFinalElemFunCall m functionName
      DictAccess dictName     -> auxFinalElemDictAccess m dictName
      Attribute  attr         -> auxFinalElemAttribute m attr

    aux (node : children) (Object m) = case Map.lookup node m of
      Nothing        -> myError $ "Detected access to non-existing key '" ++ node ++ "'"
      Just (Value v) -> -- "foo....QUUX" "{foo: v}"
                        case children of
        [] -> -- "foo.QUUX" "{foo: v}"
              case finalElem of
          FunCall _ -> -- "foo.quux(" "{foo: v}"
            auxEmptyChildrenFunCall m v node
          DictAccess _ -> -- "foo.quux[" "{foo: v}"
            auxEmptyChildrenDictAccess v
          Attribute _ -> -- "foo.quux" "{foo: v}"
            auxEmptyChildrenAttribute v
        (_ : _) -> -- "foo....QUUX" "{foo: v}"
          let badPrefix = take (length path - length children) path
          in  myError
                $  "Detected dict-like acces ('"
                ++ format path
                ++ "'), but '"
                ++ format badPrefix
                ++ "' is a simple value'"
      Just l@(Object _) -> do
        y <- aux children l
        return $ Object $ Map.alter (const $ Just y) node m

-- | Parse localization calls from a list of files.
readLocalizations :: [FilePath] -> IO [LocalizationCall]
readLocalizations paths = concat <$> mapM aux paths
  where
    aux path = do
      contents <- readFile path
      let numberedLines     = zip [1 ..] $ lines contents
          localizationRegex = "localization(\\.[a-zA-Z0-9_]+)+[([]?" :: String
          results           = concatMap
            (\(lineNumber, line) ->
              map (\x -> (x, path, lineNumber))
                $  getAllTextMatches
                $  line
                =~ localizationRegex
            )
            numberedLines
      return $ map (uncurry3 localizationCallFromString) results

main :: IO ()
main = do
  _                <- setupAppPaths

  mainLocalization <- localizationsFromFile "templates/javascript-langs.st"
  normal_js_files  <- filter (".js" `isSuffixOf`)
    <$> directoryFilesRecursive "frontend/app/js"
  jsx_compiled_files <- filter (".jsx" `isSuffixOf`)
    <$> directoryFilesRecursive "frontend/app/scripts"
  templates_files <- filter (".st" `isSuffixOf`) <$> directoryFilesRecursive "templates"
  let files = normal_js_files ++ jsx_compiled_files ++ templates_files
  localizationCalls <- readLocalizations files
  let
    results = map (\lc -> removeLocalizationCallFromLocalization lc mainLocalization)
                  localizationCalls
    (logs, cleanedLocalizations) = splitEithers results
    unusedLocalization =
      foldl intersectLocalizations mainLocalization cleanedLocalizations
  hPutStrLn stderr "Warnings:"
  forM_ logs $ hPutStrLn stderr
  putStrLn "******************************"
  putStrLn "Unused localization calls:"
  let prunedLocalization = pruneLocalization unusedLocalization
  print prunedLocalization
  let noWarnings = null logs
      noUnused   = nullLocalization prunedLocalization
  if noWarnings && noUnused then exitSuccess else exitFailure

-- UTILS

directoryEntriesRecursive
  :: FilePath                    -- ^ dir path to be searched for recursively
  -> IO ([FilePath], [FilePath]) -- ^ (list of all subdirs, list of all files)
directoryEntriesRecursive path
  | "." `isSuffixOf` path = return ([], [])
  | otherwise = do
    isDir <- doesDirectoryExist path
    if isDir
      then do
        entries <- getDirectoryContents path
        let properEntries = map ((path ++ "/") ++) entries
        results <- mapM directoryEntriesRecursive properEntries
        let (dirs, files) = biConcat results
        return (path : dirs, files)
      else return ([], [path])
  where biConcat = (\(x, y) -> (concat x, concat y)) . unzip

directoryFilesRecursive
  :: FilePath -- ^ dir path to be searched for recursively
  -> IO [FilePath] -- ^ list of all files in that dir
directoryFilesRecursive path = snd `fmap` directoryEntriesRecursive path
