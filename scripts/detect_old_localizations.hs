-- you need language-javascript-0.5.7 to use this script
-- usage: runhaskell scripts/detect_old_localizations.hs

module Main where

import Data.List (isPrefixOf, unfoldr, intersperse)
import Data.Foldable (foldlM)
import Data.Maybe
import Control.Monad (forM)
import Control.Applicative
import Language.JavaScript.Parser
import Text.Regex.TDFA
import System.FilePath.Find
import qualified Data.Map as Map
import System.IO
import System.Exit

------------------------------
-- utils
splitEithers :: [Either a b] -> ([a], [b])
splitEithers [] = ([], [])
splitEithers (e:es) =
    let (xs, ys) = splitEithers es
    in case e of
         Left x -> (x:xs, ys)
         Right y -> (xs, y:ys)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

dropPrefix :: Eq a => [a] -> [a] -> [a]
dropPrefix prefix s | prefix `isPrefixOf` s = drop (length prefix) s
                    | otherwise             = s

-- union of two maps or an error for some key that's present in both maps (and its values)
disjointUnion :: Ord k => (k -> a -> a -> x) -> Map.Map k a -> Map.Map k a -> Either x (Map.Map k a)
disjointUnion f m1 m2 = case Map.minViewWithKey intersection of
                          Just ((k, v), _) -> Left $ f k v $ m2 Map.! k
                          Nothing -> Right $ m1 `Map.union` m2
    where intersection = m1 `Map.intersection` m2

disjointUnions :: Ord k => (k -> a -> a -> x) -> [Map.Map k a] -> Either x (Map.Map k a)
disjointUnions f = foldlM (disjointUnion f) Map.empty
------------------------------
-- Localizations

-- string from js code, e.g. "$_email()$"
type Template = String

-- Localization is a tree, where leaves are templates
-- and branches are [nested] dictionaries from string names to localizations
-- e.g. this code:
-- var foo = { bar: 'baz', quux: {corge: 'grault'}};
-- corresponds to the following value:
-- Object "foo" (Map.fromList [("bar", Value "baz"), ("quux", Object (Map.fromList [("corge", Value "grault")]))])
data Localization = Value Template
                  | Object (Map.Map String Localization)

instance Show Localization where
    show = aux 0
        where aux _ (Value t) = "\"" ++ t ++ "\""
              aux n (Object m) =
                  let foo (k, l) = concat (replicate n " ") ++ k ++ ": " ++ aux (n+4) l
                  in unlines $ "": (map foo $ Map.assocs m)

-- intersect two localizations
-- this assumes that the the two localizations have the same shapes (of internal branches)
-- but one of them could miss some leaves
-- must be called on localizations with Object ctor
intersectLocalizations :: Localization -> Localization -> Localization
intersectLocalizations (Object m1) (Object m2) = Object $ Map.fromList items'
    where items = Map.toList m1
          aux x@(k, Value _) = if k `Map.member` m2 then
                                   Just x
                               else
                                   Nothing
          aux (k, m@(Object _)) =
              let m' = m2 Map.! k
              in Just (k, intersectLocalizations m m')
          items' = catMaybes $ map aux items
intersectLocalizations _ _ = undefined

-- diff two localizations (remove leaves present in the second localization from the first one)
-- this assumes that the the two localizations have the same shapes (of internal branches)
-- must be called on localizations with Object ctor
diffLocalization :: Localization -> Localization -> Localization
diffLocalization (Object m1) (Object m2) = Object $ Map.fromList items'
    where items = Map.toList m1
          aux x@(k, Value _) = if not $ k `Map.member` m2 then
                                   Just x
                               else
                                   Nothing
          aux (k, m@(Object _)) =
              let m' = m2 Map.! k
              in Just (k, diffLocalization m m')
          items' = catMaybes $ map aux items
diffLocalization _ _ = undefined

------------------------------
-- parsing Localization from javascript
propsToLocalization :: String -> [JSNode] -> Either String Localization
propsToLocalization path props = do
  subProps <- mapM aux props
  Object <$> disjointUnions merger subProps
    where aux (NN (JSPropertyNameandValue (NT (JSIdentifier varName) _ _) _ [NT (JSStringLiteral _ template) _ _])) =
              return $ Map.singleton varName $ Value template
          aux (NN (JSPropertyNameandValue (NT (JSIdentifier varName) _ _) _ [NN (JSObjectLiteral _ props' _)])) = do
              children <- propsToLocalization (path ++ "." ++ varName) props'
              return $ Map.singleton varName children
          aux (NN (JSPropertyNameandValue (NT (JSIdentifier varName) _ _) _ [NN (JSFunctionExpression _ _ _ _ _ _)])) =
              return $ Map.singleton varName $ Value "<function>"
          aux (NN (JSPropertyNameandValue (NT (JSIdentifier varName) _ _) _ [NN (JSArrayLiteral _ _ _)])) =
              return $ Map.singleton varName $ Value "<array>"
          aux _ = return $ Map.empty
          merger k x y = concat [ "Multiple values (in file 'javascript-langs.st') for key '"
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
  langsTemplate <- readFile path
  let template = unlines $ tail $ filter (not . ("#" `isPrefixOf`)) $ lines langsTemplate
      node = parse template path
      Right (NN top) = node
      JSSourceElementsTop [NN vars, _] = top
      JSVariables _ [NN vardecl] _ = vars
      JSVarDecl _ [_, NN obj] = vardecl
      JSObjectLiteral _ props _ = obj
  case propsToLocalization "localization" props of
    Left e -> hPutStrLn stderr e >> exitFailure
    Right localization -> return $ Object $ Map.singleton "localization" localization

------------------------------
-- localization calls

-- localization call is a lvalue usage of values from localization objects
-- so if there's something like this in a file.js:
-- box.html(localization.foo.bar);
-- we extract from that (34 is a line number of ^^ line):
-- LocalizationCall ["localization", "foo", "bar"] "file.js" 34
data LocalizationCall = LocalizationCall [String] FilePath Int

-- parsing
localizationCallFromString :: String -> FilePath -> Int -> LocalizationCall
localizationCallFromString s = LocalizationCall (unfoldr (aux . break (== '.')) s)
    where aux ("", "") = Nothing
          aux (x, "") = Just (x, "")
          aux (x, '.':y) = Just (x, y)
          aux _ = Nothing

-- remove one leaf (matching localization call) from a localization
-- localization must be an Object, and localization call cannot be empty
-- errors:
-- localization call points to non-existing value in a localization
-- localization call points to a whole [sub]dictionary of the localization
-- dict-like access to simple values (e.g. localization call 'foo.bar' for localization '{foo: "quuz"}')
removeLocalizationCallFromLocalization :: LocalizationCall -> Localization -> Either String Localization
removeLocalizationCallFromLocalization (LocalizationCall path filePath' lineNumber) loc = aux path loc
    where myError s = Left $ s ++ " at " ++ filePath' ++ ":" ++ show lineNumber ++ " (from call: '" ++ format path ++ "')"
          format = concat . intersperse "."

          aux [] _ = error "Trying to remove empty path"
          aux _ (Value _) = error "Trying to remove from Value Localization"

          aux [node] (Object m) =
              if last node == '(' then
                  -- looks like a function call
                  let functionName = init node
                  in case Map.lookup functionName m of
                       Nothing -> myError $ "Detected function/method like access to non-existing key '" ++ node ++ "'"
                       Just (Object _) -> myError $ "Detected function/method like access to a sub-dict from localization '" ++ node ++ "'"
                       Just (Value "<function>") -> Right $ Object $ Map.delete node m
                       Just (Value _) -> Right $ Object $ Map.delete node m -- functionName is probably a string method
              else
                  case Map.lookup node m of
                    Nothing -> myError $ "Detected access to non-existing key '" ++ node ++ "'"
                    Just (Object _) -> myError $ "Detected access to a whole dict from localization '" ++ node ++ "'"
                    Just (Value _) -> Right $ Object $ Map.delete node m

          aux (node:children) (Object m) =
              case Map.lookup node m of
                Nothing -> myError $ "Detected access to non-existing key '" ++ node ++ "'"
                Just l@(Object _) -> do
                  y <- aux children l
                  return $ Object $ Map.alter (const $ Just y) node m
                Just (Value _) ->
                    case children of
                      [finalNode] | last finalNode == '(' -> Right $ Object $ Map.delete node m -- method is immediately called
                      _ -> let badPrefix = take (length path - length children) path
                          in myError $ "Detected dict-like acces ('" ++ format path ++ "'), but '" ++ format badPrefix ++ "' is a simple value'"

-- parse localization calls from a list of files
readLocalizations :: [FilePath] -> IO [LocalizationCall]
readLocalizations paths = concat <$> mapM aux paths
    where aux path = do
            contents <- readFile path
            let numberedLines = zip [1..] $ lines contents
                localizationRegex = "localization(\\.[a-zA-Z0-9_]+)*\\(?" :: String
                results = map (\(lineNumber, line) -> (line =~ localizationRegex, path, lineNumber)) numberedLines
                properResults = filter (\(line, _, _) -> not $ null line) results -- filter out lines that don't match the regex
            return $ map (uncurry3 localizationCallFromString) properResults

main :: IO ()
main = do
  mainLocalization <- localizationsFromFile "templates/javascript-langs.st"
  files <- find (return True) (extension ==? ".js") "public/js"
  localizationCalls <- readLocalizations files
  let results = map (flip removeLocalizationCallFromLocalization mainLocalization) localizationCalls
      (logs, cleanedLocalizations) = splitEithers results
      unusedLocalization = foldl intersectLocalizations mainLocalization cleanedLocalizations
  _ <- forM logs $ \warn -> hPutStrLn stderr $ "Warning: " ++ warn
  putStrLn "******************************"
  putStrLn "Unused localization calls:"
  print unusedLocalization
