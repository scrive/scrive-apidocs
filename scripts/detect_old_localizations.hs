-- you need language-javascript-0.5.7 to use this script
-- usage: runhaskell scripts/detect_old_localizations.hs

module Main where

-- TODO:
-- regex currently parses valid tokens like 'localization2' as 'localization',
-- which results in a false-positive.

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
-- WHITELIST
whitelist :: Localization
whitelist =
    o[("localization",
      o[ ("statusToolTip", o[ ("draft", d)
                            , ("cancelled", d)
                            , ("sent", d)
                            , ("delivered", d)
                            , ("reviewed", d)
                            , ("read", d)
                            , ("opened", d)
                            , ("signed", d)
                            , ("rejected", d)
                            , ("deliveryproblem", d)
                            , ("timeouted", d)
                            , ("problem", d)
                            ])
       , ("process", o[ ("contract", o[("shortName", d)])
                      , ("offer", o[("shortName", d)])
                      , ("order", o[("shortName", d)])
                      ])
       , ("signatoryMessage", o[ ("datamismatch", d)
                               , ("timedout", d)
                               , ("rejected", d)
                               , ("seen", d)
                               , ("read", d)
                               , ("delivered", d)
                               , ("signed", d)
                               , ("cancelled", d)
                               , ("other", d)
                               , ("waitingForSignature", d)
                               ])
       , ("docsignview", o[ ("newAccountTitle", d)
                          , ("newAccountSubTitle", d)
                          , ("emailLabel", d)
                          , ("passwordLabel", d)
                          , ("password2Label", d)
                          , ("acceptTOSLabel", d)
                          , ("newAccountButton", d)
                          , ("createdAccountTitle", d)
                          , ("createdAccountSubtitle", d)
                          , ("shareTitle", d)
                          , ("facebookButtonLabel", d)
                          , ("tweetButtonLabel", d)
                          , ("phoneButtonLabel", d)
                          , ("phoneFormDescription", d)
                          , ("phoneSubmitButtonLabel", d)
                          , ("phoneConfirmationText", d)
                          , ("startButtonLabel", d) 
                          ])
       , ("payments", o[ ("plans", o[ ("team", o[ ("name", d)
                                               , ("tag", d)
                                               , ("price", d)
                                               , ("price3", d)])
                                   , ("form", o[ ("name", d)
                                               , ("tag", d)
                                               , ("price", d)
                                               , ("price3", d)])
                                   , ("enterprise", o[ ("name", d)
                                                     , ("tag", d)
                                                     , ("price", d)
                                                     , ("price3", d)])
                                   ])
                       , ("errors", o[ ("acceptTOS", d)
                                     , ("cardDeclined", d)
                                     , ("emptyField", d)
                                     , ("invalidCC", d)
                                     , ("invalidCVV", d)
                                     , ("invalidCoupon", d)
                                     , ("invalidEmail", d)
                                     , ("invalidQuantity", d)
                                     , ("missingFullAddress", d)
                                     ])])
       , ("apiDashboard", o[ ("apiTokenCreate", d)
                           , ("apiTokenDescription", d)
                           , ("deleteAll", d)
                           , ("deleteToken", d)
                           , ("grantedPriviligesDescription", d)
                           , ("personOrCompany", d)
                           , ("personalTokenCreate", d)
                           , ("personalTokenDescription", d)
                           , ("privilige", d)])
       ]
      )
     ]
        where d = Value "" -- dummy value
              o = Object . Map.fromList

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

-- recursively remove internal nodes that have no leaves
pruneLocalization :: Localization -> Localization
pruneLocalization l@(Value _) = l
pruneLocalization (Object m) = Object $ Map.fromList $ filter (\(_, v) -> nonEmptyElem v) prunnedElems
    where elems = Map.toList m
          prunnedElems = map (\(k, v) -> (k, pruneLocalization v)) elems
          nonEmptyElem (Value _) = True
          nonEmptyElem (Object m') = not $ Map.null m'

-- get key, must be called on Objects and with existing keys
(!) :: Localization -> String -> Localization
(!) (Value _) _ = error "(!) must be called on Object localization"
(!) (Object m) k = m Map.! k

nullLocalization :: Localization -> Bool
nullLocalization (Value _) = False
nullLocalization (Object m) = Map.null m

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
              if k `Map.member` m2 then
                  let m' = m2 Map.! k
                  in Just (k, diffLocalization m m')
              else
                  Just (k, m)
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

-- final element of localization call
-- e.g. X in 'localization.foo.bar.X'
data LocalizationCallFinalElem = Attribute String  -- X = baz
                               | FunCall String    -- X = baz(
                               | DictAccess String -- X = baz[

-- localization call is a lvalue usage of values from localization objects
-- so if there's something like this in a file.js:
-- box.html(localization.foo.bar);
-- we extract from that (34 is a line number of ^^ line):
-- LocalizationCall ["localization", "foo"] (Attribute "bar") "file.js" 34
data LocalizationCall = LocalizationCall [String] LocalizationCallFinalElem FilePath Int

-- parsing
localizationCallFromString :: String -> FilePath -> Int -> LocalizationCall
localizationCallFromString s = LocalizationCall path finalElem
    where aux ("", "") = Nothing
          aux (x, "") = Just (x, "")
          aux (x, '.':y) = Just (x, y)
          aux _ = Nothing

          path' = unfoldr (aux . break (== '.')) s
          path = init path'
          finalElem' = last path'
          finalElem = case last finalElem' of
                        '[' -> DictAccess $ init finalElem'
                        '(' -> FunCall $ init finalElem'
                        _ -> Attribute finalElem'


-- remove one leaf (matching localization call) from a localization
-- localization must be an Object, and localization call cannot be empty
-- errors:
-- localization call points to non-existing value in a localization
-- localization call points to a whole [sub]dictionary of the localization
-- dict-like access to simple values (e.g. localization call 'foo.bar' for localization '{foo: "quuz"}')
removeLocalizationCallFromLocalization :: LocalizationCall -> Localization -> Localization -> Either String Localization
removeLocalizationCallFromLocalization (LocalizationCall path finalElem filePath' lineNumber) whiteList loc = aux path loc whiteList
    where myError s = Left $ filePath' ++ ":" ++ show lineNumber ++ ": " ++ s ++ " (from call: '" ++ wholePathFormatted ++ "')"
          wholePathFormatted = format $ path ++ [case finalElem of
                                                   DictAccess x -> x ++ "["
                                                   FunCall x -> x ++ "("
                                                   Attribute x -> x]
          format = concat . intersperse "."

          aux _ (Value _) _ = error "Trying to remove from Value Localization"
          aux [] (Object m) wl =
              case finalElem of
                FunCall functionName ->
                    case Map.lookup functionName m of
                      Nothing -> myError $ "Detected function/method like access to non-existing key '" ++ functionName ++ "'"
                      Just (Object _) -> myError $ "Detected function/method like access to a sub-dict '" ++ functionName ++ "'"
                      Just (Value "<function>") -> Right $ Object $ Map.delete functionName m
                      Just (Value "<array>") -> myError $ "Detected function/method like access to an array '" ++ functionName ++ "'"
                      Just (Value _) -> myError $ "Detected function/method like access to a string '" ++ functionName ++ "'"
                DictAccess dictName ->
                    case Map.lookup dictName m of
                      Nothing -> myError $ "Detected dict/array like access to non-existing key '" ++ dictName ++ "'"
                      Just l@(Object _) -> myError $
                          if nullLocalization $ pruneLocalization $ l `diffLocalization` (wl ! dictName) then
                              "(WHITELISTED) Detected dict access (" ++ dictName ++ "), probably using dynamic keys."
                          else
                              "Detected dict access (" ++ dictName ++ "), probably using dynamic keys. Please manually add needed keys to the whitelist"
                      Just (Value "<function>") -> myError $ "Detected dict/array like access to a function '" ++ dictName ++ "'"
                      Just (Value "<array>") -> Right $ Object $ Map.delete dictName m
                      Just (Value _) -> myError $ "Detected dict/array like access to a string '" ++ dictName ++ "'"
                Attribute attr ->
                    case Map.lookup attr m of
                      Nothing -> myError $ "Detected access to non-existing key '" ++ attr ++ "'"
                      Just (Object _) -> myError $ "Detected access to a whole sub-dict '" ++ attr ++ "'. Please manually add needed keys to the whitelist."
                      Just (Value "<function>") -> myError $ "Detected attribute access to a function '" ++ attr ++ "'"
                      Just (Value "<array>") -> myError $ "Detected attribute access to an array '" ++ attr ++ "'"
                      Just (Value _) -> Right $ Object $ Map.delete attr m

          aux (node:children) (Object m) wl =
              case Map.lookup node m of
                Nothing -> myError $ "Detected access to non-existing key '" ++ node ++ "'"
                Just (Value v) -> -- "foo....QUUX" "{foo: v}"
                    case children of
                      [] -> -- "foo.QUUX" "{foo: v}"
                        case finalElem of
                          FunCall _ -> -- "foo.quux(" "{foo: v}"
                            case v of
                              "<function>" -> -- "foo.quux(" "{foo: function() {}}"
                                myError $ "Detected function call on a function '" ++ format path ++ "'"
                              "<array>" -> -- "foo.quux(" "{foo: []}"
                                myError $ "Detected function call on an array '" ++ format path ++ "'"
                              _ -> -- "foo.quux(" "{foo: 'bar'}", string method call
                                return $ Object $ Map.delete node m
                          DictAccess _ -> -- "foo.quux[" "{foo: v}"
                            case v of
                              "<function>" -> -- "foo.quux[" "{foo: function() {}}"
                                myError $ "Detected attribute access (followed by a key retrieval) to a function '" ++ format path ++ "'"
                              "<array>" -> -- "foo.quux[" "{foo: []}"
                                myError $ "Detected attribute access (followed by a key retrieval) to an array '" ++ format path ++ "'"
                              _ -> -- "foo.quux[" "{foo: 'bar'}"
                                myError $ "Detected attribute access (followed by a key retrieval) to a string '" ++ format path ++ "'"
                          Attribute _ -> -- "foo.quux" "{foo: v}"
                            case v of
                              "<function>" -> -- "foo.quux" "{foo: function() {}}"
                                myError $ "Detected attribute access to a function '" ++ format path ++ "'"
                              "<array>" -> -- "foo.quux" "{foo: []}"
                                myError $ "Detected attribute access to an array '" ++ format path ++ "'"
                              _ -> -- "foo.quux" "{foo: 'bar'}"
                                myError $ "Detected attribute access to a string '" ++ format path ++ "'"
                      _ -> -- "foo....QUUX" "{foo: v}"
                        let badPrefix = take (length path - length children) path
                        in myError $ "Detected dict-like acces ('" ++ format path ++ "'), but '" ++ format badPrefix ++ "' is a simple value'"
                Just l@(Object _) -> do
                  y <- aux children l $ wl ! node
                  return $ Object $ Map.alter (const $ Just y) node m

-- parse localization calls from a list of files
readLocalizations :: [FilePath] -> IO [LocalizationCall]
readLocalizations paths = concat <$> mapM aux paths
    where aux path = do
            contents <- readFile path
            let numberedLines = zip [1..] $ lines contents
                localizationRegex = "localization(\\.[a-zA-Z0-9_]+)*[([]?" :: String
                results = concatMap (\(lineNumber, line) -> map (\x -> (x, path, lineNumber)) $ getAllTextMatches $ line =~ localizationRegex) numberedLines
            return $ map (uncurry3 localizationCallFromString) results

main :: IO ()
main = do
  mainLocalization <- localizationsFromFile "templates/javascript-langs.st"
  files <- find (return True) (extension ==? ".js") "public/js"
  localizationCalls <- readLocalizations files
  let results = map (\lc -> removeLocalizationCallFromLocalization lc whitelist mainLocalization) localizationCalls
      (logs, cleanedLocalizations) = splitEithers results
      mainLocalizationWithWhiteList = mainLocalization `diffLocalization` whitelist
      unusedLocalization = foldl intersectLocalizations mainLocalizationWithWhiteList cleanedLocalizations
  hPutStrLn stderr "Warnings:"
  _ <- forM logs $ hPutStrLn stderr
  putStrLn "******************************"
  putStrLn "Unused localization calls:"
  print $ pruneLocalization unusedLocalization
