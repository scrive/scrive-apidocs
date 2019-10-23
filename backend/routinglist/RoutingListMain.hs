module RoutingListMain where

{-------------------------------------------------------------------------------
run this script as: cabal new-run routinglist <filepath> <default nginx rule> < input.json
e.g. cabal new-run routinglist /tmp/urls.txt "include /etc/nginx/includes/something;" < input.json

input.json should contain a dict mapping url strings to string with rules (they can contain newlines)
e.g.:
{"/some/url": "include /etc/nginx/includes/some_include;"}

This script will go through all the registered urls in kontrakcja, sort them
from most to least specific and output nginx config that looks like list of:

location ~ ^/some/url {
    include some_rule;
}

For from kontrakcja, this script will output a location rule for it,
containing rules from input.json, or the default rule if missing (taken from 2nd cli param)

Additionally, at the end it will list urls with rules that only exist in input.json,
but not in kontrakcja
-------------------------------------------------------------------------------}

import Data.Aeson (decode)
import Happstack.Server (Method, Response)
import Happstack.StaticRouting (Route)
import System.Environment (getArgs)
import System.IO
import Unsafe.Coerce (unsafeCoerce)
import qualified Control.Monad.State as S
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import AppDir (setupAppPaths)
import Kontra
import RoutingTable (staticRoutes)
import User.Lang

------------------------------------------------------------------------------
-- MyRoute is an exact copy of Route, but since Route's ctors are not exported
-- we use unsafeCoerce to get access to them
data MyRoute a =
    MyDir MySegment (MyRoute a)
  | MyParam (MyRoute a)
  | MyHandler MyEndSegment MyCheckApply a
  | MyChoice [MyRoute a]

instance Show (MyRoute a) where
    show (MyDir seg x) = "Dir " ++ show seg ++ " (" ++ show x ++ ")"
    show (MyParam x) = "Param (" ++ show x ++ ")"
    show (MyHandler segment _ _) = "Handler(" ++ show segment ++ ")"
    show (MyChoice xs) = "Choice (" ++ show xs  ++ ")"

data MySegment = MyStringS String
               | MyParams

instance Show (MySegment) where
    show (MyStringS s) = s
    show MyParams = "Params"

type MyEndSegment = (Maybe Int, Method)

type MyCheckApply = [String] -> Bool

coerce :: Route a -> MyRoute a
coerce = unsafeCoerce
------------------------------------------------------------------------------
-- go over Route object (recursively) and return list of urls inside
-- this a stateful computation (for counting how many parameters are in the url)
worker :: MyRoute (Kontra Response) -> S.State Int [String]
worker (MyDir _segment (MyHandler (Nothing, _) _ _)) = return [] -- [segment ++ " <SERVING FILES FROM SOME DIRECTORY>"]
worker (MyDir segment (MyHandler (Just n, _method') _ _)) = do
  nParams <- S.get
  let n' = n - nParams
  case n' of
    0 -> return [show segment]
    _ -> return [show segment ++ "/"]
worker (MyDir segment route) = mapM (\s -> return $ show segment ++ "/" ++ s) =<< worker route
worker (MyHandler (Nothing, _) _ _) = return [] -- ["<SERVING FILES FROM SOME DIRECTORY>"]
worker (MyHandler (Just n, _method') _ _) = do
  nParams <- S.get
  let n' = n - nParams
  case n' of
    0 -> return [""]
    -- this will happen for rule without dir part inside choice
    -- e.g. dir "a" $ choice [ hGet $ tok1 $ foo]
    -- empty string should be ok in this case, might review this later
    _ -> return [""]
worker (MyChoice routes) = do
  let localState :: S.State s a -> S.State s a
      localState f = do
        s <- S.get
        x <- f
        S.put s
        return x
  concat <$> mapM (localState . worker) routes
worker (MyParam route) = do
  S.modify (+1)
  mapM (\s -> return $ if s == "" then "[a-zA-Z0-9_-]+" else "[a-zA-Z0-9_-]+/" ++ s) =<< worker route

getUrls :: Route (Kontra Response) -> [String]
getUrls route = nub $ concatMap exceptions $ filter (not . isRoot) $ map makeAbsoluteUrl result
  where route' = coerce route
        makeAbsoluteUrl url = "/" ++ url
        isRoot url = url == "/"
        result = fst $ S.runState (worker route') 0

-- handle exceptions
-- 1)
-- urls like /s are special, they need to both handle "/s" and "/s/1/2/3"
-- but we can't use "/s" because that would catch all urls starting with letter s (e.g. /something)
-- so turn those cases (/s, /d, /a) into two rules, one with explicit end of line regex match $,
-- and one rule that matches longer urls with additional path elems after another slash
-- 2)
-- urls like /sv /en (where it's a lang code), should not cover anything longer, like /now-hiring
-- except for /no which is needed in front page stuff
-- 3)
-- /pricing and /en/pricing (or /sv/pricing) should disappear for some reason because IT said so
-- 4)
-- /verify gets its own special rule at the end
exceptions :: String -> [String]
exceptions "/no" = []
exceptions ('/':c1:c2:[]) | [c1, c2] `elem` (map (T.unpack . codeFromLang) allLangs) = [['/', c1, c2, '$']]
exceptions ('/':c:[]) | c `elem` ("asd"::String) = [['/', c, '/'], ['/', c, '$']]
exceptions "/pricing" = []
exceptions "/verify" = []
exceptions ('/':c1:c2:"/pricing") | [c1, c2] `elem` map (T.unpack . codeFromLang) allLangs = []
exceptions s = [s]

main :: IO ()
main = do
  _ <- setupAppPaths
  [fileName, include] <- getArgs
  input <- BSL.getContents
  let stringifyPair (x, y) = (T.unpack x, T.unpack y)
      Just rules = map stringifyPair <$> HM.toList <$> decode input
      overrides = map fst rules
  withFile fileName WriteMode $ \h -> do
    let urls = getUrls $ staticRoutes True
        -- sort DESC to have "/a/b" coming before "/a"
        sortedUrls = sortBy (\u1 u2 -> compare u2 u1) urls
        printInstructions is = forM_ is $ \i -> hPutStrLn h $ "    " ++ i
    forM_ sortedUrls $ \url -> do
      let urlRules = lines $ fromMaybe include (lookup url rules)
      hPutStrLn h $ "location ~ ^" ++ url ++ " {"
      printInstructions urlRules
      hPutStrLn h "}"
    forM_ (overrides \\ sortedUrls) $ \url -> do
      hPutStrLn h $ "location ~ ^" ++ url ++ " {"
      printInstructions $ lines $ fromJust $ lookup url rules
      hPutStrLn h "}"
