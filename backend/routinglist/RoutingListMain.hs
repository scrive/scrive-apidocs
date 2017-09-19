module RoutingListMain where

import Control.Monad.State
import Happstack.Server (Method, Response)
import Happstack.StaticRouting (Route)
import System.Environment (getArgs)
import System.IO
import Unsafe.Coerce (unsafeCoerce)

import Kontra
import KontraPrelude
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
worker :: MyRoute (Kontra Response) -> State Int [String]
worker (MyDir _segment (MyHandler (Nothing, _) _ _)) = return [] -- [segment ++ " <SERVING FILES FROM SOME DIRECTORY>"]
worker (MyDir segment (MyHandler (Just n, _method') _ _)) = do
  nParams <- get
  let n' = n - nParams
  case n' of
    0 -> return [show segment]
    _ -> return [show segment ++ "/"]
worker (MyDir segment route) = mapM (\s -> return $ show segment ++ "/" ++ s) =<< worker route
worker (MyHandler (Nothing, _) _ _) = return [] -- ["<SERVING FILES FROM SOME DIRECTORY>"]
worker (MyHandler (Just n, _method') _ _) = do
  nParams <- get
  let n' = n - nParams
  case n' of
    0 -> return [""]
    _ -> return ["SHOULD NOT HAPPEN"]
worker (MyChoice routes) = do
  let localState :: State s a -> State s a
      localState f = do
        s <- get
        x <- f
        put s
        return x
  concat <$> mapM (localState . worker) routes
worker (MyParam route) = do
  modify (+1)
  mapM (\s -> return $ if s == "" then "[a-zA-Z0-9_-]+" else "[a-zA-Z0-9_-]+/" ++ s) =<< worker route

getUrls :: Route (Kontra Response) -> [String]
getUrls route = nub $ concatMap exceptions $ filter (not . isRoot) $ map makeAbsoluteUrl result
  where route' = coerce route
        makeAbsoluteUrl url = "/" ++ url
        isRoot url = url == "/"
        result = fst $ runState (worker route') 0

-- handle exceptions
-- 1)
-- urls like /s are special, they need to both handle "/s" and "/s/1/2/3"
-- but we can't use "/s" because that would catch all urls starting with letter s (e.g. /something)
-- so turn those cases (/s, /d, /a) into two rules, one with explicit end of line regex match $,
-- and one rule that matches longer urls with additional path elems after another slash
-- 2)
-- urls like /no /en (where it's a lang code), should not cover anything longer, like /now-hiring
-- 3)
-- /pricing and /en/pricing (or /sv/pricing) should disappear for some reason because IT said so
exceptions :: String -> [String]
exceptions ('/':c1:c2:[]) | [c1, c2] `elem` map codeFromLang allLangs = [['/', c1, c2, '$']]
exceptions ('/':c:[]) | c `elem` ("asd"::String) = [['/', c, '/'], ['/', c, '$']]
exceptions "/pricing" = []
exceptions ('/':c1:c2:"/pricing") | [c1, c2] `elem` map codeFromLang allLangs = []
exceptions s = [s]

main :: IO ()
main = do
  [include] <- getArgs
  withFile "urls.txt" WriteMode $ \h -> do
    let urls = getUrls $ staticRoutes True
        urls' = ["/api/", "/adminonly"] ++ urls
    forM_ urls' $ \url -> do
      hPutStrLn h $ "location ~ ^" ++ url ++ " {"
      hPutStrLn h $ "    include " ++ include ++ ";"
      hPutStrLn h "}"
    -- verify upload cap hack
    hPutStrLn h $ "location ~ ^/verify {"
    hPutStrLn h $ "    client_max_body_size 30M;"
    hPutStrLn h $ "    include " ++ include ++ ";"
    hPutStrLn h "}"
