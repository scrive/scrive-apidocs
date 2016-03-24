module UrlList where

import Control.Monad.State
import Unsafe.Coerce (unsafeCoerce)
import System.Environment (getArgs)
import System.IO

import Happstack.Server (Method, Response)
import Happstack.StaticRouting (Route)

import Kontra
import KontraPrelude
import RoutingTable (staticRoutes)


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
worker (MyHandler (Just 0, _method') _ _) = return [""]
worker (MyHandler (Just n, _method') _ _) = return [intersperse '/' (replicate n 'X')]
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
  mapM (\s -> return $ "[a-zA-Z0-9_-]+/" ++ s) =<< worker route

getUrls :: Route (Kontra Response) -> [String]
getUrls route = nub $ concatMap exceptions $ filter (not . isRoot) $ map makeAbsoluteUrl result
  where route' = coerce route
        makeAbsoluteUrl url = "/" ++ url
        isRoot url = url == "/"
        result = fst $ runState (worker route') 0

-- handle exceptions
-- urls like /s are special, they need to both handle "/s" and "/s/1/2/3"
-- but we can't use "/s" because that would catch all urls starting with letter s (e.g. /something)
-- so turn those cases (/s, /d, /a) into two rules, one with explicit end of line regex match $,
-- and one rule that matches longer urls with additional path elems after another slash
exceptions :: String -> [String]
exceptions s@('/':c:[]) | c `elem` "asd" = ['/':c:'/':[], '/':c:'$':[]]
                        | otherwise = [s]
exceptions s = [s]

main :: IO ()
main = do
  [include] <- getArgs
  withFile "urls.txt" WriteMode $ \h -> do
    forM_ (getUrls $ staticRoutes True) $ \url -> do
      hPutStrLn h $ "location ~ ^" ++ url ++ " {"
      hPutStrLn h $ "    include " ++ include ++ ";"
      hPutStrLn h "}"
