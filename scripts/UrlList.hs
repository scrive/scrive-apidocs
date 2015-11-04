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

bar :: MyRoute (Kontra Response) -> State Int [String]
bar (MyDir _segment (MyHandler (Nothing, _) _ _)) = return [] -- [segment ++ " <SERVING FILES FROM SOME DIRECTORY>"]
bar (MyDir segment (MyHandler (Just n, _method') _ _)) = do
  nParams <- get
  let n' = n - nParams
  case n' of
    0 -> return [show segment]
    _ -> return [show segment ++ "/"]
bar (MyDir segment route) = mapM (\s -> return $ show segment ++ "/" ++ s) =<< bar route
bar (MyHandler (Nothing, _) _ _) = return [] -- ["<SERVING FILES FROM SOME DIRECTORY>"]
bar (MyHandler (Just 0, _method') _ _) = return [""]
bar (MyHandler (Just n, _method') _ _) = return [intersperse '/' (replicate n 'X')]
bar (MyChoice routes) = do
  let localState :: State s a -> State s a
      localState f = do
        s <- get
        x <- f
        put s
        return x
  concat <$> mapM (localState . bar) routes
bar (MyParam route) = do
  modify (+1)
  mapM (\s -> return $ "[a-zA-Z0-9_-]+/" ++ s) =<< bar route

foo :: Route (Kontra Response) -> [String]
foo = fst . flip runState 0 . bar . coerce

coerce :: Route (Kontra Response) -> MyRoute (Kontra Response)
coerce = unsafeCoerce

baz :: Route (Kontra Response) -> [String]
baz = nub . concatMap exceptions . filter (not . (== "/")) . map ("/" ++) . foo

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
    forM_ (baz $ staticRoutes True) $ \url -> do
      hPutStrLn h $ "location ~ ^" ++ url ++ " {"
      hPutStrLn h $ "    include " ++ include ++ ";"
      hPutStrLn h "}"
