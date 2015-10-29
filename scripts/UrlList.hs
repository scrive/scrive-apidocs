module UrlList where

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
    show (MyHandler _ _ _) = "Handler"
    show (MyChoice xs) = "Choice (" ++ show xs  ++ ")"

data MySegment = MyStringS String
               | MyParams

instance Show (MySegment) where
    show (MyStringS s) = s
    show MyParams = "Params"

type MyEndSegment = (Maybe Int, Method)

type MyCheckApply = [String] -> Bool

bar :: MyRoute (Kontra Response) -> [String]
bar (MyDir _segment (MyHandler (Nothing, _) _ _)) = [] -- [segment ++ " <SERVING FILES FROM SOME DIRECTORY>"]
bar (MyDir segment (MyHandler (Just 0, _method') _ _)) = [show segment]
bar (MyDir segment (MyHandler (Just _n, _method') _ _)) = [show segment ++ "/"]
bar (MyDir segment route) = map (\s -> show segment ++ "/" ++ s) $ bar route
bar (MyHandler (Nothing, _) _ _) = [] -- ["<SERVING FILES FROM SOME DIRECTORY>"]
bar (MyHandler (Just 0, _method') _ _) = [""]
bar (MyHandler (Just n, _method') _ _) = [intersperse '/' (replicate n 'X')]
bar (MyChoice routes) = concatMap bar routes
bar (MyParam _) = $unexpectedError "Params"

foo :: Route (Kontra Response) -> [String]
foo = bar . coerce

coerce :: Route (Kontra Response) -> MyRoute (Kontra Response)
coerce = unsafeCoerce

baz :: Route (Kontra Response) -> [String]
baz = filter (not . (== "/")) . nub . map ("/" ++) . foo

main :: IO ()
main = do
  [include] <- getArgs
  withFile "urls.txt" WriteMode $ \h -> do
    forM_ (baz $ staticRoutes True) $ \url -> do
      hPutStrLn h $ "location ~ ^" ++ url ++ " {"
      hPutStrLn h $ "    include " ++ include ++ ";"
      hPutStrLn h "}"
