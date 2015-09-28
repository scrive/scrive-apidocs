module UrlList where

import Unsafe.Coerce (unsafeCoerce)

import Happstack.Server (Method, Response)
import Happstack.StaticRouting (Route)

import Kontra
import KontraPrelude
import RoutingTable (staticRoutes)


data MyRoute a =
    MyDir MySegment (MyRoute a)
  | MyHandler MyEndSegment a
  | MyChoice [MyRoute a]

newtype MySegment = MyStringS String

type MyEndSegment = (Maybe Int, Method)

bar :: MyRoute (Kontra Response) -> [String]
bar (MyDir (MyStringS segment) (MyHandler (Nothing, _) _)) = [segment ++ " <SERVING FILES FROM SOME DIRECTORY>"]
bar (MyDir (MyStringS segment) (MyHandler (Just 0, method') _)) = [segment ++ " " ++ show method']
bar (MyDir (MyStringS segment) (MyHandler (Just n, method') _)) = [segment ++ "/" ++ intersperse '/' (replicate n 'X') ++ " " ++ show method']
bar (MyDir (MyStringS segment) route) = map (\s -> segment ++ "/" ++ s) $ bar route
bar (MyHandler (Nothing, _) _) = ["<SERVING FILES FROM SOME DIRECTORY>"]
bar (MyHandler (Just 0, method') _) = [" " ++ show method']
bar (MyHandler (Just n, method') _) = [intersperse '/' (replicate n 'X') ++ " " ++ show method']
bar (MyChoice routes) = concatMap bar routes

foo :: Route (Kontra Response) -> [String]
foo = bar . unsafeCoerce

baz :: Route (Kontra Response) -> [String]
baz = map ("/" ++) . foo

main :: IO ()
main = writeFile "/tmp/urls.txt" $ intercalate "\n" $ baz $ staticRoutes True
