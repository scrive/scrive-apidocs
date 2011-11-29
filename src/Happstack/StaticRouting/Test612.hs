{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Happstack.StaticRouting.Test612 where

-- Backport of Happstack.StaticRouting.Test to GHC 6.12.3

import qualified Data.ListTrie.Map as Trie

import Happstack.Server(ServerPartT, nullConf, simpleHTTP, FilterMonad, Response)
import Happstack.Server(FromReqURI, ServerMonad)
import Control.Monad(MonadPlus)

import qualified Happstack.Server as H

import Happstack.StaticRouting.Internal hiding (path)
import qualified Happstack.StaticRouting.Internal as I

-- Workaround for GHC 6.12.3:

class Path' a where
  pathHandler' :: (ServerPartT IO String -> ServerPartT IO String) -> a -> ServerPartT IO String
  arity' :: a -> Int

instance (FromReqURI d, Path' a) => Path' (d -> a) where
  pathHandler' w f = H.path (pathHandler' w . f)
  arity' f = 1 + arity' (f undefined)

instance Path' (ServerPartT IO String) where
  pathHandler' w m = w m
  arity' _ = 0

-- | Expect the given method, and exactly 'n' more segments, where 'n' is the arity of the handler
path :: Path' a => H.Method -> (ServerPartT IO String -> ServerPartT IO String) -> a -> Route (ServerPartT IO String)
path m w h = Handler (Just (arity' h),m) (pathHandler' w h)

main :: IO ()
main = simpleHTTP nullConf (fst (compile rs1))

hGet :: Path' a => a -> Route (ServerPartT IO String)
hGet = path H.GET id

hok :: String -> ServerPartT IO String
hok = H.ok

ok :: String -> Route (ServerPartT IO String)
ok s = hGet $ (hok s)

rs1,rs2 :: Route (ServerPartT IO [Char])
rs1 = choice
    [ dir "p1" $ ok "p1"
    , dir "p2" $ ok "p2"
    , dir "p2" $ dir "a" $ ok "p2/a"
    , dir "p3" $ remainingPath H.GET $ hok "p3/**"
    , dir "p3" $ dir "a" $ ok "p3/a"
    , dir "p4" $ param "P1" $ ok "p4/P1"
    , dir "p4" $ param "P2" $ ok "p4/P2"
    , dir "p5" $ dir "a" $ ok "p5.a"
    , dir "p5" $ dir "b" $ ok "p5/b"
    , dir "p6" $ choice [ ok "p6"
                          , dir "b" $ ok "p6/b"
                          , dir "a" $ ok "p6/a"
                          , dir "a" $ dir "b" $ ok "p6/a/b"
                          , dir "a" $ hGet $ \p -> 
                               hok ("p6:"++p)
                          , dir "a" $ hGet $ \p q -> 
                              hok ("p6:"++p++":"++q)
                          ]
    ]

rs2 = choice
      [ dir "p1" $ remainingPath H.GET $ hok "remainingPath"
      , dir "p1" $ dir "path1" $ ok "path1"
      , dir "p2" $ hGet $ \p q r -> hok (p++q++r)
      , dir "p2" $ dir "p3" $ remainingPath H.GET $ hok "remainingPath"
      , dir "p3" $ hGet $ \p -> hok p
      , dir "p3" $ dir "p4" $ ok "p4"
      , dir "p4" $ dir "p5" $ ok "p5"
      , dir "p4" $ dir "p5" $ ok "p5"
      ]

instance Show (ServerPartT IO a) where
  show _ = "IO"

test :: IO ()
test = do let t = routeTree rs1
          putStrLn $ Trie.showTrie (unR t) ""
          putStrLn $ unlines $ map show $ flattenTree t
          putStrLn $ unlines $ concatMap showPath $ flattenTree t
          putStrLn "Overlaps in rs1"
          putStrLn $ showOverlaps $ overlaps True t
          putStrLn "Overlaps in rs2"
          putStrLn $ showOverlaps $ overlaps True (routeTree rs2)
