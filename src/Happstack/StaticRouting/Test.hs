{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Happstack.StaticRouting.Test where

import qualified Data.ListTrie.Map as Trie

import Happstack.Server(ServerPartT, nullConf, simpleHTTP, FilterMonad, Response)

import qualified Happstack.Server as H

import Happstack.StaticRouting.Internal

main :: IO ()
main = simpleHTTP nullConf (fst (compile rs1))

hGet :: Path m h a a => h -> Route (m a)
hGet = path H.GET id

ok :: (Path m (m a) a a, FilterMonad Response m) => a -> Route (m a)
ok s = hGet $ H.ok s

rs1,rs2 :: Route (ServerPartT IO [Char])
rs1 = choice
    [ dir "p1" $ ok "p1"
    , dir "p2" $ ok "p2"
    , dir "p2" $ dir "a" $ ok "p2/a"
    , dir "p3" $ remainingPath H.GET $ H.ok "p3/**"
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
                               H.ok ("p6:"++p)
                          , dir "a" $ hGet $ \p q -> 
                              H.ok ("p6:"++p++":"++q)
                          ]
    ]

rs2 = choice
      [ dir "p1" $ remainingPath H.GET $ H.ok "remainingPath"
      , dir "p1" $ dir "path1" $ ok "path1"
      , dir "p2" $ hGet $ \p q r -> H.ok (p++q++r)
      , dir "p2" $ dir "p3" $ remainingPath H.GET $ H.ok "remainingPath"
      , dir "p3" $ hGet $ \p -> H.ok p
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
