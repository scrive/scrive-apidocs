module Main (main) where

import System.Environment

import AppDir (setupAppPaths)
import Transifex.Synch

main :: IO ()
main = do
  _ <- setupAppPaths
  main' =<< getArgs
