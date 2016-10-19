module Main where

import System.Environment

import Transifex.Synch

main :: IO ()
main = main' =<< getArgs
