{-# OPTIONS_GHC -fglasgow-exts #-}

module Main where
import PdfModel
import System.Environment
import Data.Maybe
import Data.Time.Clock
import Control.Parallel.Strategies


main = do
    [sourceFileName,destinationFileName] <- getArgs
    start <- getCurrentTime
    putStrLn "Reading file..."
    Just doc <- PdfModel.parseFile sourceFileName
    -- putStrLn "Reducing structure..."
    -- rnf doc `seq` return ()
    putStrLn "Writting file..."
    writeFileX destinationFileName doc
    end <- getCurrentTime
    putStrLn ("Done in " ++ show (end `diffUTCTime` start))
    return ()

