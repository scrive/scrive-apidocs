{-# LANGUAGE CPP #-}
module DocConverterMain where


import Data.Char
import System.Environment
import System.FilePath
import qualified Data.ByteString as BS

import Configuration
import LiveDocx
import qualified Log (withLogger, docConverter)

main :: IO ()
main = Log.withLogger $ do
  args <- getArgs
  appname <- getProgName
  case args of
    (filein : []) -> do
      conf <- readConfig Log.docConverter appname [] "doc_converter.conf"
      let fileout = replaceExtension filein "pdf"
      filecontents <- BS.readFile filein
      let fileformat = read . map toUpper . tail $ takeExtension filein
      Log.docConverter $ "Converting from " ++ show fileformat ++ " to PDF"
      result <- convertToPDF conf filecontents fileformat
      case result of
        Left msg -> do
          putStrLn $ "Error: " ++ msg
          return ()
        Right pdfcontents -> do
          BS.writeFile fileout pdfcontents
          putStrLn $ "Written file to " ++ fileout
          return ()
    _ -> do
      putStrLn $ "Usage: doc-converter inputfile.doc"
      return ()
