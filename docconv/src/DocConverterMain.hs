{-# LANGUAGE CPP #-}
module DocConverterMain where

import qualified Data.ByteString as BS

import LiveDocx

main :: IO ()
main = do
  let
    conf = LiveDocxConf {
      url = "https://api.livedocx.com/1.2/mailmerge.asmx"
    , username = "emilymaygreen"
    , password = "monkey69"
    }
    filepathin = "/home/emily/Scrive - Executive Summary - em.docx"
    filepathout = "/home/emily/10000-emdev.pdf"
    filename = "10000-emdev.docx"
  filecontents <- BS.readFile filepathin
  let result = convertToPDF conf filename filecontents
  case result of
    Left msg -> do
      putStrLn $ "Error: " ++ msg
      return ()
    Right pdfcontents -> do
      BS.writeFile filepathout pdfcontents
      putStrLn $ "Written file to " ++ filepathout
      return ()
