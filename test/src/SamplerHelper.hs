module SamplerHelper (sampleMail, sampleFlashMsg, sampleView, sample) where

import Test.HUnit (assert, assertEqual, assertFailure, Assertion(..))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Templates.Templates
import Mails.SendMail

import System.Directory
import System.FilePath

import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import System.IO.UTF8 as UTF8

import Data.Either

import Text.XML.HaXml.Parse (xmlParse')

sampleMail name action = sample name "mail" action mailSaver

sampleFlashMsg name action = sample name "flash_msg" action stringSaver

sampleView name action = sample name "view" action stringSaver

sample name suffix action saver = withSampleDirectory $ \tmp -> do
   t <- readTemplates
   sample <- action t
   let file = tmp ++ "/" ++ name ++ "_" ++ suffix ++ ".html"
   saver file sample
   content <- Prelude.readFile file
   let xml = xmlParse' file content
   either assertFailure (\_ -> assert True) xml

mailSaver file mail = BS.writeFile file (content mail)

stringSaver file str = UTF8.writeFile file ("<html><head><meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\"/></head><body>" ++ str ++ "</body></html>")

withSampleDirectory :: (FilePath -> IO a) -> IO a
withSampleDirectory action = do 
  let sampleDir = "kontrakcja-content-samples"
  createDirectoryIfMissing False sampleDir
  action sampleDir
  
