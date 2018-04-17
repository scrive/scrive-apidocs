module NetsXMLTest (netsXmlTests) where

import Control.Monad.IO.Class
import Log
import Test.Framework
import Text.XML hiding (Document)
import Text.XML.Cursor
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import EID.Nets.Data
import TestingUtil
import TestKontra
import Text.XML.Parser

netsXmlTests :: TestEnvSt -> Test
netsXmlTests env = testGroup "NetsXML" [
  testThat "Nets XML Parses" env testNetsXMLParsing
  ]

testNetsXMLParsing :: TestEnv ()
testNetsXMLParsing = do
  file <- liftIO . BSL.readFile $ inTestDir "xml/nets_sdo.xml"
  let xml = parseLBS_ def file
  --logInfo_ . ("XML: "<>) . T.pack . show $ xml
  let cursor = fromDocument xml
  --logInfo_ . ("Cursor: "<>) . T.pack . show $ cursor
  let result = runParser xpGetSDOAttributes cursor
  logInfo_ . ("runParser result: "<>) . T.pack . show $ result
