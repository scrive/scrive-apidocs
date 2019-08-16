module NetsXMLTest (netsXmlTests) where

import Log
import Test.Framework
import Text.XML hiding (Document)
import Text.XML.Cursor

import EID.Nets.Types
import TestingUtil
import TestKontra
import Text.XML.Parser

netsXmlTests :: TestEnvSt -> Test
netsXmlTests env = testGroup "NetsXML" [
  testThat "Nets XML Parses" env testNetsXMLParsing
  ]

testNetsXMLParsing :: TestEnv ()
testNetsXMLParsing = do
  file <- readTestFile "xml/nets_sdo.xml"
  let xml = parseLBS_ def file
  --logInfo_ . ("XML: "<>) . showt $ xml
  let cursor = fromDocument xml
  --logInfo_ . ("Cursor: "<>) . showt $ cursor
  let result = runParser xpGetSDOAttributes cursor
  logInfo_ . ("runParser result: "<>) . showt $ result
