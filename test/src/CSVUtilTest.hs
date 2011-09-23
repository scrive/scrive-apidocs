module CSVUtilTest (csvUtilTests) where


import qualified Data.ByteString.Lazy as BSL

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assert, assertEqual, Assertion)

import Util.CSVUtil

csvUtilTests :: Test
csvUtilTests = testGroup "CSVUtil"
    [ testGroup "parseCSV"
      [ testCase "mac csv with swedish chars works" testMacCSVWithSwedishChars
      ]
    ]

testMacCSVWithSwedishChars :: Assertion
testMacCSVWithSwedishChars = do
  contents <- BSL.readFile "test/csv/mac_csv_with_swedish.csv"
  let mresults = parseCSV contents
  case mresults of
    Left err -> do
      putStrLn err
      assert False
    Right results -> do
      assertEqual "parsed csv" [["Stuff","More stuff","_ö"],["","ÖÄ"]] results
  
  
