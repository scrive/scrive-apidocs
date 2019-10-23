module CSVUtilTest (csvUtilTests) where

import System.FilePath
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assert, assertEqual)

import TestKontra
import Util.CSVUtil

csvUtilTests :: TestEnvSt -> Test
csvUtilTests _ = testGroup
  "CSVUtil"
  [ testGroup
      "parseCSV"
      [ testCase "exported from excel on a mac in comma separated format"
        $ testCSVFile "mac_csv.csv"
      , testCase "exported from excel on a mac in windows csv format"
        $ testCSVFile "mac_windows_csv.csv"
      , testCase "exported from excel on a mac in dos csv format"
        $ testCSVFile "mac_dos_csv.csv"
      , testCase "exported from excel on windows in csv format"
        $ testCSVFile "windows_csv.csv"
      , testCase "exported from excel on windows in mac format"
        $ testCSVFile "windows_mac.csv"
      , testCase "exported from excel on windows in ms-dos format"
        $ testCSVFile "windows_ms_dos.csv"
      , testCase "has empty cols" $ testCSVFile "with_empty_cols.csv"
      , testCase "has empty rows" $ testCSVFile "with_empty_rows.csv"
      , testCase "uses a different separator character (;)"
        $ testCSVFile "with_diff_sep.csv"
      ]
  ]

testCSVFile :: String -> Assertion
testCSVFile = assertMatchesLinux

assertMatchesLinux :: String -> Assertion
assertMatchesLinux file = do
  linuxcsv <- getLinuxCSV
  csv      <- getCSV file
  assertEqual "mismatched csvs" linuxcsv csv

getLinuxCSV :: IO [[String]]
getLinuxCSV = getCSV "linux.csv"

getCSV :: String -> IO [[String]]
getCSV file = do
  contents <- readTestFile $ "csv" </> file
  let mresults = parseCSV contents
  case mresults of
    Left err -> do
      putStrLn err
      assert False
      return [[]]
    Right results -> return results
