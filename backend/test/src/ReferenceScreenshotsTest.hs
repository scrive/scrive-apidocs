module ReferenceScreenshotsTest (screenshotTests) where

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion)
import Text.JSON

import MinutesTime
import TestingUtil
import TestKontra

screenshotTests :: TestEnvSt -> Test
screenshotTests _env = testGroup
  "ReferenceScreenshots"
  [ testCase "Desktop reference screenshots are up to date"
    $ testRefScreenshotsUp2Date "standard"
  , testCase "Mobile reference screenshots are up to date"
    $ testRefScreenshotsUp2Date "mobile"
  ]

testRefScreenshotsUp2Date :: String -> Assertion
testRefScreenshotsUp2Date screenshotName = do
  screenshotStr <- readFile $ "files/reference_screenshots/" ++ screenshotName ++ ".json"
  now           <- currentTime
  case decode screenshotStr of
    Ok (JSObject obj) -> case find ((== "time") . fst) $ fromJSObject obj of
      Just (_, JSString s) -> case parseTimeISO (fromJSString s) of
        Just time -> if (28 `daysBefore` now) < time
          then return ()
          else
            assertFailure
            $  "Reference screenshot in "
            ++ screenshotName
            ++ ".json is too old"
        Nothing -> assertFailure $ "Invalid time format in " ++ screenshotName ++ ".json"
      _ -> assertFailure $ "Invalid json structure in " ++ screenshotName ++ ".json"
    _ -> assertFailure $ "Invalid json structure in " ++ screenshotName ++ ".json"
