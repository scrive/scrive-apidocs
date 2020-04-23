import Test.Tasty
import Test.Tasty.HUnit

import qualified Test.Aggregator as Aggregator

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Aggregator.tests]
