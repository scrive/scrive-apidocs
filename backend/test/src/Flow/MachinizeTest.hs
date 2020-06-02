{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Strict #-}

module Flow.MachinizeTest where

import Data.Either.Extra
import Data.Text.Encoding
import Data.Yaml
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit.Base ((@=?), Assertion)
import Text.RawString.QQ
import qualified Data.Set as Set

import Flow.Machinize
import Flow.Transducer

tests :: Test
tests = testGroup "Machinize"
                  [testCase "Rejection leads to failure" testRejectionLeadsToFailure]

process :: Text
process = [r|
stages:
  - sign:
      actions: []
      expect:
        signed-by:
          users: [user1]
          documents: [document1]
  |]

data MyError = YamlError ParseException | MachineError Error
  deriving (Show)

testRejectionLeadsToFailure :: Assertion
testRejectionLeadsToFailure = check $ do
  tongue  <- mapLeft YamlError . decodeEither' $ encodeUtf8 process
  machine <- mapLeft MachineError $ linear tongue
  mapLeft MachineError $ step machine "sign" input
  where
    check = \case
      Left  e -> fail $ show e
      Right x -> expected @=? x
    expected = TransducerEdge input [Fail] "failure"
    input    = Set.singleton $ EventInfo Rejection "user1" "document1"
