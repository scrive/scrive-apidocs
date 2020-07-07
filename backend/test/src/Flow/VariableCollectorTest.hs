{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}

module Flow.VariableCollectorTest where

import Data.Text.Encoding
import Data.Yaml
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit.Base ((@=?), Assertion)
import Text.RawString.QQ
import qualified Data.Set as Set

import Flow.OrphanTestInstances ()
import Flow.Transducer
import Flow.VariableCollector

tests :: Test
tests =
  testGroup "VariableCollector" [testCase "Collect variables" testCollectFlowVariables]

process :: Text
process = [r|
dsl-version: "1"
stages:
  - view:
      actions:
        - notify:
            users: [user3]
            message: get-data
      expect:
        viewed-by:
          users: [user1, user2]
          documents: [document1, document2]
  - sign:
      actions: []
      expect:
        signed-by:
          users: [user1]
          documents: [document1]
  |]

data MyError = YamlError ParseException | MachineError Error
  deriving (Show)

testCollectFlowVariables :: Assertion
testCollectFlowVariables = do
  tongue <- decodeThrow $ encodeUtf8 process
  expected @=? collectVariables tongue
  where
    expected = FlowVariables
      { users                   = Set.fromList ["user1", "user2", "user3"]
      , documents               = Set.fromList ["document1", "document2"]
      , messages                = Set.fromList ["get-data"]
      , documentUserAssociation =
        Set.fromList
          [ DocRoleFor { role = Viewer, user = "user1", document = "document1" }
          , DocRoleFor { role = Viewer, user = "user1", document = "document2" }
          , DocRoleFor { role = Viewer, user = "user2", document = "document1" }
          , DocRoleFor { role = Viewer, user = "user2", document = "document2" }
          , DocRoleFor { role = SigningParty, user = "user1", document = "document1" }
          ]
      }
