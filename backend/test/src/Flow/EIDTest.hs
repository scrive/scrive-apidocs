{-# LANGUAGE QuasiQuotes #-}
module Flow.EIDTest (tests) where

import Control.Monad.Reader.Class
import Test.Framework
import Text.RawString.QQ
import qualified Data.Map as Map

import DB hiding (JSON)
import Doc.Types.Document
import Doc.Types.DocumentStatus
import Doc.Types.SignatoryLink
import Flow.Client
import Flow.EID.AuthConfig
import Flow.Model.Types.FlowUserId
import Flow.OrphanTestInstances ()
import Flow.Process.Internal
import Flow.Routes.Api
import Flow.TestUtil
import TestEnvSt.Internal (flowPort)
import TestingUtil hiding (assertLeft, assertRight)
import TestKontra
import Util.HasSomeUserInfo

tests :: TestEnvSt -> Test
tests env = testGroup "Flow EID Integration"
                      [testThat "EID authentication config works" env testAuthConfig]

processWithAuth :: Process
processWithAuth = Process [r|
dsl-version: "0.2.0"
stages:
  - some-stage:
      actions: []
      expect:
        signed-by:
          users: [author, signatory]
          documents: [doc]
|]

testAuthConfig :: TestEnv ()
testAuthConfig = do
  TestEnvSt {..} <- ask
  user           <- instantiateRandomUser
  oauth          <- getToken (user ^. #id)
  let ac = mkApiClient (Left oauth)

  -- Prepare a document with two signatories (one of them is the author).
  doc <- addRandomDocument (rdaDefault user)
    { rdaTypes       = OneOf [Signable]
    , rdaStatuses    = OneOf [Preparation]
    , rdaSignatories = let signatory = OneOf
                             [ [ RSC_IsSignatoryThatHasntSigned
                               , RSC_DeliveryMethodIs EmailDelivery
                               , RSC_AuthToViewIs StandardAuthenticationToView
                               , RSC_AuthToSignIs StandardAuthenticationToSign
                               , RSC_HasConsentModule False
                               ]
                             ]
                       in  OneOf [[signatory, signatory]]
    }

  let
    [authorSigLink, signatorySigLink] = documentsignatorylinks doc
    authorConfig    = UserConfig (Email $ getEmail authorSigLink) Nothing Nothing
    signatoryConfig = UserConfig (Email $ getEmail signatorySigLink)
                                 (Just $ AuthConfig Onfido 1)
                                 (Just $ AuthConfig SmsPin 3)
    userConfigs = Map.fromList [("author", authorConfig), ("signatory", signatoryConfig)]
    templateParams =
      TemplateParameters (Map.fromList [("doc", documentid doc)]) userConfigs mempty

  -- Flow stuff runs in another DB session so we need to commit before calling
  -- any flow related functions.
  commit

  i <- assertRight "start template"
    $ createInstance ac "dummy" processWithAuth templateParams

  assertEqual "UserConfigs sent to API and retrieved from DB are equal"
              userConfigs
              (i ^. #templateParameters % #users)

