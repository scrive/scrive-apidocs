module SSO.SAMLTest (samlSignatureTest) where

import Control.Monad.Base
import Crypto.Store.X509
import Data.ByteString
import Data.Text.Encoding
import Data.X509
import SAML2.XML.Signature
import Test.Framework
import Text.XML.HXT.Core as HXT

import SSO.SAML
import TestingUtil
import TestKontra

samlSignatureTest :: TestEnvSt -> Test
samlSignatureTest env = testGroup
  "SAML Signatures"
  [ testThat "Azure signature verifies " env testAzureSignatureVerifies
  , testThat "Okta signature verifies"   env testOktaSignatureVerifies
  , testThat "WAME signature verifies"   env testWAMESignatureVerifies
  ]

readBase64XmlFromFile :: FilePath -> IO HXT.XmlTree
readBase64XmlFromFile path = do
  eXml <- decodeUtf8 <$> Data.ByteString.readFile path >>= parseSAMLXML
  case eXml of
    Right xml -> return xml
    Left  msg -> fail $ path <> "can't be read: " <> msg

readPubKeyRSA :: FilePath -> IO PublicKeys
readPubKeyRSA path = do
  (PubKeyRSA rsaPublicKey) <- Prelude.head <$> readPubKeyFile path
  return $ PublicKeys Nothing (Just rsaPublicKey)

testAzureSignatureVerifies :: TestEnv ()
testAzureSignatureVerifies = liftBase $ do
  parsedXml <- readBase64XmlFromFile "backend/test/src/SSO/dustin-response.b64"
  publicKeys <- readPubKeyRSA "backend/test/src/SSO/dustin-pubkey.pem"
  result <- verifySignature publicKeys "_1357f79d-688d-47bd-b2a3-852002252400" parsedXml
  assertEqual "Dustin signature verified" result (Just True)
  return ()

testOktaSignatureVerifies :: TestEnv ()
testOktaSignatureVerifies = liftBase $ do
  parsedXml  <- readBase64XmlFromFile "backend/test/src/SSO/okta-response.b64"
  publicKeys <- readPubKeyRSA "backend/test/src/SSO/okta-pubkey.pem"
  result     <- verifySignature publicKeys "id19445555358897071620478623" parsedXml
  assertEqual "Okta response signature verified" result (Just True)
  result2 <- verifySignature publicKeys "id194455553589685681012477200" parsedXml
  assertEqual "Okta assertion signature verified" result2 (Just True)
  return ()

testWAMESignatureVerifies :: TestEnv ()
testWAMESignatureVerifies = liftBase $ do
  parsedXml  <- readBase64XmlFromFile "backend/test/src/SSO/wame-response.b64"
  publicKeys <- readPubKeyRSA "backend/test/src/SSO/wame-pubkey.pem"
  result     <- verifySignature publicKeys
                                "FIMRSP_ed7abb99-0170-145f-beea-d0d544148baa"
                                parsedXml
  assertEqual "WAME response signature verified" result (Just True)
  result2 <- verifySignature publicKeys
                             "Assertion-uuided7abb95-0170-1012-96b2-d0d544148baa"
                             parsedXml
  assertEqual "WAME assertion signature verified" result2 (Just True)
  return ()

