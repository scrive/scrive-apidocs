module SSO.SAML (getVerifiedAssertionsFromSAML, getIDPID, parseSAMLXML, getFirstNonEmptyAttribute, getNonEmptyNameID) where

import Control.Exception
import Control.Monad.Base
import Data.Bifunctor
import Data.List.NonEmpty (toList)
import Text.XML.HXT.Core ((/>), (>>>))
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Either as E
import qualified Data.Text as T
import qualified SAML2.Core.Assertions as A
import qualified SAML2.Core.Protocols as SAMLP
import qualified SAML2.Core.Signature as SIG
import qualified SAML2.XML as XML
import qualified SAML2.XML.Signature as SIG
import qualified Text.XML.HXT.Core as HXT
import qualified Text.XML.HXT.HTTP as HXT (withHTTP)

parseSAMLXML :: (MonadBase IO) m => Text -> m (Either String HXT.XmlTree)
parseSAMLXML txt = liftBase $ do
  let decoded = BSU.toString . B64.decodeLenient . BSU.fromString . T.unpack $ txt
  eXmlTree <-
    try
      $ (   Prelude.head
        <$> (HXT.runX $ HXT.readString
              [ HXT.withCheckNamespaces HXT.yes
              , HXT.withHTTP []
              , HXT.withRemoveWS HXT.no
              ]
              decoded
            )
        )
  return $ first (\e -> show $ (e :: IOException)) eXmlTree

getVerifiedAssertionsFromSAML
  :: (MonadBase IO) m => SIG.PublicKeys -> HXT.XmlTree -> m (Either String [A.Assertion])
getVerifiedAssertionsFromSAML publicKeys xmlTree = liftBase $ do
  saml <- SIG.verifySAMLProtocol' publicKeys xmlTree
  let assertions = SAMLP.responseAssertions saml
  results <- E.partitionEithers <$> mapM verifySAMLAssertion assertions
  case results of
    ([]    , verifiedAssertions) -> return . Right $ verifiedAssertions
    (errors, _                 ) -> return . Left . intercalate " " $ errors
  where
    verifySAMLAssertion
      :: A.PossiblyEncrypted A.Assertion -> IO (Either String A.Assertion)
    verifySAMLAssertion (A.NotEncrypted assertion) = do
      let id = A.assertionID assertion
      verification <- SIG.verifySignature publicKeys id xmlTree
      case verification of
        Nothing -> return . Left $ "No matching algorithm found for " <> show assertion
        Just False ->
          return . Left $ "Signature verification failed for " <> show assertion
        _ -> return . Right $ assertion
    verifySAMLAssertion _ =
      return . Left $ "Encryption of assertions is not supported (for now at least)"

getIDPID :: HXT.XmlTree -> Maybe String
getIDPID = listToMaybe . HXT.runLA
  (   HXT.getChildren
  >>> HXT.isElem
  />  HXT.hasQName (HXT.mkQName "saml2" "Issuer" "urn:oasis:names:tc:SAML:2.0:assertion")
  >>> HXT.getChildren
  >>> HXT.getText
  )

getFirstNonEmptyAttribute :: String -> A.Assertion -> Maybe String
getFirstNonEmptyAttribute attributeName assertion =
  listToMaybe . catMaybes $ findAttributeInStatement <$> A.assertionStatement assertion
  where
    findAttributeInStatement :: A.Statement -> Maybe String
    findAttributeInStatement (A.StatementAttribute attribute) =
      listToMaybe
        .   catMaybes
        $   findAttributeInPossiblyEncrypted
        <$> (toList $ A.attributeStatement attribute)
    findAttributeInStatement _ = Nothing

    findAttributeInPossiblyEncrypted :: A.PossiblyEncrypted A.Attribute -> Maybe String
    findAttributeInPossiblyEncrypted (A.NotEncrypted a) =
      if ((A.attributeName a) == attributeName)
        then listToMaybe . catMaybes $ findFirstAttributeInNodes <$> A.attributeValues a
        else Nothing
    findAttributeInPossiblyEncrypted _ = Nothing

    findFirstAttributeInNodes :: XML.Nodes -> Maybe String
    findFirstAttributeInNodes =
      listToMaybe . catMaybes . fmap findFirstNonEmptyAttributeValue

    findFirstNonEmptyAttributeValue :: HXT.XmlTree -> Maybe String
    findFirstNonEmptyAttributeValue xmlTree =
      listToMaybe $ HXT.runLA (HXT.neg HXT.isWhiteSpace >>> HXT.getText) xmlTree

getNonEmptyNameID :: A.Assertion -> Maybe String
getNonEmptyNameID assertion =
  getDecryptedSubjectIdentifier =<< (A.subjectIdentifier $ A.assertionSubject assertion)
  where
    getDecryptedSubjectIdentifier :: A.PossiblyEncrypted A.Identifier -> Maybe XML.XString
    getDecryptedSubjectIdentifier (A.NotEncrypted (A.IdentifierName nameID)) =
      Just . getSimpleBaseIDName $ nameID
    getDecryptedSubjectIdentifier _ = Nothing

    getSimpleBaseIDName :: A.NameID -> XML.XString
    getSimpleBaseIDName nameID = A.baseID . A.nameBaseID $ nameID
