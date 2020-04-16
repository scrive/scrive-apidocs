module SSO.SAML (getVerifiedAssertionsFromSAML, getIDPID, parseSAMLXML, getFirstNonEmptyAttribute, getNonEmptyNameID, SAMLException(..)) where

import Control.Exception
import Control.Monad.Base
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

data SAMLException = XMLParseException String | SignatureVerificationException String
  deriving Show
instance Exception SAMLException

parseSAMLXML :: MonadBase IO m => Text -> m HXT.XmlTree
parseSAMLXML txt = liftBase $ do
  let decoded = BSU.toString . B64.decodeLenient . BSU.fromString . T.unpack $ txt
  catch
    (Prelude.head <$> HXT.runX
      (HXT.readString
        [HXT.withCheckNamespaces HXT.yes, HXT.withHTTP [], HXT.withRemoveWS HXT.no]
        decoded
      )
    )
    (\e -> throw $ XMLParseException (show (e :: IOException)))

getVerifiedAssertionsFromSAML
  :: MonadBase IO m => SIG.PublicKeys -> HXT.XmlTree -> m [A.Assertion]
getVerifiedAssertionsFromSAML publicKeys xmlTree = do
  saml <- optionallyVerifyResponseSignature publicKeys xmlTree
  let assertions = SAMLP.responseAssertions saml
  results <- E.partitionEithers <$> mapM verifySAMLAssertion assertions
  case results of
    ([], verifiedAssertions) -> return verifiedAssertions
    (errors, _) -> throw . SignatureVerificationException . unwords $ errors
  where
    verifySAMLAssertion
      :: (MonadBase IO) m
      => A.PossiblyEncrypted A.Assertion
      -> m (Either String A.Assertion)
    verifySAMLAssertion (A.NotEncrypted assertion) = liftBase $ do
      let id = A.assertionID assertion
      verification <- SIG.verifySignature publicKeys id xmlTree
      case verification of
        Nothing -> return . Left $ "No matching algorithm found for " <> show assertion
        Just False ->
          return . Left $ "Signature verification failed for " <> show assertion
        _ -> return . Right $ assertion
    verifySAMLAssertion _ =
      return . Left $ "Encryption of assertions is not supported (for now at least)"

optionallyVerifyResponseSignature
  :: MonadBase IO m => SIG.PublicKeys -> HXT.XmlTree -> m SAMLP.Response
optionallyVerifyResponseSignature publicKeys xmlTree = do
  if hasSignature
    then do
      liftBase . handle handleErr $ SIG.verifySAMLProtocol' publicKeys xmlTree
    else either (throw . SignatureVerificationException) return $ XML.docToSAML xmlTree
  where
    handleErr :: ErrorCall -> IO SAMLP.Response
    handleErr = throw . SignatureVerificationException . show

    hasSignature :: Bool
    hasSignature =
      case
          HXT.runLA
            (HXT.getChildren HXT.>>> HXT.isElem HXT.>>> HXT.hasQName
              (XML.mkNName SIG.ns "Signature")
            )
            xmlTree
        of
          [_] -> True
          _   -> False

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
      listToMaybe . catMaybes $ findAttributeInPossiblyEncrypted <$> toList
        (A.attributeStatement attribute)
    findAttributeInStatement _ = Nothing

    findAttributeInPossiblyEncrypted :: A.PossiblyEncrypted A.Attribute -> Maybe String
    findAttributeInPossiblyEncrypted (A.NotEncrypted a) =
      if A.attributeName a == attributeName
        then listToMaybe . catMaybes $ findFirstAttributeInNodes <$> A.attributeValues a
        else Nothing
    findAttributeInPossiblyEncrypted _ = Nothing

    findFirstAttributeInNodes :: XML.Nodes -> Maybe String
    findFirstAttributeInNodes = listToMaybe . mapMaybe findFirstNonEmptyAttributeValue

    findFirstNonEmptyAttributeValue :: HXT.XmlTree -> Maybe String
    findFirstNonEmptyAttributeValue xmlTree =
      listToMaybe $ HXT.runLA (HXT.neg HXT.isWhiteSpace >>> HXT.getText) xmlTree

getNonEmptyNameID :: A.Assertion -> Maybe String
getNonEmptyNameID assertion = getDecryptedSubjectIdentifier
  =<< A.subjectIdentifier (A.assertionSubject assertion)
  where
    getDecryptedSubjectIdentifier :: A.PossiblyEncrypted A.Identifier -> Maybe XML.XString
    getDecryptedSubjectIdentifier (A.NotEncrypted (A.IdentifierName nameID)) =
      Just . getSimpleBaseIDName $ nameID
    getDecryptedSubjectIdentifier _ = Nothing

    getSimpleBaseIDName :: A.NameID -> XML.XString
    getSimpleBaseIDName = A.baseID . A.nameBaseID
