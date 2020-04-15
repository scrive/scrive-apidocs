-- Due to a possibly bug in Brittany, we'll disable the formatter altogether
-- brittany --exactprint-only

module SSO.Control(sso)
  where

import Control.Monad.Catch
import Data.Foldable (asum)
import Data.Text as T hiding (map)
import Data.Time.Clock (addUTCTime, nominalDay)
import Data.Time.Format
import Happstack.Server hiding
  ( badRequest, dir, host, https, path, simpleHTTP, unauthorized
  )
import Happstack.StaticRouting (Route, choice, dir)
import Log
import qualified Network.URI as U
import qualified SAML2.XML.Signature as SIG
import qualified Text.StringTemplates.Fields as F

import API.V2.Errors
import API.V2.MonadUtils
import API.V2.Parameters
import AppView
import Context.Internal
import DB
import InternalResponse
import Kontra
import KontraLink
import LoginAuth.LoginAuthMethod
import Routing
import Session.Model
import Session.Types
import SSO.Conf
import SSO.Guards
import SSO.SAML
import Templates
import User.Action
import User.Email (Email(..))
import User.Model
import User.Utils
import UserGroup.Model
import UserGroup.Types
import Util.MonadUtils
import Utils.HTTP (isSecure)

sso :: Route (Kontra Response)
sso = choice
  [ (dir "sso" . dir "saml" . dir "acs" . hPost . toK0) consumeAssertions
  , (dir "sso" . dir "saml" . dir "metadata" . hGet) renderMetadata
  ]

consumeAssertions :: Kontrakcja m => m InternalKontraResponse
consumeAssertions = guardHttps . handle handleSAMLException $ do
  ssoConf            <- guardJustM getConf
  samlResponse       <- apiV2ParameterObligatory $ ApiV2ParameterText "SAMLResponse"
  xmlTree            <- parseSAMLXML samlResponse
  idpID              <- maybe idpApiErr (return . T.pack) (getIDPID xmlTree)
  ug <- dbQuery $ UserGroupGetBySSOIDPID idpID
  ugSSOConf <- maybe (ssoConfigErr idpID) return $
      (ug >>= view #settings >>= view #ssoConfig)
      <|>
      getSSOConfFromConfig ssoConf idpID
  verifiedAssertions <- getVerifiedAssertionsFromSAML (getPublicKeys ugSSOConf) xmlTree
  guardAssertionsConditionsAreMet (scSamlEntityBaseURI ssoConf)
                                  (const currentTime)
                                  verifiedAssertions
  (mNameID, memailRaw, mFirstName, mLastName) <- do
    maybe noAssertionsApiErr (\a -> do
           let maybeGetSomeAttribute as =
                 T.pack <$> (asum . map (`getFirstNonEmptyAttribute` a) $ as)
           return (T.pack <$> getNonEmptyNameID a,
                   maybeGetSomeAttribute ["email", "emailaddress", "http://schemas.xmlsoap.org/ws/2005/05/identity/claims/emailaddress"],
                   maybeGetSomeAttribute ["firstname", "givenname", "http://schemas.xmlsoap.org/ws/2005/05/identity/claims/givenname"],
                   maybeGetSomeAttribute ["lastname", "surname", "http://schemas.xmlsoap.org/ws/2005/05/identity/claims/surname"]))
           (listToMaybe verifiedAssertions)
  case memailRaw of
          Just emailRaw -> do
            let email = Email emailRaw
            loginOrCreateNewAccount ugSSOConf
                                    (SAMLPrincipal
                                      (fromMaybe "" mFirstName)
                                      (fromMaybe "" mLastName)
                                      email
                                      (fromMaybe "" mNameID))
          Nothing ->
              badRequest "No valid 'email' attribute provided in passed SAML assertions."
  where
    handleSAMLException :: Kontrakcja m => SAMLException -> m InternalKontraResponse
    handleSAMLException = \case
                    XMLParseException msg -> badRequest $ "Parsing of \"SAMLResponse\" parameter failed: " <> T.pack msg
                    SignatureVerificationException msg -> unauthorized $ T.pack msg

    guardHttps :: Kontrakcja m => m InternalKontraResponse -> m InternalKontraResponse
    guardHttps action = do
      secure   <- isSecure
      useHttps <- view #useHttps <$> getContext
      if secure || not useHttps then
                                  action
                                else
                                  unexpectedError "The system is not set up correctly"

    noAssertionsApiErr :: (Kontrakcja m) => m a
    noAssertionsApiErr = badRequest "No verified SAML assertions found."

    idpApiErr :: (Kontrakcja m) => m a
    idpApiErr = badRequest "idpID not found (ie. Issuer element in the SAMLResponse)"

    ssoConfigErr :: (Kontrakcja m) => Text -> m a
    ssoConfigErr idpID = unauthorized $ "No SSO configuration for IDP with ID: " <> idpID

    getSSOConfFromConfig :: SSOConf -> Text -> Maybe UserGroupSSOConfiguration
    getSSOConfFromConfig systemSSOConfig idpID = Prelude.find (\i -> (i ^. #idpID) == idpID) $ scSamlIdps systemSSOConfig

    getPublicKeys :: UserGroupSSOConfiguration -> SIG.PublicKeys
    getPublicKeys ssoConf = SIG.PublicKeys { publicKeyRSA = Just $ ssoConf ^. #publicKey, publicKeyDSA = Nothing }

    loginOrCreateNewAccount :: Kontrakcja m => UserGroupSSOConfiguration -> SAMLPrincipal -> m InternalKontraResponse
    loginOrCreateNewAccount ugSSOConf p = do
      mAccount <- getAccountInAcceptedStateOrFail $ spEmail p
      let ugID = ugSSOConf ^. #userInitialGroupID
      account <- maybe (createAccount p ugID) return mAccount
      startSessionForSAMLUser account
      withPositionUpdated <- updateUserWithNameIdInCompanyPosition ugSSOConf account (spNameID p)
      withTosCheck (\_ -> return . internalResponse $ LinkLogin LANG_EN) withPositionUpdated

    getAccountInAcceptedStateOrFail :: Kontrakcja m => Email -> m (Maybe User)
    getAccountInAcceptedStateOrFail email = do
      (dbQuery . GetUserWithStatusByEmail $ email) >>=
        mapM (\(user, isDeleted) -> do
            when (isDeleted || (user ^. #accountSuspended)) $ unauthorized ("can't log in " <> showt email)
            return user)

    createAccount :: Kontrakcja m => SAMLPrincipal -> UserGroupID -> m User
    createAccount p ugID = do
      cuctx <- getCreateUserContextFromContext
      createUser (spEmail p)
                      (spFirstname p               , spLastname p)
                      (ugID, False)
                      LANG_EN
                      BySSO
                      cuctx
            >>= \case
                  Nothing   -> unexpectedError "User creation failed."
                  Just user -> do
                    void . dbUpdate $ SetLoginAuth (user ^. #id) LoginAuthSSO
                    return $ set #sysAuth LoginAuthSSO user

    updateUserWithNameIdInCompanyPosition :: Kontrakcja m => UserGroupSSOConfiguration -> User -> Text -> m User
    updateUserWithNameIdInCompanyPosition ssoConfig user nameID = do
      if ssoConfig ^. #putNameIDInCompanyPosition then do
        let positionWithNameID = "NameID:" <> nameID
            oldUI = view #info user
            newUI = set #companyPosition positionWithNameID oldUI
        void . dbUpdate $ SetUserInfo (user ^. #id) newUI
        return $ set #info newUI user
      else
        return user

    startSessionForSAMLUser :: Kontrakcja m => User -> m ()
    startSessionForSAMLUser user = do
      let userID = user ^. #id
      when (user ^. #sysAuth /= LoginAuthSSO) $ apiError insufficientPrivileges
      logInfo_ "Creating session for SSO:SAML"
      session  <- startNewSessionWithUser userID
      msession <- updateSession session tempSessionID (Just userID) Nothing
      case msession of
        Nothing -> unexpectedError "No session could be established"
        Just _  -> return ()

data SAMLPrincipal =
  SAMLPrincipal
  {
    spFirstname    :: Text
  , spLastname     :: Text
  , spEmail        :: Email
  , spNameID       :: Text
  }

renderMetadata :: Kontrakcja m => m Response
renderMetadata = do
  conf <- guardJustM getConf
  let baseURI = scSamlEntityBaseURI conf
  validUntil <- formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" . calculateValidUntil <$> currentTime
  response <- renderTextTemplate "samlMetadata" $ do
    F.value "samlEntityBaseURI" . show $ baseURI
    F.value "samlLogoutURI" . show $ relativeURI baseURI "logout"
    F.value "samlACSURI" . show $ relativeURI baseURI "sso/saml/acs"
    F.value "validUntil" validUntil
  simpleXMLResponse response
  where
    relativeURI :: U.URI -> String -> U.URI
    relativeURI base path =
      U.relativeTo (fromJust . U.parseRelativeReference $ path) base
    calculateValidUntil :: UTCTime -> UTCTime
    calculateValidUntil = addUTCTime (7 * nominalDay)

getConf :: Kontrakcja m => m (Maybe SSOConf)
getConf = ssoConf <$> getContext
