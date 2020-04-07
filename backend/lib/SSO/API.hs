-- Due to a possibly bug in Brittany, we'll disable the formatter altogether
-- brittany --exactprint-only

module SSO.API(sso)
  where

import Data.Foldable (asum)
import Data.Text as T hiding (map)
import Data.Time.Clock (addUTCTime, nominalDay)
import Data.Time.Format
import Happstack.Server hiding (dir, host, https, path, simpleHTTP)
import Happstack.StaticRouting (Route, choice, dir)
import Log
import qualified Crypto.PubKey.RSA.Types as RSA
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
import Util.MonadUtils
import Utils.HTTP (isSecure)

sso :: Route (Kontra Response)
sso = choice
  [ (dir "sso" . dir "saml" . dir "acs" . hPost . toK0) consumeAssertions
  , (dir "sso" . dir "saml" . dir "metadata" . hGet) renderMetadata
  ]

consumeAssertions :: Kontrakcja m => m InternalKontraResponse
consumeAssertions = guardHttps $ do
  ssoConf      <- guardJustM getConf
  samlResponse <- apiV2ParameterObligatory $ ApiV2ParameterText "SAMLResponse"
  parseSAMLXML samlResponse >>= \case
    Right xmlTree -> do
      idpID      <- maybe idpApiErr return (getIDPID xmlTree)
      publicKeys <- maybe (pubKeyApiErr idpID) return (getPublicKeys ssoConf idpID)
      getVerifiedAssertionsFromSAML publicKeys xmlTree >>= \case
        (Left  msg               ) -> apiError . invalidAuthorizationWithMsg $ T.pack msg
        (Right verifiedAssertions) -> do
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
                    loginOrCreateNewAccount (SAMLPrincipal
                                              (fromMaybe "" mFirstName)
                                              (fromMaybe "" mLastName)
                                              email
                                              (fromMaybe "" mNameID))
                                            idpID
                  Nothing ->
                    apiError
                      . requestFailed
                      $ "No valid 'email' attribute provided in passed SAML assertions."
    Left message ->
      apiError . requestParameterParseError "SAMLResponse" . T.pack $ message
  where
    guardHttps :: Kontrakcja m => m InternalKontraResponse -> m InternalKontraResponse
    guardHttps action = do
      secure   <- isSecure
      useHttps <- view #useHttps <$> getContext
      if secure || not useHttps then
                                  action
                                else
                                  unexpectedError "The system is not set up correctly"

    noAssertionsApiErr :: (Kontrakcja m) => m a
    noAssertionsApiErr = apiError . requestFailed $ "No verified SAML assertions found."
    idpApiErr :: (Kontrakcja m) => m a
    idpApiErr =
      apiError
        . requestFailed
        $ "idpID not found (ie. Issuer element in the SAMLResponse)"

    pubKeyApiErr :: (Kontrakcja m, TextShow t) => t -> m a
    pubKeyApiErr t =
      apiError
        .  requestFailed
        $  "No public keys configured for the provider with id "
        <> showt t

    getPublicKeys :: SSOConf -> String -> Maybe SIG.PublicKeys
    getPublicKeys ssoConf idpID = do
      let rsaPublicKey = getRSAPublicKey ssoConf idpID
      fmap (\k -> SIG.PublicKeys { publicKeyRSA = Just k, publicKeyDSA = Nothing })
           rsaPublicKey

    getIDPConf :: String -> SSOConf -> Maybe IDPConf
    getIDPConf idpID ssoc =
      Prelude.find (\ic -> icID ic == T.pack idpID) (scIdpConfs ssoc)

    getRSAPublicKey :: SSOConf -> String -> Maybe RSA.PublicKey
    getRSAPublicKey ssoc idpID = icPublicKey <$> getIDPConf idpID ssoc

    loginOrCreateNewAccount :: Kontrakcja m => SAMLPrincipal -> String -> m InternalKontraResponse
    loginOrCreateNewAccount p idpID = do
      ctx     <- getContext
      ssoc    <- guardJust $ ssoConf ctx
      idpconf <- guardJust $ getIDPConf idpID ssoc
      mAccount <- getAccountInAcceptedStateOrFail $ spEmail p
      account <- maybe (createAccount idpconf p) return mAccount
      startSessionForSAMLUser account
      withPositionUpdated <- updateUserWithNameIdInCompanyPosition idpconf account (spNameID p)
      withTosCheck (\_ -> return . internalResponse $ LinkLogin LANG_EN) withPositionUpdated

    getAccountInAcceptedStateOrFail :: Kontrakcja m => Email -> m (Maybe User)
    getAccountInAcceptedStateOrFail email = do
      (dbQuery . GetUserWithStatusByEmail $ email) >>=
        mapM (\(user, isDeleted) -> do
            when (isDeleted || (user ^. #accountSuspended)) . apiError $ invalidAuthorizationWithMsg ("can't log in " <> showt email <> ", account either deleted or suspended")
            return user)

    createAccount :: Kontrakcja m => IDPConf -> SAMLPrincipal -> m User
    createAccount idpconf p = do
      cuctx <- getCreateUserContextFromContext
      createUser (spEmail p)
                      (spFirstname p               , spLastname p)
                      (icUserInitialGroupID idpconf, False)
                      LANG_EN
                      BySSO
                      cuctx
            >>= \case
                  Nothing   -> unexpectedError "User creation failed."
                  Just user -> do
                    void . dbUpdate $ SetLoginAuth (user ^. #id) LoginAuthSSO
                    return $ set #sysAuth LoginAuthSSO user

    updateUserWithNameIdInCompanyPosition :: Kontrakcja m => IDPConf -> User -> Text -> m User
    updateUserWithNameIdInCompanyPosition idpconf user nameID = do
      if icPutNameIDInCompanyPosition idpconf then do
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

getConf :: Kontrakcja m => m (Maybe SSOConf)
getConf = ssoConf <$> getContext

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
  validUntil <- formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%SZ") . calculateValidUntil <$> currentTime
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
