module User.API (
    userAPI,
    apiCallGetUserPersonalToken,
    apiCallGetUserProfile,
    apiCallLoginUser,
    apiCallChangeUserPassword,
    apiCallUpdateUserProfile,
    apiCallChangeEmail,
    apiCallSignup,
    apiCallLoginUserAndGetSession,
    setup2FA,
    confirm2FA,
    disable2FA,
    apiCallDeleteUser,
    apiCallSetDataRetentionPolicy,
    apiCallGetTokenForPersonalCredentials
  ) where

import Control.Monad (when)
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Aeson
import Data.Unjson
import Happstack.Server hiding
  ( dir, forbidden, host, lookCookieValue, ok, path, resp, simpleHTTP )

import Happstack.StaticRouting
import Log
import Text.JSON.Gen hiding (object)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Text.JSON as J

import AccessControl.Types
import API.Monad.V1
import API.V2.Parameters
import API.V2.Utils
import Context
import DataRetentionPolicy
import DataRetentionPolicy.Guards
import DB
import Doc.API.V2.Guards (guardThatUserExists)
import Folder.Model
import Happstack.Fields
import InputValidation
import Kontra
import KontraLink
import Log.Identifier
import Mails.SendMail
import MinutesTime
import OAuth.Model
import OAuth.Util
import PasswordService.Control
import Redirect
import Routing
import Salesforce.AuthorizationWorkflow
import Session.Cookies
import Session.Model
import ThirdPartyStats.Core
import User.Action
import User.CallbackScheme.Model
import User.Email
import User.EmailChangeRequest
import User.History.Model
import User.JSON
import User.Model
import User.PasswordReminder
import User.TwoFactor
import User.UserAccountRequest
import User.UserControl
import User.UserView
import UserGroup.Model
import UserGroup.Types
import UserGroup.Types.Subscription
import Util.HasSomeUserInfo
import Util.QRCode (encodeQR, unQRCode)
import Utils.Monad
import qualified API.V2 as V2
import qualified API.V2.Errors as V2
import qualified API.V2.Parameters as V2

userAPI :: Route (Kontra Response)
userAPI = dir "api" $ choice
  [ dir "frontend" $ userAPIV2
  , userAPIV1 -- Temporary backwards compatibility for clients accessing version-less API
  , dir "v1" $ userAPIV1
  , dir "v2" $ userAPIV2
  ]

userAPIV1 :: Route (Kontra Response)
userAPIV1 = choice
  [ dir "getpersonaltoken" $ hPost $ toK0 $ apiCallGetUserPersonalToken
  , dir "signup" $ hPost $ toK0 $ apiCallSignup
  , dir "login" $ hPostNoXToken $ toK0 $ apiCallLoginUser
  , dir "sendpasswordresetmail" $ hPost $ toK0 $ apiCallSendPasswordReminder
  , dir "getprofile" $ hGet $ toK0 $ apiCallGetUserProfile
  , dir "getsubscription" $ hGet $ toK0 $ apiCallGetSubscription
  , dir "changepassword" $ hPost $ toK0 $ apiCallChangeUserPassword
  , dir "updateprofile" $ hPost $ toK0 $ apiCallUpdateUserProfile
  , dir "changeemail" $ hPost $ toK0 $ apiCallChangeEmail
  , dir "getcallbackscheme" $ hGet $ toK0 $ apiCallUserGetCallbackScheme
  , dir "testsalesforceintegration" $ hGet $ toK0 $ apiCallTestSalesforceIntegration
  , dir "setsalesforcecallbacks" $ hPost $ toK0 $ apiCallSetSalesforceCallbacks
  ]

userAPIV2 :: Route (Kontra Response)
userAPIV2 = choice
  [ dir "loginandgetsession" $ hPost $ toK0 $ apiCallLoginUserAndGetSession
  , dir "2fa" $ dir "setup" $ hPost $ toK0 $ setup2FA
  , dir "2fa" $ dir "confirm" $ hPost $ toK0 $ confirm2FA
  , dir "2fa" $ dir "disable" $ hPost $ toK0 $ disable2FA
  , dir "isuserdeletable" $ hPost $ toK0 $ apiCallIsUserDeletable
  , dir "deleteuser" $ hPost $ toK0 $ apiCallDeleteUser
  , dir "dataretentionpolicy" $ hGet $ toK0 $ apiCallGetDataRetentionPolicy
  , dir "dataretentionpolicy" $ dir "set" $ hPost $ toK0 $ apiCallSetDataRetentionPolicy
  , dir "usagestats"
  $ dir "days"
  $ hGet
  $ toK0
  $ User.UserControl.handleUsageStatsJSONForUserDays
  , dir "usagestats"
  $ dir "months"
  $ hGet
  $ toK0
  $ User.UserControl.handleUsageStatsJSONForUserMonths
  , dir "gettokenforpersonalcredentials"
  $ hPost
  $ toK1
  $ apiCallGetTokenForPersonalCredentials
  , dir "updateusersprofile" $ hPost $ toK1 $ apiCallUpdateOtherUserProfile
  , dir "changeusersemail" $ hPost $ toK1 $ apiCallChangeOtherUserEmail
  , dir "usagestats"
  $ dir "shareablelink"
  $ dir "days"
  $ hGet
  $ toK0
  $ User.UserControl.handleUsageStatsJSONForShareableLinks PartitionByDay
  , dir "usagestats"
  $ dir "shareablelink"
  $ dir "months"
  $ hGet
  $ toK0
  $ User.UserControl.handleUsageStatsJSONForShareableLinks PartitionByMonth
  , dir "checkpassword" $ hPost $ toK0 $ apiCallCheckPassword
  , dir "activateuser" $ hPost $ toK0 $ apiCallActivateAccount
  , userAPIV1
  ]

apiCallGetUserPersonalToken :: Kontrakcja m => m Response
apiCallGetUserPersonalToken = api $ do
  memail      <- getField "email"
  mpasswd     <- getField "password"
  mtotpcode   <- getOptionalField asWord32 "totp"
  mlogintoken <- apiV2ParameterOptional (ApiV2ParameterRead "login_token")
  case (memail, mpasswd, mlogintoken) of
    (Just email, Just passwd, Nothing) -> do
        -- check the user things here
      muser <- dbQuery $ GetUserByEmail (Email email)
      case muser of
        Nothing ->
          -- use an ambiguous message, so that this cannot be used to determine
          -- whether a user has an account with Scrive
          throwM . SomeDBExtraException $ forbidden wrongPassMsg
        Just user -> do
          ctx <- getContext
          if maybeVerifyPassword (userpassword user) passwd
            then case (usertotp user, usertotpactive user, mtotpcode) of
              -- No TOTP is active
              (_        , False, _            ) -> returnTokenFor ctx user
              -- TOTP is active and present
              (Just totp, True , Just totpcode) -> do
                now <- currentTime
                if verifyTOTPCode totp now totpcode
                  then returnTokenFor ctx user
                  else return . Left $ forbidden "TOTP incorrect"
              -- TOTP is active but not present
              (_, True, Nothing) -> return . Left $ forbidden "TOTP code missing"
              (Nothing, True, Just _) ->
                unexpectedError "TOTP condition should not happen"
            else do
              void . dbUpdate $ LogHistoryAPIGetPersonalTokenFailure
                (userid user)
                (ctx ^. #ctxIpNumber)
                (ctx ^. #ctxTime)
              logInfo "getpersonaltoken failed (invalid password)" $ logObject_ user
              -- we do not want rollback here, so we don't raise exception
              return . Left $ forbidden wrongPassMsg
    (Nothing, Nothing, Just logintoken) -> do
        -- validate the login token here
      now             <- currentTime
      muserAndExpired <- dbQuery $ GetUserByTempLoginToken now logintoken
      case muserAndExpired of
        Nothing              -> return . Left $ forbidden invalidTokenMsg
        Just (user, expired) -> if expired
          then return . Left $ forbidden "This token has expired"
          else (`returnTokenFor` user) =<< getContext
    (Just _, Just _, Just _) -> do
      return . Left $ forbidden
        "You must provide either an Email/password or a login_token (but not both)."
    _ ->
      throwM . SomeDBExtraException $ forbidden "Email/password or login_token is missing"
  where
    wrongPassMsg    = "Email and password don't match"
    invalidTokenMsg = "The login_token provided is invalid"
    returnTokenFor ctx user = do
      let uid = userid user
      _success <- dbUpdate $ CreatePersonalToken uid
      token    <- dbQuery $ GetPersonalToken uid
      case token of
        Nothing -> throwM . SomeDBExtraException $ serverError
          "No token found, this should not happend"
        Just t -> do
          attemptCount <- dbQuery $ GetUserRecentAuthFailureCount (userid user)
          if attemptCount <= 5
            then do
              void $ dbUpdate $ LogHistoryAPIGetPersonalTokenSuccess
                uid
                (ctx ^. #ctxIpNumber)
                (ctx ^. #ctxTime)
              return $ Right $ Ok (unjsonOAuthAuthorization, t)
            else
              -- use an ambiguous message, so that this cannot be used to determine
              -- whether a user has an account with Scrive
                 throwM . SomeDBExtraException $ forbidden wrongPassMsg


setup2FA :: Kontrakcja m => m Response
setup2FA = V2.api $ do
  (user, _) <- V2.getAPIUserWithAnyPrivileges
  if usertotpactive user
    then V2.Ok <$> runJSONGenT (value "twofactor_active" True)
    else do
      key <- createTOTPKey
      ok  <- dbUpdate $ SetUserTOTPKey (userid user) key
      if ok
        then do
          let email = useremail . userinfo $ user
          url    <- view ctxDomainUrl <$> getContext
          qrCode <- liftIO $ makeQRFromURLEmailAndKey (T.unpack url) email key
          return . V2.Ok <$> runJSONGen $ do
            value "twofactor_active" False
            value "qr_code"          (BS.unpack . Base64.encode . unQRCode $ qrCode)
        else V2.apiError $ V2.serverError "Could not set TOTP key"

confirm2FA :: Kontrakcja m => m Response
confirm2FA = V2.api $ do
  ctx       <- getContext
  (user, _) <- V2.getAPIUserWithAnyPrivileges
  totpcode  <- V2.apiV2ParameterObligatory $ V2.ApiV2ParameterInt "totp"
  now       <- currentTime
  case (usertotp user, usertotpactive user) of
    (Just totpkey, False) -> if verifyTOTPCode totpkey now (fromIntegral totpcode)
      then do
        r <- dbUpdate $ ConfirmUserTOTPSetup (userid user)
        if r
          then do
            void $ dbUpdate $ LogHistoryTOTPEnable (userid user)
                                                   (ctx ^. #ctxIpNumber)
                                                   (ctx ^. #ctxTime)
            return . V2.Ok <$> runJSONGen $ do
              value "twofactor_active" r
              value "totp_valid"       True
          else V2.apiError $ V2.serverError "Could not confirm user TOTP setup"
      else V2.apiError $ V2.requestParameterInvalid "totp" "The code is not valid"
    (_, tfa) -> V2.Ok <$> runJSONGenT (value "twofactor_active" tfa)

disable2FA :: Kontrakcja m => m Response
disable2FA = V2.api $ do
  ctx       <- getContext
  (user, _) <- V2.getAPIUserWithAnyPrivileges
  if usertotpactive user
    then do
      r <- dbUpdate $ DisableUserTOTP (userid user)
      if r
        then do
          void $ dbUpdate $ LogHistoryTOTPDisable (userid user)
                                                  (ctx ^. #ctxIpNumber)
                                                  (ctx ^. #ctxTime)
          V2.Ok <$> runJSONGenT (value "twofactor_active" False)
        else V2.apiError $ V2.serverError "Could not disable TOTP"
    else V2.Ok <$> runJSONGenT (value "twofactor_active" False)

apiCallGetUserProfile :: Kontrakcja m => m Response
apiCallGetUserProfile = api $ do
  (user, _, _) <- getAPIUserWithAnyPrivileges
  ugwp         <- dbQuery . UserGroupGetWithParentsByUserID . userid $ user
  return $ Ok $ userJSONWithCompany user ugwp

apiCallGetSubscription :: Kontrakcja m => m Response
apiCallGetSubscription = api $ do
  (user, _, _) <- getAPIUserWithAnyPrivileges
  ugwp         <- dbQuery . UserGroupGetWithParentsByUserID . userid $ user
  sub          <- getSubscription ugwp
  return . Ok $ unjsonToJSON unjsonDef sub

apiCallChangeUserPassword :: Kontrakcja m => m Response
apiCallChangeUserPassword = api $ do
  ctx          <- getContext
  (user, _, _) <- getAPIUserWithAnyPrivileges
  oldpassword  <- getField' "oldpassword"
  password     <- getField' "password"
  goodPassword <- checkPassword (ctx ^. #ctxPasswordServiceConf) password
  if goodPassword
    then if (maybeVerifyPassword (userpassword user) oldpassword)
      then do
        passwordhash <- createPassword password
        void $ dbUpdate $ SetUserPassword (userid user) passwordhash
        void $ dbUpdate $ LogHistoryPasswordSetup (userid user)
                                                  (ctx ^. #ctxIpNumber)
                                                  (ctx ^. #ctxTime)
                                                  (Just $ userid $ user)
        terminateAllUserSessionsExceptCurrent (userid user)
        Ok <$> (runJSONGenT $ value "changed" True)
      else do
        void $ dbUpdate $ LogHistoryPasswordSetupReq (userid user)
                                                     (ctx ^. #ctxIpNumber)
                                                     (ctx ^. #ctxTime)
                                                     (Just $ userid $ user)
        Ok <$> (runJSONGenT $ value "changed" False)
    else throwM . SomeDBExtraException $ serverError
      "New password fields do not match Scrive standard"

apiCallLoginUser :: Kontrakcja m => m Response
apiCallLoginUser = api $ do
  ctx          <- getContext
  (user, _, _) <- getAPIUser APIPersonal

  redirectUrl  <- apiGuardJustM (badInput "Redirect URL not provided or invalid.")
    $ getField "redirect"

  void $ dbUpdate $ LogHistoryLoginSuccess (userid user)
                                           (ctx ^. #ctxIpNumber)
                                           (ctx ^. #ctxTime)
  logUserToContext $ Just user
  sendRedirect $ LinkExternal redirectUrl

apiCallUpdateUserProfile :: forall  m . Kontrakcja m => m Response
apiCallUpdateUserProfile = api $ do
  (user, _, _) <- getAPIUser APIPersonal
  ctx          <- getContext

  -- allow empty strings through validation
  let getParameter :: Text -> (Text -> InputValidation.Result Text) -> Text -> m Text
      getParameter name validation prevValue = do
        mx <- apiV2ParameterOptional $ ApiV2ParameterTextWithValidation name $ emptyOK
          validation
        return $ fromMaybe prevValue mx

      getUserParameter
        :: Text -> (Text -> InputValidation.Result Text) -> (UserInfo -> Text) -> m Text
      getUserParameter n v getPrevValue = getParameter n v $ getPrevValue $ userinfo user

  companyposition <- getUserParameter "companyposition"
                                      asValidPosition
                                      usercompanyposition
  phone          <- getUserParameter "phone" asValidPhone userphone
  personalnumber <- getUserParameter "personalnumber"
                                     asValidPersonalNumber
                                     userpersonalnumber
  fstname <- getUserParameter "fstname" asValidName userfstname
  sndname <- getUserParameter "sndname" asValidName usersndname

  let ui = (userinfo user) { userfstname         = fstname
                           , usersndname         = sndname
                           , userpersonalnumber  = personalnumber
                           , usercompanyposition = companyposition
                           , userphone           = phone
                           }

  mlang <- (join . (fmap langFromCode)) <$> getField "lang"
  when_ (isJust mlang) $ dbUpdate $ SetUserSettings (userid user) $ (usersettings user)
    { lang = fromJust mlang
    }

  void $ dbUpdate $ SetUserInfo (userid user) ui
  void $ dbUpdate $ LogHistoryUserInfoChanged (userid user)
                                              (ctx ^. #ctxIpNumber)
                                              (ctx ^. #ctxTime)
                                              (userinfo user)
                                              ui
                                              (userid <$> ctx ^. #ctxMaybeUser)
  if (useriscompanyadmin user)
    then do
      ugwp <- dbQuery . UserGroupGetWithParentsByUserID . userid $ user
      let ug = ugwpUG ugwp
      companyname <- getParameter "companyname" asValidCompanyName $ ugName ug
      let getAddrParameter n v prevValue =
            getParameter n v $ prevValue . ugwpAddress $ ugwp
      number     <- getAddrParameter "companynumber" asValidCompanyNumber ugaCompanyNumber
      entityname <- getAddrParameter "companyentityname" asValidCompanyName ugaEntityName
      address    <- getAddrParameter "companyaddress" asValidAddress ugaAddress
      zip'       <- getAddrParameter "companyzip" asValidZip ugaZip
      city       <- getAddrParameter "companycity" asValidCity ugaCity
      country    <- getAddrParameter "companycountry" asValidCountry ugaCountry
      let ug'         = set #ugName companyname ug
          new_address = UserGroupAddress { ugaCompanyNumber = number
                                         , ugaEntityName    = entityname
                                         , ugaAddress       = address
                                         , ugaZip           = zip'
                                         , ugaCity          = city
                                         , ugaCountry       = country
                                         }
          ug'' = case ugAddress ug' of
            Just _ ->
              -- change address directly if it wasn't inherited
              set #ugAddress (Just new_address) ug'
            Nothing -> case new_address == ugwpAddress ugwp of
              True  -> ug'  -- no change => we keep inheriting the address
              False -> set #ugAddress (Just new_address) ug'  -- stop inheriting
      void $ dbUpdate $ UserGroupUpdate ug''
      Ok <$> (runJSONGenT $ value "changed" True)
    else do
      fs <- mapM
        getField
        [ "companyname"
        , "companynumber"
        , "companyentityname"
        , "companyaddress"
        , "companyzip"
        , "companycity"
        , "companycountry"
        ]
      when (any isJust fs) $ V2.apiError $ V2.APIError
        { errorType     = V2.InsufficientPrivileges
        , errorHttpCode = 403
        , errorMessage  =
          "You do not have permission to perform this action on company settings"
        }
      Ok <$> (runJSONGenT $ value "changed" True)

apiCallChangeEmail :: Kontrakcja m => m Response
apiCallChangeEmail = api $ do
  ctx          <- getContext
  (user, _, _) <- getAPIUser APIPersonal
  mnewemail    <- getOptionalField asValidEmail "newemail"
  case (Email <$> mnewemail) of
    (Just newemail) -> do
      mexistinguser <- dbQuery $ GetUserByEmail newemail
      case mexistinguser of
        Just _existinguser -> do
          sendChangeToExistingEmailInternalWarningMail user newemail
          Ok <$> (runJSONGenT $ value "send" False)
        Nothing -> do
          changeemaillink <- newEmailChangeRequestLink (userid user) newemail
          mail <- mailEmailChangeRequest ctx Nothing user newemail changeemaillink
          scheduleEmailSendout
            (mail
              { to = [ MailAddress { fullname = getFullName user
                                   , email    = unEmail newemail
                                   }
                     ]
              }
            )
          Ok <$> (runJSONGenT $ value "send" True)
    Nothing -> Ok <$> (runJSONGenT $ value "send" False)



{- |
   Handles submission of the signup form.
   Normally this would create the user, (in the process mailing them an activation link),
   but if the user already exists, we check to see if they have accepted the tos.  If they haven't,
   then we send them a new activation link because probably the old one expired or was lost.
   If they have then we stop the signup.
-}
apiCallSignup :: Kontrakcja m => m Response
apiCallSignup = api $ do
  ctx    <- getContext
  memail <- getOptionalField asValidEmail "email"
  when (isNothing memail) $ do
    throwM . SomeDBExtraException $ serverError "Email not provided or invalid"
  let email = fromJust memail
  firstname       <- fromMaybe "" <$> getOptionalField asValidName "firstName"
  lastname        <- fromMaybe "" <$> getOptionalField asValidName "lastName"
  phone           <- fromMaybe "" <$> getOptionalField asValidPhone "phone"
  companyName     <- fromMaybe "" <$> getOptionalField asValidCompanyName "companyName"
  companyPosition <- fromMaybe "" <$> getOptionalField asValidPosition "companyPosition"
  lang            <- fromMaybe (ctx ^. #ctxLang) <$> langFromCode <$> getField' "lang"
  switchLang lang
  muser  <- dbQuery $ GetUserByEmail $ Email email
  muser' <- case muser of
               -- creating account that already exists should send password reminder so customers stopp calling us
               -- CORE-631
    Just user
      | isJust $ userhasacceptedtermsofservice user -> sendPasswordReminder user
      >> return Nothing
      | otherwise -> return $ Just user
    Nothing -> do
      ugFolder <- dbUpdate . FolderCreate $ defaultFolder
      let ug0 = defaultUserGroup { ugName         = companyName
                                 , ugHomeFolderID = Just $ folderID ugFolder
                                 }
      ug <- dbUpdate $ UserGroupCreate ug0
      createUser (Email email) (firstname, lastname) (ugID ug, True) lang AccountRequest
  case muser' of
    -- return ambiguous response in both cases to prevent a security issue
    Nothing   -> runJSONGenT $ value "sent" True
    Just user -> do
      void $ dbUpdate $ SetUserInfo (userid user) $ (userinfo user)
        { userphone           = phone
        , usercompanyposition = companyPosition
        }
      sendNewUserMail user
      l <- newUserAccountRequestLink lang (userid user) AccountRequest
      asyncLogEvent
        "Send account confirmation email"
        [ UserIDProp $ userid user
        , IPProp $ ctx ^. #ctxIpNumber
        , TimeProp $ ctx ^. #ctxTime
        , someProp "Context" ("Acount request" :: String)
        ]
        EventMixpanel
      asyncLogEvent
        SetUserProps
        [ UserIDProp $ userid user
        , someProp "Account confirmation email" $ ctx ^. #ctxTime
        , NameProp (firstname <> " " <> lastname)
        , FirstNameProp firstname
        , LastNameProp lastname
        , someProp "Confirmation link" $ show l
        ]
        EventMixpanel
      runJSONGenT $ value "sent" True

apiCallSendPasswordReminder :: Kontrakcja m => m Response
apiCallSendPasswordReminder = api $ do
  memail <- getOptionalField asValidEmail "email"
  case memail of
    Nothing    -> runJSONGenT $ value "send" False >> value "badformat" True
    Just email -> do
      -- Password reset could be abused to find out, whether an email is registered
      -- with Scrive. Se we return ambiguous response in all cases to prevent a security issue.
      muser <- dbQuery $ GetUserByEmail $ Email email
      case muser of
        Nothing -> do
          runJSONGenT $ value "send" True
        Just user -> do
          sendPasswordReminder user
          runJSONGenT $ value "send" True

sendPasswordReminder :: Kontrakcja m => User -> m ()
sendPasswordReminder user = do
  ctx  <- getContext
  minv <- dbQuery $ GetPasswordReminder $ userid user
  case minv of
    Just pr@PasswordReminder {..} -> case prRemainedEmails of
      0 -> return ()
      n -> do
        void $ dbUpdate $ UpdatePasswordReminder $ pr { prRemainedEmails = n - 1 }
        sendResetPasswordMail ctx (LinkPasswordReminder prUserID prToken)
    _ -> do
      link <- newPasswordReminderLink $ userid user
      sendResetPasswordMail ctx link
  where
    sendResetPasswordMail ctx link = do
      mail <- resetPasswordMail ctx user link
      scheduleEmailSendout $ mail { to = [getMailAddress user] }

apiCallUserGetCallbackScheme :: Kontrakcja m => m Response
apiCallUserGetCallbackScheme = api $ do
  (user, _, _) <- getAPIUser APIPersonal
  scheme       <- dbQuery $ GetUserCallbackSchemeByUserID $ userid user
  fmap Ok $ case scheme of
    Just (ConstantUrlScheme url) -> runJSONGenT $ do
      value "scheme"      ("constant" :: String)
      value "api_version" (1 :: Int)
      value "url"         url
    Just (ConstantUrlSchemeV2 url) -> runJSONGenT $ do
      value "scheme"      ("constant" :: String)
      value "api_version" (2 :: Int)
      value "url"         url
    Just (SalesforceScheme _key) -> runJSONGenT $ do
      value "scheme" ("salesforce" :: String)
    Just (BasicAuthScheme _lg _pwd) -> runJSONGenT $ do
      value "scheme" ("basic_auth" :: String)
    Just (OAuth2Scheme _name _password _url _scope) -> runJSONGenT $ do
      value "scheme" ("oauth2" :: String)
    Just (Hi3GScheme _name _password _url _scope) -> runJSONGenT $ do
      value "scheme" ("hi3g" :: String)
    Nothing -> runJSONGenT $ do
      value "scheme" ("none" :: String)


apiCallTestSalesforceIntegration :: Kontrakcja m => m Response
apiCallTestSalesforceIntegration = do
  api $ do
    (user, _, _) <- getAPIUser APIDocCheck
    scheme       <- dbQuery $ GetUserCallbackSchemeByUserID $ userid user
    murl         <- getField "url"
    when (isNothing murl) $ do
      throwM . SomeDBExtraException $ badInput $ "No 'url' parameter provided"
    let url = fromJust murl
    fmap Ok $ case scheme of
      Just (SalesforceScheme token) -> do
        ctx <- getContext
        case ctx ^. #ctxSalesforceConf of
          Nothing -> noConfigurationError "Salesforce"
          Just sc -> do
            res <- flip runReaderT sc $ testSalesforce token url
            case res of
              Right (http_code, resp) -> runJSONGenT $ do
                value "status"    ("ok" :: String)
                value "http_code" http_code
                value "response"  resp
              Left (msg, curl_err, stdout, stderr, http_code) -> runJSONGenT $ do
                value "status"         ("error" :: String)
                value "error_message"  msg
                value "http_code"      http_code
                value "curl_exit_code" curl_err
                value "curl_stdout"    stdout
                value "curl_stderr"    stderr
      _ -> throwM . SomeDBExtraException $ conflictError
        "Salesforce callback scheme is not set for this user"

apiCallSetSalesforceCallbacks :: Kontrakcja m => m Response
apiCallSetSalesforceCallbacks = do
  V2.api $ do
    -- We allow all permission although workflow with Partners API
    -- should use APIPersonal.
    (user, _) <- V2.getAPIUserWithAnyPrivileges
    ctx       <- getContext
    case (ctx ^. #ctxSalesforceConf) of
      Nothing ->
        V2.apiError $ V2.serverError $ "No configuration for Salesforce integration"
      Just sc -> do
        code   <- V2.apiV2ParameterObligatory (V2.ApiV2ParameterText "code")
        mtoken <- flip runReaderT sc (getRefreshTokenFromCode code)
        case mtoken of
          Left  emsg  -> V2.apiError $ V2.requestFailed emsg
          Right token -> do
            dbUpdate $ UpdateUserCallbackScheme (userid user) (SalesforceScheme token)
            return $ V2.Ok $ runJSONGen $ value "status" ("ok" :: String)

apiCallLoginUserAndGetSession :: Kontrakcja m => m Response
apiCallLoginUserAndGetSession = V2.api $ do
  -- parse oauth from json
  oauth <- V2.apiV2ParameterObligatory
    $ V2.ApiV2ParameterJSON "personal_token" unjsonOAuthAuthorization
  euser <- getUserFromOAuthWithAnyPrivileges oauth
  case euser of
    Left  err -> V2.apiError $ V2.invalidAuthorizationWithMsg err
    Right (User { userid }, _actor) -> do
      ctx <- getContext
      asyncLogEvent
        "Login"
        [UserIDProp userid, IPProp $ ctx ^. #ctxIpNumber, TimeProp $ ctx ^. #ctxTime]
        EventMixpanel
      asyncLogEvent SetUserProps
                    [UserIDProp userid, someProp "Last login" $ ctx ^. #ctxTime]
                    EventMixpanel
      session <- startNewSessionWithUser userid
      return $ V2.Ok $ runJSONGen $ do
        value "session_id" (showt $ sessionCookieInfoFromSession session)

apiCallIsUserDeletable :: Kontrakcja m => m Response
apiCallIsUserDeletable = V2.api $ do
  (user, _, _) <- getAPIUser APIPersonal

  mReason      <- dbQuery $ IsUserDeletable user

  return $ V2.Ok $ runJSONGen $ case mReason of
    Just reason -> do
      value "deletable" False
      value "reason"    reason
    Nothing -> do
      value "deletable" True

apiCallDeleteUser :: Kontrakcja m => m Response
apiCallDeleteUser = V2.api $ do
  (user, _, _) <- getAPIUser APIPersonal
  ctx          <- getContext

  email        <- V2.apiV2ParameterObligatory $ V2.ApiV2ParameterText "email"
  unless (unEmail (useremail (userinfo user)) == email)
    $ V2.apiError
    $ V2.requestParameterParseError
        "email"
        "the email provided does not match that of the user account"

  mReason <- dbQuery $ IsUserDeletable user
  case mReason of
    Just reason -> V2.apiError $ V2.conflictError $ userNotDeletableReasonToString reason
    Nothing     -> return ()

  void $ dbUpdate $ DeleteUser (userid user)
  void $ dbUpdate $ LogHistoryAccountDeleted (userid user)
                                             (userid user)
                                             (ctx ^. #ctxIpNumber)
                                             (ctx ^. #ctxTime)

  return $ V2.Ok ()

{-
 - User's data retention policy
 -}

apiCallGetDataRetentionPolicy :: Kontrakcja m => m Response
apiCallGetDataRetentionPolicy = V2.api $ do
  (user, _, _) <- getAPIUser APIPersonal

  let drp = dataretentionpolicy $ usersettings user
  ugwp <- dbQuery $ UserGroupGetWithParentsByUserID $ userid user
  let ugDRP = ugsDataRetentionPolicy $ ugwpSettings ugwp

  return $ V2.Ok $ object
    [ "data_retention_policy" .= unjsonToJSON unjsonDataRetentionPolicy drp
    , "company_data_retention_policy" .= unjsonToJSON unjsonDataRetentionPolicy ugDRP
    ]

apiCallSetDataRetentionPolicy :: Kontrakcja m => m Response
apiCallSetDataRetentionPolicy = V2.api $ do
  (user, _, _) <- getAPIUser APIPersonal

  ugwp         <- dbQuery $ UserGroupGetWithParentsByUserID $ userid user
  let ugDRP = ugsDataRetentionPolicy $ ugwpSettings ugwp
  drp <- V2.apiV2ParameterObligatory
    $ V2.ApiV2ParameterJSON "data_retention_policy" unjsonDataRetentionPolicy
  guardThatDataRetentionPolicyIsValid drp $ Just ugDRP

  let settings' = (usersettings user) { dataretentionpolicy = drp }
  void $ dbUpdate $ SetUserSettings (userid user) settings'

  return $ V2.Ok ()

apiCallGetTokenForPersonalCredentials :: Kontrakcja m => UserID -> m Response
apiCallGetTokenForPersonalCredentials uid = V2.api $ do
  -- Guards
  void $ guardThatUserExists uid
  apiAccessControl [mkAccPolicyItem (UpdateA, UserR, uid)] $ do
    minutes <- apiV2ParameterDefault defaultMinutes $ ApiV2ParameterInt "minutes"
    when (minutes < 1 || minutes > maxMinutes) invalidMinsParamError
    -- Create login token
    expirationTime <- (minutes `minutesAfter`) <$> currentTime
    hash           <- dbUpdate $ NewTemporaryLoginToken uid expirationTime
    qrCode         <- liftIO . encodeQR . J.encode . runJSONGen $ do
      value "type" ("token_personal_credentials_v1" :: String)
      value "token" $ show hash
    -- Return token info and QR code as JSON
    return . V2.Ok . runJSONGen $ do
      value "login_token" $ show hash
      value "qr_code" $ BS.unpack . Base64.encode . unQRCode $ qrCode
      value "expiration_time" $ show expirationTime
  where
    defaultMinutes = 5
    maxMinutes     = 30
    invalidMinsParamError =
      V2.apiError
        .  V2.requestParameterInvalid "minutes"
        $  "The value given is larger than the allowed maximum of "
        <> (showt maxMinutes)
        <> " or below 1."

apiCallCheckPassword :: Kontrakcja m => m Response
apiCallCheckPassword = api $ do
  ctx          <- getContext
  password     <- getField' "password"
  goodPassword <- checkPassword (ctx ^. #ctxPasswordServiceConf) password
  Ok <$> (runJSONGenT $ value "valid" goodPassword)

guardCanChangeUser :: Kontrakcja m => User -> User -> m ()
guardCanChangeUser adminuser otheruser = do
  unless
      (useriscompanyadmin adminuser && (usergroupid adminuser == usergroupid otheruser))
    $ do
        throwM . SomeDBExtraException $ forbidden "Can't change this user details"

apiCallUpdateOtherUserProfile :: forall  m . Kontrakcja m => UserID -> m Response
apiCallUpdateOtherUserProfile affectedUserID = V2.api $ do
  ctx                  <- getContext
  (authorizingUser, _) <- V2.getAPIUserWithPrivileges [APIPersonal]
  mAffectedUser        <- dbQuery $ GetUserByID affectedUserID
  case mAffectedUser of
    Nothing           -> throwM . SomeDBExtraException $ forbidden "User doesn't exist"
    Just affectedUser -> do
      guardCanChangeUser authorizingUser affectedUser
      let getUserParameter
            :: Text
            -> (Text -> InputValidation.Result Text)
            -> (UserInfo -> Text)
            -> m Text
          getUserParameter n v getPrevValue = do
            mx <- apiV2ParameterOptional $ ApiV2ParameterTextWithValidation n $ emptyOK v
            return $ fromMaybe (getPrevValue $ userinfo affectedUser) mx

      companyposition <- getUserParameter "companyposition"
                                          asValidPosition
                                          usercompanyposition
      phone          <- getUserParameter "phone" asValidPhone userphone
      personalnumber <- getUserParameter "personalnumber"
                                         asValidPersonalNumber
                                         userpersonalnumber
      fstname <- getUserParameter "fstname" asValidName userfstname
      sndname <- getUserParameter "sndname" asValidName usersndname
      let affectedUserNewInfo = (userinfo affectedUser) { userfstname = fstname
                                                        , usersndname = sndname
                                                        , userpersonalnumber = personalnumber
                                                        , usercompanyposition = companyposition
                                                        , userphone = phone
                                                        }
      void $ dbUpdate $ SetUserInfo affectedUserID affectedUserNewInfo
      void $ dbUpdate $ LogHistoryUserInfoChanged affectedUserID
                                                  (ctx ^. #ctxIpNumber)
                                                  (ctx ^. #ctxTime)
                                                  (userinfo affectedUser)
                                                  affectedUserNewInfo
                                                  (Just $ userid authorizingUser)

      mlang <- (join . (fmap langFromCode)) <$> getField "lang"
      when_ (isJust mlang)
        $ dbUpdate
        $ SetUserSettings affectedUserID
        $ (usersettings affectedUser) { lang = fromJust mlang }


      return $ V2.Ok $ (runJSONGen $ value "changed" True)


apiCallChangeOtherUserEmail :: Kontrakcja m => UserID -> m Response
apiCallChangeOtherUserEmail affectedUserID = V2.api $ do
  ctx                  <- getContext
  (authorizingUser, _) <- V2.getAPIUserWithPrivileges [APIPersonal]
  mAffectedUser        <- dbQuery $ GetUserByID affectedUserID
  case mAffectedUser of
    Nothing           -> throwM . SomeDBExtraException $ forbidden "User doesn't exist"
    Just affectedUser -> do
      guardCanChangeUser authorizingUser affectedUser
      newemail <-
        Email
          <$> ( apiV2ParameterObligatory
              $ ApiV2ParameterTextWithValidation "newemail"
              $ asValidEmail
              )
      mexistinguser <- dbQuery $ GetUserByEmail newemail
      case mexistinguser of
        Just _existinguser -> do
          sendChangeToExistingEmailInternalWarningMail affectedUser newemail
          return $ V2.Ok $ (runJSONGen $ value "sent" False)
        Nothing -> do
          changeemaillink <- newEmailChangeRequestLink affectedUserID newemail
          mail            <- mailEmailChangeRequest ctx
                                                    (Just authorizingUser)
                                                    affectedUser
                                                    newemail
                                                    changeemaillink
          scheduleEmailSendout
            (mail
              { to = [ MailAddress { fullname = getFullName affectedUser
                                   , email    = unEmail newemail
                                   }
                     ]
              }
            )
          return $ V2.Ok $ (runJSONGen $ value "sent" True)



apiCallActivateAccount :: Kontrakcja m => m Response
apiCallActivateAccount = V2.api $ do
  ctx         <- getContext
  tosAccepted <- apiV2ParameterObligatory (ApiV2ParameterBool "accepted_terms_of_service")
  unless (tosAccepted) $ do
    V2.apiError $ V2.requestParameterInvalid "accepted_terms_of_service"
                                             "Terms of service have to be accepted"
  email <- apiV2ParameterObligatory
    $ ApiV2ParameterTextWithValidation "email" asValidEmail
  muser <- dbQuery $ GetUserByEmail $ Email $ email
  let errMsgToPreventAddressHarvesting = "Token is not valid"
  case (muser, join $ userhasacceptedtermsofservice <$> muser) of
    (Nothing, _) -> V2.apiError $ V2.requestFailed errMsgToPreventAddressHarvesting
    (_, Just _) -> V2.apiError $ V2.requestFailed errMsgToPreventAddressHarvesting
    (Just user, Nothing) -> do
      token        <- apiV2ParameterObligatory (ApiV2ParameterRead "token")
      validRequest <- isJust <$> getUserAccountRequestUser (userid user) token
      unless (validRequest) $ do
        V2.apiError $ V2.requestFailed errMsgToPreventAddressHarvesting
      void $ dbUpdate $ DeleteUserAccountRequest (userid user)
      void $ dbUpdate $ AcceptTermsOfService (userid user) (ctx ^. #ctxTime)
      void $ dbUpdate $ LogHistoryTOSAccept (userid user)
                                            (ctx ^. #ctxIpNumber)
                                            (ctx ^. #ctxTime)
                                            Nothing
      password      <- apiV2ParameterObligatory (ApiV2ParameterText "password")
      validPassword <- checkPassword (ctx ^. #ctxPasswordServiceConf) password
      unless (validPassword) $ do
        V2.apiError $ V2.requestParameterInvalid "password" "Password is weak"
      passwordhash <- createPassword password
      void . dbUpdate $ SetUserPassword (userid user) passwordhash
      void $ dbUpdate $ LogHistoryPasswordSetup (userid user)
                                                (ctx ^. #ctxIpNumber)
                                                (ctx ^. #ctxTime)
                                                Nothing
      return $ V2.Ok $ runJSONGen $ do
        value "activated" True
