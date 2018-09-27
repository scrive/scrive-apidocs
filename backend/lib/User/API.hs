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
    apiCallSetDataRetentionPolicy
  ) where

import Control.Concurrent.Lifted
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Aeson
import Data.Label (modify)
import Data.Unjson
import Happstack.Server hiding (dir, forbidden, host, lookCookieValue, ok, path, resp, simpleHTTP)
import Happstack.Server.Internal.Cookie
import Happstack.StaticRouting
import Log
import Text.JSON.Gen hiding (object)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Map as Map
import qualified Data.Text as T

import API.Monad.V1
import Context
import DataRetentionPolicy
import DataRetentionPolicy.Guards
import DB
import Happstack.Fields
import InputValidation
import IPAddress
import Kontra
import KontraLink
import Log.Identifier
import Mails.SendMail
import OAuth.Model
import OAuth.Util
import Redirect
import Routing
import Salesforce.AuthorizationWorkflow
import Session.Cookies
import Session.Data
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
import User.Utils
import UserGroup.Data
import UserGroup.Data.Subscription
import UserGroup.Model
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.QRCode (unQRCode)
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
userAPIV1 = choice [
  dir "getpersonaltoken"     $ hPost $ toK0 $ apiCallGetUserPersonalToken,
  dir "signup"          $ hPost $ toK0 $ apiCallSignup,
  dir "login"          $ hPostNoXToken $ toK0 $ apiCallLoginUser,
  dir "sendpasswordresetmail" $ hPost $ toK0 $ apiCallSendPasswordReminder,
  dir "getprofile"      $ hGet $ toK0 $ apiCallGetUserProfile,
  dir "getsubscription"      $ hGet $ toK0 $ apiCallGetSubscription,
  dir "changepassword"  $ hPost $ toK0 $ apiCallChangeUserPassword,
  dir "updateprofile"   $ hPost $ toK0 $ apiCallUpdateUserProfile,
  dir "changeemail"     $ hPost $ toK0 $ apiCallChangeEmail,
  dir "getcallbackscheme" $ hGet $ toK0 $ apiCallUserGetCallbackScheme,
  dir "testsalesforceintegration" $ hGet $ toK0 $ apiCallTestSalesforceIntegration,
  dir "setsalesforcecallbacks" $ hPost $ toK0 $ apiCallSetSalesforceCallbacks
  ]

userAPIV2 :: Route (Kontra Response)
userAPIV2 = choice [
  dir "loginandgetsession" $ hPost $ toK0 $ apiCallLoginUserAndGetSession,

  dir "2fa" $ dir "setup"   $ hPost $ toK0 $ setup2FA,
  dir "2fa" $ dir "confirm" $ hPost $ toK0 $ confirm2FA,
  dir "2fa" $ dir "disable" $ hPost $ toK0 $ disable2FA,

  dir "isuserdeletable" $ hPost $ toK0 $ apiCallIsUserDeletable,
  dir "deleteuser" $ hPost $ toK0 $ apiCallDeleteUser,

  dir "dataretentionpolicy" $ hGet $ toK0 $ apiCallGetDataRetentionPolicy,
  dir "dataretentionpolicy" $ dir "set" $ hPost $ toK0 $ apiCallSetDataRetentionPolicy,

  userAPIV1
  ]

apiCallGetUserPersonalToken :: Kontrakcja m => m Response
apiCallGetUserPersonalToken = api $ do
    memail  <- getField "email"
    mpasswd <- getField "password"
    mtotpcode <- getOptionalField asWord32 "totp"
    case (memail, mpasswd) of
        (Just email, Just passwd) -> do
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
                    (_, False, _) -> returnTokenFor ctx user
                    (Just totp, True, Just totpcode) -> do
                      now <- currentTime
                      if verifyTOTPCode totp now totpcode
                         then returnTokenFor ctx user
                         else return . Left $ forbidden "TOTP incorrect"
                    (_, True, Nothing) ->
                      return . Left $ forbidden "TOTP code missing"
                    (Nothing, True, Just _) ->
                      unexpectedError "TOTP condition should not happen"
                  else do
                    _ <- dbUpdate $ LogHistoryAPIGetPersonalTokenFailure (userid user) (get ctxipnumber ctx) (get ctxtime ctx)
                    logInfo "getpersonaltoken failed (invalid password)" $ logObject_ user
                    -- we do not want rollback here, so we don't raise exception
                    return . Left $ forbidden wrongPassMsg
        _ -> throwM . SomeDBExtraException $ forbidden "Email or password is missing"
  where
    wrongPassMsg = "Email and password don't match"
    returnTokenFor ctx user = do
      let uid = userid user
      _success <- dbUpdate $ CreatePersonalToken uid
      token <- dbQuery $ GetPersonalToken uid
      case token of
           Nothing -> throwM . SomeDBExtraException $ serverError "No token found, this should not happend"
           Just t  -> do
             attemptCount <- dbQuery $ GetUserRecentAuthFailureCount (userid user)
             if attemptCount <= 5
               then do
                 _ <- dbUpdate $ LogHistoryAPIGetPersonalTokenSuccess uid (get ctxipnumber ctx) (get ctxtime ctx)
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
       ok <- dbUpdate $ SetUserTOTPKey (userid user) key
       if ok
         then do
           let email = useremail . userinfo $ user
           url <- ctxDomainUrl <$> getContext
           qrCode <- liftIO $ makeQRFromURLEmailAndKey url email key
           return . V2.Ok <$> runJSONGen $ do
             value "twofactor_active" False
             value "qr_code" (BS.unpack . Base64.encode . unQRCode $ qrCode)
         else V2.apiError $ V2.serverError "Could not set TOTP key"

confirm2FA :: Kontrakcja m => m Response
confirm2FA = V2.api $ do
  ctx <- getContext
  (user, _) <- V2.getAPIUserWithAnyPrivileges
  totpcode <- V2.apiV2ParameterObligatory $ V2.ApiV2ParameterInt "totp"
  now <- currentTime
  case (usertotp user, usertotpactive user) of
    (Just totpkey, False) ->
      if verifyTOTPCode totpkey now (fromIntegral totpcode)
         then do
           r <- dbUpdate $ ConfirmUserTOTPSetup (userid user)
           if r
              then do
                _ <- dbUpdate $ LogHistoryTOTPEnable (userid user) (get ctxipnumber ctx) (get ctxtime ctx)
                return . V2.Ok <$> runJSONGen $ do
                  value "twofactor_active" r
                  value "totp_valid" True
              else
                V2.apiError $ V2.serverError "Could not confirm user TOTP setup"
         else
           V2.apiError $ V2.requestParameterInvalid "totp" "The code is not valid"
    (_, tfa) -> V2.Ok <$> runJSONGenT (value "twofactor_active" tfa)

disable2FA :: Kontrakcja m => m Response
disable2FA = V2.api $ do
  ctx <- getContext
  (user, _) <- V2.getAPIUserWithAnyPrivileges
  if usertotpactive user
     then do
       r <- dbUpdate $ DisableUserTOTP (userid user)
       if r
          then do
            _ <- dbUpdate $ LogHistoryTOTPDisable (userid user) (get ctxipnumber ctx) (get ctxtime ctx)
            V2.Ok <$> runJSONGenT (value "twofactor_active" False)
          else
            V2.apiError $ V2.serverError "Could not disable TOTP"
     else V2.Ok <$> runJSONGenT (value "twofactor_active" False)

apiCallGetUserProfile :: Kontrakcja m => m Response
apiCallGetUserProfile =  api $ do
  (user, _ , _) <- getAPIUserWithAnyPrivileges
  ug <- getUserGroupForUser user
  return $ Ok $ userJSON user $ ug

apiCallGetSubscription :: Kontrakcja m => m Response
apiCallGetSubscription =  api $ do
  (user, _ , _) <- getAPIUserWithAnyPrivileges
  ug <- getUserGroupForUser user
  sub <- getSubscription ug
  return . Ok $ unjsonToJSON unjsonDef sub

apiCallChangeUserPassword :: Kontrakcja m => m Response
apiCallChangeUserPassword = api $ do
  ctx <- getContext
  (user, _ , _) <- getAPIUserWithAnyPrivileges
  oldpassword <- getField' "oldpassword"
  mpassword <- getOptionalField asValidPassword "password"
  case (mpassword) of
     (Just password) ->
          if (maybeVerifyPassword (userpassword user) oldpassword)
            then do
              passwordhash <- createPassword password
              _ <- dbUpdate $ SetUserPassword (userid user) passwordhash
              _ <- dbUpdate $ LogHistoryPasswordSetup (userid user) (get ctxipnumber ctx) (get ctxtime ctx) (Just $ userid $ user)
              Ok <$> (runJSONGenT $ value "changed" True)
            else do
              _ <- dbUpdate $ LogHistoryPasswordSetupReq (userid user) (get ctxipnumber ctx) (get ctxtime ctx) (Just $ userid $ user)
              Ok <$> (runJSONGenT $ value "changed" False)
     _ ->  throwM . SomeDBExtraException $ serverError "Newpassword fields do not match Scrive standard"

apiCallLoginUser :: Kontrakcja m => m Response
apiCallLoginUser = api $ do
  ctx <- getContext
  (user, _ , _) <- getAPIUser APIPersonal

  redirectUrl <- apiGuardJustM (badInput "Redirect URL not provided or invalid.") $ getField "redirect"

  _ <- dbUpdate $ LogHistoryLoginSuccess (userid user) (get ctxipnumber ctx) (get ctxtime ctx)
  logUserToContext $ Just user
  sendRedirect $ LinkExternal redirectUrl

apiCallUpdateUserProfile :: Kontrakcja m => m Response
apiCallUpdateUserProfile = api $ do
  (user, _ , _) <- getAPIUser APIPersonal
  ctx <- getContext
  infoUpdate <- getUserInfoUpdate

  mlang <- (join . (fmap langFromCode)) <$> getField "lang"
  when_ (isJust mlang) $ dbUpdate $ SetUserSettings (userid user) $ (usersettings user) { lang = fromJust mlang  }

  _ <- dbUpdate $ SetUserInfo (userid user) (infoUpdate $ userinfo user)
  _ <- dbUpdate $ LogHistoryUserInfoChanged (userid user) (get ctxipnumber ctx) (get ctxtime ctx)
                                               (userinfo user) (infoUpdate $ userinfo user)
                                               (userid <$> get ctxmaybeuser ctx)
  if (useriscompanyadmin user)
    then do
      ug <- getUserGroupForUser user
      mcompanyname <- getDefaultedField "" asValidCompanyName "companyname"
      ugaddressupdate <- getUserGroupAddressUpdate
      _ <- dbUpdate . UserGroupUpdate
        . maybe id (set ugName . T.pack) mcompanyname
        . modify ugAddress ugaddressupdate
        $ ug
      Ok <$> (runJSONGenT $ value "changed" True)
    else  Ok <$> (runJSONGenT $ value "changed" True)

apiCallChangeEmail :: Kontrakcja m => m Response
apiCallChangeEmail = api $ do
  ctx <- getContext
  (user, _ , _) <- getAPIUser APIPersonal
  mnewemail <- getOptionalField asValidEmail "newemail"
  case (Email <$> mnewemail) of
    (Just newemail) -> do
       mexistinguser <- dbQuery $ GetUserByEmail newemail
       case mexistinguser of
         Just _existinguser -> do
           sendChangeToExistingEmailInternalWarningMail user newemail
           Ok <$> (runJSONGenT $ value "send" False)
         Nothing -> do
            changeemaillink <- newEmailChangeRequestLink (userid user) newemail
            mail <- mailEmailChangeRequest ctx user newemail changeemaillink
            scheduleEmailSendout (mail{to = [MailAddress{
                                    fullname = getFullName user
                                  , email = unEmail newemail }]})
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
  ctx <- getContext
  memail <- getOptionalField asValidEmail "email"
  when (isNothing memail) $ do
    throwM . SomeDBExtraException $ serverError "Email not provided or invalid"
  let email = fromJust memail
  firstname       <- fromMaybe "" <$> getOptionalField asValidName "firstName"
  lastname        <- fromMaybe "" <$> getOptionalField asValidName "lastName"
  phone           <- fromMaybe "" <$> getOptionalField asValidPhone "phone"
  companyName     <- fromMaybe "" <$> getOptionalField asValidCompanyName "companyName"
  companyPosition <- fromMaybe "" <$> getOptionalField asValidPosition "companyPosition"
  lang <- fromMaybe (get ctxlang ctx) <$> langFromCode <$> getField' "lang"
  switchLang lang
  muser <- dbQuery $ GetUserByEmail $ Email email
  muser' <- case muser of
               -- creating account that already exists should send password reminder so customers stopp calling us
               -- CORE-631
               Just user | isJust $ userhasacceptedtermsofservice user -> sendPasswordReminder user >> return Nothing
                         | otherwise -> return $ Just user
               Nothing ->  do
                 let ug0 = set ugName (T.pack companyName) def
                 ug <- dbUpdate $ UserGroupCreate ug0
                 createUser (Email email) (firstname,lastname) (get ugID ug,True) lang AccountRequest
  case muser' of
    -- return ambiguous response in both cases to prevent a security issue
    Nothing -> runJSONGenT $ value "sent" True
    Just user -> do
          _ <- dbUpdate $ SetUserInfo (userid user) $ (userinfo user)
            { userphone = phone, usercompanyposition = companyPosition }
          sendNewUserMail user
          l <- newUserAccountRequestLink lang (userid user) AccountRequest
          asyncLogEvent "Send account confirmation email"
                        [ UserIDProp $ userid user
                        , IPProp     $ get ctxipnumber ctx
                        , TimeProp   $ get ctxtime ctx
                        , someProp "Context" ("Acount request" :: String) ]
                        EventMixpanel
          asyncLogEvent SetUserProps
                        [ UserIDProp $ userid user
                        , someProp "Account confirmation email" $ get ctxtime ctx
                        , NameProp (firstname ++ " " ++ lastname)
                        , FirstNameProp firstname
                        , LastNameProp lastname
                        , someProp "Confirmation link" $ show l ]
                        EventMixpanel
          runJSONGenT $ value "sent" True

apiCallSendPasswordReminder :: Kontrakcja m => m Response
apiCallSendPasswordReminder = api $ do
  memail <- getOptionalField asValidEmail "email"
  case memail of
    Nothing -> runJSONGenT $ value "send" False >> value "badformat" True
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
  ctx <- getContext
  minv <- dbQuery $ GetPasswordReminder $ userid user
  case minv of
    Just pr@PasswordReminder{..} -> case prRemainedEmails of
      0 -> return ()
      n -> do
        _ <- dbUpdate $ UpdatePasswordReminder $ pr { prRemainedEmails = n - 1 }
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
  (user, _ , _) <- getAPIUser APIPersonal
  scheme <- dbQuery $ GetUserCallbackSchemeByUserID $ userid user
  fmap Ok $ case scheme of
      Just (ConstantUrlScheme url) -> runJSONGenT $ do
                    value "scheme" ("constant"::String)
                    value "api_version" (1::Int)
                    value "url" url
      Just (ConstantUrlSchemeV2 url) -> runJSONGenT $ do
                    value "scheme" ("constant"::String)
                    value "api_version" (2::Int)
                    value "url" url
      Just (SalesforceScheme _key)  -> runJSONGenT $ do
                    value "scheme" ("salesforce"::String)
      Just (BasicAuthScheme _lg _pwd)  -> runJSONGenT $ do
                    value "scheme" ("basic_auth"::String)
      Just (OAuth2Scheme _name _password _url _scope)  -> runJSONGenT $ do
                    value "scheme" ("oauth2"::String)
      Nothing -> runJSONGenT $ do
                    value "scheme" ("none"::String)


apiCallTestSalesforceIntegration :: Kontrakcja m => m Response
apiCallTestSalesforceIntegration = do
  salesforceFullDebugLog
  api $ do
    (user, _ , _) <- getAPIUser APIDocCheck
    scheme <- dbQuery $ GetUserCallbackSchemeByUserID $ userid user
    murl <- getField "url"
    when (isNothing murl) $ do
      throwM . SomeDBExtraException $ badInput $ "No 'url' parameter provided"
    let url = fromJust murl
    fmap Ok $ case scheme of
        Just (SalesforceScheme token)  -> do
          ctx <- getContext
          case get ctxsalesforceconf ctx of
            Nothing -> noConfigurationError "Salesforce"
            Just sc -> do
              res <- flip runReaderT sc $ testSalesforce token url
              case res of
                Right (http_code, resp)-> runJSONGenT $ do
                  value "status" ("ok"::String)
                  value "http_code" http_code
                  value "response" resp
                Left (msg, curl_err, stdout, stderr, http_code) -> runJSONGenT $ do
                  value "status" ("error"::String)
                  value "error_message" msg
                  value "http_code" http_code
                  value "curl_exit_code" curl_err
                  value "curl_stdout" stdout
                  value "curl_stderr" stderr
        _ -> throwM . SomeDBExtraException $ conflictError "Salesforce callback scheme is not set for this user"

apiCallSetSalesforceCallbacks :: Kontrakcja m => m Response
apiCallSetSalesforceCallbacks = do
  salesforceFullDebugLog
  V2.api $ do
    -- We allow all permission although workflow with Partners API should use APIPersonal.
    (user, _ ) <- V2.getAPIUserWithAnyPrivileges
    ctx <- getContext
    case (get ctxsalesforceconf ctx) of
      Nothing -> V2.apiError $ V2.serverError $ "No configuration for Salesforce integration"
      Just sc -> do
        code <- V2.apiV2ParameterObligatory (V2.ApiV2ParameterText "code")
        mtoken <- flip runReaderT sc (getRefreshTokenFromCode $ T.unpack code)
        case mtoken of
          Left emsg -> V2.apiError $ V2.requestFailed $ T.pack emsg
          Right token -> do
            dbUpdate $ UpdateUserCallbackScheme (userid user) (SalesforceScheme token)
            return $ V2.Ok $ runJSONGen $ value "status" ("ok"::String)

apiCallLoginUserAndGetSession :: Kontrakcja m => m Response
apiCallLoginUserAndGetSession = V2.api $ do
  -- parse oauth from json
  oauth <- V2.apiV2ParameterObligatory $ V2.ApiV2ParameterJSON "personal_token" unjsonOAuthAuthorization
  euser <- getUserFromOAuthWithAnyPrivileges oauth
  case euser of
    Left err -> V2.apiError $ V2.invalidAuthorizationWithMsg err
    Right (User{userid}, _actor) -> do
      ctx <- getContext
      asyncLogEvent "Login"
                    [ UserIDProp userid
                    , IPProp   $ get ctxipnumber ctx
                    , TimeProp $ get ctxtime ctx ]
                    EventMixpanel
      asyncLogEvent SetUserProps
                    [ UserIDProp userid
                    , someProp "Last login" $ get ctxtime ctx ]
                    EventMixpanel
      emptysession <- emptySession
      ses <- startNewSession emptysession (Just userid) Nothing
      return $ V2.Ok $ runJSONGen $ do
        value "session_id" (show $ sessionCookieInfoFromSession ses)

apiCallIsUserDeletable :: Kontrakcja m => m Response
apiCallIsUserDeletable = V2.api $ do
  (user, _ , _) <- getAPIUser APIPersonal

  mReason <- dbQuery $ IsUserDeletable user

  return $ V2.Ok $ runJSONGen $ case mReason of
    Just reason -> do
      value "deletable" False
      value "reason" reason
    Nothing -> do
      value "deletable" True

apiCallDeleteUser :: Kontrakcja m => m Response
apiCallDeleteUser = V2.api $ do
  (user, _ , _) <- getAPIUser APIPersonal
  ctx <- getContext

  email <- V2.apiV2ParameterObligatory $ V2.ApiV2ParameterText "email"
  unless (unEmail (useremail (userinfo user)) == T.unpack email) $
    V2.apiError $ V2.requestParameterParseError "email"
      "the email provided does not match that of the user account"

  mReason <- dbQuery $ IsUserDeletable user
  case mReason of
    Just reason -> V2.apiError $
      V2.conflictError $ userNotDeletableReasonToString reason
    Nothing -> return ()

  _ <- dbUpdate $ DeleteUser (userid user)
  _ <- dbUpdate $ LogHistoryAccountDeleted (userid user) noIP (get ctxtime ctx)

  return $ V2.Ok ()

{-
 - User's data retention policy
 -}

apiCallGetDataRetentionPolicy :: Kontrakcja m => m Response
apiCallGetDataRetentionPolicy = V2.api $ do
  ctx  <- getContext
  user <- guardJust $ getContextUser ctx

  let drp = dataretentionpolicy $ usersettings user
  ug <- dbQuery $ UserGroupGetByUserID $ userid user
  let ugDRP = get (ugsDataRetentionPolicy . ugSettings) ug

  return $ V2.Ok $ object
    [ "data_retention_policy" .= unjsonToJSON unjsonDataRetentionPolicy drp
    , "company_data_retention_policy" .=
        unjsonToJSON unjsonDataRetentionPolicy ugDRP
    ]

apiCallSetDataRetentionPolicy :: Kontrakcja m => m Response
apiCallSetDataRetentionPolicy = V2.api $ do
  ctx  <- getContext
  user <- guardJust $ getContextUser ctx

  ug <- dbQuery $ UserGroupGetByUserID $ userid user
  let ugDRP = get (ugsDataRetentionPolicy . ugSettings) ug
  drp <- V2.apiV2ParameterObligatory $
    V2.ApiV2ParameterJSON "data_retention_policy"
    unjsonDataRetentionPolicy
  guardThatDataRetentionPolicyIsValid drp $ Just ugDRP

  let settings' = (usersettings user) { dataretentionpolicy = drp }
  _ <- dbUpdate $ SetUserSettings (userid user) settings'

  return $ V2.Ok ()

-- salesforceFullDebugLog is for debugging salesforce issues - should be removed before 1.10.2018
salesforceFullDebugLog :: Kontrakcja m => m ()
salesforceFullDebugLog = do
  rq <- askRq
  mbody <- liftIO (tryReadMVar $ rqInputsBody rq)
  logInfo "Full info for salesforce related requests - should be removed before 1.10.2018" $ object [
      "uri" .= rqUri rq
    , "request" .= (show (rqMethod rq) ++ " " ++ rqUri rq ++ rqQuery rq)
    , "post variables" .= (map showNamedInput $ fromMaybe [] mbody)
    , "http headers" .= (concatMap showNamedHeader . Map.toList $ rqHeaders rq)
    , "http cookies" .= (map showNamedCookie $ rqCookies rq)
    ]

showNamedCookie :: (String, Cookie) -> String
showNamedCookie (_name,cookie) = mkCookieHeader Nothing cookie

showNamedHeader :: (a, HeaderPair) -> [String]
showNamedHeader (_nm,hd) | hName hd == UTF8.fromString "cookie" = []
showNamedHeader (_nm,hd) = map showHeaderLine (hValue hd)
  where
    showHeaderLine value' = UTF8.toString (hName hd) ++ ": " ++ UTF8.toString value'

showNamedInput :: (String, Input) -> String
showNamedInput (name,input) = name ++ ": " ++
    case inputFilename input of
      Just filename -> filename
      _ -> case inputValue input of
             Left _tmpfilename -> "<<content in /tmp>>"
             Right value' -> show (BSL.toString value')
