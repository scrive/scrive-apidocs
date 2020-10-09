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
    apiCallGetTokenForPersonalCredentials,
    apiCallGetTags,
    apiCallUpdateTags
  ) where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Aeson
import Data.Unjson
import Happstack.Server hiding
  ( dir, forbidden, host, lookCookieValue, ok, path, resp, simpleHTTP
  )
import Happstack.StaticRouting
import Log
import Text.JSON.Gen hiding (object)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Text.JSON as J

import AccessControl.Check
import AccessControl.Types
import API.Monad.V1
import API.V2.Parameters
import API.V2.Utils
import DataRetentionPolicy
import DataRetentionPolicy.Guards
import DB
import Folder.Model
import Happstack.Fields
import InputValidation
import Kontra
import KontraLink
import Log.Identifier
import LoginAuth.LoginAuthMethod
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
import Tag
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
import User.UserFeatures
import User.UserView
import UserGroup.FreeDocumentTokens.Model
import UserGroup.Model
import UserGroup.Types
import UserGroup.Types.Subscription
import Util.HasSomeUserInfo
import Util.QRCode (encodeQR, unQRCode)
import Utils.Monad
import qualified API.V2 as V2
import qualified API.V2.Errors as V2
import qualified API.V2.Parameters as V2
import qualified UserGroup.Internal

userAPI :: Route (Kontra Response)
userAPI = dir "api" $ choice
  [ dir "frontend" userAPIV2
  , userAPIV1 -- Temporary backwards compatibility for clients accessing version-less API
  , dir "v1" userAPIV1
  , dir "v2" userAPIV2
  ]

userAPIV1 :: Route (Kontra Response)
userAPIV1 = choice
  [ (dir "getpersonaltoken" . hPost . toK0) apiCallGetUserPersonalToken
  , (dir "signup" . hPost . toK0) apiCallSignup
  , (dir "login" . hPostNoXToken . toK0) apiCallLoginUser
  , (dir "sendpasswordresetmail" . hPost . toK0) apiCallSendPasswordReminder
  , (dir "getprofile" . hGet . toK0) apiCallGetUserProfile
  , (dir "getsubscription" . hGet . toK0) apiCallGetSubscription
  , (dir "changepassword" . hPost . toK0) apiCallChangeUserPassword
  , (dir "updateprofile" . hPost . toK0) apiCallUpdateUserProfile
  , (dir "changeemail" . hPost . toK0) apiCallChangeEmail
  , (dir "getcallbackscheme" . hGet . toK0) apiCallUserGetCallbackScheme
  , (dir "testsalesforceintegration" . hGet . toK0) apiCallTestSalesforceIntegration
  , (dir "setsalesforcecallbacks" . hPost . toK0) apiCallSetSalesforceCallbacks
  ]

userAPIV2 :: Route (Kontra Response)
userAPIV2 = choice
  [ (dir "loginandgetsession" . hPost . toK0) apiCallLoginUserAndGetSession
  , (dir "2fa" . dir "setup" . hPost . toK0) setup2FA
  , (dir "2fa" . dir "confirm" . hPost . toK0) confirm2FA
  , (dir "2fa" . dir "disable" . hPost . toK0) disable2FA
  , (dir "isuserdeletable" . hPost . toK0) apiCallIsUserDeletable
  , (dir "deleteuser" . hPost . toK0) apiCallDeleteUser
  , (dir "dataretentionpolicy" . hGet . toK0) apiCallGetDataRetentionPolicy
  , (dir "dataretentionpolicy" . dir "set" . hPost . toK0) apiCallSetDataRetentionPolicy
  , (dir "usagestats" . dir "days" . hGet . toK0)
    User.UserControl.handleUsageStatsJSONForUserDays
  , (dir "usagestats" . dir "months" . hGet . toK0)
    User.UserControl.handleUsageStatsJSONForUserMonths
  , (dir "gettokenforpersonalcredentials" . hPost . toK1)
    apiCallGetTokenForPersonalCredentials
  , (dir "updateusersprofile" . hPost . toK1) apiCallUpdateOtherUserProfile
  , (dir "changeusersemail" . hPost . toK1) apiCallChangeOtherUserEmail
  , (dir "usagestats" . dir "shareablelink" . dir "days" . hGet . toK0)
    $ User.UserControl.handleUsageStatsJSONForShareableLinks PartitionByDay
  , (dir "usagestats" . dir "shareablelink" . dir "months" . hGet . toK0)
    $ User.UserControl.handleUsageStatsJSONForShareableLinks PartitionByMonth
  , (dir "checkpassword" . hPost . toK0) apiCallCheckPassword
  , (dir "activateuser" . hPost . toK0) apiCallActivateAccount
  , (dir "getusersfeatures" . hGet . toK0) apiCallGetUsersFeatures
  , (dir "usertags" . hGet . toK0) apiCallGetTags
  , (dir "usertags" . dir "update" . hPost . toK0) apiCallUpdateTags
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
          if maybeVerifyPassword (user ^. #password) passwd
            then case (user ^. #totpKey, user ^. #totpActive, mtotpcode) of
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
              void . dbUpdate $ LogHistoryAPIGetPersonalTokenFailure (user ^. #id)
                                                                     (ctx ^. #ipAddr)
                                                                     (ctx ^. #time)
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
      let uid = user ^. #id
      _success <- dbUpdate $ CreatePersonalToken uid
      token    <- dbQuery $ GetPersonalToken uid
      case token of
        Nothing -> throwM . SomeDBExtraException $ serverError
          "No token found, this should not happend"
        Just t -> do
          attemptCount <- dbQuery $ GetUserRecentAuthFailureCount (user ^. #id)
          if attemptCount <= 5
            then do
              void . dbUpdate $ LogHistoryAPIGetPersonalTokenSuccess uid
                                                                     (ctx ^. #ipAddr)
                                                                     (ctx ^. #time)
              return . Right $ Ok (unjsonOAuthAuthorization, t)
            else
              -- use an ambiguous message, so that this cannot be used to determine
              -- whether a user has an account with Scrive
                 throwM . SomeDBExtraException $ forbidden wrongPassMsg

setup2FA :: Kontrakcja m => m Response
setup2FA = V2.api $ do
  (user, _)    <- V2.getAPIUserWithAnyPrivileges
  requiredPerm <- apiRequirePermission . canDo UpdateA . UserR $ (user ^. #id)
  apiAccessControl user requiredPerm $ do
    if user ^. #totpActive
      then V2.Ok <$> runJSONGenT (value "twofactor_active" True)
      else do
        key <- createTOTPKey
        ok  <- dbUpdate $ SetUserTOTPKey (user ^. #id) key
        if ok
          then do
            let email = user ^. #info % #email
            url    <- view (#brandedDomain % #url) <$> getContext
            qrCode <- liftIO $ makeQRFromURLEmailAndKey (T.unpack url) email key
            return . V2.Ok <$> runJSONGen $ do
              value "twofactor_active" False
              value "qr_code"          (BS.unpack . Base64.encode . unQRCode $ qrCode)
          else V2.apiError $ V2.serverError "Could not set TOTP key"

confirm2FA :: Kontrakcja m => m Response
confirm2FA = V2.api $ do
  ctx          <- getContext
  (user, _)    <- V2.getAPIUserWithAnyPrivileges
  totpcode     <- V2.apiV2ParameterObligatory $ V2.ApiV2ParameterInt "totp"
  now          <- currentTime
  requiredPerm <- apiRequirePermission . canDo UpdateA . UserR $ (user ^. #id)
  apiAccessControl user requiredPerm $ do
    case (user ^. #totpKey, user ^. #totpActive) of
      (Just totpkey, False) -> if verifyTOTPCode totpkey now (fromIntegral totpcode)
        then do
          r <- dbUpdate $ ConfirmUserTOTPSetup (user ^. #id)
          if r
            then do
              void . dbUpdate $ LogHistoryTOTPEnable (user ^. #id)
                                                     (ctx ^. #ipAddr)
                                                     (ctx ^. #time)
              return . V2.Ok <$> runJSONGen $ do
                value "twofactor_active" r
                value "totp_valid"       True
            else V2.apiError $ V2.serverError "Could not confirm user TOTP setup"
        else V2.apiError $ V2.requestParameterInvalid "totp" "The code is not valid"
      (_, tfa) -> V2.Ok <$> runJSONGenT (value "twofactor_active" tfa)

disable2FA :: Kontrakcja m => m Response
disable2FA = V2.api $ do
  ctx          <- getContext
  (user, _)    <- V2.getAPIUserWithAnyPrivileges
  requiredPerm <- apiRequirePermission . canDo UpdateA . UserR $ (user ^. #id)
  apiAccessControl user requiredPerm $ do
    if user ^. #totpActive
      then do
        r <- dbUpdate $ DisableUserTOTP (user ^. #id)
        if r
          then do
            void . dbUpdate $ LogHistoryTOTPDisable (user ^. #id)
                                                    (ctx ^. #ipAddr)
                                                    (ctx ^. #time)
            V2.Ok <$> runJSONGenT (value "twofactor_active" False)
          else V2.apiError $ V2.serverError "Could not disable TOTP"
      else V2.Ok <$> runJSONGenT (value "twofactor_active" False)

apiCallGetUserProfile :: Kontrakcja m => m Response
apiCallGetUserProfile = api $ do
  (user, _, _) <- getAPIUserWithAnyPrivileges
  requiredPerm <- apiRequirePermission . canDo ReadA . UserR $ (user ^. #id)
  apiAccessControl user requiredPerm $ do
    ugwp <- dbQuery . UserGroupGetWithParentsByUserID $ user ^. #id
    return . Ok $ userJSONWithCompany user ugwp

apiCallGetSubscription :: Kontrakcja m => m Response
apiCallGetSubscription = api $ do
  (user, _, _) <- getAPIUserWithAnyPrivileges
  requiredPerm <- apiRequirePermission . canDo ReadA . UserR $ (user ^. #id)
  apiAccessControl user requiredPerm $ do
    ugwp <- dbQuery . UserGroupGetWithParentsByUserID $ user ^. #id
    sub  <- getSubscription ugwp
    return . Ok $ unjsonToJSON unjsonDef sub

apiCallGetUsersFeatures :: Kontrakcja m => m Response
apiCallGetUsersFeatures = V2.api $ do
  user         <- V2.getAPIUserWithFullAccess
  requiredPerm <- apiRequirePermission . canDo ReadA . UserR $ (user ^. #id)
  apiAccessControl user requiredPerm $ do
    jsonWithData <- getUserFeaturesJSON user
    return . V2.Ok $ jsonWithData

apiCallChangeUserPassword :: Kontrakcja m => m Response
apiCallChangeUserPassword = api $ do
  ctx          <- getContext
  (user, _, _) <- getAPIUserWithAnyPrivileges
  oldpassword  <- getField' "oldpassword"
  password     <- getField' "password"
  goodPassword <- checkPassword (ctx ^. #passwordServiceConf) password
  if goodPassword
    then if maybeVerifyPassword (user ^. #password) oldpassword
      then do
        passwordhash <- createPassword password
        void . dbUpdate $ SetUserPassword (user ^. #id) passwordhash
        void . dbUpdate $ LogHistoryPasswordSetup (user ^. #id)
                                                  (ctx ^. #ipAddr)
                                                  (ctx ^. #time)
                                                  (Just $ user ^. #id)
        terminateAllUserSessionsExceptCurrent (user ^. #id)
        Ok <$> runJSONGenT (value "changed" True)
      else do
        void . dbUpdate $ LogHistoryPasswordSetupReq (user ^. #id)
                                                     (ctx ^. #ipAddr)
                                                     (ctx ^. #time)
                                                     (Just $ user ^. #id)
        Ok <$> runJSONGenT (value "changed" False)
    else throwM . SomeDBExtraException $ serverError
      "New password fields do not match Scrive standard"

apiCallLoginUser :: Kontrakcja m => m Response
apiCallLoginUser = api $ do
  ctx         <- getContext
  user        <- V2.getAPIUserWithFullAccess

  redirectUrl <- apiGuardJustM (badInput "Redirect URL not provided or invalid.")
    $ getField "redirect"

  void . dbUpdate $ LogHistoryLoginSuccess (user ^. #id) (ctx ^. #ipAddr) (ctx ^. #time)
  logUserToContext $ Just user
  sendRedirect $ LinkExternal redirectUrl

apiCallUpdateUserProfile :: forall  m . Kontrakcja m => m Response
apiCallUpdateUserProfile = api $ do
  user         <- V2.getAPIUserWithFullAccess
  ctx          <- getContext
  requiredPerm <- apiRequirePermission . canDo UpdateA . UserR $ (user ^. #id)
  apiAccessControl user requiredPerm $ do
    -- allow empty strings through validation
    let
      getParameter :: Text -> (Text -> InputValidation.Result Text) -> Text -> m Text
      getParameter name validation prevValue = do
        mx <- apiV2ParameterOptional . ApiV2ParameterTextWithValidation name $ emptyOK
          validation
        return $ fromMaybe prevValue mx

      getUserParameter
        :: Text -> (Text -> InputValidation.Result Text) -> (UserInfo -> Text) -> m Text
      getUserParameter n v getPrevValue = getParameter n v . getPrevValue $ user ^. #info

    companyposition <- getUserParameter "companyposition"
                                        asValidPosition
                                        (^. #companyPosition)
    phone          <- getUserParameter "phone" asValidPhone (^. #phone)
    personalnumber <- getUserParameter "personalnumber"
                                       asValidPersonalNumber
                                       (^. #personalNumber)
    fstname         <- getUserParameter "fstname" asValidName (^. #firstName)
    sndname         <- getUserParameter "sndname" asValidName (^. #lastName)
    mHasAcceptedTOS <- apiV2ParameterOptional $ ApiV2ParameterBool "has_accepted_tos"

    let ui =
          (user ^. #info)
            & (#firstName .~ fstname)
            & (#lastName .~ sndname)
            & (#personalNumber .~ personalnumber)
            & (#companyPosition .~ companyposition)
            & (#phone .~ phone)

    mlang <- (langFromCode =<<) <$> getField "lang"
    when_ (isJust mlang) . dbUpdate $ SetUserSettings
      (user ^. #id)
      (user ^. #settings & #lang .~ fromJust mlang)

    void . dbUpdate $ SetUserInfo (user ^. #id) ui
    void . dbUpdate $ LogHistoryUserInfoChanged (user ^. #id)
                                                (ctx ^. #ipAddr)
                                                (ctx ^. #time)
                                                (user ^. #info)
                                                ui
                                                (ctx ^? #maybeUser % _Just % #id)

    whenJust mHasAcceptedTOS $ \case
      True  -> void . dbUpdate $ AcceptTermsOfService (user ^. #id) (ctx ^. #time)
      False -> V2.apiError $ V2.requestParameterParseError
        "has_accepted_tos"
        "TOS acceptance cannot be revoked"

    if user ^. #isCompanyAdmin
      then do
        ugwp <- dbQuery . UserGroupGetWithParentsByUserID $ user ^. #id
        let ug = ugwpUG ugwp
        companyname <- getParameter "companyname" asValidCompanyName $ ug ^. #name
        let getAddrParameter n v prevValue =
              getParameter n v $ ugwpAddress ugwp ^. prevValue
        number     <- getAddrParameter "companynumber" asValidCompanyNumber #companyNumber
        entityname <- getAddrParameter "companyentityname" asValidCompanyName #entityName
        address    <- getAddrParameter "companyaddress" asValidAddress #address
        zip'       <- getAddrParameter "companyzip" asValidZip #zipCode
        city       <- getAddrParameter "companycity" asValidCity #city
        country    <- getAddrParameter "companycountry" asValidCountry #country
        let ug'         = set #name companyname ug
            new_address = UserGroupAddress { companyNumber = number
                                           , entityName    = entityname
                                           , address       = address
                                           , zipCode       = zip'
                                           , city          = city
                                           , country       = country
                                           }
            ug'' = case ug' ^. #address of
              Just _ ->
                -- change address directly if it wasn't inherited
                set #address (Just new_address) ug'
              Nothing -> if new_address == ugwpAddress ugwp
                then ug'
                else set #address (Just new_address) ug'  -- stop inheriting
        void . dbUpdate $ UserGroupUpdate ug''
        Ok <$> runJSONGenT (value "changed" True)
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
        when (any isJust fs) . V2.apiError $ V2.APIError
          { errorType     = V2.InsufficientPrivileges
          , errorHttpCode = 403
          , errorMessage  =
            "You do not have permission to perform this action on company settings"
          }
        Ok <$> runJSONGenT (value "changed" True)

apiCallChangeEmail :: Kontrakcja m => m Response
apiCallChangeEmail = api $ do
  ctx          <- getContext
  user         <- V2.getAPIUserWithFullAccess
  mnewemail    <- getOptionalField asValidEmail "newemail"
  requiredPerm <- apiRequirePermission . canDo UpdateA . UserR $ (user ^. #id)
  apiAccessControl user requiredPerm $ do
    case Email <$> mnewemail of
      (Just newemail) -> do
        mexistinguser <- dbQuery $ GetUserByEmail newemail
        case mexistinguser of
          Just _existinguser -> do
            sendChangeToExistingEmailInternalWarningMail user newemail
            Ok <$> runJSONGenT (value "send" False)
          Nothing -> do
            changeemaillink <- newEmailChangeRequestLink (user ^. #id) newemail
            mailRequest     <- mailEmailChangeRequest ctx
                                                      Nothing
                                                      user
                                                      newemail
                                                      changeemaillink
            scheduleEmailSendout
              (mailRequest
                { to = [ MailAddress { fullname = getFullName user
                                     , email    = unEmail newemail
                                     }
                       ]
                }
              )
            let oldemail = user ^. #info % #email
            mailNotification <- mailEmailChangeRequestedNotification ctx
                                                                     False
                                                                     user
                                                                     newemail
            scheduleEmailSendout
              (mailNotification
                { to = [ MailAddress { fullname = getFullName user
                                     , email    = unEmail oldemail
                                     }
                       ]
                }
              )
            Ok <$> runJSONGenT (value "send" True)
      Nothing -> Ok <$> runJSONGenT (value "send" False)

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
  lang            <- fromMaybe (ctx ^. #lang) . langFromCode <$> getField' "lang"
  switchLang lang
  muser  <- dbQuery . GetUserByEmail $ Email email
  muser' <- case muser of
               -- creating account that already exists should send password reminder so customers stopp calling us
               -- CORE-631
    Just user
      | isJust $ user ^. #hasAcceptedTOS -> sendPasswordReminder user >> return Nothing
      | otherwise -> return $ Just user
    Nothing -> do
      ugFolder <- dbUpdate . FolderCreate $ defaultFolder
      let address = defaultUserGroupAddress & (#entityName .~ companyName)
          ug0 =
            defaultUserGroup
              & (#homeFolderID ?~ ugFolder ^. #id)
              & (#address ?~ address)
              & (#settings % _Just % #appFrontend .~ True)
      ug                    <- dbUpdate $ UserGroupCreate ug0
      freeDocumentsValidity <- (31 `daysAfter`) <$> currentTime
      let freeDocumentsCount = 3
          freeDocuments =
            freeDocumentTokensFromValues freeDocumentsCount freeDocumentsValidity
      dbUpdate $ UserGroupFreeDocumentTokensUpdate (ug ^. #id) freeDocuments
      createUser (Email email) (firstname, lastname) (ug ^. #id, True) lang AccountRequest
        =<< getCreateUserContextFromContext
  case muser' of
    -- return ambiguous response in both cases to prevent a security issue
    Nothing   -> runJSONGenT $ value "sent" True
    Just user -> do
      void . dbUpdate $ SetUserInfo
        (user ^. #id)
        ((user ^. #info) & (#phone .~ phone) & (#companyPosition .~ companyPosition))
      sendNewUserMail user
      runJSONGenT $ value "sent" True

apiCallSendPasswordReminder :: Kontrakcja m => m Response
apiCallSendPasswordReminder = api $ do
  memail <- getOptionalField asValidEmail "email"
  case memail of
    Nothing    -> runJSONGenT $ value "send" False >> value "badformat" True
    Just email -> do
      -- Password reset could be abused to find out, whether an email is registered
      -- with Scrive. Se we return ambiguous response in all cases to prevent a security issue.
      muser <- dbQuery . GetUserByEmail $ Email email
      case muser of
        Nothing -> do
          runJSONGenT $ value "send" True
        Just user -> do
          case user ^. #sysAuth of
            LoginAuthSSO -> do
              -- we need to pretend that the SSO users do not exist
              -- for password reminder feature (so no actual reminder email)
              -- but still not leak any information about them
              runJSONGenT $ value "send" True
            LoginAuthNative -> do
              sendPasswordReminder user
              runJSONGenT $ value "send" True

sendPasswordReminder :: Kontrakcja m => User -> m ()
sendPasswordReminder user = do
  ctx  <- getContext
  minv <- dbQuery . GetPasswordReminder $ user ^. #id
  case minv of
    Just pr@PasswordReminder {..} -> case prRemainedEmails of
      0 -> return ()
      n -> do
        void . dbUpdate . UpdatePasswordReminder $ pr { prRemainedEmails = n - 1 }
        sendResetPasswordMail ctx (LinkPasswordReminder prUserID prToken)
    _ -> do
      link <- newPasswordReminderLink $ user ^. #id
      sendResetPasswordMail ctx link
  where
    sendResetPasswordMail ctx link = do
      mail <- resetPasswordMail ctx user link
      scheduleEmailSendout $ mail { to = [getMailAddress user] }

apiCallUserGetCallbackScheme :: Kontrakcja m => m Response
apiCallUserGetCallbackScheme = api $ do
  user         <- V2.getAPIUserWithFullAccess
  requiredPerm <- apiRequirePermission . canDo ReadA . UserR $ (user ^. #id)
  apiAccessControl user requiredPerm $ do
    scheme <- dbQuery . GetUserCallbackSchemeByUserID $ user ^. #id
    Ok <$> case scheme of
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
apiCallTestSalesforceIntegration = api $ do
  (user, _, _) <- getAPIUser APIDocCheck
  scheme       <- dbQuery . GetUserCallbackSchemeByUserID $ user ^. #id
  murl         <- getField "url"
  url          <- case murl of
    Nothing  -> throwM . SomeDBExtraException . badInput $ "No 'url' parameter provided"
    Just url -> return url
  requiredPerm <- apiRequirePermission . canDo UpdateA . UserR $ (user ^. #id)
  apiAccessControl user requiredPerm $ do
    Ok <$> case scheme of
      Just (SalesforceScheme token) -> do
        logInfo "Testing salesforce integration with user that has sf callback scheme"
          $ logObject_ user
        ctx <- getContext
        case ctx ^. #salesforceConf of
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
      _ -> do
        logAttention
            "Testing salesforce integration with user that doesn't have sf callback scheme"
          $ logObject_ user
        throwM . SomeDBExtraException $ conflictError
          "Salesforce callback scheme is not set for this user"

apiCallSetSalesforceCallbacks :: Kontrakcja m => m Response
apiCallSetSalesforceCallbacks = do
  V2.api $ do
    -- We allow all permission although workflow with Partners API
    -- should use APIPersonal.
    user         <- fst <$> V2.getAPIUserWithAnyPrivileges
    requiredPerm <- apiRequirePermission . canDo UpdateA . UserR $ (user ^. #id)
    apiAccessControl user requiredPerm $ do
      ctx <- getContext
      case ctx ^. #salesforceConf of
        Nothing ->
          V2.apiError $ V2.serverError "No configuration for Salesforce integration"
        Just sc -> do
          code   <- V2.apiV2ParameterObligatory (V2.ApiV2ParameterText "code")
          mtoken <- runReaderT (getRefreshTokenFromCode code) sc
          case mtoken of
            Left emsg -> do
              logAttention "Setting sf callback scheme failed"
                $ object ["user_id" .= (user ^. #id), "msg" .= emsg]
              V2.apiError $ V2.requestFailed emsg
            Right token -> do
              logInfo "Setting sf callback scheme worked" $ logObject_ user
              dbUpdate $ UpdateUserCallbackScheme (user ^. #id) (SalesforceScheme token)
              return . V2.Ok $ runJSONGen (value "status" ("ok" :: String))

apiCallLoginUserAndGetSession :: Kontrakcja m => m Response
apiCallLoginUserAndGetSession = V2.api $ do
  -- parse oauth from json
  oauth <- V2.apiV2ParameterObligatory
    $ V2.ApiV2ParameterJSON "personal_token" unjsonOAuthAuthorization
  euser <- getUserFromOAuthWithAnyPrivileges oauth
  case euser of
    Left  err            -> V2.apiError $ V2.invalidAuthorizationWithMsg err
    Right (user, _actor) -> do
      session <- startNewSessionWithUser $ user ^. #id
      return . V2.Ok . runJSONGen $ do
        value "session_id" (showt $ sessionCookieInfoFromSession session)

apiCallIsUserDeletable :: Kontrakcja m => m Response
apiCallIsUserDeletable = V2.api $ do
  user         <- V2.getAPIUserWithFullAccess
  requiredPerm <- apiRequirePermission . canDo ReadA . UserR $ (user ^. #id)
  apiAccessControl user requiredPerm $ do
    mReason <- dbQuery $ IsUserDeletable user
    return . V2.Ok . runJSONGen $ case mReason of
      Just reason -> do
        value "deletable" False
        value "reason"    reason
      Nothing -> do
        value "deletable" True

apiCallDeleteUser :: Kontrakcja m => m Response
apiCallDeleteUser = V2.api $ do
  user         <- V2.getAPIUserWithFullAccess
  ctx          <- getContext
  email        <- V2.apiV2ParameterObligatory $ V2.ApiV2ParameterText "email"
  requiredPerm <- apiRequirePermission . canDo DeleteA . UserR $ (user ^. #id)
  apiAccessControl user requiredPerm $ do
    unless (unEmail (user ^. #info % #email) == email)
      . V2.apiError
      $ V2.requestParameterParseError
          "email"
          "the email provided does not match that of the user account"
    mReason <- dbQuery $ IsUserDeletable user
    case mReason of
      Just reason ->
        V2.apiError . V2.conflictError $ userNotDeletableReasonToString reason
      Nothing -> return ()
    void . dbUpdate $ DeleteUser (user ^. #id)
    void . dbUpdate $ LogHistoryAccountDeleted (user ^. #id)
                                               (user ^. #id)
                                               (ctx ^. #ipAddr)
                                               (ctx ^. #time)
    return $ V2.Ok ()

{-
 - User's data retention policy
 -}
apiCallGetDataRetentionPolicy :: Kontrakcja m => m Response
apiCallGetDataRetentionPolicy = V2.api $ do
  user         <- V2.getAPIUserWithFullAccess
  requiredPerm <- apiRequirePermission . canDo ReadA . UserR $ (user ^. #id)
  apiAccessControl user requiredPerm $ do
    let drp = user ^. #settings % #dataRetentionPolicy
    ugwp <- dbQuery . UserGroupGetWithParentsByUserID $ user ^. #id
    let ugDRP = ugwpSettings ugwp ^. #dataRetentionPolicy
    return . V2.Ok $ object
      [ "data_retention_policy" .= unjsonToJSON unjsonDataRetentionPolicy drp
      , "company_data_retention_policy" .= unjsonToJSON unjsonDataRetentionPolicy ugDRP
      ]

apiCallSetDataRetentionPolicy :: Kontrakcja m => m Response
apiCallSetDataRetentionPolicy = V2.api $ do
  user         <- V2.getAPIUserWithFullAccess
  ugwp         <- dbQuery . UserGroupGetWithParentsByUserID $ user ^. #id
  requiredPerm <- apiRequirePermission . canDo UpdateA . UserR $ (user ^. #id)
  apiAccessControl user requiredPerm $ do
    let ugDRP = ugwpSettings ugwp ^. #dataRetentionPolicy
    drp <- V2.apiV2ParameterObligatory
      $ V2.ApiV2ParameterJSON "data_retention_policy" unjsonDataRetentionPolicy
    guardThatDataRetentionPolicyIsValid drp $ Just ugDRP
    let settings' = user ^. #settings & #dataRetentionPolicy .~ drp
    void . dbUpdate $ SetUserSettings (user ^. #id) settings'
    return $ V2.Ok ()

apiCallGetTokenForPersonalCredentials :: Kontrakcja m => UserID -> m Response
apiCallGetTokenForPersonalCredentials uid = V2.api $ do
  -- Guards
  user         <- V2.getAPIUserWithFullAccess
  requiredPerm <- apiRequirePermission . canDo UpdateA $ UserR uid
  apiAccessControl user requiredPerm $ do
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
      value "qr_code" . BS.unpack . Base64.encode . unQRCode $ qrCode
      value "expiration_time" $ show expirationTime
  where
    defaultMinutes = 5
    maxMinutes     = 30
    invalidMinsParamError =
      V2.apiError
        .  V2.requestParameterInvalid "minutes"
        $  "The value given is larger than the allowed maximum of "
        <> showt maxMinutes
        <> " or below 1."

apiCallCheckPassword :: Kontrakcja m => m Response
apiCallCheckPassword = api $ do
  ctx          <- getContext
  password     <- getField' "password"
  goodPassword <- checkPassword (ctx ^. #passwordServiceConf) password
  Ok <$> runJSONGenT (value "valid" goodPassword)

guardCanChangeUser :: Kontrakcja m => User -> User -> m ()
guardCanChangeUser adminuser otheruser = do
  requiredPerm <- apiRequirePermission . canDo UpdateA . UserR $ (otheruser ^. #id)
  apiAccessControlOrIsAdmin adminuser requiredPerm $ return ()

apiCallUpdateOtherUserProfile :: forall  m . Kontrakcja m => UserID -> m Response
apiCallUpdateOtherUserProfile affectedUserID = V2.api $ do
  ctx             <- getContext
  authorizingUser <- V2.getAPIUserWithFullAccess
  mAffectedUser   <- dbQuery $ GetUserByID affectedUserID
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
            mx <- apiV2ParameterOptional . ApiV2ParameterTextWithValidation n $ emptyOK v
            return $ fromMaybe (getPrevValue $ affectedUser ^. #info) mx

      companyposition <- getUserParameter "companyposition"
                                          asValidPosition
                                          (^. #companyPosition)
      phone          <- getUserParameter "phone" asValidPhone (^. #phone)
      personalnumber <- getUserParameter "personalnumber"
                                         asValidPersonalNumber
                                         (^. #personalNumber)
      fstname <- getUserParameter "fstname" asValidName (^. #firstName)
      sndname <- getUserParameter "sndname" asValidName (^. #lastName)
      let affectedUserNewInfo =
            (affectedUser ^. #info)
              & (#firstName .~ fstname)
              & (#lastName .~ sndname)
              & (#personalNumber .~ personalnumber)
              & (#companyPosition .~ companyposition)
              & (#phone .~ phone)
      void . dbUpdate $ SetUserInfo affectedUserID affectedUserNewInfo
      void . dbUpdate $ LogHistoryUserInfoChanged affectedUserID
                                                  (ctx ^. #ipAddr)
                                                  (ctx ^. #time)
                                                  (affectedUser ^. #info)
                                                  affectedUserNewInfo
                                                  (Just $ authorizingUser ^. #id)

      mlang <- (langFromCode =<<) <$> getField "lang"

      forM_ mlang $ \lang -> dbUpdate
        $ SetUserSettings affectedUserID (affectedUser ^. #settings & #lang .~ lang)


      return $ V2.Ok (runJSONGen $ value "changed" True)

apiCallChangeOtherUserEmail :: Kontrakcja m => UserID -> m Response
apiCallChangeOtherUserEmail affectedUserID = V2.api $ do
  ctx             <- getContext
  authorizingUser <- V2.getAPIUserWithFullAccess
  mAffectedUser   <- dbQuery $ GetUserByID affectedUserID
  case mAffectedUser of
    Nothing           -> throwM . SomeDBExtraException $ forbidden "User doesn't exist"
    Just affectedUser -> do
      guardCanChangeUser authorizingUser affectedUser
      newemail <- Email <$> apiV2ParameterObligatory
        (ApiV2ParameterTextWithValidation "newemail" asValidEmail)
      mexistinguser <- dbQuery $ GetUserByEmail newemail
      case mexistinguser of
        Just _existinguser -> do
          sendChangeToExistingEmailInternalWarningMail affectedUser newemail
          return $ V2.Ok (runJSONGen $ value "sent" False)
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
          let oldemail = affectedUser ^. #info % #email
          mailNotification <- mailEmailChangeRequestedNotification ctx
                                                                   True
                                                                   affectedUser
                                                                   newemail
          scheduleEmailSendout
            (mailNotification
              { to = [ MailAddress { fullname = getFullName affectedUser
                                   , email    = unEmail oldemail
                                   }
                     ]
              }
            )
          return $ V2.Ok (runJSONGen $ value "sent" True)

apiCallActivateAccount :: Kontrakcja m => m Response
apiCallActivateAccount = V2.api $ do
  ctx         <- getContext
  tosAccepted <- apiV2ParameterObligatory (ApiV2ParameterBool "accepted_terms_of_service")
  unless tosAccepted $ do
    V2.apiError $ V2.requestParameterInvalid "accepted_terms_of_service"
                                             "Terms of service have to be accepted"
  email <- apiV2ParameterObligatory
    $ ApiV2ParameterTextWithValidation "email" asValidEmail
  muser <- dbQuery . GetUserByEmail $ Email email
  -- we are throwing same error in all places to prevent address harvesting
  let throwApiTokenInvalid = V2.apiError $ V2.requestFailed "Token is not valid"
  case (muser, view #hasAcceptedTOS =<< muser) of
    (Nothing  , _      ) -> throwApiTokenInvalid
    (_        , Just _ ) -> throwApiTokenInvalid
    (Just user, Nothing) -> do
      requiredPerm <- apiRequirePermission . canDo UpdateA . UserR $ (user ^. #id)
      apiAccessControlWithError user requiredPerm throwApiTokenInvalid $ do
        token        <- apiV2ParameterObligatory (ApiV2ParameterRead "token")
        validRequest <- isJust <$> getUserAccountRequestUser (user ^. #id) token
        unless validRequest $ void throwApiTokenInvalid
        void . dbUpdate $ DeleteUserAccountRequest (user ^. #id)
        void . dbUpdate $ AcceptTermsOfService (user ^. #id) (ctx ^. #time)
        void . dbUpdate $ LogHistoryTOSAccept (user ^. #id)
                                              (ctx ^. #ipAddr)
                                              (ctx ^. #time)
                                              Nothing
        password      <- apiV2ParameterObligatory (ApiV2ParameterText "password")
        validPassword <- checkPassword (ctx ^. #passwordServiceConf) password
        unless validPassword $ do
          V2.apiError $ V2.requestParameterInvalid "password" "Password is weak"
        passwordhash <- createPassword password
        void . dbUpdate $ SetUserPassword (user ^. #id) passwordhash
        void . dbUpdate $ LogHistoryPasswordSetup (user ^. #id)
                                                  (ctx ^. #ipAddr)
                                                  (ctx ^. #time)
                                                  Nothing
        return . V2.Ok . runJSONGen $ do
          value "activated" True

apiCallGetTags :: Kontrakcja m => m Response
apiCallGetTags = V2.api $ do
  user <- V2.getAPIUserWithFullAccess
  return . V2.Ok $ toJSON (user ^. #externalTags)

apiCallUpdateTags :: Kontrakcja m => m Response
apiCallUpdateTags = V2.api $ do
  user       <- V2.getAPIUserWithFullAccess
  tagUpdates <- V2.apiV2ParameterObligatory $ V2.ApiV2ParameterAeson "tags"
  let tags = updateTags (user ^. #externalTags) tagUpdates
  void . dbUpdate $ SetUserTags (user ^. #id) (user ^. #internalTags) tags
  return $ V2.Ok ()
