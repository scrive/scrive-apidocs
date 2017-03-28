module User.API (
    userAPI,
    apiCallGetUserPersonalToken,
    apiCallGetUserProfile,
    apiCallLoginUser,
    apiCallChangeUserPassword,
    apiCallUpdateUserProfile,
    apiCallChangeEmail,
    apiCallSignup,
    apiCallLoginUserAndGetSession
  ) where

import Control.Conditional ((<|), (|>))
import Control.Monad.Catch
import Control.Monad.Reader
import Happstack.Server.Types
import Happstack.StaticRouting
import Text.JSON.Gen hiding (object)

import ActionQueue.Core
import ActionQueue.EmailChangeRequest
import ActionQueue.PasswordReminder
import ActionQueue.UserAccountRequest
import API.Monad.V1
import Chargeable.Model
import Company.Model
import Context
import DB
import Happstack.Fields
import InputValidation
import Kontra
import KontraLink
import KontraPrelude
import Mails.SendMail
import OAuth.Model
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
import User.History.Model
import User.Model
import User.UserControl
import User.UserView
import User.Utils
import Util.HasSomeUserInfo
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
  dir "testsalesforceintegration" $ hGet $ toK0 $ apiCallTestSalesforceIntegration
  ]

userAPIV2 :: Route (Kontra Response)
userAPIV2 = choice [
  dir "loginandgetsession" $ hPost $ toK0 $ apiCallLoginUserAndGetSession,
  userAPIV1
  ]

apiCallGetUserPersonalToken :: Kontrakcja m => m Response
apiCallGetUserPersonalToken = api $ do
    memail  <- getField "email"
    mpasswd <- getField "password"
    case (memail, mpasswd) of
        (Just email, Just passwd) -> do
            -- check the user things here
            muser <- dbQuery $ GetUserByEmail (Email email)
            if (isJust muser && verifyPassword (userpassword $ fromJust muser) passwd )
              then do
                  let uid = userid $ fromJust muser
                  _success <- dbUpdate $ CreatePersonalToken uid
                  token <- dbQuery $ GetPersonalToken uid
                  case token of
                       Nothing -> throwM . SomeDBExtraException $ serverError "No token found, this should not happend"
                       Just t  -> return $ Ok (unjsonOAuthAuthorization, t)
              else throwM . SomeDBExtraException $ serverError "Email and password don't match"
        _ -> throwM . SomeDBExtraException $ serverError "Email or password is missing"

apiCallGetUserProfile :: Kontrakcja m => m Response
apiCallGetUserProfile =  api $ do
  (user, _ , _) <- getAPIUserWithAnyPrivileges
  company <- getCompanyForUser user
  Ok <$> userJSON user company


apiCallGetSubscription :: Kontrakcja m => m Response
apiCallGetSubscription =  api $ do
  (user, _ , _) <- getAPIUserWithAnyPrivileges
  company <- getCompanyForUser user
  users <- dbQuery $ GetCompanyAccounts $ companyid company
  docsStartedThisMonth <- fromIntegral <$> (dbQuery $ GetNumberOfDocumentsStartedThisMonth $ companyid company)
  Ok <$> subscriptionJSON company users docsStartedThisMonth

apiCallChangeUserPassword :: Kontrakcja m => m Response
apiCallChangeUserPassword = api $ do
  ctx <- getContext
  (user, _ , _) <- getAPIUserWithAnyPrivileges
  oldpassword <- getField' "oldpassword"
  mpassword <- getOptionalField asValidPassword "password"
  case (mpassword) of
     (Just password) ->
          if (verifyPassword (userpassword user) oldpassword)
            then do
              passwordhash <- createPassword password
              _ <- dbUpdate $ SetUserPassword (userid user) passwordhash
              _ <- dbUpdate $ LogHistoryPasswordSetup (userid user) (ctxipnumber ctx) (ctxtime ctx) (Just $ userid $ user)
              Ok <$> (runJSONGenT $ value "changed" True)
            else do
              _ <- dbUpdate $ LogHistoryPasswordSetupReq (userid user) (ctxipnumber ctx) (ctxtime ctx) (Just $ userid $ user)
              Ok <$> (runJSONGenT $ value "changed" False)
     _ ->  throwM . SomeDBExtraException $ serverError "Newpassword fields do not match Scrive standard"

apiCallLoginUser :: Kontrakcja m => m Response
apiCallLoginUser = api $ do
  ctx <- getContext
  (user, _ , _) <- getAPIUser APIPersonal

  redirectUrl <- apiGuardJustM (badInput "Redirect URL not provided or invalid.") $ getField "redirect"

  _ <- dbUpdate $ LogHistoryLoginSuccess (userid user) (ctxipnumber ctx) (ctxtime ctx)
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
  _ <- dbUpdate $ LogHistoryUserInfoChanged (userid user) (ctxipnumber ctx) (ctxtime ctx)
                                               (userinfo user) (infoUpdate $ userinfo user)
                                               (userid <$> ctxmaybeuser ctx)
  if (useriscompanyadmin user)
    then do
      company <- getCompanyForUser user
      companyinfoupdate <- getCompanyInfoUpdate
      _ <- dbUpdate $ SetCompanyInfo (companyid company) (companyinfoupdate $ companyinfo company)
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
  lang <- fromMaybe (ctxlang ctx) <$> langFromCode <$> getField' "lang"
  switchLang lang
  muser <- dbQuery $ GetUserByEmail $ Email email
  muser' <- case muser of
               Just user ->   return $ Nothing <| (isJust (userhasacceptedtermsofservice user)) |> Just user
               Nothing ->  do
                 company <- dbUpdate $ CreateCompany
                 let newCompanyInfo = (companyinfo company) { companyname = companyName }
                 _ <- dbUpdate $ SetCompanyInfo (companyid company) newCompanyInfo
                 createUser (Email email) (firstname,lastname) (companyid company,True) lang AccountRequest
  case muser' of
    -- return ambiguous response in both cases to prevent a security issue
    Nothing -> runJSONGenT $ value "sent" True
    Just user -> do
          _ <- dbUpdate $ SetUserInfo (userid user) $ (userinfo user)
            { userphone = phone, usercompanyposition = companyPosition }
          sendNewUserMail user
          l <- newUserAccountRequestLink lang (userid user) AccountRequest
          asyncLogEvent "Send account confirmation email" [
                UserIDProp $ userid user,
                IPProp $ ctxipnumber ctx,
                TimeProp $ ctxtime ctx,
                someProp "Context" ("Acount request" :: String)
                ]
          asyncLogEvent SetUserProps [
                UserIDProp $ userid user,
                someProp "Account confirmation email" $ ctxtime ctx,
                NameProp (firstname ++ " " ++ lastname),
                FirstNameProp firstname,
                LastNameProp lastname,
                someProp "Confirmation link" $ show l
                ]
          runJSONGenT $ value "sent" True

apiCallSendPasswordReminder :: Kontrakcja m => m Response
apiCallSendPasswordReminder = api $ do
  ctx <- getContext
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
          minv <- dbQuery $ GetAction passwordReminder $ userid user
          case minv of
            Just pr@PasswordReminder{..} -> case prRemainedEmails of
              0 -> runJSONGenT $ value "send" True
              n -> do
                _ <- dbUpdate $ UpdateAction passwordReminder $ pr { prRemainedEmails = n - 1 }
                sendResetPasswordMail ctx (LinkPasswordReminder prUserID prToken) user
                runJSONGenT $ value "send" True
            _ -> do
              link <- newPasswordReminderLink $ userid user
              sendResetPasswordMail ctx link user
              runJSONGenT $ value "send" True
 where
  sendResetPasswordMail ctx link user = do
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
apiCallTestSalesforceIntegration = api $ do
  (user, _ , _) <- getAPIUser APIDocCheck
  scheme <- dbQuery $ GetUserCallbackSchemeByUserID $ userid user
  murl <- getField "url"
  when (isNothing murl) $ do
    throwM . SomeDBExtraException $ badInput $ "No 'url' parameter provided"
  let url = fromJust murl
  fmap Ok $ case scheme of
      Just (SalesforceScheme token)  -> do
        ctx <- getContext
        case ctxsalesforceconf ctx of
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

apiCallLoginUserAndGetSession :: Kontrakcja m => m Response
apiCallLoginUserAndGetSession = V2.api $ do
  -- parse oauth from json
  oauth <- V2.apiV2ParameterObligatory $ V2.ApiV2ParameterJSON "personal_token" unjsonOAuthAuthorization
  euser <- V2.getUserFromOAuthWithAnyPrivileges oauth
  case euser of
    Left err -> V2.apiError $ V2.invalidAuthorizationWithMsg err
    Right (User{userid}, _actor) -> do
      ctx <- getContext
      asyncLogEvent "Login"
        [ UserIDProp userid
        , IPProp $ ctxipnumber ctx
        , TimeProp $ ctxtime ctx
        ]
      asyncLogEvent SetUserProps
        [ UserIDProp userid
        , someProp "Last login" $ ctxtime ctx
        ]
      emptysession <- emptySession
      ses <- startNewSession emptysession (Just userid) Nothing
      return $ V2.Ok $ runJSONGen $ do
        value "session_id" (show $ sessionCookieInfoFromSession ses)
