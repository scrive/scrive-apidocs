module Login (
    forgotPasswordPagePost
  , signupPageGet
  , signupPagePost
  , handleLoginGet
  , handleLoginPost
  , handleLogout
  , handleSignup
  ) where

import ActionQueue.Core
import ActionQueue.PasswordReminder
import DB
import InputValidation hiding (Result)
import Happstack.Fields
import Kontra
import KontraLink
import Mails.SendMail
import Redirect
import User.Action
import User.Model
import User.UserView as UserView
import qualified Log (security, debug)
import qualified User.UserControl as UserControl
import Util.HasSomeUserInfo
import Stats.Control
import User.History.Model
import ActionQueue.UserAccountRequest

import Control.Applicative
import Data.Maybe
import Happstack.Server hiding (simpleHTTP, host, dir, path)
import Text.JSON.Gen as J
import Text.JSON
import qualified Templates.Fields as F
import Templates.Templates
import Routing
import Utils.HTTP
import ThirdPartyStats.Core

handleLoginGet :: Kontrakcja m => m (Either KontraLink ThinPage)
handleLoginGet = do
  ctx <- getContext
  case (ctxmaybeuser ctx) of
       Nothing -> do
          referer <- getField "referer"
          content <- renderTemplate "loginPage" $ do
                    F.value "referer" $ fromMaybe "/" referer
          return $ Right $ ThinPage content
       Just _ -> return $ Left LinkDesignView   

{- |
   Handles submission of the password reset form
-}
forgotPasswordPagePost :: Kontrakcja m => m JSValue
forgotPasswordPagePost = do
  ctx <- getContext
  memail <- getOptionalFieldNoFlash asValidEmail "email"
  case memail of
    Nothing -> runJSONGenT $ value "send" False >> value "badformat" True
    Just email -> do
      muser <- dbQuery $ GetUserByEmail $ Email email
      case muser of
        Nothing -> do
          Log.security $ "ip " ++ (show $ ctxipnumber ctx) ++ " made a failed password reset request for non-existant account " ++ email
          runJSONGenT $ value "send" False >> value "nouser" True
        Just user -> do
          minv <- dbQuery $ GetAction passwordReminder $ userid user
          case minv of
            Just pr@PasswordReminder{..} -> case prRemainedEmails of
              0 -> runJSONGenT $ value "send" False >> value "toomuch" True
              n -> do
                _ <- dbUpdate $ UpdateAction passwordReminder $ pr { prRemainedEmails = n - 1 }
                sendResetPasswordMail ctx (LinkPasswordReminder prUserID prToken) user
                runJSONGenT $ value "send" True
            _ -> do
              link <- newPasswordReminderLink $ userid user
              sendResetPasswordMail ctx link user
              runJSONGenT $ value "send" True

sendResetPasswordMail :: Kontrakcja m => Context -> KontraLink -> User -> m ()
sendResetPasswordMail ctx link user = do
  mail <- UserView.resetPasswordMail (ctxhostpart ctx) user link
  scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [getMailAddress user] }

signupPageGet :: Kontrakcja m => m ThinPage
signupPageGet = do
  memail <- getField "email"
  fmap ThinPage $ renderTemplate "signupPage" $ do
    F.value "email" memail

{- |
   Handles submission of the signup form.
   Normally this would create the user, (in the process mailing them an activation link),
   but if the user already exists, we check to see if they have accepted the tos.  If they haven't,
   then we send them a new activation link because probably the old one expired or was lost.
   If they have then we stop the signup.
-}
signupPagePost :: Kontrakcja m => m JSValue
signupPagePost = do
  me <- handleSignup
  runJSONGenT $ value "sent" $ isJust me

{- |
   Try to sign up a new user. Returns the email and the new user id. If the
   user already existed, don't return the userid.
 -}
handleSignup :: Kontrakcja m => m (Maybe (Email, Maybe UserID))
handleSignup = do
  memail <- getOptionalFieldNoFlash asValidEmail "email"
  mfirstname <- getOptionalFieldNoFlash asValidName "firstName"
  mlastname <- getOptionalFieldNoFlash asValidName "lastName"
  ctx <- getContext
  case memail of
    Nothing -> return Nothing
    Just email -> do
      muser <- dbQuery $ GetUserByEmail $ Email email
      case (muser, muser >>= userhasacceptedtermsofservice) of
        (Just user, Nothing) -> do
          -- there is an existing user that hasn't been activated
          -- send them another invite
          UserControl.sendNewUserMail user
          l <- newUserAccountRequestLink (ctxlang ctx) (userid user) AccountRequest
          asyncLogEvent "Send account confirmation email" [
            UserIDProp $ userid user,
            IPProp $ ctxipnumber ctx,
            TimeProp $ ctxtime ctx,
            someProp "Context" ("Acount request" :: String)
            ]
          asyncLogEvent SetUserProps [    
            UserIDProp $ userid user,
            someProp "Account confirmation email" $ ctxtime ctx,
            someProp "Confirmation link" $ show l
            ]
          return $ Just (Email email, Nothing)
        (Nothing, Nothing) -> do
          -- this email address is new to the system, so create the user
          -- and send an invite
          lang <- ctxlang <$> getContext
          mnewuser <- createUser (Email email) (fromMaybe "" mfirstname, fromMaybe "" mlastname) Nothing lang
          case mnewuser of
            Nothing -> return $ Just (Email email, Nothing)
            Just newuser -> do
              UserControl.sendNewUserMail newuser
              l <- newUserAccountRequestLink (ctxlang ctx) (userid newuser) AccountRequest
              asyncLogEvent "Send account confirmation email" [
                UserIDProp $ userid newuser,
                IPProp $ ctxipnumber ctx,
                TimeProp $ ctxtime ctx,
                someProp "Context" ("Acount request" :: String)
                ]
              asyncLogEvent SetUserProps [    
                UserIDProp $ userid newuser,
                someProp "Account confirmation email" $ ctxtime ctx,
                NameProp (fromMaybe "" mfirstname ++ " " ++ fromMaybe "" mlastname),
                FirstNameProp $ fromMaybe "" mfirstname,
                LastNameProp $ fromMaybe "" mlastname,
                someProp "Confirmation link" $ show l
                ]
              return $ Just (Email email, Just $ userid newuser)
        (_, _) -> return Nothing

{- |
   Handles submission of a login form.  On failure will redirect back to referer, if there is one.
-}
handleLoginPost :: Kontrakcja m => m JSValue
handleLoginPost = do
    padlogin <- isFieldSet "pad"
    ctx <- getContext
    memail  <- getOptionalFieldNoFlash asDirtyEmail    "email"
    mpasswd <- getOptionalFieldNoFlash asDirtyPassword "password"
    case (memail, mpasswd) of
        (Just email, Just passwd) -> do
            -- check the user things here
            maybeuser <- dbQuery $ GetUserByEmail (Email email)
            case maybeuser of
                Just user@User{userpassword}
                    | verifyPassword userpassword passwd -> do
                        Log.debug $ "User " ++ show email ++ " logged in"
                        _ <- dbUpdate $ SetUserSettings (userid user) $ (usersettings user) {
                          lang = ctxlang ctx
                        }
                        muuser <- dbQuery $ GetUserByID (userid user)
                        _ <- addUserLoginStatEvent (ctxtime ctx) (fromJust muuser)
                        case muuser of
                          Just User{userid = uid} -> do
                            asyncLogEvent "Login" [
                              UserIDProp uid,
                              IPProp $ ctxipnumber ctx,
                              TimeProp $ ctxtime ctx                              
                              ]
                            asyncLogEvent SetUserProps [    
                              UserIDProp uid,
                              someProp "Last login" $ ctxtime ctx
                              ]
                          _ -> return ()
                        if padlogin
                          then do
                            _ <- dbUpdate $ LogHistoryPadLoginSuccess (userid user) (ctxipnumber ctx) (ctxtime ctx)
                            logPadUserToContext muuser
                          else do
                            _ <- dbUpdate $ LogHistoryLoginSuccess (userid user) (ctxipnumber ctx) (ctxtime ctx)
                            logUserToContext muuser
                        runJSONGenT $ value "logged" True
                Just u -> do
                        Log.debug $ "User " ++ show email ++ " login failed (invalid password)"
                        _ <- if padlogin
                          then dbUpdate $ LogHistoryPadLoginAttempt (userid u) (ctxipnumber ctx) (ctxtime ctx)
                          else dbUpdate $ LogHistoryLoginAttempt (userid u) (ctxipnumber ctx) (ctxtime ctx)
                        runJSONGenT $ value "logged" False
                Nothing -> do
                    Log.debug $ "User " ++ show email ++ " login failed (user not found)"
                    runJSONGenT $ value "logged" False
        _ -> runJSONGenT $ value "logged" False

{- |
   Handles the logout, and sends user back to main page.
-}
handleLogout :: Kontrakcja m => m Response
handleLogout = do
    logUserToContext Nothing
    (sendRedirect . LinkExternal) =<< getHttpHostpart
