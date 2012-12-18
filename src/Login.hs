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

import Control.Applicative
import Data.Maybe
import Happstack.Server hiding (simpleHTTP, host, dir, path)
import Text.JSON.Gen as J
import Text.JSON
import qualified Templates.Fields as F
import Templates.Templates

handleLoginGet :: Kontrakcja m => m (Either KontraLink String)
handleLoginGet = do
  ctx <- getContext
  case (ctxmaybeuser ctx) of
       Nothing -> do
          referer <- getField "referer"
          content <- renderTemplate "loginPage" $ do
                    F.value "referer" $ fromMaybe "/" referer
          return $ Right content
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

signupPageGet :: Kontrakcja m => m String
signupPageGet = do
  memail <- getField "email"
  renderTemplate "signupPage" $ do
    F.value "email" memail
    F.value "signuplink" $ show LinkSignup

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
  case memail of
    Nothing -> return Nothing
    Just email -> do
      muser <- dbQuery $ GetUserByEmail $ Email email
      case (muser, muser >>= userhasacceptedtermsofservice) of
        (Just user, Nothing) -> do
          -- there is an existing user that hasn't been activated
          -- send them another invite
          UserControl.sendNewUserMail user
          return $ Just (Email email, Nothing)
        (Nothing, Nothing) -> do
          -- this email address is new to the system, so create the user
          -- and send an invite
          lang <- ctxlang <$> getContext
          mnewuser <- createUser (Email email) (fromMaybe "" mfirstname, fromMaybe "" mlastname) Nothing lang
          maybe (return ()) UserControl.sendNewUserMail mnewuser
          return $ Just (Email email, userid <$> mnewuser)
        (_, _) -> return $ Just (Email email, Nothing)
        -- whatever happens we want the same outcome, we just claim we sent the activation link,
        -- because we don't want any security problems with user information leaks

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
    ctx <- getContext
    logUserToContext Nothing
    sendRedirect $ LinkHome (ctxlang ctx)
