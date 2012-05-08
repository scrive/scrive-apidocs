module Login (
    forgotPasswordPagePost
  , signupPagePost
  , handleLoginGet
  , handleLoginPost
  , handleLogout
  , handleSignup
  ) where

import ActionQueue.Core
import ActionQueue.PasswordReminder
import AppView as V
import DB
import InputValidation
import Kontra
import KontraLink
import Mails.SendMail
import Misc
import Redirect
import User.Model
import User.UserView as UserView
import qualified Log (security, debug)
import qualified User.UserControl as UserControl
import Util.FlashUtil
import Util.HasSomeUserInfo
import Stats.Control
import User.History.Model

import Control.Applicative
import Data.Maybe
import Happstack.Server hiding (simpleHTTP, host, dir, path)

{- |
   Handles submission of the password reset form
-}
forgotPasswordPagePost :: Kontrakcja m => m KontraLink
forgotPasswordPagePost = do
  ctx <- getContext
  memail <- getOptionalField asValidEmail "email"
  case memail of
    Nothing -> return LoopBack
    Just email -> do
      muser <- dbQuery $ GetUserByEmail Nothing $ Email email
      case muser of
        Nothing -> do
          Log.security $ "ip " ++ (show $ ctxipnumber ctx) ++ " made a failed password reset request for non-existant account " ++ email
        Just user -> do
          minv <- dbQuery $ GetAction passwordReminder $ userid user
          case minv of
            Just pr@PasswordReminder{..} -> case prRemainedEmails of
              0 -> addFlashM flashMessageNoRemainedPasswordReminderEmails
              n -> do
                _ <- dbUpdate $ UpdateAction passwordReminder $ pr { prRemainedEmails = n - 1 }
                sendResetPasswordMail ctx (LinkPasswordReminder prUserID prToken) user
            _ -> do
              link <- newPasswordReminderLink $ userid user
              sendResetPasswordMail ctx link user
      addFlashM flashMessageChangePasswordEmailSend
      return LinkUpload

sendResetPasswordMail :: Kontrakcja m => Context -> KontraLink -> User -> m ()
sendResetPasswordMail ctx link user = do
  mail <- UserView.resetPasswordMail (ctxhostpart ctx) user link
  scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [getMailAddress user] }

{- |
   Handles submission of the signup form.
   Normally this would create the user, (in the process mailing them an activation link),
   but if the user already exists, we check to see if they have accepted the tos.  If they haven't,
   then we send them a new activation link because probably the old one expired or was lost.
   If they have then we stop the signup.
-}
signupPagePost :: Kontrakcja m => m KontraLink
signupPagePost = do
  me <- handleSignup
  maybeM (addFlashM . modalUserSignupDone . fst) me
  return LoopBack

{- |
   Try to sign up a new user. Returns the email and the new user id. If the 
   user already existed, don't return the userid.
 -}
handleSignup :: Kontrakcja m => m (Maybe (Email, Maybe UserID))
handleSignup = do
  memail <- getOptionalField asValidEmail "email"
  case memail of
    Nothing -> return Nothing
    Just email -> do
      muser <- dbQuery $ GetUserByEmail Nothing $ Email $ email
      case (muser, muser >>= userhasacceptedtermsofservice) of
        (Just user, Nothing) -> do
          -- there is an existing user that hasn't been activated
          -- send them another invite
          UserControl.sendNewUserMail user
          return $ Just (Email email, Nothing)
        (Nothing, Nothing) -> do
          -- this email address is new to the system, so create the user
          -- and send an invite
          mnewuser <- UserControl.createUser (Email email) "" "" Nothing
          maybe (return ()) UserControl.sendNewUserMail mnewuser
          return $ Just (Email email, userid <$> mnewuser)
        (_, _) -> return $ Just (Email email, Nothing)
        -- whatever happens we want the same outcome, we just claim we sent the activation link,
        -- because we don't want any security problems with user information leaks
        

{- |
   Handles viewing of the login page
-}
handleLoginGet :: Kontrakcja m => m Response
handleLoginGet = do
  ctx <- getContext
  case ctxmaybeuser ctx of
       Just _  -> sendRedirect LinkUpload
       Nothing -> do
         referer <- getField "referer"
         email   <- getField "email"
         content <- V.pageLogin referer email
         V.renderFromBody V.kontrakcja content

{- |
   Handles submission of a login form.  On failure will redirect back to referer, if there is one.
-}
handleLoginPost :: Kontrakcja m => m KontraLink
handleLoginPost = do
    ctx <- getContext
    memail  <- getOptionalField asDirtyEmail    "email"
    mpasswd <- getOptionalField asDirtyPassword "password"
    let linkemail = fromMaybe "" memail
    case (memail, mpasswd) of
        (Just email, Just passwd) -> do
            -- check the user things here
            maybeuser <- dbQuery $ GetUserByEmail Nothing (Email email)
            case maybeuser of
                Just user@User{userpassword}
                    | verifyPassword userpassword passwd -> do
                        Log.debug $ "User " ++ show email ++ " logged in"
                        _ <- dbUpdate $ SetUserSettings (userid user) $ (usersettings user) {
                          locale = ctxlocale ctx
                        }
                        muuser <- dbQuery $ GetUserByID (userid user)
                        _ <- addUserLoginStatEvent (ctxtime ctx) (fromJust muuser)
                        _ <- dbUpdate $ LogHistoryLoginSuccess (userid user) (ctxipnumber ctx) (ctxtime ctx)
                        logUserToContext muuser
                        return BackToReferer
                Just u -> do
                        Log.debug $ "User " ++ show email ++ " login failed (invalid password)"
                        _ <- dbUpdate $ LogHistoryLoginAttempt (userid u) (ctxipnumber ctx) (ctxtime ctx)
                        return $ LinkLogin (ctxlocale ctx) $ InvalidLoginInfo linkemail
                Nothing -> do
                    Log.debug $ "User " ++ show email ++ " login failed (user not found)"
                    return $ LinkLogin (ctxlocale ctx) $ InvalidLoginInfo linkemail
        _ -> return $ LinkLogin (ctxlocale ctx) $ InvalidLoginInfo linkemail

{- |
   Handles the logout, and sends user back to main page.
-}
handleLogout :: Kontrakcja m => m Response
handleLogout = do
    ctx <- getContext
    logUserToContext Nothing
    sendRedirect $ LinkHome (ctxlocale ctx)
