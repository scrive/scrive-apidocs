module Login (
    forgotPasswordPagePost
  , signupPagePost
  , handleLoginGet
  , handleLoginPost
  , handleLogout
  ) where

import ActionSchedulerState
import AppView as V
import DB.Classes
import InputValidation
import Kontra
import KontraLink
import Mails.SendMail
import MinutesTime
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

import Control.Monad.Error
import Data.Functor
import Data.Maybe
import Happstack.Server hiding (simpleHTTP, host, dir, path)
import Happstack.State (query, update)

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
      muser <- runDBQuery $ GetUserByEmail Nothing $ Email email
      case muser of
        Nothing -> do
          Log.security $ "ip " ++ (show $ ctxipnumber ctx) ++ " made a failed password reset request for non-existant account " ++ email
        Just user -> do
          now <- liftIO getMinutesTime
          minv <- checkValidity now <$> (query $ GetPasswordReminder $ userid user)
          case minv of
            Just Action{ actionID, actionType = PasswordReminder { prToken, prRemainedEmails, prUserID } } ->
              case prRemainedEmails of
                0 -> addFlashM flashMessageNoRemainedPasswordReminderEmails
                n -> do
                  -- I had to make it PasswordReminder because it was complaining about not giving cases
                  -- for the constructors of ActionType
                  _ <- update $ UpdateActionType actionID $ PasswordReminder {
                      prToken          = prToken
                    , prRemainedEmails = n - 1
                    , prUserID         = prUserID}
                  sendResetPasswordMail ctx (LinkPasswordReminder actionID prToken) user
            _ -> do -- Nothing or other ActionTypes (which should not happen)
              link <- newPasswordReminderLink user
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
  memail <- getOptionalField asValidEmail "email"
  case memail of
    Nothing -> return LoopBack
    Just email -> do
      muser <- runDBQuery $ GetUserByEmail Nothing $ Email $ email
      case (muser, muser >>= userhasacceptedtermsofservice) of
        (Just user, Nothing) -> do
          -- there is an existing user that hasn't been activated
          -- send them another invite
          UserControl.sendNewUserMail user
        (Nothing, Nothing) -> do
          -- this email address is new to the system, so create the user
          -- and send an invite
          mnewuser <- UserControl.createUser (Email email) "" "" Nothing
          maybe (return ()) UserControl.sendNewUserMail mnewuser
        (_, _) -> return ()
      -- whatever happens we want the same outcome, we just claim we sent the activation link,
      -- because we don't want any security problems with user information leaks
      addFlashM $ modalUserSignupDone (Email email)
      return LoopBack

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
            maybeuser <- runDBQuery $ GetUserByEmail Nothing (Email email)
            case maybeuser of
                Just user@User{userpassword}
                    | verifyPassword userpassword passwd -> do
                        Log.debug $ "User " ++ show email ++ " logged in"
                        _ <- runDBUpdate $ SetUserSettings (userid user) $ (usersettings user) {
                          locale = ctxlocale ctx
                        }
                        muuser <- runDBQuery $ GetUserByID (userid user)
                        _ <- addUserLoginStatEvent (ctxtime ctx) (fromJust muuser)
                        _ <- runDBUpdate $ LogHistoryLoginSuccess (userid user) (ctxipnumber ctx) (ctxtime ctx)
                        logUserToContext muuser
                        return BackToReferer
                Just u -> do
                        Log.debug $ "User " ++ show email ++ " login failed (invalid password)"
                        _ <- runDBUpdate $ LogHistoryLoginAttempt (userid u) (ctxipnumber ctx) (ctxtime ctx)
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
