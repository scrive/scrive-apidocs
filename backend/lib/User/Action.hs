module User.Action (
    handleActivate
  , createUser
  ) where

import Control.Conditional (unlessM, whenM)
import Control.Monad.Catch
import Crypto.RNG
import Log
import Text.StringTemplates.Templates

import DB
import Doc.Model
import Folder.Model
import Happstack.Fields
import Kontra
import Log.Identifier
import MinutesTime
import PasswordService.Control
import Session.Model
import User.Email
import User.History.Model
import User.Model
import UserGroup.Model
import UserGroup.Types
import Util.HasSomeUserInfo
import Util.MonadUtils

handleActivate
  :: (Kontrakcja m)
  => Maybe Text
  -> Maybe Text
  -> (User, UserGroup)
  -> SignupMethod
  -> m User
handleActivate mfstname msndname (actvuser, ug) signupmethod = do
  logInfo "Attempting to activate account for user" $ logObject_ actvuser
  -- Don't remove - else people will be able to hijack accounts
  when (isJust $ userhasacceptedtermsofservice actvuser) $ do
    internalError

  whenM (not <$> isFieldSet "tos") $ do
    logInfo_ "Can't activate account, 'tos' parameter is missing"
    internalError

  switchLang (getLang actvuser)
  ctx      <- getContext
  phone    <- fromMaybe (getMobile actvuser) <$> getField "phone"
  ugname   <- fromMaybe (ug ^. #name) <$> getField "company"
  position <- fromMaybe "" <$> getField "position"

  void $ dbUpdate $ SetUserInfo (userid actvuser) $ (userinfo actvuser)
    { userfstname         = fromMaybe "" mfstname
    , usersndname         = fromMaybe "" msndname
    , userphone           = phone
    , usercompanyposition = position
    }
  void $ dbUpdate . UserGroupUpdate . set #name ugname $ ug
  void $ dbUpdate $ LogHistoryUserInfoChanged
    (userid actvuser)
    (ctx ^. #ipAddr)
    (ctx ^. #time)
    (userinfo actvuser)
    ((userinfo actvuser) { userfstname = fromMaybe "" mfstname
                         , usersndname = fromMaybe "" msndname
                         }
    )
    (userid <$> ctx ^. #maybeUser)
  void $ dbUpdate $ LogHistoryPasswordSetup (userid actvuser)
                                            (ctx ^. #ipAddr)
                                            (ctx ^. #time)
                                            (userid <$> ctx ^. #maybeUser)
  void $ dbUpdate $ AcceptTermsOfService (userid actvuser) (ctx ^. #time)
  void $ dbUpdate $ LogHistoryTOSAccept (userid actvuser)
                                        (ctx ^. #ipAddr)
                                        (ctx ^. #time)
                                        (userid <$> ctx ^. #maybeUser)
  void $ dbUpdate $ SetSignupMethod (userid actvuser) signupmethod

  dbUpdate $ ConnectSignatoriesToUser (Email $ getEmail actvuser)
                                      (userid actvuser)
                                      (14 `daysBefore` (ctx ^. #time))

  mpassword <- getField "password"
  case (mpassword) of
    Just password -> do
      unlessM (checkPassword (ctx ^. #passwordServiceConf) password) $ do
        logInfo_ "Can't activate account, 'password' is not good"
        internalError
      passwordhash <- createPassword password
      void . dbUpdate $ SetUserPassword (userid actvuser) passwordhash
      terminateAllUserSessionsExceptCurrent (userid actvuser)
    Nothing -> return ()

  tosuser <- guardJustM $ dbQuery $ GetUserByID (userid actvuser)

  logInfo "Attempt successful, user logged in" $ logObject_ actvuser

  whenM (not <$> isFieldSet "stoplogin") $ do
    logUserToContext $ Just tosuser

  return tosuser

createUser
  :: (CryptoRNG m, KontraMonad m, MonadDB m, MonadThrow m, TemplatesMonad m)
  => Email
  -> (Text, Text)
  -> (UserGroupID, Bool)
  -> Lang
  -> SignupMethod
  -> m (Maybe User)
createUser email names (ugid, iscompanyadmin) lang sm = do
  ctx    <- getContext
  passwd <- randomPassword
  dbQuery (UserGroupGet ugid) >>= \case
    Nothing -> return Nothing
    Just ug -> do
      -- create User home Folder, if the UserGroup has one
      mUserFolder <- case ug ^. #homeFolderID of
        Nothing -> return Nothing
        Just ugFid ->
          fmap Just . dbUpdate . FolderCreate . set #parentID (Just ugFid) $ defaultFolder
      muser <- dbUpdate $ AddUser names
                                  (unEmail email)
                                  (Just passwd)
                                  (ugid, view #id <$> mUserFolder, iscompanyadmin)
                                  lang
                                  (ctx ^. #brandedDomain % #id)
                                  sm
      whenJust muser $ \user -> void . dbUpdate $ LogHistoryAccountCreated
        (userid user)
        (ctx ^. #ipAddr)
        (ctx ^. #time)
        email
        (userid <$> contextUser ctx)
      return muser
