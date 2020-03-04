module User.Action (
    handleActivate
  , createUser
  , getCreateUserContextFromContext
  , getCreateUserContextWithoutContext
  , CreateUserContext
  ) where

import Control.Conditional (unlessM, whenM)
import Control.Monad.Catch
import Crypto.RNG
import Log
import qualified Data.Set as S

import BrandedDomain.Model
import DB
import Doc.Model
import Folder.Model
import Happstack.Fields
import IPAddress
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
  when (isJust $ actvuser ^. #hasAcceptedTOS) $ do
    internalError

  whenM (not <$> isFieldSet "tos") $ do
    logInfo_ "Can't activate account, 'tos' parameter is missing"
    internalError

  switchLang (getLang actvuser)
  ctx      <- getContext
  phone    <- fromMaybe (getMobile actvuser) <$> getField "phone"
  ugname   <- fromMaybe (ug ^. #name) <$> getField "company"
  position <- fromMaybe "" <$> getField "position"

  void
    $ dbUpdate
    $ SetUserInfo (actvuser ^. #id)
    $ (actvuser ^. #info)
    & (#firstName .~ fromMaybe "" mfstname)
    & (#lastName .~ fromMaybe "" msndname)
    & (#phone .~ phone)
    & (#companyPosition .~ position)

  void $ dbUpdate . UserGroupUpdate . set #name ugname $ ug
  case ug ^. #address of
    Just addr -> do
      let newAddress = Just $ set #entityName ugname addr
      void $ dbUpdate $ UserGroupUpdateAddress (ug ^. #id) newAddress
    Nothing -> return () -- dont udate entity name for inherited addresses

  void $ dbUpdate $ LogHistoryUserInfoChanged
    (actvuser ^. #id)
    (ctx ^. #ipAddr)
    (ctx ^. #time)
    (actvuser ^. #info)
    ( (actvuser ^. #info)
    & (#firstName .~ fromMaybe "" mfstname)
    & (#lastName .~ fromMaybe "" msndname)
    )
    (ctx ^? #maybeUser % _Just % #id)
  void $ dbUpdate $ LogHistoryPasswordSetup (actvuser ^. #id)
                                            (ctx ^. #ipAddr)
                                            (ctx ^. #time)
                                            (ctx ^? #maybeUser % _Just % #id)
  void $ dbUpdate $ AcceptTermsOfService (actvuser ^. #id) (ctx ^. #time)
  void $ dbUpdate $ LogHistoryTOSAccept (actvuser ^. #id)
                                        (ctx ^. #ipAddr)
                                        (ctx ^. #time)
                                        (ctx ^? #maybeUser % _Just % #id)
  void $ dbUpdate $ SetSignupMethod (actvuser ^. #id) signupmethod

  dbUpdate $ ConnectSignatoriesToUser (Email $ getEmail actvuser)
                                      (actvuser ^. #id)
                                      (14 `daysBefore` (ctx ^. #time))

  mpassword <- getField "password"
  case (mpassword) of
    Just password -> do
      unlessM (checkPassword (ctx ^. #passwordServiceConf) password) $ do
        logInfo_ "Can't activate account, 'password' is not good"
        internalError
      passwordhash <- createPassword password
      void . dbUpdate $ SetUserPassword (actvuser ^. #id) passwordhash
      terminateAllUserSessionsExceptCurrent (actvuser ^. #id)
    Nothing -> return ()

  tosuser <- guardJustM $ dbQuery $ GetUserByID (actvuser ^. #id)

  logInfo "Attempt successful, user logged in" $ logObject_ actvuser

  whenM (not <$> isFieldSet "stoplogin") $ do
    logUserToContext $ Just tosuser

  return tosuser

data CreateUserContext = CreateUserContext {
      ipAddr :: IPAddress
    , creationTime :: UTCTime
    , creatingUser :: Maybe User
    , brandedDomain :: BrandedDomain
  }

getCreateUserContextFromContext :: KontraMonad m => m CreateUserContext
getCreateUserContextFromContext = do
  ctx <- getContext
  return $ CreateUserContext { ipAddr        = ctx ^. #ipAddr
                             , creationTime  = ctx ^. #time
                             , creatingUser  = contextUser ctx
                             , brandedDomain = ctx ^. #brandedDomain
                             }

getCreateUserContextWithoutContext
  :: (MonadDB m, MonadThrow m, MonadTime m, MonadLog m) => m CreateUserContext
getCreateUserContextWithoutContext = do
  now <- currentTime
  bd  <- dbQuery $ GetMainBrandedDomain
  return $ CreateUserContext { ipAddr        = noIP
                             , creationTime  = now
                             , creatingUser  = Nothing
                             , brandedDomain = bd
                             }

createUser
  :: (CryptoRNG m, CryptoRNG m, MonadDB m, MonadThrow m)
  => Email
  -> (Text, Text)
  -> (UserGroupID, Bool)
  -> Lang
  -> SignupMethod
  -> CreateUserContext
  -> m (Maybe User)
createUser email names (ugid, iscompanyadmin) lang sm ctx = do
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
                                  ((brandedDomain ctx) ^. #id)
                                  sm
                                  S.empty
                                  S.empty
      whenJust muser $ \user -> void . dbUpdate $ LogHistoryAccountCreated
        (user ^. #id)
        (ipAddr ctx)
        (creationTime ctx)
        email
        (view #id <$> (creatingUser ctx))
      return muser
