module User.API (
    userAPI,
    apiCallGetUserProfile,
    apiCallChangeUserPassword,
    apiCallChangeUserLanguage
  ) where


import Happstack.StaticRouting
import KontraMonad
import Happstack.Server.Types
import Routing
import API.APIVersion (APIVersion(..))
import Control.Applicative
import User.Model
import Kontra
import API.Monad
import Control.Monad.Error
import Happstack.Fields
import Text.JSON.Gen
import OAuth.Model
import OAuth.View
import Data.Maybe
import DB
import User.UserView
import User.Utils
import ScriveByMail.Model
import Control.Logic
import InputValidation
import User.History.Model

userAPI :: Route (KontraPlus Response)
userAPI = dir "api" $ choice
  [ dir "frontend" $ versionedAPI Frontend
  , versionedAPI V1 -- Temporary backwards compatibility for clients accessing version-less API
  , dir "v1" $ versionedAPI V1
  ]

versionedAPI :: APIVersion -> Route (KontraPlus Response)
versionedAPI _version = choice [
  dir "getpersonaltoken"     $ hPostNoXTokenHttp $ toK0 $ apiCallGetUserPersonalToken,
  dir "getprofile" $ hGet $ toK0 $ apiCallGetUserProfile,
  dir "changepassword"  $ hPostNoXTokenHttp $ toK0 $ apiCallChangeUserPassword,
  dir "changelanguage"  $ hPostNoXTokenHttp $ toK0 $ apiCallChangeUserLanguage,
  dir "changefooter"    $ hPostNoXTokenHttp $ toK0 $ apiCallChangeUserFooter,
  dir "updateprofile"   $ hPostNoXTokenHttp $ toK0 $ apiCallUpdateUserProfile
  ]


apiCallGetUserPersonalToken :: Kontrakcja m => m Response
apiCallGetUserPersonalToken = api $ do
    memail  <- lift $ getField "email"
    mpasswd <- lift $ getField "password"
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
                       Nothing ->  throwError $ serverError "No token found, this should not happend"
                       Just t ->  return $ Ok $ jsonFromPersonalToken t
              else throwError $ serverError "Email and password don't match"
        _ -> throwError $ serverError "Email or password is missing"

apiCallGetUserProfile :: Kontrakcja m => m Response
apiCallGetUserProfile =  api $ do
  ctx <- getContext
  (user, _ , _) <- getAPIUser APIPersonal
  mumailapi <- dbQuery $ GetUserMailAPI $ userid user
  mcompany <- getCompanyForUser user
  mcmailapi <- maybe (return Nothing) (dbQuery . GetCompanyMailAPI) $ usercompany user
  Ok <$> userJSON user mumailapi mcompany mcmailapi (useriscompanyadmin user || (isAdmin ||^ isSales) ctx)


apiCallChangeUserPassword :: Kontrakcja m => m Response
apiCallChangeUserPassword = api $ do
  ctx <- getContext
  (user, _ , _) <- getAPIUser APIPersonal
  oldpassword <- lift $ getField' "oldpassword"
  mpassword <- lift $ getOptionalField asValidPassword "password1"
  mpassword2 <- lift $ getOptionalField asDirtyPassword "password2"
  case (mpassword, mpassword == mpassword2) of
     (Just password, True) ->
          if (verifyPassword (userpassword user) oldpassword)
            then do
              passwordhash <- createPassword password
              _ <- dbUpdate $ SetUserPassword (userid user) passwordhash
              _ <- dbUpdate $ LogHistoryPasswordSetup (userid user) (ctxipnumber ctx) (ctxtime ctx) (Just $ userid $ user)
              Ok <$> (runJSONGenT $ value "changed" True)
            else do
              _ <- dbUpdate $ LogHistoryPasswordSetupReq (userid user) (ctxipnumber ctx) (ctxtime ctx) (Just $ userid $ user)
              Ok <$> (runJSONGenT $ value "changed" False)
     _ ->  throwError $ serverError "One of parameters is missing, newpassword fields do not match or password is too week"


apiCallChangeUserLanguage :: Kontrakcja m => m Response
apiCallChangeUserLanguage = api $ do
  (user, _ , _) <- getAPIUser APIPersonal
  mlang <- lift $  (join . (fmap langFromCode)) <$> getField "lang"
  case mlang of
       Just lang -> do
         _ <- dbUpdate $ SetUserSettings (userid user) $ (usersettings user) {
             lang = lang
           }
         Ok <$> (runJSONGenT $ value "changed" True)
       Nothing -> do
         Ok <$> (runJSONGenT $ value "changed" False)


apiCallChangeUserFooter :: Kontrakcja m => m Response
apiCallChangeUserFooter = api $ do
  (user, _ , _) <- getAPIUser APIPersonal
  customfooter <- lift $ getField "customfooter"
  _ <- dbUpdate $ SetUserSettings (userid user) $ (usersettings user) {
             customfooter = customfooter
           }
  Ok <$> (runJSONGenT $ value "changed" True)



apiCallUpdateUserProfile :: Kontrakcja m => m Response
apiCallUpdateUserProfile = api $ do
  _ <- throwError $ serverError "Call not ready"
  Ok <$> (runJSONGenT $ return ())
