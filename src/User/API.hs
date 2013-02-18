module User.API (
    userAPI,
    apiCallGetUserProfile,
    apiCallChangeUserPassword,
    apiCallChangeUserLanguage,
    apiCallUpdateUserProfile,
    apiCallChangeEmail,
    apiCallCreateCompany
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
import User.UserControl
import Payments.Action
import Company.Model
import Mails.SendMail
import ActionQueue.EmailChangeRequest
import Stats.Control
import Util.HasSomeUserInfo
import Util.MonadUtils
import Crypto.RNG

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
  dir "updateprofile"   $ hPostNoXTokenHttp $ toK0 $ apiCallUpdateUserProfile,
  dir "createcompany"   $ hPostNoXTokenHttp $ toK0 $ apiCallCreateCompany,
  dir "changeemail"     $ hPostNoXTokenHttp $ toK0 $ apiCallChangeEmail
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
  mpassword <- lift $ getOptionalField asValidPassword "password"
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
  (user, _ , _) <- getAPIUser APIPersonal
  ctx <- getContext
  infoUpdate <- lift $ getUserInfoUpdate
  _ <- dbUpdate $ SetUserInfo (userid user) (infoUpdate $ userinfo user)
  _ <- dbUpdate $ LogHistoryUserInfoChanged (userid user) (ctxipnumber ctx) (ctxtime ctx)
                                               (userinfo user) (infoUpdate $ userinfo user)
                                               (userid <$> ctxmaybeuser ctx)
  mcompany <- getCompanyForUser user
  case (useriscompanyadmin user, mcompany) of
    (True, Just company) -> do
      companyinfoupdate <- lift $ getCompanyInfoUpdate
      _ <- dbUpdate $ SetCompanyInfo (companyid company) (companyinfoupdate $ companyinfo company)
      Ok <$> (runJSONGenT $ value "changed" True)
    _ ->   Ok <$> (runJSONGenT $ value "changed" True)

apiCallChangeEmail :: Kontrakcja m => m Response
apiCallChangeEmail = api $ do
  ctx <- getContext
  (user, _ , _) <- getAPIUser APIPersonal
  mnewemail <- lift $ getRequiredField asValidEmail "newemail"
  case (Email <$> mnewemail) of
    (Just newemail) -> do
       mexistinguser <- dbQuery $ GetUserByEmail newemail
       case mexistinguser of
         Just _existinguser -> do
           lift $ sendChangeToExistingEmailInternalWarningMail user newemail
           Ok <$> (runJSONGenT $ value "send" False)
         Nothing -> do
            changeemaillink <- newEmailChangeRequestLink (userid user) newemail
            mail <- mailEmailChangeRequest (ctxhostpart ctx) user newemail changeemaillink
            scheduleEmailSendout (ctxmailsconfig ctx)
                        (mail{to = [MailAddress{
                                    fullname = getFullName user
                                  , email = unEmail newemail }]})
            Ok <$> (runJSONGenT $ value "send" True)
    Nothing -> Ok <$> (runJSONGenT $ value "send" False)



apiCallCreateCompany :: Kontrakcja m => m Response
apiCallCreateCompany =  api $  do
  ctx <- getContext
  (user, _ , _) <- getAPIUser APIPersonal
  company <- dbUpdate $ CreateCompany Nothing
  mailapikey <- random
  _ <- dbUpdate $ SetCompanyMailAPIKey (companyid company) mailapikey 1000
  _ <- dbUpdate $ SetUserCompany (userid user) (Just $ companyid company)
  _ <- dbUpdate $ SetUserCompanyAdmin (userid user) True
  -- payment plan needs to migrate to company
  _ <- lift $ switchPlanToCompany (userid user) (companyid company)
  upgradeduser <- lift $ guardJustM $ dbQuery $ GetUserByID $ userid user
  _ <- lift $ addUserCreateCompanyStatEvent (ctxtime ctx) upgradeduser
  _ <- dbUpdate $ LogHistoryDetailsChanged (userid user) (ctxipnumber ctx) (ctxtime ctx)
                                              [("is_company_admin", "false", "true")]
                                              (Just $ userid user)
  Ok <$> (runJSONGenT $ value "created" True)

