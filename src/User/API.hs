module User.API (
    userAPI,
    apiCallGetUserProfile,
    apiCallChangeUserPassword,
    apiCallChangeUserLanguage,
    apiCallUpdateUserProfile,
    apiCallChangeEmail,
    apiCallSignup
  ) where


import Happstack.StaticRouting
import KontraMonad
import Happstack.Server.Types
import Routing
import API.APIVersion (APIVersion(..))
import Control.Applicative
import Control.Exception.Lifted
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
import DB.SQL2
import User.UserView
import User.Utils
import Control.Logic
import InputValidation
import User.History.Model
import User.UserControl
import Payments.Action
import Company.Model
import Company.CompanyUI
import Mails.SendMail
import ActionQueue.EmailChangeRequest
import Util.HasSomeUserInfo
import Util.FlashUtil
import ActionQueue.UserAccountRequest
import ThirdPartyStats.Core
import User.Action
import Payments.Model
import MinutesTime
import Doc.Model
import ActionQueue.Core
import ActionQueue.PasswordReminder
import KontraLink
import Utils.Monad

userAPI :: Route (KontraPlus Response)
userAPI = dir "api" $ choice
  [ dir "frontend" $ versionedAPI Frontend
  , versionedAPI V1 -- Temporary backwards compatibility for clients accessing version-less API
  , dir "v1" $ versionedAPI V1
  ]

versionedAPI :: APIVersion -> Route (KontraPlus Response)
versionedAPI _version = choice [
  dir "getpersonaltoken"     $ hPost $ toK0 $ apiCallGetUserPersonalToken,
  dir "signup"          $ hPost $ toK0 $ apiCallSignup,
  dir "sendpasswordresetmail" $ hPost $ toK0 $ apiCallSendPasswordReminder,
  dir "getprofile"      $ hGet $ toK0 $ apiCallGetUserProfile,
  dir "changepassword"  $ hPost $ toK0 $ apiCallChangeUserPassword,
  dir "changelanguage"  $ hPost $ toK0 $ apiCallChangeUserLanguage,
  dir "updateprofile"   $ hPost $ toK0 $ apiCallUpdateUserProfile,
  dir "changeemail"     $ hPost $ toK0 $ apiCallChangeEmail,
  dir "addflash"        $ hPost $ toK0 $ apiCallAddFlash,
  dir "paymentinfo"     $ hGet $ toK0 $ apiCallPaymentInfo,
  dir "userbrandingforsignview" $ hGet $ toK0 $ apiCallUserSignviewBranding
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
                       Nothing ->  throwIO . SomeKontraException $ serverError "No token found, this should not happend"
                       Just t ->  return $ Ok $ jsonFromPersonalToken t
              else throwIO . SomeKontraException $ serverError "Email and password don't match"
        _ -> throwIO . SomeKontraException $ serverError "Email or password is missing"

apiCallGetUserProfile :: Kontrakcja m => m Response
apiCallGetUserProfile =  api $ do
  ctx <- getContext
  (user, _ , _) <- getAPIUser APIPersonal
  company <- getCompanyForUser user
  companyui <- dbQuery $ GetCompanyUI (companyid company)
  Ok <$> userJSON ctx user (company,companyui)


apiCallUserSignviewBranding :: Kontrakcja m => m Response
apiCallUserSignviewBranding =  api $ do
  ctx <- getContext
  (user, _ , _) <- getAPIUserWithPad APIPersonal
  company <- getCompanyForUser user
  companyui <- dbQuery $ GetCompanyUI (companyid company)
  Ok <$> signviewBrandingJSON ctx user company companyui

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
     _ ->  throwIO . SomeKontraException $ serverError "Newpassword fields do not match Scrive standard"


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


apiCallUpdateUserProfile :: Kontrakcja m => m Response
apiCallUpdateUserProfile = api $ do
  (user, _ , _) <- getAPIUser APIPersonal
  ctx <- getContext
  infoUpdate <- lift $ getUserInfoUpdate

  mlang <- lift $  (join . (fmap langFromCode)) <$> getField "lang"
  when_ (isJust mlang) $ dbUpdate $ SetUserSettings (userid user) $ (usersettings user) { lang = fromJust mlang  }

  _ <- dbUpdate $ SetUserInfo (userid user) (infoUpdate $ userinfo user)
  _ <- dbUpdate $ LogHistoryUserInfoChanged (userid user) (ctxipnumber ctx) (ctxtime ctx)
                                               (userinfo user) (infoUpdate $ userinfo user)
                                               (userid <$> ctxmaybeuser ctx)
  if (useriscompanyadmin user)
    then do
      company <- getCompanyForUser user
      companyinfoupdate <- lift $ getCompanyInfoUpdate
      _ <- dbUpdate $ SetCompanyInfo (companyid company) (companyinfoupdate $ companyinfo company)
      Ok <$> (runJSONGenT $ value "changed" True)
    else  Ok <$> (runJSONGenT $ value "changed" True)

apiCallChangeEmail :: Kontrakcja m => m Response
apiCallChangeEmail = api $ do
  ctx <- getContext
  (user, _ , _) <- getAPIUser APIPersonal
  mnewemail <- lift $ getOptionalField asValidEmail "newemail"
  case (Email <$> mnewemail) of
    (Just newemail) -> do
       mexistinguser <- dbQuery $ GetUserByEmail newemail
       case mexistinguser of
         Just _existinguser -> do
           lift $ sendChangeToExistingEmailInternalWarningMail user newemail
           Ok <$> (runJSONGenT $ value "send" False)
         Nothing -> do
            changeemaillink <- newEmailChangeRequestLink (userid user) newemail
            mail <- mailEmailChangeRequest ctx user newemail changeemaillink
            scheduleEmailSendout (ctxmailsconfig ctx)
                        (mail{to = [MailAddress{
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
  memail <- lift $ getOptionalField asValidEmail "email"
  when (isNothing memail) $ do
    throwIO . SomeKontraException $ serverError "Email not provided or invalid"
  let email = fromJust memail
  firstname <- lift $ fromMaybe "" <$> getOptionalField asValidName "firstName"
  lastname <- lift $ fromMaybe "" <$> getOptionalField asValidName "lastName"
  lang <- lift $ fromMaybe (ctxlang ctx) <$> langFromCode <$> getField' "lang"
  lift $ switchLang lang
  muser <- dbQuery $ GetUserByEmail $ Email email
  muser' <- case muser of
               Just user ->   return $ Nothing <| (isJust (userhasacceptedtermsofservice user)) |> Just user
               Nothing ->  do
                 company <- dbUpdate $ CreateCompany
                 lift $ createUser (Email email) (firstname,lastname) (companyid company,True) lang
  case muser' of
    Nothing -> runJSONGenT $ value "sent" $ False
    Just user -> do
          lift $ sendNewUserMail user
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
          runJSONGenT $ value "sent" $ True


apiCallSendPasswordReminder :: Kontrakcja m => m Response
apiCallSendPasswordReminder = api $ do
  ctx <- getContext
  memail <- lift $ getOptionalField asValidEmail "email"
  case memail of
    Nothing -> runJSONGenT $ value "send" False >> value "badformat" True
    Just email -> do
      muser <- dbQuery $ GetUserByEmail $ Email email
      case muser of
        Nothing -> do
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
 where
  sendResetPasswordMail ctx link user = do
    mail <- resetPasswordMail ctx user link
    scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [getMailAddress user] }

apiCallAddFlash :: Kontrakcja m => m Response
apiCallAddFlash = api $  do
  color <- lift $ getField' "color"
  content <- lift $ getField' "content"
  case color of
       "red"   -> lift $ addFlashMsg (FlashMessage OperationFailed content)
       "green" -> lift $ addFlashMsg (FlashMessage OperationDone content)
       "blue"  -> lift $ addFlashMsg (FlashMessage SigningRelated content)
       _ -> return ()
  Ok <$> (runJSONGenT $ return ())

apiCallPaymentInfo :: Kontrakcja m => m Response
apiCallPaymentInfo = api $ do
  (user, _ , _) <- getAPIUser APIPersonal
  admin <- isAdmin <$> getContext
  time <- ctxtime <$> getContext

  docsusedthismonth <- lift $ dbQuery $ GetDocsSentBetween (userid user) (beginingOfMonth time) time
  mpaymentplan <- lift $ dbQuery $ GetPaymentPlan $ (usercompany user)
  quantity <- dbQuery $ GetCompanyQuantity (usercompany user)

  let paymentplan = maybe "free" (getNonTrialPlanName . ppPricePlan) mpaymentplan
      status      = maybe "active" (show . ppStatus) mpaymentplan
      dunning     = maybe False (isJust . ppDunningStep) mpaymentplan
      canceled    = Just CanceledStatus == (ppPendingStatus <$> mpaymentplan)
      billingEnds = (formatMinutesTimeRealISO . ppBillingEndDate) <$> mpaymentplan
      docTotal = case (ppStatus <$> mpaymentplan, ppPricePlan <$> mpaymentplan) of
        (Just DeactivatedStatus, _)   -> 0
        (Just CanceledStatus, _)      -> 3
        (_, Just EnterprisePricePlan) -> 5000000
        (_, Just FreePricePlan)       -> 3
        (_, Nothing)                  -> 3
        (_, _)                        -> 100
  fmap Ok $ runJSONGenT $ do
        value "adminuser" admin
        value "docsUsed"  docsusedthismonth
        value "plan"      paymentplan
        value "status"    status
        value "dunning"   dunning
        value "canceled"  canceled
        value "quantity"  quantity
        value "billingEnds" billingEnds
        value "docsTotal" (docTotal::Int)

