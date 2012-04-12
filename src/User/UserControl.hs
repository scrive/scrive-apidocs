module User.UserControl where

import Control.Monad.State
import Data.Functor
import Data.Maybe
import Happstack.Server hiding (simpleHTTP)
import Happstack.State (update, query)
import Text.JSON (JSValue(..), toJSObject, showJSON)

import ActionSchedulerState
import AppView
import Crypto.RNG
import DB.Classes hiding (update, query)
import Doc.Action
import Doc.Model
import Company.Model
import Control.Logic
import InputValidation
import Kontra
import KontraLink
import MagicHash (MagicHash)
import Mails.SendMail
import MinutesTime
import Misc
import Redirect
import Templates.Templates
import User.Model
import User.UserView
import Util.FlashUtil
import Util.HasSomeUserInfo
import qualified Log
import Util.KontraLinkUtils
import Util.MonadUtils
import Stats.Control
import User.Action
import User.Utils
import User.History.Model
import ScriveByMail.Model

handleUserGet :: Kontrakcja m => m (Either KontraLink Response)
handleUserGet = checkUserTOSGet $ do
    ctx <- getContext
    createcompany <- isFieldSet "createcompany"  --we could dump this stupid flag if we improved javascript validation
    case (ctxmaybeuser ctx) of
         Just user -> do
           mcompany <- getCompanyForUser user
           showUser user mcompany createcompany >>= renderFromBody kontrakcja
         Nothing -> sendRedirect $ LinkLogin (ctxlocale ctx) NotLogged

handleUserPost :: Kontrakcja m => m KontraLink
handleUserPost = do
  guardLoggedIn
  createcompany <- isFieldSet "createcompany"
  changeemail <- isFieldSet "changeemail"
  mlink <- case True of
             _ | createcompany -> Just <$> handleCreateCompany
             _ | changeemail -> Just <$> handleRequestChangeEmail
             _ -> return Nothing

  --whatever happens run the update in case they changed things in other places
  ctx <- getContext
  user' <- guardJust $ ctxmaybeuser ctx
  --requery for the user as they may have been upgraded
  user <- guardJustM $ dbQuery $ GetUserByID (userid user')
  infoUpdate <- getUserInfoUpdate
  _ <- dbUpdate $ SetUserInfo (userid user) (infoUpdate $ userinfo user)
  _ <- dbUpdate $ LogHistoryUserInfoChanged (userid user) (ctxipnumber ctx) (ctxtime ctx)
                                               (userinfo user) (infoUpdate $ userinfo user)
                                               (userid <$> ctxmaybeuser ctx)
  mcompany <- getCompanyForUser user
  case (useriscompanyadmin user, mcompany) of
    (True, Just company) -> do
      companyinfoupdate <- getCompanyInfoUpdate
      _ <- dbUpdate $ SetCompanyInfo (companyid company) (companyinfoupdate $ companyinfo company)
      return ()
    _ -> return ()

  case mlink of
    Just link -> return link
    Nothing -> do
       addFlashM flashMessageUserDetailsSaved
       return $ LinkAccount

-- please treat this function like a public query form, it's not secure
handleRequestPhoneCall :: Kontrakcja m => m KontraLink
handleRequestPhoneCall = do
  Context{ctxmaybeuser} <- getContext
  memail <- getOptionalField asValidEmail "email"
  mphone <-  getOptionalField asValidPhone "phone"
  case (memail, mphone) of
    (Just email, Just phone) -> do
      user <- guardJustM $ dbQuery $ GetUserByEmail (ctxmaybeuser >>= userservice) (Email email)
      --only set the phone number if they're actually logged in
      -- it is possible to request a phone call from the sign view without being logged in!
      -- this function could be called by anyone!
      when (isJust ctxmaybeuser && fmap userid ctxmaybeuser == Just (userid user)) $ do
        _ <- dbUpdate $ SetUserInfo (userid user) $ (userinfo user){ userphone = phone }
        return ()
      phoneMeRequest user phone
    _ -> return ()
  return $ LinkUpload

handleRequestChangeEmail :: Kontrakcja m => m KontraLink
handleRequestChangeEmail = do
  ctx <- getContext
  user <- guardJust $ ctxmaybeuser ctx
  mnewemail <- getRequiredField asValidEmail "newemail"
  mnewemailagain <- getRequiredField asValidEmail "newemailagain"
  case (Email <$> mnewemail, Email <$> mnewemailagain) of
    (Just newemail, Just newemailagain) | newemail == newemailagain -> do
       mexistinguser <- dbQuery $ GetUserByEmail (userservice user) newemail
       case mexistinguser of
         Just _existinguser ->
           sendChangeToExistingEmailInternalWarningMail user newemail
         Nothing ->
           sendRequestChangeEmailMail user newemail
       --so there's no info leaking show this flash either way
       addFlashM $ flashMessageChangeEmailMailSent newemail
    (Just newemail, Just newemailagain) | newemail /= newemailagain -> do
       addFlashM flashMessageMismatchedEmails
    _ -> return ()
  return $ LinkAccount

sendChangeToExistingEmailInternalWarningMail :: Kontrakcja m => User -> Email -> m ()
sendChangeToExistingEmailInternalWarningMail user newemail = do
  ctx <- getContext
  let securitymsg =
        "User " ++ getEmail user ++ " (" ++ show (userid user) ++ ")"
        ++ " has requested that their email be changed to " ++ unEmail newemail
        ++ " but this email is already used by another account."
      content =
        securitymsg
        ++ "Maybe they're trying to attempt to merge accounts and need help, "
        ++ "or maybe they're a hacker trying to figure out who is and isn't a user."
  Log.security securitymsg
  scheduleEmailSendout (ctxmailsconfig ctx) $ emptyMail {
      to = [MailAddress { fullname = "info@skrivapa.se", email = "info@skrivapa.se" }]
    , title = "Request to Change Email to Existing Account"
    , content = content
    }

sendRequestChangeEmailMail :: Kontrakcja m => User -> Email -> m ()
sendRequestChangeEmailMail user newemail = do
  ctx <- getContext
  changeemaillink <- newRequestChangeEmailLink user newemail
  mail <- mailRequestChangeEmail (ctxhostpart ctx) user newemail changeemaillink
  scheduleEmailSendout (ctxmailsconfig ctx)
                        (mail{to = [MailAddress{
                                    fullname = getFullName user
                                  , email = unEmail newemail }]})

newRequestChangeEmailLink :: (MonadIO m, CryptoRNG m) => User -> Email -> m KontraLink
newRequestChangeEmailLink user newemail = do
    action <- newRequestEmailChange user newemail
    return $ LinkChangeUserEmail (actionID action)
                                 (recToken $ actionType action)


handleCreateCompany :: Kontrakcja m => m KontraLink
handleCreateCompany = do
  ctx <- getContext
  user <- guardJust $ ctxmaybeuser ctx
  company <- dbUpdate $ CreateCompany Nothing Nothing
  mailapikey <- random
  _ <- dbUpdate $ SetCompanyMailAPIKey (companyid company) mailapikey 1000
  _ <- dbUpdate $ SetUserCompany (userid user) (Just $ companyid company)
  _ <- dbUpdate $ SetUserCompanyAdmin (userid user) True
  upgradeduser <- guardJustM $ dbQuery $ GetUserByID $ userid user
  _ <- addUserCreateCompanyStatEvent (ctxtime ctx) upgradeduser
  _ <- dbUpdate $ LogHistoryDetailsChanged (userid user) (ctxipnumber ctx) (ctxtime ctx)
                                              [("is_company_admin", "false", "true")]
                                              (Just $ userid user)
  companyinfoupdate <- getCompanyInfoUpdate -- This is redundant to standard usage - bu I want to leave it here because of consistency
  _ <- dbUpdate $ SetCompanyInfo (companyid company) (companyinfoupdate $ companyinfo company)
  addFlashM flashMessageCompanyCreated
  return LoopBack

handleGetChangeEmail :: Kontrakcja m => ActionID -> MagicHash -> m (Either KontraLink Response)
handleGetChangeEmail actionid hash = withUserGet $ do
  mnewemail <- getNewEmailFromAction actionid hash
  case mnewemail of
    Nothing -> addFlashM $ flashMessageProblemWithEmailChange
    Just newemail -> addFlashM $ modalDoYouWantToChangeEmail newemail
  Context{ctxmaybeuser = Just user} <- getContext
  mcompany <- getCompanyForUser user
  content <- showUser user mcompany False
  renderFromBody kontrakcja content

handlePostChangeEmail :: Kontrakcja m => ActionID -> MagicHash -> m KontraLink
handlePostChangeEmail actionid hash = withUserPost $ do
  mnewemail <- getNewEmailFromAction actionid hash
  Context{ctxmaybeuser = Just user, ctxipnumber, ctxtime} <- getContext
  mpassword <- getRequiredField asDirtyPassword "password"
  case mpassword of
    Nothing -> return ()
    Just password | verifyPassword (userpassword user) password -> do
      changed <- maybe (return False)
                      (dbUpdate . SetUserEmail (userservice user) (userid user))
                      mnewemail
      if changed
        then do
            _ <- dbUpdate $ LogHistoryDetailsChanged (userid user) ctxipnumber ctxtime
                                                     [("email", unEmail $ useremail $ userinfo user, unEmail $ fromJust mnewemail)]
                                                     (Just $ userid user)
            addFlashM $ flashMessageYourEmailHasChanged
        else addFlashM $ flashMessageProblemWithEmailChange
    Just _password -> do
      addFlashM $ flashMessageProblemWithPassword
  return $ LinkAccount

getNewEmailFromAction :: Kontrakcja m => ActionID -> MagicHash -> m (Maybe Email)
getNewEmailFromAction actionid hash = do
  Context{ctxmaybeuser = Just user} <- getContext
  maction <- getActionByActionID actionid
  case actionType <$> maction of
    Just (RequestEmailChange recUser recNewEmail recToken)
      | hash == recToken
        && userid user == recUser -> do
      return $ Just recNewEmail
    _ -> return Nothing

getUserInfoUpdate :: Kontrakcja m => m (UserInfo -> UserInfo)
getUserInfoUpdate  = do
    -- a lot doesn't have validation rules defined, but i put in what we do have
    mfstname          <- getValidField asValidName "fstname"
    msndname          <- getValidField asValidName "sndname"
    mpersonalnumber   <- getField "personalnumber"
    mphone            <- getField "phone"
    mcompanyposition  <- getValidField asValidPosition "companyposition"
    mcompanyname      <- getField "companyname"
    mcompanynumber    <- getField "companynumber"
    return $ \ui ->
        ui {
            userfstname = fromMaybe (userfstname ui) mfstname
          , usersndname = fromMaybe (usersndname ui) msndname
          , userpersonalnumber = fromMaybe (userpersonalnumber ui) mpersonalnumber
          , usercompanyposition = fromMaybe (usercompanyposition ui) mcompanyposition
          , userphone  = fromMaybe (userphone ui) mphone
          , usercompanyname = fromMaybe (usercompanyname ui) mcompanyname
          , usercompanynumber = fromMaybe (usercompanynumber ui) mcompanynumber
        }
    where
        getValidField = getDefaultedField ""

getCompanyInfoUpdate :: Kontrakcja m => m (CompanyInfo -> CompanyInfo)
getCompanyInfoUpdate = do
    -- a lot doesn't have validation rules defined, but i put in what we do have
  mcompanyname <- getValidField asValidCompanyName "companyname"
  mcompanynumber <- getValidField asValidCompanyNumber "companynumber"
  mcompanyaddress <- getValidField asValidAddress "companyaddress"
  mcompanyzip <- getField "companyzip"
  mcompanycity <- getField "companycity"
  mcompanycountry <- getField "companycountry"
  return $ \ci ->
      ci {
         companyname = fromMaybe (companyname ci) mcompanyname
      ,  companynumber = fromMaybe (companynumber ci) mcompanynumber
      ,  companyaddress = fromMaybe (companyaddress ci) mcompanyaddress
      ,  companyzip = fromMaybe (companyzip ci) mcompanyzip
      ,  companycity = fromMaybe (companycity ci) mcompanycity
      ,  companycountry = fromMaybe (companycountry ci) mcompanycountry
      }
  where
    getValidField = getDefaultedField ""

handleUsageStatsForUser :: Kontrakcja m => m (Either KontraLink Response)
handleUsageStatsForUser = withUserGet $ do
  Context{ctxmaybeuser = Just user} <- getContext
  showUsageStats user >>= renderFromBody kontrakcja

handleUsageStatsJSONForUserDays :: Kontrakcja m => m JSValue
handleUsageStatsJSONForUserDays = do
  Context{ctxtime, ctxmaybeuser} <- getContext
  totalS <- renderTemplate_ "statsOrgTotal"
  user <- guardJust ctxmaybeuser
  let som  = asInt $ daysBefore 30 ctxtime
      sixm = asInt $ monthsBefore 6 ctxtime
  if useriscompanyadmin user && isJust (usercompany user)
    then do
      (statsByDay, _) <- getUsageStatsForCompany (fromJust $ usercompany user) som sixm
      return $ JSObject $ toJSObject [("list", companyStatsDayToJSON totalS statsByDay),
                                      ("paging", JSObject $ toJSObject [
                                          ("pageMax",showJSON (0::Int)),
                                          ("pageCurrent", showJSON (0::Int)),
                                          ("itemMin",showJSON $ (0::Int)),
                                          ("itemMax",showJSON $ (length statsByDay) - 1),
                                          ("itemTotal",showJSON $ (length statsByDay))])]
    else do
      (statsByDay, _) <- getUsageStatsForUser (userid user) som sixm
      return $ JSObject $ toJSObject [("list", userStatsDayToJSON statsByDay),
                                      ("paging", JSObject $ toJSObject [
                                          ("pageMax",showJSON (0::Int)),
                                          ("pageCurrent", showJSON (0::Int)),
                                          ("itemMin",showJSON $ (0::Int)),
                                          ("itemMax",showJSON $ (length statsByDay) - 1),
                                          ("itemTotal",showJSON $ (length statsByDay))])]

handleUsageStatsJSONForUserMonths :: Kontrakcja m => m JSValue
handleUsageStatsJSONForUserMonths = do
  Context{ctxtime, ctxmaybeuser} <- getContext
  totalS <- renderTemplate_ "statsOrgTotal"
  user <- guardJust ctxmaybeuser
  let som  = asInt $ daysBefore 30 ctxtime
      sixm = asInt $ monthsBefore 6 ctxtime
  if useriscompanyadmin user && isJust (usercompany user)
    then do
    (_, statsByMonth) <- getUsageStatsForCompany (fromJust $ usercompany user) som sixm
    return $ JSObject $ toJSObject [("list", companyStatsMonthToJSON totalS statsByMonth),
                                    ("paging", JSObject $ toJSObject [
                                        ("pageMax",showJSON (0::Int)),
                                        ("pageCurrent", showJSON (0::Int)),
                                        ("itemMin",showJSON $ (0::Int)),
                                        ("itemMax",showJSON $ (length statsByMonth) - 1),
                                        ("itemTotal",showJSON $ (length statsByMonth))])]
    else do
    (_, statsByMonth) <- getUsageStatsForUser (userid user) som sixm
    return $ JSObject $ toJSObject [("list", userStatsMonthToJSON statsByMonth),
                                    ("paging", JSObject $ toJSObject [
                                        ("pageMax",showJSON (0::Int)),
                                        ("pageCurrent", showJSON (0::Int)),
                                        ("itemMin",showJSON $ (0::Int)),
                                        ("itemMax",showJSON $ (length statsByMonth) - 1),
                                        ("itemTotal",showJSON $ (length statsByMonth))])]


handleGetUserMailAPI :: Kontrakcja m => m (Either KontraLink Response)
handleGetUserMailAPI = withUserGet $ do
    Context{ctxmaybeuser = Just user@User{userid}} <- getContext
    mapi <- dbQuery $ GetUserMailAPI userid
    mcapi <- maybe (return Nothing) (dbQuery . GetCompanyMailAPI) $ usercompany user
    showUserMailAPI user mapi mcapi >>= renderFromBody kontrakcja

handlePostUserMailAPI :: Kontrakcja m => m KontraLink
handlePostUserMailAPI = withUserPost $ do
    User{userid} <- fromJust . ctxmaybeuser <$> getContext
    mapi <- dbQuery $ GetUserMailAPI userid
    getDefaultedField False asValidCheckBox "api_enabled"
      >>= maybe (return LinkUserMailAPI) (\enabledapi -> do
        case mapi of
             Nothing -> do
                 when enabledapi $ do
                     apikey <- random
                     _ <- dbUpdate $ SetUserMailAPIKey userid apikey 50
                     return ()
             Just api -> do
                 if not enabledapi
                    then do
                        _ <- dbUpdate $ RemoveUserMailAPI userid
                        return ()
                    else do
                        mresetkey <- getDefaultedField False asValidCheckBox "reset_key"
                        mresetsenttoday <- getDefaultedField False asValidCheckBox "reset_senttoday"
                        mdailylimit <- getRequiredField asValidNumber "daily_limit"
                        case (mresetkey, mresetsenttoday, mdailylimit) of
                             (Just resetkey, Just resetsenttoday, Just dailylimit) -> do
                                 newkey <- if resetkey
                                   then random
                                   else return $ umapiKey api
                                 _ <- dbUpdate $ SetUserMailAPIKey userid newkey dailylimit
                                 when_ resetsenttoday $ dbUpdate $ ResetUserMailAPI userid
                                 return ()
                             _ -> return ()
        return LinkUserMailAPI)

handleGetUserSecurity :: Kontrakcja m => m Response
handleGetUserSecurity = do
    ctx <- getContext
    case (ctxmaybeuser ctx) of
         Just user -> showUserSecurity user >>= renderFromBody kontrakcja
         Nothing -> sendRedirect $ LinkLogin (ctxlocale ctx) NotLogged

handlePostUserLocale :: Kontrakcja m => m KontraLink
handlePostUserLocale = do
  ctx <- getContext
  user <- guardJust $ ctxmaybeuser ctx
  mregion <- readField "region"
  _ <- dbUpdate $ SetUserSettings (userid user) $ (usersettings user) {
           locale = maybe (locale $ usersettings user) mkLocaleFromRegion mregion
         }
  referer <- getField "referer"
  case referer of
    Just _ -> return BackToReferer
    Nothing -> return LoopBack

handlePostUserSecurity :: Kontrakcja m => m KontraLink
handlePostUserSecurity = do
  ctx <- getContext
  case (ctxmaybeuser ctx) of
    Just user -> do
      moldpassword <- getOptionalField asDirtyPassword "oldpassword"
      mpassword <- getOptionalField asValidPassword "password"
      mpassword2 <- getOptionalField asDirtyPassword "password2"
      case (moldpassword, mpassword, mpassword2) of
        (Just oldpassword, Just password, Just password2) ->
          case (verifyPassword (userpassword user) oldpassword,
                  checkPasswordsMatch password password2) of
            (False,_) -> do
              _ <- dbUpdate $ LogHistoryPasswordSetupReq (userid user) (ctxipnumber ctx) (ctxtime ctx) (userid <$> ctxmaybeuser ctx)
              addFlashM flashMessageBadOldPassword
            (_, Left f) -> do
              _ <- dbUpdate $ LogHistoryPasswordSetupReq (userid user) (ctxipnumber ctx) (ctxtime ctx) (userid <$> ctxmaybeuser ctx)
              addFlashM f
            _ ->  do
              passwordhash <- createPassword password
              _ <- dbUpdate $ SetUserPassword (userid user) passwordhash
              _ <- dbUpdate $ LogHistoryPasswordSetup (userid user) (ctxipnumber ctx) (ctxtime ctx) (userid <$> ctxmaybeuser ctx)
              addFlashM flashMessageUserDetailsSaved
        _ | isJust moldpassword || isJust mpassword || isJust mpassword2 -> do
              _ <- dbUpdate $ LogHistoryPasswordSetupReq (userid user) (ctxipnumber ctx) (ctxtime ctx) (userid <$> ctxmaybeuser ctx)
              addFlashM flashMessageMissingRequiredField
        _ -> return ()
      mregion <- readField "region"
      advancedMode <- isFieldSet "advancedMode"
      footer <- getField "customfooter"
      footerCheckbox <- isFieldSet "footerCheckbox"
      _ <- dbUpdate $ SetUserSettings (userid user) $ (usersettings user) {
             locale = maybe (locale $ usersettings user) mkLocaleFromRegion mregion,
             preferreddesignmode = Just AdvancedMode  <| advancedMode |> Nothing,
             customfooter = footer <| footerCheckbox |> Nothing
           }
      return LinkAccountSecurity
    Nothing -> return $ LinkLogin (ctxlocale ctx) NotLogged

{- |
    Checks for live documents owned by the user.
-}
isUserDeletable :: Kontrakcja m => User -> m Bool
isUserDeletable user = do
  userdocs <- dbQuery $ GetDocumentsByAuthor (userid user)
  return $ all isDeletableDocument userdocs

handleViralInvite :: Kontrakcja m => m KontraLink
handleViralInvite = withUserPost $ do
  getOptionalField asValidEmail "invitedemail" >>= maybe (return ())
    (\invitedemail -> do
        ctx@Context{ctxmaybeuser = Just user} <- getContext
        muser <- dbQuery $ GetUserByEmail Nothing $ Email invitedemail
        if isJust muser
           -- we leak user information here! SECURITY!!!!
           -- you can find out if a given email is already a user
          then addFlashM flashMessageUserWithSameEmailExists
          else do
            now <- liftIO getMinutesTime
            minv <- checkValidity now <$> (query $ GetViralInvitationByEmail $ Email invitedemail)
            case minv of
              Just Action{ actionID, actionType = ViralInvitationSent{ visEmail
                                                                     , visTime
                                                                     , visInviterID
                                                                     , visRemainedEmails
                                                                     , visToken } } -> do
                if visInviterID == userid user
                  then case visRemainedEmails of
                    0 -> addFlashM flashMessageNoRemainedInvitationEmails
                    n -> do
                      _ <- update $ UpdateActionType actionID $ ViralInvitationSent { visEmail = visEmail
                                                                                    , visTime = visTime
                                                                                    , visInviterID = visInviterID
                                                                                    , visRemainedEmails = n -1
                                                                                    , visToken = visToken }
                      sendInvitation ctx (LinkViralInvitationSent actionID visToken invitedemail) invitedemail
                  else addFlashM flashMessageOtherUserSentInvitation
              _ -> do
                link <- newViralInvitationSentLink (Email invitedemail) (userid . fromJust $ ctxmaybeuser ctx)
                sendInvitation ctx link invitedemail
        )
  return LoopBack
    where
      sendInvitation ctx link invitedemail = do
        addFlashM flashMessageViralInviteSent
        mail <- viralInviteMail ctx invitedemail link
        scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [MailAddress { fullname = "", email = invitedemail }]}

--there must be a better way than all of these weird user create functions
-- TODO clean up

createUser :: Kontrakcja m => Email
                              -> String
                              -> String
                              -> Maybe Company
                              -> m (Maybe User)
createUser email fstname sndname mcompany = do
  ctx <- getContext
  passwd <- createPassword =<< randomPassword
  muser <- dbUpdate $ AddUser (fstname, sndname) (unEmail email) (Just passwd) False Nothing (fmap companyid mcompany) (ctxlocale ctx)
  case muser of
    Just user -> do
                 _ <- dbUpdate $
                      LogHistoryAccountCreated (userid user) (ctxipnumber ctx)
                                               (ctxtime ctx) email (userid <$> ctxmaybeuser ctx)
                 return muser
    _         -> return muser


sendNewUserMail :: Kontrakcja m => User -> m ()
sendNewUserMail user = do
  ctx <- getContext
  al <- newAccountCreatedLink user
  mail <- newUserMail (ctxhostpart ctx) (getEmail user) (getSmartName user) al
  scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [MailAddress { fullname = getSmartName user, email = getEmail user }]}
  return ()

createNewUserByAdmin :: Kontrakcja m => Context -> (String, String) -> String -> Maybe MinutesTime -> Maybe String -> Locale -> m (Maybe User)
createNewUserByAdmin ctx names email _freetill custommessage locale = do
    muser <- createInvitedUser names email (Just locale)
    case muser of
         Just user -> do
             let fullname = composeFullName names
             now <- liftIO $ getMinutesTime
             _ <- dbUpdate $ SetInviteInfo (userid <$> ctxmaybeuser ctx) now Admin (userid user)
             chpwdlink <- newAccountCreatedLink user
             mail <- mailNewAccountCreatedByAdmin ctx (getLocale user) fullname email chpwdlink custommessage
             scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [MailAddress { fullname = fullname, email = email }]}
             return muser
         Nothing -> return muser

handleAcceptTOSGet :: Kontrakcja m => m (Either KontraLink Response)
handleAcceptTOSGet = withUserGet $ do
    renderFromBody kontrakcja =<< pageAcceptTOS

handleAcceptTOSPost :: Kontrakcja m => m KontraLink
handleAcceptTOSPost = withUserPost $ do
  Context{ctxmaybeuser = Just User{userid}, ctxtime, ctxipnumber} <- getContext
  tos <- getDefaultedField False asValidCheckBox "tos"
  case tos of
    Just True -> do
      _ <- dbUpdate $ AcceptTermsOfService userid ctxtime
      user <- guardJustM $ dbQuery $ GetUserByID userid
      _ <- addUserSignTOSStatEvent user
      _ <- dbUpdate $ LogHistoryTOSAccept userid ctxipnumber ctxtime (Just userid)
      addFlashM flashMessageUserDetailsSaved
      return LinkUpload
    Just False -> do
      addFlashM flashMessageMustAcceptTOS
      return LinkAcceptTOS
    Nothing -> return LinkAcceptTOS

handleQuestion :: Kontrakcja m => m KontraLink
handleQuestion = do
    ctx <- getContext
    name <- getField "name"
    memail <- getDefaultedField "" asValidEmail "email"
    phone <- getField "phone"
    message <- getField "message"
    case memail of
         Nothing -> return LoopBack
         Just email -> do
             let content = "name: "    ++ fromMaybe "" name ++ "<BR/>"
                        ++ "email: "   ++ email ++ "<BR/>"
                        ++ "phone "    ++ fromMaybe "" phone ++ "<BR/>"
                        ++ "message: " ++ fromMaybe "" message
             scheduleEmailSendout (ctxmailsconfig ctx) $ emptyMail {
                   to = [MailAddress { fullname = "info@skrivapa.se", email = "info@skrivapa.se" }]
                 , title = "Question"
                 , content = content
             }
             addFlashM flashMessageThanksForTheQuestion
             return LoopBack

handleAccountSetupGet :: Kontrakcja m => ActionID -> MagicHash -> m Response
handleAccountSetupGet aid hash = do
  maction <- getActionByActionID aid
  muser <-
    case maction of
      Just action -> do
        guardMagicTokenMatch hash action
        getUserFromAction action
      Nothing -> getUserByEmail
  when (isJust muser) $ switchLocale (getLocale $ fromJust muser)
  case (maybe False (isJust . userhasacceptedtermsofservice) muser, maction) of
    (True, _) -> do
      -- seems like a security risk.  you can just feed random numbers with emails in the get param
      -- and work out who has an account or not
      sendRedirect LinkUpload
    (False, Just _action) -> do
      extendActionEvalTimeToOneDayMinimum aid
      addFlashM $ modalAccountSetup (LinkAccountCreated aid hash $ maybe "" getEmail muser)
                                    (maybe "" getFirstName muser)
                                    (maybe "" getLastName muser)
      ctx <- getContext
      sendRedirect $ LinkHome (ctxlocale ctx)
    (False, Nothing) -> do
      -- this is a very disgusting page.  i didn't even know it existed
      content <- activatePageViewNotValidLink ""
      renderFromBody kontrakcja content
  where
    -- looks up the user using the value in the optional email param
    getUserByEmail = do
      memail <- getOptionalField asValidEmail "email"
      case memail of
        Nothing -> return Nothing
        Just email -> dbQuery $ GetUserByEmail Nothing (Email email)

handleAccountSetupPost :: Kontrakcja m => ActionID -> MagicHash -> m KontraLink
handleAccountSetupPost aid hash = do
  maction <- getActionByActionID aid
  case maction of
    Just action -> do
      guardMagicTokenMatch hash action
      user <- guardJustM $ getOrCreateActionUser action
      switchLocale (getLocale user)
      if isJust $ userhasacceptedtermsofservice user
        then addFlashM flashMessageUserAlreadyActivated
        else do
          mfstname <- getRequiredField asValidName "fstname"
          msndname <- getRequiredField asValidName "sndname"
          signupmethod <- guardJust $ getSignupMethod action
          mactivateduser <- handleActivate mfstname msndname user signupmethod
          case mactivateduser of
            Just (_activateduser, docs) -> do
              dropExistingAction aid

              forM_ docs postDocumentPreparationChange

              addFlashM flashMessageUserActivated
              return ()
            Nothing -> do
              addFlashM $ modalAccountSetup (LinkAccountCreated aid hash $ getEmail user)
                                            (fromMaybe "" mfstname)
                                            (fromMaybe "" msndname)
              return ()
          return ()
      getHomeOrUploadLink
    Nothing ->
      getOptionalField asValidEmail "email" >>= maybe internalError generateActivationLink
  where
    -- If this is a user activating a viral invitation then we create their user
    -- if needed, otherwise we fetch the user indicated inside the action details.
    getOrCreateActionUser action = do
      mactionuser <- getUserFromAction action
      case (mactionuser, actionType action) of
        (Nothing, ViralInvitationSent email invtime inviterid _ _) -> do
          muser <- createInvitedUser ("", "") (unEmail email) Nothing
          case muser of
            Just user -> do -- user created, we need to fill in some info
              minviter <- dbQuery $ GetUserByID inviterid
              _ <- dbUpdate $ SetInviteInfo (userid <$> minviter) invtime Viral (userid user)
              return $ Just user
            Nothing -> return Nothing
        _ -> return mactionuser
    -- Gets the signup method for the action's type.
    getSignupMethod action =
      case actionType action of
        ViralInvitationSent _ _ _ _ _ -> Just ViralInvitation
        AccountCreated _ _ -> Just AccountRequest
        _ -> Nothing
    -- Generates another activation link
    generateActivationLink email = do
      user <- guardJustM $ dbQuery $ GetUserByEmail Nothing (Email email)
      if isNothing $ userhasacceptedtermsofservice user
        then do
          ctx <- getContext
          al <- newAccountCreatedLink user
          mail <- newUserMail (ctxhostpart ctx) email email al
          scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [MailAddress { fullname = email, email = email}] }
          addFlashM flashMessageNewActivationLinkSend
          getHomeOrUploadLink
        else internalError

 -- Retrieves the action for the given id
getActionByActionID :: Kontrakcja m => ActionID -> m (Maybe Action)
getActionByActionID actionid = do
  now <- liftIO $ getMinutesTime
  (query $ GetAction actionid) >>= maybe (return Nothing) (\action -> do
  -- action may be in state when it has expired, but hasn't yet been
  -- transformed into next form. below code checks for that.
  return $ checkValidity now $ Just action)

-- Looks up any user id on the action, and then returns the relevant user
getUserFromAction :: Kontrakcja m => Action -> m (Maybe User)
getUserFromAction action =
  case actionType action of
    (ViralInvitationSent email _ _ _ _) -> dbQuery $ GetUserByEmail Nothing email
    (AccountCreated uid _) ->  dbQuery $ GetUserByID uid
    _ -> return Nothing

-- Guards so that the token in the given action matches the given magic hash.
guardMagicTokenMatch :: Kontrakcja m => MagicHash -> Action -> m ()
guardMagicTokenMatch expectedtoken action =
  unless (getMagicTokenFromAction == Just expectedtoken) internalError
  where
    getMagicTokenFromAction =
      case actionType action of
        ViralInvitationSent _ _ _ _ token -> Just token
        AccountCreated _ token -> Just token
        _ -> Nothing

{- |
    This is where we get to when the user clicks the link in their password reminder
    email.  This'll show them the usual landing page, but with a modal dialog
    for changing their password.
-}
handlePasswordReminderGet :: Kontrakcja m => ActionID -> MagicHash -> m Response
handlePasswordReminderGet aid hash = do
    muser <- getUserFromActionOfType PasswordReminderID aid hash
    case muser of
         Just user -> do
             switchLocale (getLocale user)
             extendActionEvalTimeToOneDayMinimum aid
             addFlashM $ modalNewPasswordView aid hash
             sendRedirect LinkUpload
         Nothing -> do
             addFlashM flashMessagePasswordChangeLinkNotValid
             linkmain <- getHomeOrUploadLink
             sendRedirect linkmain

handlePasswordReminderPost :: Kontrakcja m => ActionID -> MagicHash -> m KontraLink
handlePasswordReminderPost aid hash = do
    muser <- getUserFromActionOfType PasswordReminderID aid hash
    case muser of
         Just user -> do
             switchLocale (getLocale user)
             handleChangePassword user
         Nothing   -> do
             addFlashM flashMessagePasswordChangeLinkNotValid
             getHomeOrUploadLink
    where
        handleChangePassword user = do
            Context{ctxtime, ctxipnumber, ctxmaybeuser} <- getContext
            mpassword <- getRequiredField asValidPassword "password"
            mpassword2 <- getRequiredField asDirtyPassword "password2"
            case (mpassword, mpassword2) of
                 (Just password, Just password2) -> do
                     case (checkPasswordsMatch password password2) of
                          Right () -> do
                              dropExistingAction aid
                              passwordhash <- createPassword password
                              _ <- dbUpdate $ SetUserPassword (userid user) passwordhash
                              _ <- dbUpdate $ LogHistoryPasswordSetup (userid user) (ctxipnumber) (ctxtime) (userid <$> ctxmaybeuser)
                              addFlashM flashMessageUserPasswordChanged
                              _ <- addUserLoginStatEvent ctxtime user
                              logUserToContext $ Just user
                              return LinkUpload
                          Left flash -> do
                              _ <- dbUpdate $ LogHistoryPasswordSetupReq (userid user) (ctxipnumber) (ctxtime) (userid <$> ctxmaybeuser)
                              addFlashM flash
                              addFlashM $ modalNewPasswordView aid hash
                              getHomeOrUploadLink
                 _ -> do
                   _ <- dbUpdate $ LogHistoryPasswordSetupReq (userid user) (ctxipnumber) (ctxtime) (userid <$> ctxmaybeuser)
                   addFlashM $ modalNewPasswordView aid hash
                   getHomeOrUploadLink

getUserFromActionOfType :: Kontrakcja m => ActionTypeID -> ActionID -> MagicHash -> m (Maybe User)
getUserFromActionOfType atypeid aid hash = do
    now <- liftIO $ getMinutesTime
    maction <- checkValidity now <$> query (GetAction aid)
    case maction of
         Just action -> do
             if atypeid == (actionTypeID $ actionType action)
                then getUID action >>= maybe
                         (return Nothing)
                         (dbQuery . GetUserByID)
                else return Nothing
         Nothing -> return Nothing
    where
        getUID action =
            case actionType action of
                 PasswordReminder uid _ token -> verifyToken token uid
                 AccountCreated uid token     -> verifyToken token uid
                 _                            -> return Nothing
        verifyToken token uid = return $
            if hash == token
               then Just uid
               else Nothing

-- | Postpone link removal. Needed to make sure that between
-- GET and POST requests action won't be removed from the system.
extendActionEvalTimeToOneDayMinimum :: Kontrakcja m => ActionID -> m ()
extendActionEvalTimeToOneDayMinimum aid = do
    dayAfterNow <- minutesAfter (60*24) <$> liftIO getMinutesTime
    maction <- checkValidity dayAfterNow <$> query (GetAction aid)
    when_ (isNothing maction) $
        update $ UpdateActionEvalTime aid dayAfterNow

dropExistingAction :: Kontrakcja m => ActionID -> m ()
dropExistingAction aid = do
  _ <- update $ DeleteAction aid
  return ()

{- |
   Fetch the xtoken param and double read it. Once as String and once as MagicHash.
 -}
readXToken :: Kontrakcja m => m (Either String MagicHash)
readXToken = do
  mxtoken <- join <$> (fmap maybeRead) <$> readField "xtoken"
  return $ maybe (Left $ "xtoken read failure" ) Right mxtoken

guardXToken :: Kontrakcja m => m ()
guardXToken = do
  Context { ctxxtoken } <- getContext
  xtoken <- guardRightM readXToken
  unless (xtoken == ctxxtoken) $ do
    Log.debug $ "xtoken failure: session: " ++ show ctxxtoken
      ++ " param: " ++ show xtoken
    internalError
