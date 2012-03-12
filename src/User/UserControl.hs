module User.UserControl where

import Control.Monad.State
import Data.Functor
import Data.Maybe
import Happstack.Server hiding (simpleHTTP)
import Happstack.State (update, query)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BS
import Text.JSON (JSValue(..), toJSObject, showJSON)

import ActionSchedulerState
import AppView
import Crypto.RNG (CryptoRNG, random)
import DB.Classes
import Doc.Model
import Doc.DocStateData
import Company.Model
import InputValidation
import Kontra
import KontraLink
import ListUtil
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
import Util.SignatoryLinkUtils
import qualified Log
import Util.KontraLinkUtils
import Util.MonadUtils
import Stats.Control
import User.Utils
import EvidenceLog.Model
import User.History.Model
import ScriveByMail.Model
import ScriveByMail.Control

checkPasswordsMatch :: TemplatesMonad m => BS.ByteString -> BS.ByteString -> Either (m FlashMessage) ()
checkPasswordsMatch p1 p2 =
    if p1 == p2
       then Right ()
       else Left flashMessagePasswordsDontMatch


handleUserGet :: Kontrakcja m => m (Either KontraLink Response)
handleUserGet = checkUserTOSGet $ do
    ctx <- getContext
    createcompany <- isFieldSet "createcompany"  --we could dump this stupid flag if we improved javascript validation
    case (ctxmaybeuser ctx) of
         Just user -> do
           mcompany <- getCompanyForUser user
           showUser user mcompany createcompany >>= renderFromBody TopAccount kontrakcja
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
  user <- guardJustM $ runDBQuery $ GetUserByID (userid user')
  infoUpdate <- getUserInfoUpdate
  _ <- runDBUpdate $ SetUserInfo (userid user) (infoUpdate $ userinfo user)
  _ <- runDBUpdate $ LogHistoryUserInfoChanged (userid user) (ctxipnumber ctx) (ctxtime ctx) 
                                               (userinfo user) (infoUpdate $ userinfo user)
                                               (userid <$> ctxmaybeuser ctx)
  mcompany <- getCompanyForUser user
  case (useriscompanyadmin user, mcompany) of
    (True, Just company) -> do
      companyinfoupdate <- getCompanyInfoUpdate
      _ <- runDBUpdate $ SetCompanyInfo (companyid company) (companyinfoupdate $ companyinfo company)
      return ()
    _ -> return ()

  case mlink of
    Just link -> return link
    Nothing -> do
       addFlashM flashMessageUserDetailsSaved
       return $ LinkAccount False

handleRequestChangeEmail :: Kontrakcja m => m KontraLink
handleRequestChangeEmail = do
  ctx <- getContext
  user <- guardJust $ ctxmaybeuser ctx
  mnewemail <- getRequiredField asValidEmail "newemail"
  mnewemailagain <- getRequiredField asValidEmail "newemailagain"
  case (Email <$> mnewemail, Email <$> mnewemailagain) of
    (Just newemail, Just newemailagain) | newemail == newemailagain -> do
       mexistinguser <- runDBQuery $ GetUserByEmail (userservice user) newemail
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
  return $ LinkAccount False

sendChangeToExistingEmailInternalWarningMail :: Kontrakcja m => User -> Email -> m ()
sendChangeToExistingEmailInternalWarningMail user newemail = do
  ctx <- getContext
  let securitymsg =
        "User " ++ BS.toString (getEmail user) ++ " (" ++ show (userid user) ++ ")"
        ++ " has requested that their email be changed to " ++ BS.toString (unEmail newemail)
        ++ " but this email is already used by another account."
      content =
        securitymsg
        ++ "Maybe they're trying to attempt to merge accounts and need help, "
        ++ "or maybe they're a hacker trying to figure out who is and isn't a user."
  Log.security securitymsg
  scheduleEmailSendout (ctxmailsconfig ctx) $ emptyMail {
      to = [MailAddress { fullname = BS.fromString "info@skrivapa.se", email = BS.fromString "info@skrivapa.se" }]
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
  mcompany <- getCompanyForUser user
  mcompanyname <- getRequiredField asValidCompanyName "companyname"
  case (mcompany, mcompanyname) of
    (Nothing, Just _companyname) -> do
          company <- runDBUpdate $ CreateCompany Nothing Nothing
          mailapikey <- random
          _ <- runDBUpdate $ SetCompanyMailAPIKey (companyid company) mailapikey 1000
          _ <- runDBUpdate $ SetUserCompany (userid user) (Just $ companyid company)
          _ <- runDBUpdate $ SetUserCompanyAdmin (userid user) True
          upgradeduser <- guardJustM $ runDBQuery $ GetUserByID $ userid user
          _ <- addUserCreateCompanyStatEvent (ctxtime ctx) upgradeduser
          _ <- runDBUpdate 
                   $ LogHistoryDetailsChanged (userid user) (ctxipnumber ctx) (ctxtime ctx) 
                                              [("is_company_admin", "false", "true")] 
                                              (Just $ userid user)
          addFlashM flashMessageCompanyCreated
          return $ LinkCompanyAccounts emptyListParams
    _ -> return $ LinkAccount True  --we could remove this ugly flag with more javascript validation

handleGetChangeEmail :: Kontrakcja m => ActionID -> MagicHash -> m (Either KontraLink Response)
handleGetChangeEmail actionid hash = withUserGet $ do
  mnewemail <- getNewEmailFromAction actionid hash
  case mnewemail of
    Nothing -> addFlashM $ flashMessageProblemWithEmailChange
    Just newemail -> addFlashM $ modalDoYouWantToChangeEmail newemail
  Context{ctxmaybeuser = Just user} <- getContext
  mcompany <- getCompanyForUser user
  content <- showUser user mcompany False
  renderFromBody TopAccount kontrakcja content

handlePostChangeEmail :: Kontrakcja m => ActionID -> MagicHash -> m KontraLink
handlePostChangeEmail actionid hash = withUserPost $ do
  mnewemail <- getNewEmailFromAction actionid hash
  Context{ctxmaybeuser = Just user, ctxipnumber, ctxtime} <- getContext
  mpassword <- getRequiredField asDirtyPassword "password"
  case mpassword of
    Nothing -> return ()
    Just password | verifyPassword (userpassword user) password -> do
      changed <- maybe (return False)
                      (runDBUpdate . SetUserEmail (userservice user) (userid user))
                      mnewemail
      if changed
        then do
            _ <- runDBUpdate $ LogHistoryDetailsChanged (userid user) ctxipnumber ctxtime 
                                                     [("email", BS.unpack $ unEmail $ useremail $ userinfo user, BS.unpack $ unEmail $ fromJust mnewemail)] 
                                                     (Just $ userid user)
            addFlashM $ flashMessageYourEmailHasChanged
        else addFlashM $ flashMessageProblemWithEmailChange
    Just _password -> do
      addFlashM $ flashMessageProblemWithPassword
  return $ LinkAccount False

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
    mpersonalnumber   <- getFieldUTF "personalnumber"
    mphone            <- getFieldUTF "phone"
    mcompanyposition  <- getValidField asValidPosition "companyposition"
    return $ \ui ->
        ui {
            userfstname = fromMaybe (userfstname ui) mfstname
          , usersndname = fromMaybe (usersndname ui) msndname
          , userpersonalnumber = fromMaybe (userpersonalnumber ui) mpersonalnumber
          , usercompanyposition = fromMaybe (usercompanyposition ui) mcompanyposition
          , userphone  = fromMaybe (userphone ui) mphone
        }
    where
        getValidField = getDefaultedField BS.empty

getCompanyInfoUpdate :: Kontrakcja m => m (CompanyInfo -> CompanyInfo)
getCompanyInfoUpdate = do
    -- a lot doesn't have validation rules defined, but i put in what we do have
  mcompanyname <- getValidField asValidCompanyName "companyname"
  mcompanynumber <- getValidField asValidCompanyNumber "companynumber"
  mcompanyaddress <- getValidField asValidAddress "companyaddress"
  mcompanyzip <- getFieldUTF "companyzip"
  mcompanycity <- getFieldUTF "companycity"
  mcompanycountry <- getFieldUTF "companycountry"
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
    getValidField = getDefaultedField BS.empty

handleUsageStatsForUser :: Kontrakcja m => m (Either KontraLink Response)
handleUsageStatsForUser = withUserGet $ do
  Context{ctxmaybeuser = Just user} <- getContext
  showUsageStats user >>= renderFromBody TopAccount kontrakcja

handleUsageStatsJSONForUserDays :: Kontrakcja m => m JSValue
handleUsageStatsJSONForUserDays = do
  Context{ctxtime, ctxmaybeuser, ctxtemplates } <- getContext
  totalS <- renderTemplate ctxtemplates "statsOrgTotal" ()
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
  Context{ctxtime, ctxmaybeuser, ctxtemplates } <- getContext
  totalS <- renderTemplate ctxtemplates "statsOrgTotal" ()
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
    mapi <- runDBQuery $ GetUserMailAPI userid
    mcapi <- maybe (return Nothing) (runDBQuery . GetCompanyMailAPI) $ usercompany user
    showUserMailAPI user mapi mcapi >>= renderFromBody TopAccount kontrakcja

handlePostUserMailAPI :: Kontrakcja m => m KontraLink
handlePostUserMailAPI = withUserPost $ do
    User{userid} <- fromJust . ctxmaybeuser <$> getContext
    mapi <- runDBQuery $ GetUserMailAPI userid
    getDefaultedField False asValidCheckBox "api_enabled"
      >>= maybe (return LinkUserMailAPI) (\enabledapi -> do
        case mapi of
             Nothing -> do
                 when enabledapi $ do
                     apikey <- random
                     _ <- runDBUpdate $ SetUserMailAPIKey userid apikey 50
                     return ()
             Just api -> do
                 if not enabledapi
                    then do
                        _ <- runDBUpdate $ RemoveUserMailAPI userid
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
                                 _ <- runDBUpdate $ SetUserMailAPIKey userid newkey dailylimit
                                 when_ resetsenttoday $ runDBUpdate $ ResetUserMailAPI userid
                                 return ()
                             _ -> return ()
        return LinkUserMailAPI)

handleGetUserSecurity :: Kontrakcja m => m Response
handleGetUserSecurity = do
    ctx <- getContext
    case (ctxmaybeuser ctx) of
         Just user -> showUserSecurity user >>= renderFromBody TopAccount kontrakcja
         Nothing -> sendRedirect $ LinkLogin (ctxlocale ctx) NotLogged

handlePostUserLocale :: Kontrakcja m => m KontraLink
handlePostUserLocale = do
  ctx <- getContext
  user <- guardJust $ ctxmaybeuser ctx
  mregion <- readField "region"
  _ <- runDBUpdate $ SetUserSettings (userid user) $ (usersettings user) {
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
              _ <- runDBUpdate $ LogHistoryPasswordSetupReq (userid user) (ctxipnumber ctx) (ctxtime ctx) (userid <$> ctxmaybeuser ctx)
              addFlashM flashMessageBadOldPassword
            (_, Left f) -> do
              _ <- runDBUpdate $ LogHistoryPasswordSetupReq (userid user) (ctxipnumber ctx) (ctxtime ctx) (userid <$> ctxmaybeuser ctx)
              addFlashM f
            _ ->  do
              passwordhash <- createPassword password
              _ <- runDBUpdate $ SetUserPassword (userid user) passwordhash
              _ <- runDBUpdate $ LogHistoryPasswordSetup (userid user) (ctxipnumber ctx) (ctxtime ctx) (userid <$> ctxmaybeuser ctx)
              addFlashM flashMessageUserDetailsSaved
        _ | isJust moldpassword || isJust mpassword || isJust mpassword2 -> do
              _ <- runDBUpdate $ LogHistoryPasswordSetupReq (userid user) (ctxipnumber ctx) (ctxtime ctx) (userid <$> ctxmaybeuser ctx)
              addFlashM flashMessageMissingRequiredField
        _ -> return ()
      mregion <- readField "region"
      advancedMode <- isFieldSet "advancedMode"
      footer <- getField "customfooter"
      footerCheckbox <- isFieldSet "footerCheckbox"
      _ <- runDBUpdate $ SetUserSettings (userid user) $ (usersettings user) {
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
  userdocs <- runDBQuery $ GetDocumentsByAuthor (userid user)
  return $ all isDeletableDocument userdocs

handleViralInvite :: Kontrakcja m => m KontraLink
handleViralInvite = withUserPost $ do
  getOptionalField asValidEmail "invitedemail" >>= maybe (return ())
    (\invitedemail -> do
        ctx@Context{ctxmaybeuser = Just user} <- getContext
        muser <- runDBQuery $ GetUserByEmail Nothing $ Email invitedemail
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
                      sendInvitation ctx (LinkViralInvitationSent actionID visToken (BS.toString invitedemail)) invitedemail
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
        scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [MailAddress { fullname = BS.empty, email = invitedemail }]}

randomPassword :: (MonadIO m, CryptoRNG m) => m BS.ByteString
randomPassword =
    BS.fromString `liftM` randomString 8 (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'])

--there must be a better way than all of these weird user create functions
-- TODO clean up

createUser :: Kontrakcja m => Email
                              -> BS.ByteString
                              -> BS.ByteString
                              -> Maybe Company
                              -> m (Maybe User)
createUser email fstname sndname mcompany = do
  ctx <- getContext
  passwd <- createPassword =<< randomPassword
  muser <- runDBUpdate $ AddUser (fstname, sndname) (unEmail $ email) (Just passwd) False Nothing (fmap companyid mcompany) (ctxlocale ctx)
  case muser of
    Just user -> do 
                 _ <- runDBUpdate $ 
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

createNewUserByAdmin :: Kontrakcja m => Context -> (BS.ByteString, BS.ByteString) -> BS.ByteString -> Maybe MinutesTime -> Maybe String -> Locale -> m (Maybe User)
createNewUserByAdmin ctx names email _freetill custommessage locale = do
    muser <- createInvitedUser names email (Just locale)
    case muser of
         Just user -> do
             let fullname = composeFullName names
             now <- liftIO $ getMinutesTime
             _ <- runDBUpdate $ SetInviteInfo (userid <$> ctxmaybeuser ctx) now Admin (userid user)
             chpwdlink <- newAccountCreatedLink user
             mail <- mailNewAccountCreatedByAdmin ctx (getLocale user) fullname email chpwdlink custommessage
             scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [MailAddress { fullname = fullname, email = email }]}
             return muser
         Nothing -> return muser

createInvitedUser :: Kontrakcja m => (BS.ByteString, BS.ByteString) -> BS.ByteString -> Maybe Locale -> m (Maybe User)
createInvitedUser names email mlocale = do
    ctx <- getContext
    let locale = fromMaybe (ctxlocale ctx) mlocale
    passwd <- createPassword =<< randomPassword
    muser <- runDBUpdate $ AddUser names email (Just passwd) False Nothing Nothing locale
    case muser of
      Just user -> do 
                   _ <- runDBUpdate $ LogHistoryAccountCreated (userid user) (ctxipnumber ctx) (ctxtime ctx) (Email email) (userid <$> ctxmaybeuser ctx)
                   return muser
      _         -> return muser

{- |
   Guard against a POST with no logged in user.
   If they are not logged in, redirect to login page.
-}
withUserPost :: Kontrakcja m => m KontraLink -> m KontraLink
withUserPost action = do
    ctx <- getContext
    case ctxmaybeuser ctx of
         Just _  -> action
         Nothing -> return $ LinkLogin (ctxlocale ctx) NotLogged

{- |
   Guard against a GET with no logged in user.
   If they are not logged in, redirect to login page.
-}
withUserGet :: Kontrakcja m => m a -> m (Either KontraLink a)
withUserGet action = do
  ctx <- getContext
  case ctxmaybeuser ctx of
    Just _  -> Right <$> action
    Nothing -> return $ Left $ LinkLogin (ctxlocale ctx) NotLogged

{- |
     Takes a document and a action
     Runs an action only if current user (from context) is author of document
| -}
withDocumentAuthor :: Kontrakcja m => Document -> m a -> m a
withDocumentAuthor document action = do
  ctx <- getContext
  user <- guardJust $ ctxmaybeuser ctx
  sl <- guardJust $ getAuthorSigLink document
  guard $ isSigLinkFor user sl
  action

{- |
   Guard against a GET with logged in users who have not signed the TOS agreement.
   If they have not, redirect to their account page.
-}
checkUserTOSGet :: Kontrakcja m => m a -> m (Either KontraLink a)
checkUserTOSGet action = do
    ctx <- getContext
    case ctxmaybeuser ctx of
        Just (User{userhasacceptedtermsofservice = Just _}) -> Right <$> action
        Just _ -> return $ Left $ LinkAcceptTOS
        Nothing -> case (ctxcompany ctx) of
             Just _company -> Right <$> action
             Nothing -> return $ Left $ LinkLogin (ctxlocale ctx) NotLogged



handleAcceptTOSGet :: Kontrakcja m => m (Either KontraLink Response)
handleAcceptTOSGet = withUserGet $ do
    renderFromBody TopNone kontrakcja =<< pageAcceptTOS

handleAcceptTOSPost :: Kontrakcja m => m KontraLink
handleAcceptTOSPost = withUserPost $ do
  Context{ctxmaybeuser = Just User{userid}, ctxtime, ctxipnumber} <- getContext
  tos <- getDefaultedField False asValidCheckBox "tos"
  case tos of
    Just True -> do
      _ <- runDBUpdate $ AcceptTermsOfService userid ctxtime
      user <- guardJustM $ runDBQuery $ GetUserByID userid
      _ <- addUserSignTOSStatEvent user
      _ <- runDBUpdate $ LogHistoryTOSAccept userid ctxipnumber ctxtime (Just userid)
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
    memail <- getDefaultedField BS.empty asValidEmail "email"
    phone <- getField "phone"
    message <- getField "message"
    case memail of
         Nothing -> return LoopBack
         Just email -> do
             let content = "name: "    ++ fromMaybe "" name ++ "<BR/>"
                        ++ "email: "   ++ BS.toString email ++ "<BR/>"
                        ++ "phone "    ++ fromMaybe "" phone ++ "<BR/>"
                        ++ "message: " ++ fromMaybe "" message
             scheduleEmailSendout (ctxmailsconfig ctx) $ emptyMail {
                   to = [MailAddress { fullname = BS.fromString "info@skrivapa.se", email = BS.fromString "info@skrivapa.se" }]
                 , title = "Question"
                 , content = content
             }
             addFlashM flashMessageThanksForTheQuestion
             return LoopBack

phoneMeRequest :: Kontrakcja m => User -> m ()
phoneMeRequest user = do
  ctx <- getContext
  let content = "<p>User " ++ (BS.toString $ getFirstName user) ++ " "
                    ++ (BS.toString $ getLastName user) ++ " "
                    ++ "&lt;" ++ (BS.toString $ getEmail user) ++ "&gt; "
                    ++ "has requested a call on "
                    ++ "&lt;" ++ (BS.toString $ userphone $ userinfo $ user ) ++ "&gt;.  "
                    ++ "They have just signed the TOS, "
                    ++ "and they're setup with lang "
                    ++ "&lt;" ++ (codeFromLang $ getLang user) ++ "&gt;.</p>"
  scheduleEmailSendout (ctxmailsconfig ctx) $ emptyMail {
            to = [MailAddress { fullname = BS.fromString "info@skrivapa.se", email = BS.fromString "info@skrivapa.se" }]
          , title = "Phone Call Request"
          , content = content
      }
  _ <- addUserPhoneAfterTOS user
  return ()

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
      addFlashM $ modalAccountSetup (LinkAccountCreated aid hash $ maybe "" (BS.toString . getEmail) muser)
                                    (maybe "" (BS.toString . getFirstName) muser)
                                    (maybe "" (BS.toString . getLastName) muser)
      ctx <- getContext
      sendRedirect $ LinkHome (ctxlocale ctx)
    (False, Nothing) -> do
      -- this is a very disgusting page.  i didn't even know it existed
      content <- activatePageViewNotValidLink ""
      renderFromBody TopNone kontrakcja content
  where
    -- looks up the user using the value in the optional email param
    getUserByEmail = do
      memail <- getOptionalField asValidEmail "email"
      case memail of
        Nothing -> return Nothing
        Just email -> runDBQuery $ GetUserByEmail Nothing (Email email)

handleAccountSetupFromSign :: Kontrakcja m => Document -> SignatoryLink -> m (Maybe User)
handleAccountSetupFromSign document signatorylink = do
  ctx <- getContext
  let firstname = getFirstName signatorylink
      lastname = getLastName signatorylink
      email = getEmail signatorylink
  muser <- runDBQuery $ GetUserByEmail (currentServiceID ctx) (Email email)
  user <- maybe (guardJustM $ createInvitedUser (firstname, lastname) email Nothing)
                return
                muser
  mactivateduser <- handleActivate (Just $ firstname) (Just $ lastname) user BySigning
  when (isJust mactivateduser) $ do
    activateduser <- guardJust mactivateduser
    let actor = SignatoryActor (ctxtime ctx) (ctxipnumber ctx)  (maybesignatory signatorylink)  (BS.toString $ getEmail $ signatorylink) (signatorylinkid signatorylink)
    _ <- runDBUpdate $ SaveDocumentForUser (documentid document) activateduser (signatorylinkid signatorylink) actor
    _ <- addUserSaveAfterSignStatEvent activateduser
    return ()
  return mactivateduser

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
            Just _activateduser -> do
              dropExistingAction aid
              addFlashM flashMessageUserActivated
              return ()
            Nothing -> do
              addFlashM $ modalAccountSetup (LinkAccountCreated aid hash $ BS.toString $ getEmail user)
                                            (maybe "" BS.toString mfstname)
                                            (maybe "" BS.toString msndname)
              return ()
          return ()
      getHomeOrUploadLink
    Nothing ->
      getOptionalField asValidEmail "email" >>= maybe mzero generateActivationLink
  where
    -- If this is a user activating a viral invitation then we create their user
    -- if needed, otherwise we fetch the user indicated inside the action details.
    getOrCreateActionUser action = do
      mactionuser <- getUserFromAction action
      case (mactionuser, actionType action) of
        (Nothing, ViralInvitationSent email invtime inviterid _ _) -> do
          muser <- createInvitedUser (BS.empty, BS.empty) (unEmail email) Nothing
          case muser of
            Just user -> do -- user created, we need to fill in some info
              minviter <- runDBQuery $ GetUserByID inviterid
              _ <- runDBUpdate $ SetInviteInfo (userid <$> minviter) invtime Viral (userid user)
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
      user <- guardJustM $ runDBQuery $ GetUserByEmail Nothing (Email email)
      if isNothing $ userhasacceptedtermsofservice user
        then do
          ctx <- getContext
          al <- newAccountCreatedLink user
          mail <- newUserMail (ctxhostpart ctx) email email al
          scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [MailAddress { fullname = email, email = email}] }
          addFlashM flashMessageNewActivationLinkSend
          getHomeOrUploadLink
        else mzero

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
    (ViralInvitationSent email _ _ _ _) -> runDBQuery $ GetUserByEmail Nothing email
    (AccountCreated uid _) ->  runDBQuery $ GetUserByID uid
    _ -> return Nothing

-- Guards so that the token in the given action matches the given magic hash.
guardMagicTokenMatch :: Kontrakcja m => MagicHash -> Action -> m ()
guardMagicTokenMatch expectedtoken action =
  if getMagicTokenFromAction == Just expectedtoken
    then return ()
    else mzero
  where
    getMagicTokenFromAction =
      case actionType action of
        ViralInvitationSent _ _ _ _ token -> Just token
        AccountCreated _ token -> Just token
        _ -> Nothing

handleActivate :: Kontrakcja m => Maybe BS.ByteString -> Maybe BS.ByteString -> User -> SignupMethod -> m (Maybe User)
handleActivate mfstname msndname actvuser signupmethod = do
  Log.debug $ "Attempting to activate account for user " ++ (show $ getEmail actvuser)
  guard (isNothing $ userhasacceptedtermsofservice actvuser)
  switchLocale (getLocale actvuser)
  ctx <- getContext
  mtos <- getDefaultedField False asValidCheckBox "tos"
  callme <- isFieldSet "callme"      
  phone <-  BS.fromString <$> fromMaybe "" <$> getField "phone"
  mpassword <- getRequiredField asValidPassword "password"
  mpassword2 <- getRequiredField asValidPassword "password2"
  case (mtos, mfstname, msndname, mpassword, mpassword2) of
    (Just tos, Just fstname, Just sndname, Just password, Just password2) -> do
      case checkPasswordsMatch password password2 of
        Right () ->
          if tos
            then do
              passwordhash <- createPassword password
              runDB $ do
                _ <- dbUpdate $ SetUserInfo (userid actvuser) $ (userinfo actvuser){
                         userfstname = fstname
                       , usersndname = sndname
                       , userphone = phone
                       }
                _ <- dbUpdate $ LogHistoryUserInfoChanged (userid actvuser) (ctxipnumber ctx) (ctxtime ctx) 
                                                          (userinfo actvuser) ((userinfo actvuser){ userfstname = fstname , usersndname = sndname })
                                                          (userid <$> ctxmaybeuser ctx)
                _ <- dbUpdate $ SetUserPassword (userid actvuser) passwordhash
                _ <- dbUpdate $ LogHistoryPasswordSetup (userid actvuser) (ctxipnumber ctx) (ctxtime ctx) (userid <$> ctxmaybeuser ctx)
                _ <- dbUpdate $ AcceptTermsOfService (userid actvuser) (ctxtime ctx)
                _ <- dbUpdate $ LogHistoryTOSAccept (userid actvuser) (ctxipnumber ctx) (ctxtime ctx) (userid <$> ctxmaybeuser ctx)
                _ <- dbUpdate $ SetSignupMethod (userid actvuser) signupmethod
                delays <- dbQuery $ GetMailAPIDelaysForEmail (BS.toString $ getEmail actvuser) (ctxtime ctx)
                forM_ delays $ \(delayid, text) -> do
                  _ <- MailAPI.doMailAPI text
                  dbUpdate $ DeleteMailAPIDelay delayid (ctxtime ctx)
                return ()
              tosuser <- guardJustM $ runDBQuery $ GetUserByID (userid actvuser)
              _ <- addUserSignTOSStatEvent tosuser
              _ <- addUserLoginStatEvent (ctxtime ctx) tosuser
              logUserToContext $ Just tosuser
              when (callme) $ phoneMeRequest tosuser
              return $ Just tosuser
            else do
              addFlashM flashMessageMustAcceptTOS
              return Nothing
        Left flash -> do
          addFlashM flash
          return Nothing
    _ -> return Nothing

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
                              _ <- runDBUpdate $ SetUserPassword (userid user) passwordhash
                              _ <- runDBUpdate $ LogHistoryPasswordSetup (userid user) (ctxipnumber) (ctxtime) (userid <$> ctxmaybeuser)
                              addFlashM flashMessageUserPasswordChanged
                              _ <- addUserLoginStatEvent ctxtime user
                              logUserToContext $ Just user
                              return LinkUpload
                          Left flash -> do
                              _ <- runDBUpdate $ LogHistoryPasswordSetupReq (userid user) (ctxipnumber) (ctxtime) (userid <$> ctxmaybeuser)
                              addFlashM flash
                              addFlashM $ modalNewPasswordView aid hash
                              getHomeOrUploadLink
                 _ -> do
                   _ <- runDBUpdate $ LogHistoryPasswordSetupReq (userid user) (ctxipnumber) (ctxtime) (userid <$> ctxmaybeuser)
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
                         (runDBQuery . GetUserByID)
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
    mzero
