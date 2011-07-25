module User.UserControl where

import Control.Monad.State
import Data.Char
import Data.Either (lefts, rights)
import Data.Functor
import Data.List
import Data.Maybe
import Happstack.Server hiding (simpleHTTP)
import Happstack.State (update, query)
import Happstack.Util.Common (readM)
import System.Random
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BS

import ActionSchedulerState
import AppView
import Doc.DocState
import Company.CompanyState
import InputValidation
import Kontra
import KontraLink
import ListUtil
import Mails.SendMail
import MinutesTime
import Misc
import Payments.PaymentsState
import Redirect
import Templates.Templates
import User.UserView
import Util.FlashUtil
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import qualified AppLogger as Log
import Util.MonadUtils

checkPasswordsMatch :: TemplatesMonad m => BS.ByteString -> BS.ByteString -> Either (m FlashMessage) ()
checkPasswordsMatch p1 p2 =
    if p1 == p2
       then Right ()
       else Left flashMessagePasswordsDontMatch

--TODO EM put somewhere better
getCompanyForUser :: Kontrakcja m => Maybe User -> m (Maybe Company)
getCompanyForUser muser =
  case muser >>= usercompany of
    Just companyid -> query $ GetCompany companyid
    _ -> return Nothing

handleUserGet :: Kontrakcja m => m Response
handleUserGet = do
    ctx <- getContext
    case (ctxmaybeuser ctx) of
         Just user -> do
           mcompany <- getCompanyForUser $ Just user
           showUser user mcompany >>= renderFromBody TopAccount kontrakcja
         Nothing -> sendRedirect $ LinkLogin NotLogged

handleUserPost :: Kontrakcja m => m KontraLink
handleUserPost = do
    ctx <- getContext
    case (ctxmaybeuser ctx) of
         Just user -> do
             infoUpdate <- getUserInfoUpdate
             _ <- update $ SetUserInfo (userid user) (infoUpdate $ userinfo user)
             mcompany <- getCompanyForUser $ Just user
             case (useriscompanyadmin user, mcompany) of
               (True, Just company) -> do
                 companyinfoupdate <- getCompanyInfoUpdate
                 _ <- update $ SetCompanyInfo company (companyinfoupdate $ companyinfo company)
                 return ()
               _ -> return ()
             addFlashM flashMessageUserDetailsSaved
             return LinkAccount
         Nothing -> return $ LinkLogin NotLogged

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
  mcompanyname <- getValidField asValidCompanyName "companyname" --TODO EM add more here
  return $ \ci ->
      ci {
         companyname = fromMaybe (companyname ci) mcompanyname
      }
  where
    getValidField = getDefaultedField BS.empty

handleGetUserMailAPI :: Kontrakcja m => m (Either KontraLink Response)
handleGetUserMailAPI = withUserGet $ do
    Context{ctxmaybeuser = Just olduser@User{userid, usermailapi}} <- getContext
    user <- case usermailapi of
         Nothing -> return olduser
         Just mailapi -> do
             today <- asInt <$> liftIO getMinutesTime
             if today /= umapiLastSentDate mailapi
                then do
                    Right user <- liftIO $ update $ SetUserMailAPI userid $ Just mailapi {
                          umapiLastSentDate = today
                        , umapiSentToday = 0
                    }
                    return user
                else return olduser
    showUserMailAPI user >>= renderFromBody TopAccount kontrakcja

handlePostUserMailAPI :: Kontrakcja m => m KontraLink
handlePostUserMailAPI = withUserPost $ do
    User{userid, usermailapi} <- fromJust . ctxmaybeuser <$> getContext
    getDefaultedField False asValidCheckBox "api_enabled"
      >>= maybe (return LinkUserMailAPI) (\enabledapi -> do
        case usermailapi of
             Nothing -> do
                 when enabledapi $ do
                     apikey <- liftIO randomIO
                     today <- asInt <$> liftIO getMinutesTime
                     _ <- liftIO $ update $ SetUserMailAPI userid $ Just UserMailAPI {
                           umapiKey = apikey
                         , umapiDailyLimit = 50
                         , umapiSentToday = 0
                         , umapiLastSentDate = today
                     }
                     return ()
             Just mailapi -> do
                 if not enabledapi
                    then do
                        _ <- liftIO $ update $ SetUserMailAPI userid Nothing
                        return ()
                    else do
                        mresetkey <- getDefaultedField False asValidCheckBox "reset_key"
                        mresetsenttoday <- getDefaultedField False asValidCheckBox "reset_senttoday"
                        mdailylimit <- getRequiredField asValidNumber "daily_limit"
                        case (mresetkey, mresetsenttoday, mdailylimit) of
                             (Just resetkey, Just resetsenttoday, Just dailylimit) -> do
                                 newkey <- if resetkey
                                   then liftIO randomIO
                                   else return $ umapiKey mailapi
                                 _ <- liftIO $ update $ SetUserMailAPI userid $ Just mailapi {
                                       umapiKey = newkey
                                     , umapiDailyLimit = max 1 dailylimit
                                     , umapiSentToday = if resetsenttoday
                                                           then 0
                                                           else umapiSentToday mailapi
                                 }
                                 return ()
                             _ -> return ()
        return LinkUserMailAPI)

handleGetUserSecurity :: Kontrakcja m => m Response
handleGetUserSecurity = do
    ctx <- getContext
    case (ctxmaybeuser ctx) of
         Just user -> showUserSecurity user >>= renderFromBody TopAccount kontrakcja
         Nothing -> sendRedirect $ LinkLogin NotLogged

handlePostUserSecurity :: Kontrakcja m => m KontraLink
handlePostUserSecurity = do
  ctx <- getContext
  case (ctxmaybeuser ctx) of
    Just user -> do
      moldpassword <- getRequiredField asDirtyPassword "oldpassword"
      mpassword <- getRequiredField asValidPassword "password"
      mpassword2 <- getRequiredField asDirtyPassword "password2"
      case (moldpassword, mpassword, mpassword2) of
        (Just oldpassword, Just password, Just password2) ->
          case (verifyPassword (userpassword user) oldpassword,
                  checkPasswordsMatch password password2) of
            (False,_) ->
              addFlashM flashMessageBadOldPassword
            (_, Left f) ->
              addFlashM f
            _ ->  do
              passwordhash <- liftIO $ createPassword password
              _ <- update $ SetUserPassword (userid user) passwordhash
              addFlashM flashMessageUserDetailsSaved
        _ -> return ()
      mlang <- readField "lang"
      case (mlang) of
          Just lang -> do
              _ <-update $ SetUserSettings (userid user) $ (usersettings user) {lang=lang}
              return ()
          Nothing -> return ()
      return LinkSecurity
    Nothing -> return $ LinkLogin NotLogged

handleGetSharing :: Kontrakcja m => m (Either KontraLink Response)
handleGetSharing = withUserGet $ do
    Context{ctxmaybeuser = Just user@User{userid}} <- getContext
    friends <- query $ GetUserFriends userid
    params <- getListParams
    viewFriends (friendsSortSearchPage params friends) user
        >>= renderFromBody TopAccount kontrakcja

-- Searching, sorting and paging
friendsSortSearchPage :: ListParams -> [User] -> PagedList User
friendsSortSearchPage  =
    listSortSearchPage companyAccountsSortFunc companyAccountsSearchFunc companyAccountsPageSize

handleGetCompanyAccounts :: Kontrakcja m => m (Either KontraLink Response)
handleGetCompanyAccounts = withUserGet $ do
    Context{ctxmaybeuser = Just user} <- getContext
    companyaccounts <- query $ GetCompanyAccounts (userid user)
    params <- getListParams
    content <- viewCompanyAccounts user (companyAccountsSortSearchPage params $ companyaccounts)
    renderFromBody TopAccount kontrakcja content

-- Searching, sorting and paging
companyAccountsSortSearchPage :: ListParams -> [User] -> PagedList User
companyAccountsSortSearchPage  =
    listSortSearchPage companyAccountsSortFunc companyAccountsSearchFunc companyAccountsPageSize

companyAccountsSearchFunc :: SearchingFunction User
companyAccountsSearchFunc s user = userMatch user s -- split s so we support spaces
    where
        match s' m = isInfixOf (map toUpper s') (map toUpper (BS.toString m))
        userMatch u s' = match s' (usercompanyposition $ userinfo u)
                      || match s' (getFirstName u)
                      || match s' (getLastName  u)
                      || match s' (getPersonalNumber u)
                      || match s' (getEmail u)

companyAccountsSortFunc :: SortingFunction User
companyAccountsSortFunc "fstname" = viewComparing getFirstName
companyAccountsSortFunc "fstnameREV" = viewComparingRev getFirstName
companyAccountsSortFunc "sndname" = viewComparing getLastName
companyAccountsSortFunc "sndnameREV" = viewComparingRev getLastName
companyAccountsSortFunc "companyposition" = viewComparing $ usercompanyposition . userinfo
companyAccountsSortFunc "companypositionREV" = viewComparingRev $ usercompanyposition . userinfo
companyAccountsSortFunc "phone" = viewComparing $  userphone . userinfo
companyAccountsSortFunc "phoneREV" = viewComparingRev $ userphone . userinfo
companyAccountsSortFunc "email" = viewComparing getEmail
companyAccountsSortFunc "emailREV" = viewComparingRev getEmail
companyAccountsSortFunc _ = const $ const EQ

companyAccountsPageSize :: Int
companyAccountsPageSize = 20

----

handlePostSharing :: Kontrakcja m => m KontraLink
handlePostSharing = do
    ctx <- getContext
    case ctxmaybeuser ctx of
         Just user -> do
             memail <- getOptionalField asValidEmail "email"
             remove <- isFieldSet "remove"
             case (memail,remove) of
                  (Just email,_) -> do
                      handleAddFriend user email
                      return $ LinkSharing emptyListParams
                  (_,True) -> return $ LinkSharing emptyListParams
                  _ -> LinkSharing <$> getListParamsForSearch
         Nothing -> return $ LinkLogin NotLogged

handleAddFriend :: Kontrakcja m => User -> BS.ByteString -> m ()
handleAddFriend User{userid} email = do
    avereturn <- update $ AddViewerByEmail userid $ Email email
    case avereturn of
      Left msg -> addFlash (OperationFailed, msg)
      Right _  -> return ()

handlePostCompanyAccounts :: Kontrakcja m => m KontraLink
handlePostCompanyAccounts = do
    ctx <- getContext
    case (ctxmaybeuser ctx) of
         Just user -> do
             add <- isFieldSet "add"
             remove <- isFieldSet "remove"
             takeover <- isFieldSet "takeover"
             case (add,remove,takeover) of
                  (True, _, _) -> do
                    handleCreateCompanyUser user
                    return $ LinkCompanyAccounts emptyListParams
                  (_, True, _) -> handleDeleteCompanyUser user
                  (_, _, True) -> do
                      memail <- getOptionalField asValidEmail "email"
                      handleTakeOverUserForCompany (fromJust memail)
                      return $ LinkCompanyAccounts emptyListParams
                  _ -> LinkCompanyAccounts <$> getListParamsForSearch
         Nothing -> return $ LinkLogin NotLogged

handleDeleteCompanyUser :: Kontrakcja m => User -> m KontraLink
handleDeleteCompanyUser user = do
  companyaccountsids <- getCriticalFieldList asValidNumber "subcheck"
  handleUserDelete user (map UserID companyaccountsids)
  return $ LinkCompanyAccounts emptyListParams

{- |
    Deletes a list of users one at a time.  It first checks the permissions
    for all users, and if it's okay for all it deletes them all.  Otherwise
    it deletes no-one.
-}
handleUserDelete :: Kontrakcja m => User -> [UserID] -> m ()
handleUserDelete deleter deleteeids = do
  mcompanyaccountss <- mapM (getUserDeletionDetails (userid deleter)) deleteeids
  case lefts mcompanyaccountss of
    (NoDeletionRights:_) -> mzero
    (UserHasLiveDocs:_) -> do
      addFlashM flashMessageUserHasLiveDocs
      return ()
    [] -> do
      mapM_ performUserDeletion (rights mcompanyaccountss)
      addFlashM flashMessageAccountsDeleted
      return ()

type UserDeletionDetails = (User, [Document])

data UserDeletionProblem = NoDeletionRights | UserHasLiveDocs

getUserDeletionDetails :: Kontrakcja m => UserID -> UserID -> m (Either UserDeletionProblem UserDeletionDetails)
getUserDeletionDetails deleterid deleteeid = do
  deleter <- queryOrFail $ GetUserByUserID deleterid
  deletee <- queryOrFail $ GetUserByUserID deleteeid
  let isSelfDelete = deleterid == deleteeid
      isAdminDelete = maybe False ((== (usercompany deleter)) . Just) (usercompany deletee)
      isPermissioned = isSelfDelete || isAdminDelete
  userdocs <- query $ GetDocumentsByUser deletee  --maybe this should be GetDocumentsBySignatory, but what happens to things yet to be activated?
  let isAllDeletable = all isDeletableDocument userdocs
  case (isPermissioned, isAllDeletable) of
    (False, _) -> return $ Left NoDeletionRights
    (_, False) -> return $ Left UserHasLiveDocs
    _ -> return $ Right (deletee, userdocs)

performUserDeletion :: Kontrakcja m => UserDeletionDetails -> m ()
performUserDeletion (user, docs) = do
  _ <- update $ DeleteUser (userid user)
  idsAndUsers <- mapM (lookupUsersRelevantToDoc . documentid) docs
  mapM_ (\iu -> update $ DeleteDocumentRecordIfRequired  (fst iu) (snd iu)) idsAndUsers

{- |
    This looks up all the users relevant for the given docid.
    This is the dodgy link between documents and users and is required
    when deleting a user or deleting a doc.  Bleurgh.
-}
lookupUsersRelevantToDoc :: Kontrakcja m => DocumentID -> m (DocumentID, [User])
lookupUsersRelevantToDoc docid = do
  doc <- queryOrFail $ GetDocumentByDocumentID docid
  musers <- mapM (query . GetUserByUserID) (linkedUserIDs doc)
  return $ (docid, catMaybes musers)
  where
    linkedUserIDs = catMaybes . map maybesignatory . documentsignatorylinks

handleTakeOverUserForCompany :: Kontrakcja m => BS.ByteString -> m ()
handleTakeOverUserForCompany email = do
  ctx@Context{ctxmaybeuser = Just supervisor} <- getContext
  Just invited <- liftIO $ query $ GetUserByEmail Nothing (Email email)
  mail <- mailInviteUserAsCompanyAccount ctx invited supervisor
  scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [getMailAddress invited] }
  addFlashM flashMessageUserInvitedAsCompanyAccount

handleCreateCompanyUser :: Kontrakcja m => User -> m ()
handleCreateCompanyUser user = when (useriscompanyadmin user) $ do
    ctx <- getContext
    memail <- getOptionalField asValidEmail "email"
    fstname <- maybe "" id <$> getField "fstname"
    sndname <- maybe "" id <$> getField "sndname"
    let fullname = (BS.fromString fstname, BS.fromString sndname)
    case memail of
      Just email -> do
        mcompany <- getCompanyForUser $ Just user
        muser <- createUser ctx (ctxhostpart ctx) fullname email (Just user) mcompany False
        case muser of
          Just newuser -> do
            infoUpdate <- getUserInfoUpdate
            _ <- update $ SetUserInfo (userid newuser) (infoUpdate $ userinfo newuser)
            return ()
          Nothing -> do
            addFlashM $ modalInviteUserAsCompanyAccount fstname sndname (BS.toString email)
            return ()
      _ -> return ()

handleViralInvite :: Kontrakcja m => m KontraLink
handleViralInvite = withUserPost $ do
  getOptionalField asValidEmail "invitedemail" >>= maybe (return ())
    (\invitedemail -> do
        ctx@Context{ctxmaybeuser = Just user} <- getContext
        muser <- query $ GetUserByEmail Nothing $ Email invitedemail
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
                      sendInvitation ctx (LinkViralInvitationSent actionID $ visToken) invitedemail
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
        scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [MailAddress { fullname = BS.empty, email = invitedemail }]}

randomPassword :: IO BS.ByteString
randomPassword =
    BS.fromString <$> randomString 8 (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'])

createUser :: TemplatesMonad m => Context -> String -> (BS.ByteString, BS.ByteString) -> BS.ByteString -> Maybe User -> Maybe Company -> Bool -> m (Maybe User)
createUser ctx hostpart names email madminuser mcompany vip = do
    passwdhash <- liftIO $ createPassword =<< randomPassword
    muser <- update $ AddUser names email passwdhash False Nothing (fmap companyid mcompany)
    case muser of
         Just user -> do
             let fullname = composeFullName names
             mail <- case (fmap useriscompanyadmin madminuser, madminuser, mcompany) of  -- TODO EM can't add company user unless you are an admin
                            (Just True, Just adminuser, Just company) -> do
                              al <- newAccountCreatedLink user
                              inviteCompanyAccountMail hostpart (getSmartName adminuser) (getCompanyName company) email fullname al
                            _ -> do
                              al <- newAccountCreatedLink user
                              newUserMail hostpart email fullname al vip
             scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [MailAddress { fullname = fullname, email = email }]}
             return muser
         Nothing -> return muser

createUserBySigning :: Context -> BS.ByteString -> (BS.ByteString, BS.ByteString) -> BS.ByteString -> (DocumentID, SignatoryLinkID) -> IO (Maybe (User, ActionID, MagicHash))
createUserBySigning _ctx _doctitle names email doclinkdata =
    createInvitedUser names email >>= maybe (return Nothing) (\user -> do
        (actionid, magichash) <- newAccountCreatedBySigningLink user doclinkdata
        return $ Just (user, actionid, magichash)
    )

createNewUserByAdmin :: TemplatesMonad m => Context -> (BS.ByteString, BS.ByteString) -> BS.ByteString -> Maybe MinutesTime -> Maybe String -> m (Maybe User)
createNewUserByAdmin ctx names email _freetill custommessage = do
    muser <- liftIO $ createInvitedUser names email
    case muser of
         Just user -> do
             let fullname = composeFullName names
             now <- liftIO $ getMinutesTime
             update $ SetInviteInfo (ctxmaybeuser ctx) now Admin (userid user)
             chpwdlink <- newAccountCreatedLink user
             mail <- mailNewAccountCreatedByAdmin ctx fullname email chpwdlink custommessage
             scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [MailAddress { fullname = fullname, email = email }]}
             return muser
         Nothing -> return muser

createInvitedUser :: (BS.ByteString, BS.ByteString) -> BS.ByteString -> IO (Maybe User)
createInvitedUser names email = do
    passwdhash <- createPassword =<< randomPassword
    update $ AddUser names email passwdhash False Nothing Nothing

{- |
   Guard against a POST with no logged in user.
   If they are not logged in, redirect to login page.
-}
withUserPost :: Kontrakcja m => m KontraLink -> m KontraLink
withUserPost action = do
    ctx <- getContext
    case ctxmaybeuser ctx of
         Just _  -> action
         Nothing -> return $ LinkLogin NotLogged

{- |
   Guard against a GET with no logged in user.
   If they are not logged in, redirect to login page.
-}
withUserGet :: Kontrakcja m => m a -> m (Either KontraLink a)
withUserGet action = do
  ctx <- getContext
  case ctxmaybeuser ctx of
    Just _  -> Right <$> action
    Nothing -> return $ Left $ LinkLogin NotLogged

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
             Nothing -> return $ Left $ LinkLogin NotLogged



handleAcceptTOSGet :: Kontrakcja m => m (Either KontraLink Response)
handleAcceptTOSGet = withUserGet $ do
    renderFromBody TopNone kontrakcja =<< pageAcceptTOS

handleAcceptTOSPost :: Kontrakcja m => m KontraLink
handleAcceptTOSPost = withUserPost $ do
  Context{ctxmaybeuser = Just User{userid}, ctxtime} <- getContext
  tos <- getDefaultedField False asValidCheckBox "tos"
  case tos of
    Just True -> do
      _ <- update $ AcceptTermsOfService userid ctxtime
      addFlashM flashMessageUserDetailsSaved
      return LinkMain
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
             scheduleEmailSendout (ctxesenforcer ctx) $ emptyMail {
                   to = [MailAddress { fullname = BS.fromString "info@skrivapa.se", email = BS.fromString "info@skrivapa.se" }]
                 , title = BS.fromString $ "Question"
                 , content = BS.fromString $ content
             }
             addFlashM flashMessageThanksForTheQuestion
             return LoopBack

handleGetBecomeCompanyAccount :: Kontrakcja m => UserID -> m (Either KontraLink Response)
handleGetBecomeCompanyAccount _supervisorid = withUserGet $ do
  addFlashM modalDoYouWantToBeCompanyAccount
  Context{ctxmaybeuser = Just user} <- getContext
  mcompany <- getCompanyForUser $ Just user
  content <- showUser user mcompany
  renderFromBody TopAccount kontrakcja content

handlePostBecomeCompanyAccount :: Kontrakcja m => UserID -> m KontraLink
handlePostBecomeCompanyAccount supervisorid = withUserPost $ do
  ctx@Context{ctxmaybeuser = Just user} <- getContext
  supervisor <- fromMaybe user <$> (query $ GetUserByUserID supervisorid)
  case (userid user /= supervisorid, usercompany supervisor) of
     (True, Just companyid) -> do
          setsupervisorresult <- update $ SetUserCompany (userid user) companyid
          case setsupervisorresult of
            Left errmsg -> do
              let msg = "Cannot become company account for " ++ show supervisorid ++ ": " ++ errmsg
              Log.debug $ msg
              addFlash (OperationFailed, msg)
            Right _ -> do
              addFlashM $ flashMessageUserHasBecomeCompanyAccount supervisor
              mail <- mailCompanyAccountAccepted ctx user supervisor
              scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [getMailAddress supervisor] }
          return LinkAccount
     _ -> return LinkAccount

handleAccountSetupGet :: Kontrakcja m => ActionID -> MagicHash -> m Response
handleAccountSetupGet aid hash = do
    now <- liftIO $ getMinutesTime
    maction <- do
        (query $ GetAction aid) >>= maybe (return Nothing) (\action -> do
            -- action may be in state when it has expired, but hasn't yet been
            -- transformed into next form. below code checks for that.
            if (actionTypeID $ actionType action) == AccountCreatedBySigningID && (acbsState $ actionType action) /= ReminderSent
               then return $ Just action
               else return $ checkValidity now $ Just action)
    case maction of
         Just action ->
             case actionType action of
                  ViralInvitationSent _ _ _ _ token ->
                      if token == hash
                         then activationPage Nothing
                         else mzero
                  AccountCreatedBySigning _ uid _ token -> do
                      if token == hash
                         then (query $ GetUserByUserID uid) >>= activationPage
                         else mzero
                  AccountCreated uid token -> do
                      if token == hash
                         then (query $ GetUserByUserID uid) >>= maybe mzero (\user ->
                             if isNothing $ userhasacceptedtermsofservice user
                                then activationPage $ Just user
                                else do
                                    addFlashM flashMessageUserAlreadyActivated
                                    sendRedirect LinkMain)
                         else mzero
                  _  -> mzero
         Nothing -> do
             muser <- liftMM (query . GetUserByEmail Nothing . Email) (getOptionalField asValidEmail "email")
             case muser of
                  Just user ->
                    if isNothing  $ join $ userhasacceptedtermsofservice <$> muser
                     then do
                        let email = getEmail user
                        content <- activatePageViewNotValidLink $ BS.toString email
                        renderFromBody TopNone kontrakcja content
                    else mzero
                  Nothing -> mzero
    where
        activationPage muser = do
            extendActionEvalTimeToOneDayMinimum aid
            addFlashM $ modalAccountSetup muser $ LinkAccountCreated aid hash $ maybe "" (BS.toString . getEmail) muser
            sendRedirect LinkMain

handleAccountSetupFromSign :: Kontrakcja m => ActionID -> MagicHash -> m (Maybe User)
handleAccountSetupFromSign aid hash = do
  muserid <- getUserIDFromAction
  case muserid of
    Just userid -> do
      user <- queryOrFail $ GetUserByUserID userid
      acctype <- getOptionalField asValidName "accounttype"
      case join (readM . BS.unpack <$> acctype) of
           Just PrivateAccount -> handleActivate' BySigning user PrivateAccount aid id
           Just CompanyAccount -> handleActivate' BySigning user CompanyAccount aid id
           _              -> do
               addFlashM flashMessageNoAccountType
               return Nothing
    Nothing -> return Nothing
  where
    getUserIDFromAction :: Kontrakcja m => m (Maybe UserID)
    getUserIDFromAction = do
      now <- liftIO $ getMinutesTime
      maction <- checkValidity now <$> (query $ GetAction aid)
      case maction of
        Just action ->
          case actionType action of
            AccountCreatedBySigning _ uid _ token ->
              if token == hash
                then do
                  return $ Just uid
                else return Nothing
            _ -> return Nothing
        _-> return Nothing

handleAccountSetupPost :: Kontrakcja m => ActionID -> MagicHash -> m KontraLink
handleAccountSetupPost aid hash = do
    now <- liftIO $ getMinutesTime
    maction <- checkValidity now <$> (query $ GetAction aid)
    case maction of
         Just action ->
             case actionType action of
                  ViralInvitationSent email invtime inviterid _ token ->
                      if token == hash
                         then getUserForViralInvite now email invtime inviterid
                              >>= maybe mzero (handleActivate ViralInvitation)
                         else mzero
                  AccountCreatedBySigning _ uid _ token ->
                      if token == hash
                         then do
                             link <- (query $ GetUserByUserID uid)
                                     >>= maybe mzero (handleActivate BySigning)
                             case link of
                                  LinkMain -> addFlashM modalWelcomeToSkrivaPa
                                  _ -> return ()
                             return link
                         else mzero
                  AccountCreated uid token ->
                      if token == hash
                         then (query $ GetUserByUserID uid) >>= maybe mzero (\user ->
                             if isNothing $ userhasacceptedtermsofservice user
                                then handleActivate AccountRequest user
                                else do
                                    addFlashM flashMessageUserAlreadyActivated
                                    return LinkMain)
                         else mzero
                  _ -> mzero
         Nothing -> do -- try to generate another activation link
             getOptionalField asValidEmail "email" >>= maybe mzero (\email ->
                 (query $ GetUserByEmail Nothing $ Email email) >>= maybe mzero (\user ->
                     if isNothing $ userhasacceptedtermsofservice user
                        then do
                            ctx <- getContext
                            al <- newAccountCreatedLink user
                            mail <- newUserMail (ctxhostpart ctx) email email al False
                            scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [MailAddress { fullname = email, email = email}] }
                            addFlashM flashMessageNewActivationLinkSend
                            return LinkMain
                        else mzero
                     )
                 )
    where
        returnToAccountSetup user = do
            addFlashM $ modalAccountSetup (Just user) $ LinkAccountCreated aid hash $ BS.toString $ getEmail user
            return LinkMain

        handleActivate signupmethod user = do
            acctype <- join . fmap (readM . BS.unpack) <$> getOptionalField asValidName "accounttype"
            case acctype of
                 Just PrivateAccount -> do
                     ifAccountTypeIsValid (usercompany user) PrivateAccount $
                         finalizeActivation signupmethod user PrivateAccount id
                 Just CompanyAccount -> do
                     mfname <- getRequiredField asValidName "fname"
                     mlname <- getRequiredField asValidName "lname"
                     mcompanyname <- maybe (getRequiredField asValidName "companyname")
                                         (\_ -> return $ Just BS.empty) $ usercompany user
                     mcompanyposition <- getRequiredField asValidName "companyposition"
                     mphone <- getRequiredField asValidPhone "phone"
                     case (mfname, mlname, mcompanyname, mcompanyposition, mphone) of
                          (Just fname, Just lname, Just _companyname, Just companytitle, Just phone) -> do
                              let infoupdatef = \info -> info {
                                       userfstname = fname
                                     , usersndname = lname
                                     , usercompanyposition = companytitle
                                     , userphone = phone
                                   } --TODO EM this stuff is a mess
                              ifAccountTypeIsValid (usercompany user) CompanyAccount $
                                finalizeActivation signupmethod user CompanyAccount infoupdatef
                          _ -> returnToAccountSetup user
                 _ -> do
                     addFlashM flashMessageNoAccountType
                     returnToAccountSetup user
            where
                -- we protect from choosing account type that doesn't match
                -- supervisor in browser, but someone may just send invalid
                -- data directly to the server
                ifAccountTypeIsValid _mcompany _acctype action = do
                  action
                  {-  case mcompany of
                         Just company -> do
                             if (accounttype $ usersettings sv) == acctype
                                then action
                                else do
                                    addFlashM flashMessageNoAccountType
                                    returnToAccountSetup user
                         Nothing -> action -} --TODO EM this is so wrong

        finalizeActivation signupmethod user acctype infoupdatefunc = do
            muser <- handleActivate' signupmethod user acctype aid infoupdatefunc
            case muser of
                 Just _ -> do
                     addFlashM flashMessageUserActivated
                     return LinkMain
                 Nothing -> do
                     -- handleActivate' might have updated user info, so we
                     -- need to query database for the newest user version
                     newuser <- fromMaybe user <$> (query $ GetUserByUserID $ userid user)
                     returnToAccountSetup newuser

        getUserForViralInvite _now invitedemail invitationtime inviterid = do
            muser <- liftIO $ createInvitedUser (BS.empty, BS.empty) $ unEmail invitedemail
            case muser of
                 Just user -> do -- user created, we need to fill in some info
                     minviter <- query $ GetUserByUserID inviterid
                     update $ SetInviteInfo minviter invitationtime Viral (userid user)
                     return muser
                 Nothing -> do -- user already exists, get her
                     query $ GetUserByEmail Nothing invitedemail

{- |
    Helper method for handling account activation.  This'll check
    the tos, and the passwords and add error flash messages as you'd
    expect.
-}
handleActivate' :: Kontrakcja m => SignupMethod -> User -> UserAccountType -> ActionID -> (UserInfo -> UserInfo) -> m (Maybe User)
handleActivate' signupmethod user acctype actionid infoupdatefunc = do
  ctx <- getContext
  mtos <- getDefaultedField False asValidCheckBox "tos"
  mpassword <- getRequiredField asValidPassword "password"
  mpassword2 <- getRequiredField asValidPassword "password2"
  -- update user info he typed on signup page, so he won't
  -- have to type in again if password validation fails.
  _ <- update $ SetUserInfo (userid user) $ infoupdatefunc (userinfo user)
  case (mtos, mpassword, mpassword2) of
    (Just tos, Just password, Just password2) -> do
      case checkPasswordsMatch password password2 of
        Right () ->
          if tos
            then do
              passwordhash <- liftIO $ createPassword password
              _ <- update $ SetUserPassword (userid user) passwordhash
              _ <- update $ AcceptTermsOfService (userid user) (ctxtime ctx)
              _ <- update $ SetSignupMethod (userid user) signupmethod
              _ <- update $ SetUserSettings (userid user) $ (usersettings user) {
                accounttype = acctype
              }
              _ <- update $ SetUserPaymentAccount (userid user) $ (userpaymentaccount user) {
                  paymentaccountfreesignatures =
                      if useriscompanyadmin user
                           then 0
                           else 100 -- later we should set here either
                           -- 10 or 20, depending on user's account type. for
                           -- now we don't modify that number anyway.
              }
              dropExistingAction actionid
              logUserToContext $ Just user
              return $ Just user
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
         Just _ -> do
             extendActionEvalTimeToOneDayMinimum aid
             addFlashM $ modalNewPasswordView aid hash
             sendRedirect LinkMain
         Nothing -> do
             addFlashM flashMessagePasswordChangeLinkNotValid
             sendRedirect LinkMain

handlePasswordReminderPost :: Kontrakcja m => ActionID -> MagicHash -> m KontraLink
handlePasswordReminderPost aid hash = do
    muser <- getUserFromActionOfType PasswordReminderID aid hash
    case muser of
         Just user -> handleChangePassword user
         Nothing   -> do
             addFlashM flashMessagePasswordChangeLinkNotValid
             return LinkMain
    where
        handleChangePassword user = do
            mpassword <- getRequiredField asValidPassword "password"
            mpassword2 <- getRequiredField asDirtyPassword "password2"
            case (mpassword, mpassword2) of
                 (Just password, Just password2) -> do
                     case (checkPasswordsMatch password password2) of
                          Right () -> do
                              dropExistingAction aid
                              passwordhash <- liftIO $ createPassword password
                              _ <- update $ SetUserPassword (userid user) passwordhash
                              addFlashM flashMessageUserPasswordChanged
                              logUserToContext $ Just user
                              return LinkMain
                          Left flash -> do
                              addFlashM flash
                              addFlashM $ modalNewPasswordView aid hash
                              return LinkMain
                 _ -> do
                   addFlashM $ modalNewPasswordView aid hash
                   return LinkMain

handleAccountRemovalGet :: Kontrakcja m => ActionID -> MagicHash -> m Response
handleAccountRemovalGet aid hash = do
  action <- guardJustM $ getAccountCreatedBySigningIDAction aid
  let AccountCreatedBySigning _ _ (docid, sigid) token = actionType action
  guard $ hash == token
  doc <- queryOrFail $ GetDocumentByDocumentID docid
  sig <- guardJust $ getSigLinkFor doc sigid
  extendActionEvalTimeToOneDayMinimum aid
  addFlashM $ modalAccountRemoval (documenttitle doc) (LinkAccountCreatedBySigning aid hash) (LinkAccountRemoval aid hash)
  sendRedirect $ LinkSignDoc doc sig

handleAccountRemovalFromSign :: Kontrakcja m => ActionID -> MagicHash -> m ()
handleAccountRemovalFromSign aid hash = do
  _doc <- handleAccountRemoval' aid hash
  return ()

handleAccountRemovalPost :: Kontrakcja m => ActionID -> MagicHash -> m KontraLink
handleAccountRemovalPost aid hash = do
  doc <- handleAccountRemoval' aid hash
  addFlashM $ modalAccountRemoved $ documenttitle doc
  return LinkMain

{- |
    Helper function for performing account removal (these are accounts setup
    by signing), this'll return the document that was removed.
-}
handleAccountRemoval' :: Kontrakcja m => ActionID -> MagicHash -> m Document
handleAccountRemoval' aid hash = do
  action <- guardJustM $ getAccountCreatedBySigningIDAction aid        
  let AccountCreatedBySigning _ _ (docid, _) token = actionType action
  guard $ hash == token
  doc <- queryOrFail $ GetDocumentByDocumentID docid
  -- there should be done account and its documents removal, but
  -- since the functionality is not there yet, we just remove
  -- this action to prevent user from account activation and
  -- receiving further reminders
  dropExistingAction aid
  return doc

getAccountCreatedBySigningIDAction :: Kontrakcja m => ActionID -> m (Maybe Action)
getAccountCreatedBySigningIDAction aid = do
  maction <- query $ GetAction aid
  case maction of
    Nothing -> return Nothing
    Just action -> 
      -- allow for account created by signing links only
      if actionTypeID (actionType action) /= AccountCreatedBySigningID
      then return Nothing
      else if acbsState (actionType action) /= ReminderSent
           then return $ Just action
           else do
             now <- liftIO $ getMinutesTime
             return $ checkValidity now $ Just action

getUserFromActionOfType :: Kontrakcja m => ActionTypeID -> ActionID -> MagicHash -> m (Maybe User)
getUserFromActionOfType atypeid aid hash = do
    now <- liftIO $ getMinutesTime
    maction <- checkValidity now <$> query (GetAction aid)
    case maction of
         Just action -> do
             if atypeid == (actionTypeID $ actionType action)
                then getUID action >>= maybe
                         (return Nothing)
                         (query . GetUserByUserID)
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
