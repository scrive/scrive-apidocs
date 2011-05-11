{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module User.UserControl where

import Control.Monad.State
import Data.Char
import Data.Functor
import Data.List
import Data.Maybe
import Happstack.Server hiding (simpleHTTP)
import Happstack.State (update, query)
import Happstack.Util.Common (readM)
import HSP.XML
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Set as Set

import ActionSchedulerState
import AppView
import Doc.DocState
import FlashMessage
import InputValidation
import Kontra
import KontraLink
import ListUtil
import Mails.SendMail
import MinutesTime
import Misc
import Payments.PaymentsState
import Redirect
import Templates.Templates (KontrakcjaTemplates)
import User.UserView
import qualified AppLogger as Log

checkPasswordsMatch :: BS.ByteString -> BS.ByteString -> Either (KontrakcjaTemplates -> IO FlashMessage) ()
checkPasswordsMatch p1 p2 =
    if p1 == p2
       then Right () 
       else Left  flashMessagePasswordsDontMatch      

handleUserGet :: Kontra Response
handleUserGet = do
    ctx <- get
    case (ctxmaybeuser ctx) of
         Just user -> do
             content <- liftIO $ showUser (ctxtemplates ctx) user
             renderFromBody TopAccount kontrakcja $ cdata content
         Nothing -> sendRedirect $ LinkLogin NotLogged    

handleUserPost :: Kontra KontraLink
handleUserPost = do
    ctx <- get
    case (ctxmaybeuser ctx) of
         Just user -> do
             infoUpdate <- getUserInfoUpdate
             update $ SetUserInfo (userid user) (infoUpdate $ userinfo user)
             addFlashMsg =<< (liftIO $ flashMessageUserDetailsSaved $ ctxtemplates ctx)
             return LinkAccount
         Nothing -> return $ LinkLogin NotLogged

getUserInfoUpdate :: Kontra (UserInfo -> UserInfo)
getUserInfoUpdate  = do
    -- a lot doesn't have validation rules defined, but i put in what we do have
    let getValidField = getDefaultedField BS.empty
    mfstname          <- getValidField asValidName "fstname" 
    msndname          <- getValidField asValidName "sndname"
    mpersonalnumber   <- getFieldUTF "personalnumber"
    maddress          <- getValidField asValidAddress "address"
    mcity             <- getFieldUTF "city"
    mcountry          <- getFieldUTF "country"
    mzip              <- getFieldUTF "zip"
    mphone            <- getFieldUTF "phone"
    mcompanyposition  <- getValidField asValidPosition "companyposition"
    mcompanynumber    <- getValidField asValidCompanyNumber "companynumber"
    mcompanyname      <- getValidField asValidCompanyName "companyname"
    return $ \ui ->
        ui {
            userfstname = fromMaybe (userfstname ui) mfstname
          , usersndname = fromMaybe (usersndname ui) msndname
          , userpersonalnumber = fromMaybe (userpersonalnumber ui) mpersonalnumber
          , usercompanyname = fromMaybe (usercompanyname ui) mcompanyname
          , usercompanyposition = fromMaybe (usercompanyposition ui) mcompanyposition
          , usercompanynumber = fromMaybe (usercompanynumber ui) mcompanynumber
          , useraddress  = fromMaybe (useraddress ui) maddress
          , userzip    = fromMaybe (userzip ui) mzip
          , usercity  = fromMaybe (usercity ui) mcity
          , usercountry  = fromMaybe (usercountry  ui) mcountry 
          , userphone  = fromMaybe (userphone ui) mphone
        }

handleGetUserSecurity :: Kontra Response
handleGetUserSecurity = do
    ctx <- get
    case (ctxmaybeuser ctx) of
         Just user -> do
             content <- liftIO $ showUserSecurity (ctxtemplates ctx) user
             renderFromBody TopAccount kontrakcja $ cdata content
         Nothing -> sendRedirect $ LinkLogin NotLogged    

handlePostUserSecurity :: Kontra KontraLink
handlePostUserSecurity = do
  ctx <- get
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
              addFlashMsg =<< (liftIO $ flashMessageBadOldPassword (ctxtemplates ctx))
            (_, Left f) ->
              addFlashMsg =<< (liftIO $ f (ctxtemplates ctx))
            _ ->  do
              passwordhash <- liftIO $ createPassword password
              update $ SetUserPassword user passwordhash
              addFlashMsg =<< (liftIO $ flashMessageUserDetailsSaved (ctxtemplates ctx))
        _ -> return ()
      return LinkSecurity
    Nothing -> return $ LinkLogin NotLogged

handleGetSharing :: Kontra (Either KontraLink Response)
handleGetSharing = withUserGet $ do
    ctx@Context{ctxmaybeuser = Just user@User{userid}} <- get
    friends <- query $ GetUserFriends userid
    params <- getListParams
    content <- liftIO $ viewFriends (ctxtemplates ctx) (friendsSortSearchPage params friends) user
    renderFromBody TopAccount kontrakcja $ cdata content

-- Searching, sorting and paging
friendsSortSearchPage :: ListParams -> [User] -> PagedList User
friendsSortSearchPage  =
    listSortSearchPage subaccountsSortFunc subaccountsSearchFunc subaccountsPageSize


handleGetSubaccount :: Kontra (Either KontraLink Response)
handleGetSubaccount = withUserGet $ do
    ctx@Context{ctxmaybeuser = Just user@User{userid}} <- get
    subaccounts <- query $ GetUserSubaccounts userid
    params <- getListParams
    content <- viewSubaccounts user (subaccountsSortSearchPage params $ Set.toList subaccounts)
    renderFromBody TopAccount kontrakcja $ cdata content

-- Searching, sorting and paging
subaccountsSortSearchPage :: ListParams -> [User] -> PagedList User
subaccountsSortSearchPage  =
    listSortSearchPage subaccountsSortFunc subaccountsSearchFunc subaccountsPageSize

subaccountsSearchFunc :: SearchingFunction User
subaccountsSearchFunc s user = userMatch user s -- split s so we support spaces
    where
        match s' m = isInfixOf (map toUpper s') (map toUpper (BS.toString m))
        userMatch u s' = match s' (usercompanyposition $ userinfo u)
                      || match s' (userfstname $ userinfo u)
                      || match s' (usersndname $ userinfo u)
                      || match s' (userpersonalnumber $ userinfo u)
                      || match s' (unEmail $ useremail $ userinfo u)

subaccountsSortFunc :: SortingFunction User
subaccountsSortFunc "fstname" = viewComparing $ userfstname . userinfo
subaccountsSortFunc "fstnameREV" = viewComparingRev $ userfstname . userinfo
subaccountsSortFunc "sndname" = viewComparing $ usersndname . userinfo
subaccountsSortFunc "sndnameREV" = viewComparingRev $ usersndname . userinfo
subaccountsSortFunc "companyposition" = viewComparing $ usercompanyposition . userinfo
subaccountsSortFunc "companypositionREV" = viewComparingRev $ usercompanyposition . userinfo
subaccountsSortFunc "phone" = viewComparing $  userphone . userinfo
subaccountsSortFunc "phoneREV" = viewComparingRev $ userphone . userinfo
subaccountsSortFunc "email" = viewComparing $ useremail . userinfo
subaccountsSortFunc "emailREV" = viewComparingRev $ useremail . userinfo
subaccountsSortFunc _ = const $ const EQ

subaccountsPageSize :: Int
subaccountsPageSize = 20

----

handlePostSharing :: Kontra KontraLink
handlePostSharing = do
    ctx <- get
    case (ctxmaybeuser ctx) of
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

handleAddFriend :: User -> BS.ByteString -> Kontra ()
handleAddFriend User{userid} email = do
    avereturn <- update $ AddViewerByEmail userid $ Email email
    case avereturn of
      Left msg -> addFlashMsg $ toFlashMsg OperationFailed msg
      Right _  -> return ()

handlePostSubaccount :: Kontra KontraLink
handlePostSubaccount = do
    ctx <- get
    case (ctxmaybeuser ctx) of
         Just user -> do
             add <- isFieldSet "add"
             remove <- isFieldSet "remove"
             takeover <- isFieldSet "takeover"
             case (add,remove,takeover) of
                  (True, _, _) -> do
                      handleCreateSubaccount user
                      return $ LinkSubaccount emptyListParams
                  (_, True, _) -> return $ LinkSubaccount emptyListParams
                  (_, _, True) -> do
                      memail <- getOptionalField asValidEmail "email"
                      handleTakeOverSubaccount (fromJust memail)
                      return $ LinkSubaccount emptyListParams
                  _ -> LinkSubaccount <$> getListParamsForSearch
         Nothing -> return $ LinkLogin NotLogged

  
handleTakeOverSubaccount :: BS.ByteString -> Kontra ()
handleTakeOverSubaccount email = do
  ctx@Context{ctxmaybeuser = Just supervisor} <- get
  Just invited <- liftIO $ query $ GetUserByEmail (Email email)
  mail <- mailInviteUserAsSubaccount ctx invited supervisor
  scheduleEmailSendout (ctxesenforcer ctx) $ mail { fullnameemails = [(userfullname invited, email)] }
  addFlashMsg =<< (liftIO $ flashMessageUserInvitedAsSubaccount (ctxtemplates ctx))
  
  
handleCreateSubaccount :: User -> Kontra ()
handleCreateSubaccount user = when (isAbleToHaveSubaccounts user) $ do
    ctx <- get
    memail <- getOptionalField asValidEmail "email"
    fstname <- maybe "" id <$> getField "fstname"
    sndname <- maybe "" id <$> getField "sndname"
    let fullname = (BS.fromString fstname, BS.fromString sndname)
    case memail of
      Just email -> do
        muser <- liftIO $ createUser ctx (ctxhostpart ctx) fullname email (Just user) False
        case muser of
          Just newuser -> do
            infoUpdate <- getUserInfoUpdate
            update $ SetUserInfo (userid newuser) (infoUpdate $ userinfo newuser)
            return ()
          Nothing -> do
            addModal $ modalInviteUserAsSubaccount fstname sndname (BS.toString email)
            return ()
      _ -> return ()

handleViralInvite :: Kontra KontraLink
handleViralInvite = withUserPost $ do
    getOptionalField asValidEmail "invitedemail" >>= maybe (return ()) (\invitedemail -> do
        ctx@Context{ctxmaybeuser = Just user} <- get
        muser <- query $ GetUserByEmail $ Email invitedemail
        if isJust muser
           then do
               addFlashMsg =<< (liftIO $ flashMessageUserWithSameEmailExists $ ctxtemplates ctx)
           else do
               now <- liftIO getMinutesTime
               minv <- checkValidity now <$> (query $ GetViralInvitationByEmail $ Email invitedemail)
               case minv of
                    Just Action{actionID, actionType} -> do
                        if visInviterID actionType == userid user
                           then do
                               case visRemainedEmails actionType of
                                    0 -> do
                                        addFlashMsg =<< (liftIO $ flashMessageNoRemainedInvitationEmails $ ctxtemplates ctx)
                                    n -> do
                                        update $ UpdateActionType actionID $ actionType { visRemainedEmails = n-1 }
                                        sendInvitation ctx (LinkViralInvitationSent actionID $ visToken actionType) invitedemail
                           else do
                               addFlashMsg =<< (liftIO $ flashMessageOtherUserSentInvitation $ ctxtemplates ctx)
                    Nothing -> do
                        link <- newViralInvitationSentLink (Email invitedemail) (userid . fromJust $ ctxmaybeuser ctx)
                        sendInvitation ctx link invitedemail
        )
    return LoopBack
    where
        sendInvitation ctx link invitedemail = do
            addFlashMsg =<< (liftIO $ flashMessageViralInviteSent $ ctxtemplates ctx)
            mail <- liftIO $ viralInviteMail (ctxtemplates ctx) ctx invitedemail link
            scheduleEmailSendout (ctxesenforcer ctx) $ mail { fullnameemails = [(BS.empty, invitedemail)] }

randomPassword :: IO BS.ByteString
randomPassword =
    BS.fromString <$> randomString 8 (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'])

createUser :: Context -> String -> (BS.ByteString, BS.ByteString) -> BS.ByteString -> Maybe User -> Bool -> IO (Maybe User)
createUser ctx hostpart names email maybesupervisor vip = do
    passwdhash <- createPassword =<< randomPassword
    muser <- update $ AddUser names email passwdhash (userid <$> maybesupervisor) Nothing
    case muser of
         Just user -> do
             let fullname = composeFullName names
             mail <- case maybesupervisor of
                          Nothing -> do
                              al <- newAccountCreatedLink user
                              newUserMail (ctxtemplates ctx) hostpart email fullname al vip
                          Just supervisor -> do
                              al <- newAccountCreatedLink user
                              inviteSubaccountMail (ctxtemplates ctx) hostpart (prettyName  supervisor) (usercompanyname $ userinfo supervisor) email fullname al
             scheduleEmailSendout (ctxesenforcer ctx) $ mail { fullnameemails = [(fullname, email)] }
             return muser
         Nothing -> return muser

createUserBySigning :: Context -> BS.ByteString -> (BS.ByteString, BS.ByteString) -> BS.ByteString -> BS.ByteString -> (DocumentID, SignatoryLinkID) -> IO (Maybe (User, ActionID, MagicHash))
createUserBySigning Context{ctxesenforcer, ctxtemplates, ctxhostpart} doctitle names email companyname doclinkdata =
    createInvitedUser names email >>= maybe (return Nothing) (\user -> do
        update $ SetUserInfo (userid user) $ (userinfo user) {
            usercompanyname = companyname
        }
        let fullname = composeFullName names
        (actionid, magichash) <- newAccountCreatedBySigningLink user doclinkdata
        return $ Just (user, actionid, magichash)
    )

createNewUserByAdmin :: Context -> (BS.ByteString, BS.ByteString) -> BS.ByteString -> Maybe MinutesTime -> Maybe String -> IO (Maybe User)
createNewUserByAdmin ctx names email freetill custommessage = do
    muser <- createInvitedUser names email
    case muser of
         Just user -> do
             let fullname = composeFullName names
             now <- liftIO $ getMinutesTime
             update $ SetInviteInfo (ctxmaybeuser ctx) now Admin (userid user)
             chpwdlink <- newAccountCreatedLink user
             mail <- mailNewAccountCreatedByAdmin (ctxtemplates ctx) ctx fullname email chpwdlink custommessage
             scheduleEmailSendout (ctxesenforcer ctx) $ mail { fullnameemails = [(fullname, email)]}
             return muser
         Nothing -> return muser

createInvitedUser :: (BS.ByteString, BS.ByteString) -> BS.ByteString -> IO (Maybe User)
createInvitedUser names email = do
    password <- randomPassword
    passwdhash <- createPassword password
    update $ AddUser names email passwdhash Nothing Nothing

{- |
   Guard against a POST with no logged in user.
   If they are not logged in, redirect to login page.
-}
withUserPost :: Kontra KontraLink -> Kontra KontraLink
withUserPost action = do
    ctx <- get
    case ctxmaybeuser ctx of
         Just _  -> action
         Nothing -> return $ LinkLogin NotLogged

{- |
   Guard against a GET with no logged in user.
   If they are not logged in, redirect to login page.
-}
withUserGet ::  Kontra a -> Kontra (Either KontraLink a)
withUserGet action = do
  ctx <- get
  case ctxmaybeuser ctx of
    Just _  -> Right <$> action
    Nothing -> return $ Left $ LinkLogin NotLogged

{- | 
     Takes a document and a action
     Runs an action only if current user (from context) is author of document
| -}
withDocumentAuthor :: Document -> Kontra a -> Kontra a
withDocumentAuthor document action = do
    ctx <- get
    case isAuthor document <$> ctxmaybeuser ctx of
         Just True -> action
         _         -> mzero

{- |
   Guard against a GET with logged in users who have not signed the TOS agreement.
   If they have not, redirect to their account page.
-}
checkUserTOSGet :: Kontra a -> Kontra (Either KontraLink a)
checkUserTOSGet action = do
    ctx <- get
    case ctxmaybeuser ctx of
        Just (User{userhasacceptedtermsofservice = Just _}) -> Right <$> action
        Just _ -> return $ Left $ LinkAcceptTOS
        Nothing -> return $ Left $ LinkLogin NotLogged


handleAcceptTOSGet :: Kontra (Either KontraLink Response)
handleAcceptTOSGet = withUserGet $ do
    ctx@Context{ctxtemplates} <- get
    content <- liftIO $ pageAcceptTOS ctxtemplates
    renderFromBody TopNone kontrakcja $ cdata content

handleAcceptTOSPost :: Kontra KontraLink
handleAcceptTOSPost = withUserPost $ do
    ctx@Context{ctxmaybeuser = Just User{userid}, ctxtime} <- get
    tos <- getDefaultedField False asValidCheckBox "tos"
    case tos of
         Just True -> do
             update $ AcceptTermsOfService userid ctxtime
             addFlashMsg =<< (liftIO $ flashMessageUserDetailsSaved (ctxtemplates ctx))
             return LinkMain
         Just False -> do
             addFlashMsg =<< (liftIO $ flashMessageMustAcceptTOS (ctxtemplates ctx))
             return LinkAcceptTOS
         Nothing -> return LinkAcceptTOS

handleRequestAccount :: Kontra KontraLink
handleRequestAccount = do
    ctx <- get
    memail <- getRequiredField asValidEmail "email"
    case memail of
         Nothing -> return LinkMain
         Just email -> do
             scheduleEmailSendout (ctxesenforcer ctx) $ emptyMail {
                 fullnameemails = [(BS.fromString "prelaunch@skrivapa.se", BS.fromString "prelaunch@skrivapa.se")]
                 , title = BS.fromString $ "New account request"
                 , content = BS.fromString $ "Request from address " ++ (BS.toString email)
             }
             addFlashMsg =<< (liftIO $ flashMessageAccountRequestSend $ ctxtemplates ctx)
             return LinkMain -- Something should happend here

handleQuestion :: Kontra KontraLink
handleQuestion = do
    ctx <- get
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
                   fullnameemails = [(BS.fromString "info@skrivapa.se",BS.fromString "info@skrivapa.se")]
                 , title = BS.fromString $ "Question"
                 , content = BS.fromString $ content
             }
             addFlashMsg =<< (liftIO $ flashMessageThanksForTheQuestion $ ctxtemplates ctx)
             return LoopBack

handleGetBecomeSubaccountOf :: UserID -> Kontra (Either KontraLink Response)
handleGetBecomeSubaccountOf supervisorid = withUserGet $ do
  addModal $ modalDoYouWantToBeSubaccount 
  ctx@Context{ctxmaybeuser = Just user} <- get
  content <- liftIO $ showUser (ctxtemplates ctx) user
  renderFromBody TopAccount kontrakcja $ cdata content
    
handlePostBecomeSubaccountOf :: UserID -> Kontra KontraLink
handlePostBecomeSubaccountOf supervisorid = withUserPost $ do
  ctx@Context{ctxmaybeuser = Just user} <- get
  if userid user /= supervisorid
     then do
          result <- update $ SetUserSupervisor (userid user) supervisorid
          case result of
            Left errmsg -> do
              let msg = "Cannot become subaccount of " ++ show supervisorid ++ ": " ++ errmsg
              Log.debug $ msg
              addFlashMsg $ toFlashMsg OperationFailed msg
            Right _ -> do
              Just supervisor <- query $ GetUserByUserID supervisorid
              addFlashMsg =<< (liftIO $ flashMessageUserHasBecomeSubaccount (ctxtemplates ctx) supervisor)
              mail <- mailSubaccountAccepted ctx user supervisor
              scheduleEmailSendout (ctxesenforcer ctx) $ mail { fullnameemails = [(userfullname supervisor, unEmail $ useremail $ userinfo supervisor)] }
          return LinkAccount
     else do     
          return LinkAccount

handleAccountSetupGet :: ActionID -> MagicHash -> Kontra Response
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
                                    templates <- ctxtemplates <$> get
                                    addFlashMsg =<< (liftIO $ flashMessageUserAlreadyActivated templates)
                                    sendRedirect LinkMain)
                         else mzero
                  _  -> mzero
         Nothing -> -- action has expired, but we may be able to generate it again
             getOptionalField asValidEmail "email" >>= maybe mzero (\email ->
                 (query $ GetUserByEmail $ Email email) >>= maybe mzero (\user ->
                     if isNothing $ userhasacceptedtermsofservice user
                        then do
                            ctx <- get
                            content <- liftIO $ activatePageViewNotValidLink (ctxtemplates ctx) $ BS.toString email
                            renderFromBody TopNone kontrakcja $ cdata content
                        else mzero
                     )
                 )
    where
        activationPage muser = do
            extendActionEvalTimeToOneDayMinimum aid
            addModalT $ modalAccountSetup muser $ LinkAccountCreated aid hash $ maybe "" (BS.toString . unEmail . useremail . userinfo) muser
            sendRedirect LinkMain

handleAccountSetupFromSign :: ActionID -> MagicHash -> Kontra (Maybe User)
handleAccountSetupFromSign aid hash = do
  muserid <- getUserIDFromAction
  case muserid of
    Just userid -> do
      user <- queryOrFail $ GetUserByUserID userid
      acctype <- getOptionalField asValidName "accounttype"
      case BS.unpack <$> acctype of
           Just "private" -> handleActivate' BySigning user PrivateAccount aid id
           Just "company" -> handleActivate' BySigning user CompanyAccount aid id
           _              -> do
               templates <- ctxtemplates <$> get
               addFlashMsg =<< (liftIO $ flashMessageNoAccountType templates)
               return Nothing
    Nothing -> return Nothing
  where
    getUserIDFromAction :: Kontra (Maybe UserID)            
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

handleAccountSetupPost :: ActionID -> MagicHash -> Kontra KontraLink
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
                                  LinkMain -> do
                                      templates <- ctxtemplates <$> get
                                      addModal $ modalWelcomeToSkrivaPa templates
                                  _ -> return ()
                             return link
                         else mzero
                  AccountCreated uid token ->
                      if token == hash
                         then (query $ GetUserByUserID uid) >>= maybe mzero (\user ->
                             if isNothing $ userhasacceptedtermsofservice user
                                then handleActivate AccountRequest user
                                else do
                                    templates <- ctxtemplates <$> get
                                    addFlashMsg =<< (liftIO $ flashMessageUserAlreadyActivated templates)
                                    return LinkMain)
                         else mzero
                  _ -> mzero
         Nothing -> do -- try to generate another activation link
             getOptionalField asValidEmail "email" >>= maybe mzero (\email ->
                 (query $ GetUserByEmail $ Email email) >>= maybe mzero (\user ->
                     if isNothing $ userhasacceptedtermsofservice user
                        then do
                            ctx <- get
                            al <- newAccountCreatedLink user
                            mail <- liftIO $ newUserMail (ctxtemplates ctx) (ctxhostpart ctx) email email al False
                            scheduleEmailSendout (ctxesenforcer ctx) $ mail { fullnameemails = [(email, email)] }
                            addFlashMsg =<< (liftIO $ flashMessageNewActivationLinkSend  (ctxtemplates ctx))
                            return LinkMain
                        else mzero
                     )
                 )
    where
        returnToAccountSetup user = do
            addModalT $ modalAccountSetup (Just user) $ LinkAccountCreated aid hash $ BS.toString . unEmail . useremail $ userinfo user
            return LinkMain

        handleActivate signupmethod user = do
            ctx <- get
            acctype <- getOptionalField asValidName "accounttype"
            case BS.unpack <$> acctype of
                 Just "private" -> finalizeActivation signupmethod user PrivateAccount id
                 Just "company" -> do
                     mfname <- getRequiredField asValidName "fname"
                     mlname <- getRequiredField asValidName "lname"
                     mcompanyname <- getRequiredField asValidName "companyname"
                     mcompanyposition <- getRequiredField asValidName "companyposition"
                     mphone <- getRequiredField asValidPhone "phone"
                     case (mfname, mlname, mcompanyname, mcompanyposition, mphone) of
                          (Just fname, Just lname, Just companyname, Just companytitle, Just phone) -> do
                              finalizeActivation signupmethod user CompanyAccount $ \info -> info {
                                    userfstname = fname
                                  , usersndname = lname
                                  , usercompanyname  = companyname
                                  , usercompanyposition = companytitle
                                  , userphone = phone
                              }
                          _ -> returnToAccountSetup user
                 _ -> do
                     addFlashMsg =<< (liftIO $ flashMessageNoAccountType $ ctxtemplates ctx)
                     returnToAccountSetup user

        finalizeActivation signupmethod user acctype infoupdatefunc = do
            muser <- handleActivate' signupmethod user acctype aid infoupdatefunc
            case muser of
                 Just _ -> do
                     templates <- ctxtemplates <$> get
                     addFlashMsg =<< (liftIO $ flashMessageUserActivated templates)
                     return LinkMain
                 Nothing -> do
                     -- handleActivate' might have updated user info, so we
                     -- need to query database for the newest user version
                     newuser <- fromMaybe user <$> (query $ GetUserByUserID $ userid user)
                     returnToAccountSetup newuser

        getUserForViralInvite now invitedemail invitationtime inviterid = do
            muser <- liftIO $ createInvitedUser (BS.empty, BS.empty) $ unEmail invitedemail
            case muser of
                 Just user -> do -- user created, we need to fill in some info
                     minviter <- query $ GetUserByUserID inviterid
                     update $ SetInviteInfo minviter invitationtime Viral (userid user)
                     return muser
                 Nothing -> do -- user already exists, get her
                     query $ GetUserByEmail invitedemail

{- |
    Helper method for handling account activation.  This'll check
    the tos, and the passwords and add error flash messages as you'd
    expect.
-}
handleActivate' :: SignupMethod -> User -> UserAccountType -> ActionID -> (UserInfo -> UserInfo) -> Kontra (Maybe User)
handleActivate' signupmethod user acctype actionid infoupdatefunc = do
  ctx <- get
  mtos <- getDefaultedField False asValidCheckBox "tos"
  mpassword <- getRequiredField asValidPassword "password"
  mpassword2 <- getRequiredField asValidPassword "password2"
  -- update user info he typed on signup page, so he won't
  -- have to type in again if password validation fails.
  update $ SetUserInfo (userid user) $ infoupdatefunc (userinfo user)
  case (mtos, mpassword, mpassword2) of
    (Just tos, Just password, Just password2) -> do
      case checkPasswordsMatch password password2 of
        Right () ->
          if tos
            then do
              passwordhash <- liftIO $ createPassword password
              update $ SetUserPassword user passwordhash
              update $ AcceptTermsOfService (userid user) (ctxtime ctx)
              update $ SetSignupMethod (userid user) signupmethod
              update $ SetUserSettings (userid user) $ (usersettings user) {
                  accounttype = acctype
              }
              update $ SetUserPaymentAccount (userid user) $ (userpaymentaccount user) {
                  paymentaccountfreesignatures =
                      case usersupervisor user of
                           Just _  -> 0
                           Nothing -> 100 -- later we should set here either
                           -- 10 or 20, depending on user's account type. for
                           -- now we don't modify that number anyway.
              }
              now <- liftIO getMinutesTime
              dropExistingAction actionid
              logUserToContext $ Just user
              return $ Just user
             else do
               addFlashMsg =<< (liftIO $ flashMessageMustAcceptTOS $ ctxtemplates ctx)
               return Nothing
        Left flash -> do
          addFlashMsg =<< (liftIO $ flash (ctxtemplates ctx))
          return Nothing
    _ -> return Nothing


{- |
    This is where we get to when the user clicks the link in their password reminder
    email.  This'll show them the usual landing page, but with a modal dialog
    for changing their password.
-}
handlePasswordReminderGet :: ActionID -> MagicHash -> Kontra Response
handlePasswordReminderGet aid hash = do
    muser <- getUserFromActionOfType PasswordReminderID aid hash
    case muser of
         Just _ -> do
             extendActionEvalTimeToOneDayMinimum aid
             addModal $ modalNewPasswordView aid hash
             sendRedirect LinkMain
         Nothing -> do
             templates <- ctxtemplates <$> get
             addFlashMsg =<< (liftIO $ flashMessagePasswordChangeLinkNotValid templates)
             sendRedirect LinkMain

handlePasswordReminderPost :: ActionID -> MagicHash -> Kontra KontraLink
handlePasswordReminderPost aid hash = do
    muser <- getUserFromActionOfType PasswordReminderID aid hash
    case muser of
         Just user -> handleChangePassword user
         Nothing   -> do
             templates <- ctxtemplates <$> get
             addFlashMsg =<< (liftIO $ flashMessagePasswordChangeLinkNotValid templates)
             return LinkMain
    where
        handleChangePassword user = do
            templates <- ctxtemplates <$> get
            mpassword <- getRequiredField asValidPassword "password"
            mpassword2 <- getRequiredField asDirtyPassword "password2"
            case (mpassword, mpassword2) of
                 (Just password, Just password2) -> do
                     case (checkPasswordsMatch password password2) of
                          Right () -> do
                              dropExistingAction aid
                              passwordhash <- liftIO $ createPassword password
                              update $ SetUserPassword user passwordhash
                              addFlashMsg =<< (liftIO $ flashMessageUserPasswordChanged templates)
                              logUserToContext $ Just user
                              return LinkMain
                          Left flash -> do
                              addFlashMsg =<< (liftIO $ flash templates)
                              addModal $ modalNewPasswordView aid hash
                              return LinkMain
                 _ -> do
                   addModal $ modalNewPasswordView aid hash
                   return LinkMain

handleAccountRemovalGet :: ActionID -> MagicHash -> Kontra Response
handleAccountRemovalGet aid hash = do
    getAccountCreatedBySigningIDAction aid >>= maybe mzero (\action -> do
        let AccountCreatedBySigning _ _ (docid, sigid) token = actionType action
        if hash == token
           then (query $ GetDocumentByDocumentID docid) >>= maybe mzero (\doc -> do
               let sigs = filter ((==) sigid . signatorylinkid) $ documentsignatorylinks doc
               case sigs of
                    [sig] -> do
                        extendActionEvalTimeToOneDayMinimum aid
                        templates <- ctxtemplates <$> get
                        addModal $ modalAccountRemoval templates (documenttitle doc) (LinkAccountCreatedBySigning aid hash) (LinkAccountRemoval aid hash)
                        sendRedirect $ LinkSignDoc doc sig
                    _ -> mzero
               )
           else mzero
        )

handleAccountRemovalFromSign :: ActionID -> MagicHash -> Kontra () 
handleAccountRemovalFromSign aid hash = do
  doc <- handleAccountRemoval' aid hash
  return ()

handleAccountRemovalPost :: ActionID -> MagicHash -> Kontra KontraLink
handleAccountRemovalPost aid hash = do
  doc <- handleAccountRemoval' aid hash
  templates <- ctxtemplates <$> get
  addModal $ modalAccountRemoved templates (documenttitle doc)
  return LinkMain

{- |
    Helper function for performing account removal (these are accounts setup
    by signing), this'll return the document that was removed.
-}
handleAccountRemoval' :: ActionID -> MagicHash -> Kontra Document
handleAccountRemoval' aid hash = do
    getAccountCreatedBySigningIDAction aid >>= maybe mzero (\action -> do
        let AccountCreatedBySigning _ _ (docid, _) token = actionType action
        if hash == token
           then (query $ GetDocumentByDocumentID docid) >>= maybe mzero (\doc -> do
               -- there should be done account and its documents removal, but
               -- since the functionality is not there yet, we just remove
               -- this action to prevent user from account activation and
               -- receiving further reminders
               dropExistingAction aid
               return doc)
           else mzero
        )

getAccountCreatedBySigningIDAction :: ActionID -> Kontra (Maybe Action)
getAccountCreatedBySigningIDAction aid =
    (query $ GetAction aid) >>= maybe (return Nothing) (\action -> do
        -- allow for account created by signing links only
        if (actionTypeID $ actionType action) /= AccountCreatedBySigningID
           then return Nothing
           else if (acbsState $ actionType action) /= ReminderSent
                   then return $ Just action
                   else do
                       now <- liftIO $ getMinutesTime
                       return $ checkValidity now $ Just action)

getUserFromActionOfType :: ActionTypeID -> ActionID -> MagicHash -> Kontra (Maybe User)
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
extendActionEvalTimeToOneDayMinimum :: ActionID -> Kontra ()
extendActionEvalTimeToOneDayMinimum aid = do
    dayAfterNow <- minutesAfter (60*24) <$> liftIO getMinutesTime
    maction <- checkValidity dayAfterNow <$> query (GetAction aid)
    when_ (isNothing maction) $
        update $ UpdateActionEvalTime aid dayAfterNow

dropExistingAction :: ActionID -> Kontra ()
dropExistingAction aid = do
    update $ DeleteAction aid
    return ()

guardXToken :: Kontra ()
guardXToken = do
    Context { ctxxtoken } <- get
    (readM <$> getDataFnM (look "xtoken")) >>= maybe mzero (\xtokenstr -> do
        maybe (readError xtokenstr) (\xtoken -> do
            unless (xtoken == ctxxtoken) (do
                Log.debug $ "xtoken failure: session: " ++ show ctxxtoken
                          ++ " param: " ++ show xtoken
                mzero)
            ) $ readM xtokenstr
        )
    where
        readError v = do
            Log.debug $ "couldn't read xtoken value: " ++ v
            mzero
