{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module User.UserControl where

import Control.Monad.State
import Data.Char
import Data.Functor
import Data.List
import Data.Maybe
import Happstack.Server hiding (simpleHTTP)
import Happstack.State (update, query)
import HSP.XML
import System.Random
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Set as Set

import ActionSchedulerState
import AppView
import Doc.DocState
import InputValidation
import Kontra
import KontraLink
import ListUtil
import Mails.SendMail
import MinutesTime
import Misc
import Redirect
import Templates.Templates (KontrakcjaTemplates)
import User.UserState
import User.UserView


checkPasswords :: BS.ByteString -> BS.ByteString -> Either (KontrakcjaTemplates -> IO FlashMessage) ()
checkPasswords p1 p2 =
    if p1 == p2
       then if isPasswordStrong p1
               then Right ()
               else Left $ flashMessagePasswordNotStrong 
       else Left  flashMessagePasswordsDontMatch      

handleUserGet :: Kontra Response
handleUserGet = do
    ctx <- get
    case (ctxmaybeuser ctx) of
         Just user -> do
             maybefriends <- mapM (query . GetUserByUserID . UserID . unFriend) (userfriends user)
             let friends = map fromJust $ filter isJust maybefriends
             content <- liftIO $ showUser (ctxtemplates ctx) user friends
             renderFromBody ctx TopAccount kontrakcja $ cdata content
         Nothing -> sendRedirect $ LinkLogin NotLogged    

handleUserPost :: Kontra KontraLink
handleUserPost = do
    ctx <- get
    case (ctxmaybeuser ctx) of
         Just user -> do
             infoUpdate <- getUserInfoUpdate
             update $ SetUserInfo (userid user) (infoUpdate $ userinfo user)
             need_to_change_password <- needToChangePassword
             if need_to_change_password
                then tryToChangeUserPassword user
                else addFlashMsg =<< (liftIO $ flashMessageUserDetailsSaved $ ctxtemplates ctx)
             return LinkAccount
         Nothing -> return $ LinkLogin NotLogged
    where
        needToChangePassword = do
            moldpassword <- joinEmpty <$> getField "oldpassword"
            mpassword <- joinEmpty <$> getField "password"
            mpassword2 <- joinEmpty <$> getField "password2"
            return $ any isJust [moldpassword,mpassword,mpassword2]

        tryToChangeUserPassword user = do
            -- Emily ussed input validation here, but I had to drop it since it missed some info
            ctx <- get
            oldpassword <- getFieldUTFWithDefault BS.empty "oldpassword"
            password <- getFieldUTFWithDefault BS.empty "password"
            password2 <- getFieldUTFWithDefault BS.empty "password2"
            if verifyPassword (userpassword user) oldpassword
               then
                   case (checkPasswords password password2) of
                        Right () -> do
                            passwordhash <- liftIO $ createPassword password
                            update $ SetUserPassword user passwordhash
                            addFlashMsg =<< (liftIO $ flashMessageUserDetailsSaved (ctxtemplates ctx))
                        Left f ->  addFlashMsg =<< (liftIO $ f (ctxtemplates ctx))
               else addFlashMsg =<< (liftIO $ flashMessageBadOldPassword (ctxtemplates ctx))

getUserInfoUpdate :: Kontra (UserInfo -> UserInfo)
getUserInfoUpdate  = do
    mfstname          <- getFieldUTF "fstname" 
    msndname          <- getFieldUTF "sndname" 
    mpersonalnumber   <- getFieldUTF "personalnumber"
    maddress          <- getFieldUTF "address" 
    mcity             <- getFieldUTF "city" 
    mcountry          <- getFieldUTF "country" 
    mzip              <- getFieldUTF "zip" 
    mphone            <- getFieldUTF "phone" 
    mcompanyposition  <- getFieldUTF "companyposition" 
    mcompanynumber    <- getFieldUTF "companynumber"
    mcompanyname      <- getFieldUTF "companyname"
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

handleGetSubaccount :: Kontra Response
handleGetSubaccount = withUserGet $ do
    ctx@Context{ctxmaybeuser = Just User{userid}} <- get
    subaccounts <- query $ GetUserSubaccounts userid
    params <- getListParams
    content <- liftIO $ viewSubaccounts (ctxtemplates ctx) (subaccountsSortSearchPage params $ Set.toList subaccounts)
    renderFromBody ctx TopAccount kontrakcja $ cdata content

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

handlePostSubaccount :: Kontra KontraLink
handlePostSubaccount = do
    ctx <- get
    case (ctxmaybeuser ctx) of
         Just user -> do
             add <- isFieldSet "add"
             remove <- isFieldSet "remove"
             case (add,remove) of
                  (True,_) -> do
                      handleCreateSubaccount user
                      return $ LinkSubaccount emptyListParams
                  (_, True) -> return $ LinkSubaccount emptyListParams
                  _ -> LinkSubaccount <$> getListParamsForSearch
         Nothing -> return $ LinkLogin NotLogged

handleCreateSubaccount :: User -> Kontra ()
handleCreateSubaccount user = do
    ctx <- get
    memail <- getOptionalField asValidEmail "email"
    fstname <- fromMaybe "" <$> getField "fstname"
    sndname <- fromMaybe "" <$> getField "sndname"
    let fullname = BS.fromString $ fstname ++ " " ++ sndname
    case memail of
         Just email -> do
             muser <- liftIO $ createUser ctx (ctxhostpart ctx) fullname email (Just user) False
             case muser of
                  Just newuser -> do
                      infoUpdate <- getUserInfoUpdate
                      update $ SetUserInfo (userid newuser) (infoUpdate $ userinfo newuser)
                      return ()
                  Nothing -> addFlashMsg =<< (liftIO $ flashMessageUserWithSameEmailExists (ctxtemplates ctx))
             return ()
         _ -> return ()

handleViralInvite :: Kontra KontraLink
handleViralInvite = withUserPost $ do
    minvitedemail <- getOptionalField asValidEmail "invitedemail"
    case minvitedemail of
         Nothing -> return LoopBack
         Just invitedemail -> do
             ctx <- get
             muser <- query $ GetUserByEmail $ Email invitedemail
             if isJust muser
                then do
                    addFlashMsg =<< (liftIO $ flashMessageUserWithSameEmailExists $ ctxtemplates ctx)
                    return LoopBack
                else do
                    addFlashMsg =<< (liftIO $ flashMessageViralInviteSent $ ctxtemplates ctx)
                    link <- newViralInvitationSentLink (Email invitedemail) (userid . fromJust $ ctxmaybeuser ctx)
                    mail <- liftIO $ viralInviteMail (ctxtemplates ctx) ctx invitedemail link
                    liftIO $ sendMail (ctxmailer ctx) $ mail { fullnameemails = [(BS.empty, invitedemail)] }
                    return LoopBack

randomPassword :: IO BS.ByteString
randomPassword = do
    let letters = ['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z']
    indexes <- liftIO $ replicateM 8 (randomRIO (0, length letters-1))
    return (BS.fromString $ map (letters!!) indexes)

createUser :: Context -> String -> BS.ByteString -> BS.ByteString -> Maybe User -> Bool -> IO (Maybe User)
createUser ctx hostpart fullname email maybesupervisor vip = do
    passwdhash <- createPassword =<< randomPassword
    muser <- update $ AddUser fullname email passwdhash (userid <$> maybesupervisor)
    case muser of
         Just user -> do
             mail <- case maybesupervisor of
                          Nothing -> do
                              al <- newAccountCreatedLink user
                              newUserMail (ctxtemplates ctx) hostpart email fullname al vip
                          Just supervisor -> do
                              al <- newAccountCreatedLink user
                              inviteSubaccountMail (ctxtemplates ctx) hostpart (prettyName  supervisor) (usercompanyname $ userinfo supervisor) email fullname al
             sendMail (ctxmailer ctx) $ mail { fullnameemails = [(fullname, email)] }
             return muser
         Nothing -> return muser

createNewUserByAdmin :: Context -> BS.ByteString -> BS.ByteString -> Maybe MinutesTime -> Maybe String -> IO (Maybe User)
createNewUserByAdmin ctx fullname email freetill custommessage = do
    muser <- createInvitedUser fullname email
    case muser of
         Just user -> do
             when (isJust freetill) $ update $ FreeUserFromPayments user (fromJust freetill)
             now <- liftIO $ getMinutesTime
             update $ SetInviteInfo (ctxmaybeuser ctx) now Admin (userid user)
             chpwdlink <- newAccountCreatedLink user
             mail <- mailNewAccountCreatedByAdmin (ctxtemplates ctx) ctx fullname email chpwdlink custommessage
             sendMail (ctxmailer ctx) $ mail { fullnameemails = [(fullname, email)]}
             return muser
         Nothing -> return muser

createInvitedUser :: BS.ByteString -> BS.ByteString -> IO (Maybe User)
createInvitedUser fullname email = do
    password <- randomPassword
    passwdhash <- createPassword password
    update $ AddUser fullname email passwdhash Nothing

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
withUserGet :: Kontra Response -> Kontra Response
withUserGet action = do
  ctx <- get
  case ctxmaybeuser ctx of
    Just _  -> action
    Nothing -> sendRedirect $ LinkLogin NotLogged

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
checkUserTOSGet :: Kontra Response -> Kontra Response
checkUserTOSGet action = withUserGet $ do
    Context{ctxmaybeuser = (Just (User{userhasacceptedtermsofservice}))} <- get
    case userhasacceptedtermsofservice of
         Nothing -> sendRedirect LinkAcceptTOS
         Just _  -> action

handleAcceptTOSGet :: Kontra Response
handleAcceptTOSGet = withUserGet $ do
    ctx@Context{ctxtemplates} <- get
    tostext <- liftIO $ BS.readFile $ "html/terms.html"
    content <- liftIO $ pageAcceptTOS ctxtemplates tostext
    renderFromBody ctx TopNone kontrakcja $ cdata content

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
             liftIO $ sendMail (ctxmailer ctx) $ emptyMail {
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
         Nothing -> return LinkMain
         Just email -> do
             let content = "name: "    ++ fromMaybe "" name ++ "<BR/>"
                        ++ "email: "   ++ BS.toString email ++ "<BR/>"
                        ++ "phone "    ++ fromMaybe "" phone ++ "<BR/>"
                        ++ "message: " ++ fromMaybe "" message
             liftIO $ sendMail (ctxmailer ctx) $ emptyMail {
                   fullnameemails = [(BS.fromString "info@skrivapa.se",BS.fromString "info@skrivapa.se")]
                 , title = BS.fromString $ "Question"
                 , content = BS.fromString $ content
             }
             return LinkMain

handleAccountSetupGet :: ActionID -> MagicHash -> Kontra Response
handleAccountSetupGet aid hash = do
    now <- liftIO $ getMinutesTime
    maction <- checkValidity now <$> (query $ GetAction aid)
    case maction of
         Just action ->
             case actionType action of
                  ViralInvitationSent _ _ _ token ->
                      if token == hash
                         then activationPage
                         else mzero
                  AccountCreated _ token ->
                      if token == hash
                         then activationPage
                         else mzero
                  _  -> mzero
         Nothing -> -- action has expired, but we may be able to generate it again
             getOptionalField asValidEmail "email" >>= maybe mzero (\email ->
                 (query $ GetUserByEmail $ Email email) >>= maybe mzero (\user ->
                     if isNothing $ userhasacceptedtermsofservice user
                        then do
                            ctx <- get
                            content <- liftIO $ activatePageViewNotValidLink (ctxtemplates ctx) $ BS.toString email
                            renderFromBody ctx TopNone kontrakcja $ cdata content
                        else mzero
                     )
                 )
    where
        activationPage = do
            extendActionEvalTimeToOneDayMinimum aid
            ctx <- get
            tostext <- liftIO $ BS.readFile "html/terms.html"
            content <- liftIO $ activatePageView (ctxtemplates ctx) (BS.toString tostext) ""
            renderFromBody ctx TopNone kontrakcja $ cdata content

handleAccountSetupPost :: ActionID -> MagicHash -> Kontra KontraLink
handleAccountSetupPost aid hash = do
    now <- liftIO $ getMinutesTime
    maction <- checkValidity now <$> (query $ GetAction aid)
    case maction of
         Just action ->
             case actionType action of
                  ViralInvitationSent email invtime inviterid token ->
                      if token == hash
                         then getUserForViralInvite now email invtime inviterid
                              >>= handleActivate
                         else mzero
                  AccountCreated uid token ->
                      if token == hash
                         then (query $ GetUserByUserID uid) >>= handleActivate
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
                            liftIO $ sendMail (ctxmailer ctx) $ mail { fullnameemails = [(email, email)] }
                            addFlashMsg =<< (liftIO $ flashMessageNewActivationLinkSend  (ctxtemplates ctx))
                            return LinkMain
                        else mzero
                     )
                 )
    where
        handleActivate Nothing = mzero -- this should never happen, but in case...
        handleActivate (Just user) = do
            ctx <- get
            let getUserField = getDefaultedField BS.empty
            mtos <- getDefaultedField False asValidCheckBox "tos"
            mfname <- getUserField asValidName "fname"
            mlname <- getUserField asValidName "lname"
            mcompanyname <- getUserField asValidName "companyname"
            mcompanyposition <- getUserField asValidName "companyposition"
            mpassword <- getRequiredField asValidPassword "password"
            mpassword2 <- getRequiredField asValidPassword "password2"
            case (mtos, mfname, mlname, mcompanyname, mcompanyposition, mpassword, mpassword2) of
                 (Just tos, Just fname, Just lname, Just companyname, Just companytitle, Just password, Just password2) -> do
                     case checkPasswords password password2 of
                          Right () ->
                              if tos
                                 then do
                                     passwordhash <- liftIO $ createPassword password
                                     update $ SetUserPassword user passwordhash
                                     update $ AcceptTermsOfService (userid user) (ctxtime ctx)
                                     update $ SetUserInfo (userid user) $ (userinfo user) {
                                           userfstname = fname
                                         , usersndname = lname
                                         , usercompanyname  = companyname
                                         , usercompanyposition = companytitle
                                     }
                                     now <- liftIO getMinutesTime
                                     update $ AddFreePaymentsForInviter now user
                                     dropExistingAction aid
                                     addFlashMsg =<< (liftIO $ flashMessageUserActivated $ ctxtemplates ctx)
                                     logUserToContext $ Just user
                                     return LinkMain
                                 else do
                                     addFlashMsg =<< (liftIO $ flashMessageMustAcceptTOS $ ctxtemplates ctx)
                                     return LoopBack
                          Left flash -> do
                              addFlashMsg =<< (liftIO $ flash (ctxtemplates ctx))
                              return LoopBack
                 _ -> return LoopBack

        getUserForViralInvite now invitedemail invitationtime inviterid = do
            muser <- liftIO $ createInvitedUser BS.empty $ unEmail invitedemail
            case muser of
                 Just user -> do -- user created, we need to fill in some info
                     minviter <- query $ GetUserByUserID inviterid
                     update $ FreeUserFromPayments user $ (60*24*60) `minutesAfter` now
                     update $ SetInviteInfo minviter invitationtime Viral (userid user)
                     return muser
                 Nothing -> do -- user already exists, get her
                     query $ GetUserByEmail invitedemail

handlePasswordReminderGet :: ActionID -> MagicHash -> Kontra Response
handlePasswordReminderGet aid hash = do
    muser <- getUserFromActionOfType PasswordReminderID aid hash
    case muser of
         Just _ -> do
             extendActionEvalTimeToOneDayMinimum aid
             ctx <- get
             content <- liftIO $ newPasswordPageView $ ctxtemplates ctx
             renderFromBody ctx TopNone kontrakcja $ cdata content
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
            mpassword <- getOptionalField asValidPassword "password"
            mpassword2 <- getOptionalField asDirtyPassword "password2"
            case (mpassword, mpassword2) of
                 (Just password, Just password2) -> do
                     case (checkPasswords password password2) of
                          Right () -> do
                              dropExistingAction aid
                              passwordhash <- liftIO $ createPassword password
                              update $ SetUserPassword user passwordhash
                              addFlashMsg =<< (liftIO $ flashMessageUserPasswordChanged templates)
                              logUserToContext $ Just user
                              return LinkMain
                          Left flash -> do
                              addFlashMsg =<< (liftIO $ flash templates)
                              return LoopBack
                 _ -> return LoopBack

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
                 PasswordReminder uid token -> verifyToken token uid
                 AccountCreated uid token   -> verifyToken token uid
                 _                          -> return Nothing
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
