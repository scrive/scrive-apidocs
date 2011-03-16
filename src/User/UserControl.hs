module User.UserControl where

import Control.Monad.State
import Control.Monad.Trans (liftIO,MonadIO,lift)
import Control.Monad
import AppView
import Data.Maybe
import Doc.DocState
import Doc.DocView
import Happstack.Data.IxSet 
import Happstack.Server hiding (simpleHTTP)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.State (Update,update,query)
import Happstack.Util.Common (readM)
import KontraLink
import Misc
import Mails.SendMail
import Session
import System.Log.Logger
import System.Process
import System.Random
import Kontra
import Redirect
import User.UserState
import User.UserView
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy as LS
import qualified Data.ByteString.Lazy.UTF8 as LS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as Set
import qualified HSP
import Templates.Templates (KontrakcjaTemplates)
import HSP.XML
import MinutesTime
import InputValidation

handleUserPasswordPost :: Kontra KontraLink
handleUserPasswordPost = do
  ctx@Context{ctxmaybeuser = Just user@User{userid}} <- get
  moldpassword <- getOptionalField asDirtyPassword "oldpassword"
  mpassword <- getOptionalField asValidPassword "password"
  mpassword2 <- getOptionalField asDirtyPassword "password2"
  case (moldpassword, mpassword, mpassword2) of
    (Just oldpassword, Just password, Just password2) -> do
      if verifyPassword (userpassword user) oldpassword
        then
              case (checkPasswords password password2) of
               Right () -> do
                            passwordhash <- liftIO $ createPassword password
                            update $ SetUserPassword user passwordhash
                            addFlashMsg =<< (liftIO $ flashMessageUserDetailsSaved (ctxtemplates ctx))
               Left f ->  addFlashMsg =<< (liftIO $ f (ctxtemplates ctx))
        else  addFlashMsg =<< (liftIO $ flashMessageBadOldPassword (ctxtemplates ctx))
      return LinkAccount
    _ -> return LinkAccount


checkPasswords::BS.ByteString -> BS.ByteString -> Either (KontrakcjaTemplates -> IO FlashMessage) ()
checkPasswords p1 p2 =  if p1 == p2
                        then 
                          if isPasswordStrong p1
                          then Right ()
                          else Left $ flashMessagePasswordNotStrong 
                        else Left  flashMessagePasswordsDontMatch      
                   
handleUserGet :: Kontra Response
handleUserGet = do
  ctx@(Context {ctxmaybeuser = Just user}) <- get
  maybefriends <- mapM (query . GetUserByUserID . UserID . unFriend) (userfriends user)
  let friends = map fromJust $ filter isJust maybefriends
  content <- liftIO $ showUser (ctxtemplates ctx) user friends           
  renderFromBody ctx TopAccount kontrakcja $ cdata content

handleUserPost :: Kontra KontraLink
handleUserPost = do
  ctx@Context{ctxmaybeuser = Just user@User{userid},ctxtime} <- get
  let getUserField = getDefaultedField BS.empty
  mfname <- getUserField asValidName "fname"
  mlname <- getUserField asValidName "lname"
  mcompanyname <- getUserField asValidCompanyName "companyname"
  mposition <- getUserField asValidPosition "companyposition"
  mcompanynumber <- getUserField asValidCompanyNumber "companynumber"
  minvoiceaddress <- getUserField asValidAddress "invoiceaddress"
  mnewvieweremail <- getUserField asValidEmail "newvieweremail"
  case (mfname, mlname, mcompanyname, mposition, mcompanynumber, minvoiceaddress, mnewvieweremail) of
    (Just fname, Just lname, Just companyname, Just companyposition, Just companynumber, Just invoiceaddress, Just newvieweremail) -> do
      when (BS8.length newvieweremail > 0) $ do
         avereturn <- update $ AddViewerByEmail userid $ Email newvieweremail
         case avereturn of
           Left msg -> addFlashMsg $ toFlashMsg OperationFailed msg
           Right _  -> return ()
         return ()
  
      newuser <- update $ SetUserDetails userid fname lname companyname companyposition companynumber invoiceaddress
      addFlashMsg =<< (liftIO $ flashMessageUserDetailsSaved (ctxtemplates ctx))

      return LinkAccount
    _ -> return LinkAccount

handleGetSubaccount :: Kontra Response
handleGetSubaccount = do
  ctx@Context { ctxmaybeuser = Just user@User { userid } } <- get
  subaccounts <- query $ GetUserSubaccounts userid
  content <- liftIO $ viewSubaccounts (ctxtemplates ctx) (Set.toList subaccounts)
  renderFromBody ctx TopAccount kontrakcja $ cdata content

handlePostSubaccount :: Kontra KontraLink
handlePostSubaccount = do
  ctx@Context { ctxmaybeuser = Just (user@User { userid }), ctxhostpart } <- get
  create <- getDataFn (look "create")
  case create of
      (Right _) -> handleCreateSubaccount ctx
      Left _ -> return LinkSubaccount

handleCreateSubaccount :: Context -> Kontra KontraLink
handleCreateSubaccount ctx@Context { ctxmaybeuser = Just (user@User { userid }), ctxhostpart, ctxtemplates } = do
  mfullname <- getDefaultedField BS.empty asValidName "fullname"
  memail <- getOptionalField asValidEmail "email"
  case (mfullname, memail) of
    (Just fullname, Just email) -> do
      muser <- liftIO $ createUser ctx ctxhostpart fullname email Nothing True (Just user) False
      when (isNothing muser) $ addFlashMsg =<< (liftIO $ flashMessageUserWithSameEmailExists ctxtemplates)
      return LinkSubaccount
    _ -> return LinkSubaccount

handleViralInvite :: Kontra KontraLink
handleViralInvite = do
    ctx <- get
    minvitedemail <- getOptionalField asValidEmail "invitedemail"
    case minvitedemail of
        Nothing ->
            return LoopBack
        Just invitedemail -> do
                    maccount <- liftIO $ createUserForViralInvite ctx invitedemail
                    case maccount of
                     Just account -> do
                          addFlashMsg =<< (liftIO $ flashMessageViralInviteSent $ ctxtemplates ctx)   
                          now <- liftIO $ getMinutesTime
                          update $ FreeUserFromPayments account ((60*24*60) `minutesAfter` now) 
                          update $ SetInviteInfo (ctxmaybeuser ctx) now Viral (userid account)
                          return LoopBack
                     Nothing -> do
                          addFlashMsg =<< (liftIO $ flashMessageUserWithSameEmailExists $ ctxtemplates ctx)
                          return LoopBack

randomPassword :: IO BS.ByteString
randomPassword = do
    let letters =['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z']
    indexes <- liftIO $ replicateM 8 (randomRIO (0,length letters-1))
    return (BS.fromString $ map (letters!!) indexes)

createUser ::  Context -> String -> BS.ByteString -> BS.ByteString -> Maybe BS.ByteString -> Bool -> Maybe User -> Bool -> IO (Maybe User)
createUser ctx hostpart fullname email maybepassword isnewuser maybesupervisor vip =
  case maybepassword of
    Nothing -> do
      password <- randomPassword
      createUser1 ctx hostpart fullname email password isnewuser maybesupervisor vip
    Just x ->
      createUser1 ctx hostpart fullname email x isnewuser maybesupervisor vip

createUserForViralInvite :: Context -> BS.ByteString -> IO (Maybe User)
createUserForViralInvite ctx invitedemail =
    do
      muser <- createInvitedUser BS.empty invitedemail
      case muser of
        Just user -> do
                      chpwdlink <- unloggedActionLink (user)
                      mail <- viralInviteMail (ctxtemplates ctx) ctx invitedemail chpwdlink
                      sendMail (ctxmailer ctx) $ mail { fullnameemails = [(BS.empty, invitedemail)]}
                      return muser
        Nothing -> return muser

createNewUserByAdmin :: Context -> BS.ByteString -> BS.ByteString -> Maybe MinutesTime -> Maybe String -> IO (Maybe User)
createNewUserByAdmin ctx fullname email freetill custommessage =
     do
      muser <- createInvitedUser fullname email
      case muser of 
       Just user -> do
                    when (isJust freetill) $ update $ FreeUserFromPayments user (fromJust freetill)
                    now <- liftIO $ getMinutesTime
                    update $ SetInviteInfo (ctxmaybeuser ctx) now Admin (userid user)
                    chpwdlink <- unloggedActionLink (user)
                    mail <- mailNewAccountCreatedByAdmin (ctxtemplates ctx) ctx fullname email chpwdlink custommessage
                    sendMail (ctxmailer ctx) $ mail { fullnameemails = [(fullname, email)]} 
                    return muser
       Nothing -> return muser

createInvitedUser :: BS.ByteString -> BS.ByteString -> IO (Maybe User)
createInvitedUser fullname email =
     do
      password <- randomPassword
      passwdhash <- createPassword password
      update $ AddUser fullname email passwdhash Nothing



createUser1 :: Context -> String -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Bool -> Maybe User -> Bool -> IO (Maybe User)
createUser1 ctx hostpart fullname email password isnewuser maybesupervisor vip = do
  passwdhash <- createPassword password
  muser <- update $ AddUser fullname email passwdhash (fmap userid maybesupervisor)
  case muser of 
   Just user -> do
        al <- unloggedActionLink (user)
        mail <- case maybesupervisor of
                  Nothing -> if not isnewuser
                             then passwordChangeMail (ctxtemplates ctx) hostpart email fullname al
                             else newUserMail (ctxtemplates ctx) hostpart email fullname al vip
                  Just supervisor -> inviteSubaccountMail (ctxtemplates ctx) hostpart (prettyName  supervisor) (usercompanyname $ userinfo supervisor)
                                        email fullname al
        sendMail (ctxmailer ctx) $ mail { fullnameemails = [(fullname, email)]}
        return muser
   Nothing -> return muser     

{- |
   Guard against a POST with no logged in user.
   If they are not logged in, redirect to login page.
-}
withUserPost :: Kontra KontraLink -> Kontra KontraLink
withUserPost action = do
  ctx <- get
  case ctxmaybeuser ctx of
    Just user -> action
    Nothing   -> return $ LinkLogin NotLogged

{- |
   Guard against a GET with no logged in user.
   If they are not logged in, redirect to login page.
-}
withUserGet :: Kontra Response -> Kontra Response
withUserGet action = do
  ctx <- get
  case ctxmaybeuser ctx of
    Just user -> action
    Nothing   -> sendRedirect $ LinkLogin NotLogged
    
{- | 
     Takes a document and a action
     Runs an action only if current user (from context) is author of document
| -}
withDocumentAuthor :: Document -> Kontra a -> Kontra a
withDocumentAuthor document action = do
                                     ctx <- get
                                     case fmap (isAuthor document) (ctxmaybeuser ctx) of
                                      Just True -> action
                                      Nothing   -> mzero                   
{- |
   Guard against a GET with logged in users who have not signed the TOS agreement.
   If they have not, redirect to their account page.
-}
checkUserTOSGet :: Kontra Response -> Kontra Response
checkUserTOSGet action =
  withUserGet $ do
    ctx@(Context {ctxmaybeuser = (Just (User {userhasacceptedtermsofservice}))}) <- get
    case userhasacceptedtermsofservice of
      Nothing -> sendRedirect LinkAcceptTOS
      Just _  -> action

handleAcceptTOSGet = withUserGet $ do
      ctx <- get
      tostext <- liftIO $ BS.readFile $ "html/terms.html"
      content <- liftIO $ pageAcceptTOS (ctxtemplates ctx) tostext
      renderFromBody ctx TopNone kontrakcja $ cdata content

handleAcceptTOSPost :: Kontra KontraLink
handleAcceptTOSPost = do
  ctx@Context{ctxmaybeuser = Just user@User{userid},ctxtime} <- get
  tos <- getDefaultedField False asValidCheckBox "tos"
  
  case tos of
    (Just True) -> do
      update $ AcceptTermsOfService userid ctxtime
      addFlashMsg =<< (liftIO $ flashMessageUserDetailsSaved (ctxtemplates ctx))
      return LinkMain
    (Just False) -> do
      addFlashMsg =<< (liftIO $ flashMessageMustAcceptTOS (ctxtemplates ctx))
      return LinkAcceptTOS
    Nothing -> return LinkAcceptTOS


handleRequestAccount :: Kontra KontraLink
handleRequestAccount = do 
                        ctx<- get
                        memail <- getRequiredField asValidEmail "email"
                        case memail of
                          Nothing -> return LinkMain
                          Just email -> do
                            liftIO $ sendMail (ctxmailer ctx) $ emptyMail 
                                                                        { fullnameemails = [(BS.fromString "prelaunch@skrivapa.se",BS.fromString "prelaunch@skrivapa.se")],
                                                                           title = BS.fromString $ "New account request",
                                                                           content = BS.fromString $ "Request from addres " ++ (BS.toString email)
                                                                        }
                            addFlashMsg =<< (liftIO $ flashMessageAccountRequestSend (ctxtemplates ctx))                       
                            return LinkMain -- Something should happend here

handleQuestion :: Kontra KontraLink
handleQuestion = do
                  ctx<- get
                  name <- getField "name" 
                  memail <- getDefaultedField BS.empty asValidEmail "email"
                  phone <- getField "phone" 
                  message <- getField "message"
                  case memail of
                    Nothing -> return LinkMain
                    (Just email) -> do 
                      let  content =   "name: "      ++ (fromMaybe "" name)    ++ "<BR/>" ++
                                       "email: "   ++ (BS.toString email)   ++ "<BR/>" ++
                                       "phone "    ++ (fromMaybe "" phone)   ++ "<BR/>" ++
                                       "message: " ++ (fromMaybe "" message) 
                      liftIO $ sendMail (ctxmailer ctx) $ emptyMail 
                                                          { fullnameemails = [(BS.fromString "info@skrivapa.se",BS.fromString "info@skrivapa.se")],
                                                            title = BS.fromString $ "Question",
                                                            content = BS.fromString $ content }
                      return LinkMain
                                                                             
                                                

unloggedActionPage::String->String -> Kontra Response                         
unloggedActionPage sid mh = do
                      muser <- userFromExternalSessionData sid mh 
                      name <- fmap  (fromMaybe "")  $ getField "name"
                      email <- fmap  (fromMaybe "") $ getField "email"
                      case muser of 
                        Just user -> if (isNothing $ userhasacceptedtermsofservice user)
                                     then activatePage muser (name,email)                           
                                     else newPasswordPage muser  (name,email)
                        Nothing -> do 
                                    muserFromEmail <- query $ GetUserByEmail $ Email $ BS.fromString email
                                    case (muserFromEmail) of 
                                     Just userFromEmail -> if (isNothing $ userhasacceptedtermsofservice userFromEmail)
                                                            then activatePage Nothing (name,email) 
                                                            else newPasswordPage Nothing (name,email) 
                                     Nothing -> newPasswordPage Nothing (name,email)              
                  
handleUnloggedAction::String->String -> Kontra KontraLink
handleUnloggedAction sid mh = do
                      muser <- userFromExternalSessionData sid mh 
                      case muser of 
                        Just user -> if (isNothing $ userhasacceptedtermsofservice user)
                                     then handleActivate muser (dropExternalSession sid mh)                           
                                     else handleChangePassword muser  (dropExternalSession sid mh)
                        Nothing -> do
                                     resendActivate <- getField "resendActivate"
                                     if (isJust resendActivate)
                                      then handleActivate Nothing (dropExternalSession sid mh)
                                      else return LoopBack             


activatePage::Maybe User -> (String,String) ->  Kontra Response                                    
activatePage muser (name,email) = do
                   ctx <- get
                   case muser of 
                    Just user -> do    
                                  tostext <- liftIO $ BS.readFile $ "html/terms.html"
                                  content <- liftIO $ activatePageView (ctxtemplates ctx) (BS.toString tostext) name
                                  renderFromBody ctx TopNone kontrakcja $ cdata content
                    Nothing -> do 
                                  content <- liftIO $ activatePageViewNotValidLink (ctxtemplates ctx) email
                                  renderFromBody ctx TopNone kontrakcja $ cdata content

handleChangePassword::(Maybe User) -> Kontra () -> Kontra KontraLink
handleChangePassword muser dropSessionAction = do
                         ctx <- get
                         case muser of 
                            Just user -> 
                               do
                                 mpassword <- getOptionalField asValidPassword "password"
                                 mpassword2 <- getOptionalField asDirtyPassword "password2"
                                 case (mpassword, mpassword2) of
                                   (Just password, Just password2) -> do
                                     case (checkPasswords password password2) of
                                      Right () ->
                                        do
                                          passwordhash <- liftIO $ createPassword password
                                          update $ SetUserPassword user passwordhash
                                          addFlashMsg =<< (liftIO $ flashMessageUserPasswordChanged  (ctxtemplates ctx))
                                          dropSessionAction
                                          logUserToContext $ Just user
                                          return LinkMain  
                                      Left f -> 
                                        do
                                          addFlashMsg =<< (liftIO $ f (ctxtemplates ctx))
                                          return LoopBack
                                   _ -> return LoopBack
                            Nothing -> return LoopBack
                                       
                                         
newPasswordPage::Maybe User -> (String,String) -> Kontra Response
newPasswordPage muser (name,email)= do
                       ctx <- get
                       case muser of 
                        Just user -> do    
                                  content <- liftIO $ newPasswordPageView (ctxtemplates ctx)
                                  renderFromBody ctx TopNone kontrakcja $ cdata content
                        Nothing -> do 
                                 addFlashMsg =<< (liftIO $ flashMessagePasswordChangeLinkNotValid (ctxtemplates ctx)) 
                                 sendRedirect LinkMain        
                                 
handleActivate::(Maybe User) ->Kontra () -> Kontra KontraLink
handleActivate muser dropSessionAction = do
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
                        case muser of 
                         Just user -> do    
                            case (checkPasswords password password2) of
                             Right () ->  if (tos)
                                           then do  
                                            passwordhash <- liftIO $ createPassword password
                                            update $ SetUserPassword user passwordhash
                                            update $ AcceptTermsOfService (userid user) (ctxtime ctx)
                                            update $ SetUserInfo (userid user) $ (userinfo user) {userfstname = fname,
                                                                                                  usersndname = lname,
                                                                                                  usercompanyname  = companyname, 
                                                                                                  usercompanyposition = companytitle
                                                                                                  }
                                            now <- liftIO getMinutesTime
                                            update $ AddFreePaymentsForInviter now user
                                            dropSessionAction
                                            addFlashMsg =<< (liftIO $ flashMessageUserActivated (ctxtemplates ctx))
                                            logUserToContext $ Just user
                                            return LinkMain 
                                           else do
                                            addFlashMsg =<< (liftIO $ flashMessageMustAcceptTOS (ctxtemplates ctx)) 
                                            return LoopBack
                             Left f ->  do
                                         addFlashMsg =<< (liftIO $ f (ctxtemplates ctx))          
                                         return LoopBack  
                         Nothing -> do 
                                     memail <- getOptionalField asDirtyEmail "email"
                                     case memail of  
                                      Just email -> do
                                                     muser <- query $ GetUserByEmail $ Email email
                                                     case  muser of
                                                      Just user -> 
                                                          if (isNothing $ userhasacceptedtermsofservice user) 
                                                          then  
                                                               do  al <- liftIO $ unloggedActionLink user
                                                                   mail <-  liftIO $ newUserMail (ctxtemplates ctx) (ctxhostpart ctx) email email al False
                                                                   liftIO $ sendMail (ctxmailer ctx) $ mail { fullnameemails = [(email, email)]}
                                                                   addFlashMsg =<< (liftIO $ flashMessageNewActivationLinkSend  (ctxtemplates ctx)) 
                                                                   return LinkMain
                                                          else do
                                                                addFlashMsg =<< (liftIO $ flashMessageUserAlreadyActivated (ctxtemplates ctx)) 
                                                                return LinkMain
                                                      Nothing -> do
                                                                 addFlashMsg =<< (liftIO $ flashMessageNoSuchUserExists (ctxtemplates ctx)) 
                                                                 return LoopBack
                                                              
                                      Nothing -> do
                                         addFlashMsg =<< (liftIO $ flashMessageActivationLinkNotValid (ctxtemplates ctx)) 
                                         return LinkMain   
                      _ -> return LinkMain

userFromExternalSessionData ::String -> String -> Kontra (Maybe User)
userFromExternalSessionData sid mh = do
                                      msession <- sequenceMM $ 
                                           do
                                            sid' <- readM sid
                                            mh' <- readM mh
                                            return $ findSession sid' mh'
                                      muser <-  sequenceMM $ 
                                           do
                                            session <- msession
                                            userId <- getSessionUserID session
                                            return $ liftIO $ query $ GetUserByUserID userId
                                      return muser
                                      
dropExternalSession::String -> String -> Kontra ()
dropExternalSession sid _ = fromMaybe (return ()) $ fmap (liftIO . dropSession) (readM sid)
