module UserControl where
import Control.Monad.State
import Control.Monad.Trans (liftIO,MonadIO,lift)
import Control.Monad
import AppView
import Data.Maybe
import DocState
import DocView
import Happstack.Data.IxSet 
import Happstack.Server hiding (simpleHTTP)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.State (Update,update,query)
import Happstack.Util.Common (readM)
import KontraLink
import Misc
import Mails.SendMail(Mail,sendMail,fullnameemails)
import Session
import System.Log.Logger
import System.Process
import System.Random
import User
import UserState
import UserView
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy as LS
import qualified Data.ByteString.Lazy.UTF8 as LS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as Set
import qualified HSP
import Templates.Templates (KontrakcjaTemplates)
import HSP.XML

handleUserPasswordPost :: Kontra KontraLink
handleUserPasswordPost = do
  ctx@Context{ctxmaybeuser = Just user@User{userid}} <- get
  oldpassword <- fmap (fromMaybe "") $ getField "oldpassword"
  password <- fmap (fromMaybe "") $ getField "password"
  password2 <- fmap (fromMaybe "") $ getField "password2"
  if verifyPassword (userpassword user) $ BS.fromString oldpassword
    then
          case (checkPasswords password password2) of
           Right () -> do
                        passwordhash <- liftIO $ createPassword $ BS.fromString password
                        update $ SetUserPassword user passwordhash
                        addFlashMsgHtmlFromTemplate =<< (liftIO $ flashMessageUserDetailsSaved  (ctxtemplates ctx))
           Left f ->  addFlashMsgText =<< (liftIO $ f (ctxtemplates ctx))
    else  addFlashMsgText =<< (liftIO $ flashMessageBadOldPassword  (ctxtemplates ctx))
  return LinkAccount


checkPasswords::String -> String -> Either (KontrakcjaTemplates -> IO String) ()
checkPasswords p1 p2 =  if p1 == p2
                        then 
                          if isPasswordStrong $ BS.fromString p1
                          then Right ()
                          else Left  flashMessagePasswordNotStrong 
                        else Left  flashMessagePasswordsDontMatch 

handleChangePassword::String->String -> Kontra KontraLink
handleChangePassword sid mh = do
                              ctx <- get
                              muser <- userFromExternalSessionData sid mh
                              case muser of 
                                Just user -> 
                                   do
                                     password <- fmap (fromMaybe "") $ getField "password"
                                     password2 <- fmap (fromMaybe "") $ getField "password2"
                                     case (checkPasswords password password2) of
                                      Right () ->
                                        do
                                          passwordhash <- liftIO $ createPassword $ BS.fromString password
                                          update $ SetUserPassword user passwordhash
                                          addFlashMsgHtmlFromTemplate =<< (liftIO $ flashMessageUserPasswordChanged  (ctxtemplates ctx))
                                          dropExternalSession sid mh
                                          return LinkMain  
                                      Left f -> 
                                        do
                                          addFlashMsgText =<< (liftIO $ f (ctxtemplates ctx))
                                          return LoopBack
                                Nothing -> return LoopBack
                                
userFromExternalSessionData ::String -> String -> Kontra (Maybe User)
userFromExternalSessionData sid mh = do
                                      msession <- sequenceMM $ 
                                           do
                                            sid' <- maybeRead sid
                                            mh' <- maybeRead mh
                                            return $ findSession sid' mh'
                                      muser <-  sequenceMM $ 
                                           do
                                            session <- msession
                                            userId <- getSessionUserID session
                                            return $ liftIO $ query $ GetUserByUserID userId
                                      return muser
                                      
dropExternalSession::String -> String -> Kontra ()
dropExternalSession sid _ = fromMaybe (return ()) $ fmap (liftIO . dropSession) (maybeRead sid)
                                         
                                         
newPasswordPage::String->String -> Kontra Response
newPasswordPage sid mh= do
                   ctx <- get
                   muser <- userFromExternalSessionData sid mh 
                   case muser of 
                    Just user -> do    
                                  content <- liftIO $ newPasswordPageView (ctxtemplates ctx)
                                  renderFromBody ctx TopNone kontrakcja $ cdata content
                    Nothing -> do 
                                 addFlashMsgText =<< (liftIO $ flashMessagePasswordChangeLinkNotValid (ctxtemplates ctx)) 
                                 sendRedirect LinkMain              
                   
handleUserGet :: Kontra Response
handleUserGet = do
  ctx@(Context {ctxmaybeuser = Just user}) <- get
  mms <- query $ GetUserByUserID $ UserID $ unDMS (userdefaultmainsignatory user)
  maybefriends <- mapM (query . GetUserByUserID . UserID . unFriend) (userfriends user)
  let friends = map fromJust $ filter isJust maybefriends
  let ms = case mms of
             Just m -> m
             Nothing -> user
  content <- liftIO $ showUser (ctxtemplates ctx) user friends           
  renderFromBody ctx TopAccount kontrakcja $ cdata content

handleUserPost :: Kontra KontraLink
handleUserPost = do
  ctx@Context{ctxmaybeuser = Just user@User{userid},ctxtime} <- get
  fullname <- g "fullname"
  companyname <- g "companyname"
  companynumber <- g "companynumber"
  invoiceaddress <- g "invoiceaddress"
  newvieweremail <- g "newvieweremail"

  when (BS8.length newvieweremail > 0) $ do
     avereturn <- update $ AddViewerByEmail userid $ Email newvieweremail
     case avereturn of
       Left msg -> addFlashMsgText msg
       Right _  -> return ()
     return ()
  
  newuser <- update $ SetUserDetails userid fullname companyname companynumber invoiceaddress
  addFlashMsgHtmlFromTemplate =<< (liftIO $ flashMessageUserDetailsSaved (ctxtemplates ctx))

  return LinkAccount


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
  remove <- getDataFn (look "remove")
  case (create, remove) of
      (Just _, _) -> handleCreateSubaccount ctx
      (_, Just _) -> handleRemoveSubaccounts ctx
      _ -> return LinkSubaccount

handleCreateSubaccount :: Context -> Kontra KontraLink
handleCreateSubaccount ctx@Context { ctxmaybeuser = Just (user@User { userid }), ctxhostpart, ctxtemplates } = do
  fullname <- g "fullname"
  email <- g "email"
  muser <- liftIO $ createUser ctx ctxhostpart fullname email Nothing True (Just user)
  when (isNothing muser) $ addFlashMsgText =<< (liftIO $ flashMessageUserWithSameEmailExists ctxtemplates)
  return LinkSubaccount

handleRemoveSubaccounts :: Context -> Kontra KontraLink
handleRemoveSubaccounts ctx@Context { ctxmaybeuser = Just (user), ctxhostpart } = do
  subidstrings <- getDataFnM (lookInputList "doccheck")
  let Just subids = sequence $ map (readM . LS.toString) subidstrings
  removed <- liftIO $ removeSubaccounts user subids
  return LinkSubaccount

removeSubaccounts :: User -> [UserID] -> IO ()
removeSubaccounts _ [] = return ()
removeSubaccounts user (subid:xs) = do
    removeSubaccount user subid
    removeSubaccounts user xs

removeSubaccount user subId = do
  Just subuser <- query $ GetUserByUserID subId
  takeover <- update $ FragileTakeOverDocuments user subuser
  maybeuser <- update $ FragileDeleteUser subId
  return ()

randomPassword :: IO BS.ByteString
randomPassword = do
    let letters =['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z']
    indexes <- liftIO $ replicateM 8 (randomRIO (0,length letters-1))
    return (BS.fromString $ map (letters!!) indexes)

createUser ::  Context -> String -> BS.ByteString -> BS.ByteString -> Maybe BS.ByteString -> Bool -> Maybe User -> IO (Maybe User)
createUser ctx hostpart fullname email maybepassword isnewuser maybesupervisor =
  case maybepassword of
    Nothing -> do
      password <- randomPassword
      createUser1 ctx hostpart fullname email password isnewuser maybesupervisor
    Just x ->
      createUser1 ctx hostpart fullname email x isnewuser maybesupervisor

createNewUserByAdmin :: Context -> BS.ByteString -> BS.ByteString -> IO (Maybe User)
createNewUserByAdmin ctx fullname email =
     do
      password <- randomPassword
      passwdhash <- createPassword password
      muser <- update $ AddUser fullname email passwdhash Nothing
      case muser of 
       Just user -> do
                    chpwdlink <- activateLink $ userid user
                    mail <- mailNewAccountCreatedByAdmin (ctxtemplates ctx) ctx fullname email chpwdlink
                    sendMail (ctxmailsconfig ctx) $ mail { fullnameemails = [(fullname, email)]} 
                    return muser
       Nothing -> return muser


createUser1 :: Context -> String -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Bool -> Maybe User -> IO (Maybe User)
createUser1 ctx hostpart fullname email password isnewuser maybesupervisor = do
  passwdhash <- createPassword password
  muser <- update $ AddUser fullname email passwdhash (fmap userid maybesupervisor)
  case muser of 
   Just user -> do
        chpwdlink <- changePasswordLink (userid user)
        mail <- case maybesupervisor of
                  Nothing -> if not isnewuser
                             then passwordChangeMail (ctxtemplates ctx) hostpart email fullname chpwdlink
                             else do
                                   al <- activateLink $ userid user
                                   newUserMail (ctxtemplates ctx) hostpart email fullname al
                  Just supervisor -> inviteSubaccountMail (ctxtemplates ctx) hostpart (prettyName  supervisor) (usercompanyname $ userinfo supervisor)
                                        email fullname chpwdlink
        sendMail (ctxmailsconfig ctx) $ mail { fullnameemails = [(fullname, email)]}
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
    Nothing   -> return LinkLogin

{- |
   Guard against a GET with no logged in user.
   If they are not logged in, redirect to login page.
-}
withUserGet :: Kontra Response -> Kontra Response
withUserGet action = do
  ctx <- get
  case ctxmaybeuser ctx of
    Just user -> action
    Nothing   -> sendRedirect LinkLogin
    
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
      tostext <- liftIO $ BS.readFile $ "html/termsofuse.html"
      content <- liftIO $ pageAcceptTOS (ctxtemplates ctx) tostext
      renderFromBody ctx TopNone kontrakcja $ cdata content

handleAcceptTOSPost :: Kontra KontraLink
handleAcceptTOSPost = do
  ctx@Context{ctxmaybeuser = Just user@User{userid},ctxtime} <- get
  tos <- getDataFn' (look "tos")
  
  if isJust tos
    then do
      update $ AcceptTermsOfService userid ctxtime
      addFlashMsgHtmlFromTemplate =<< (liftIO $ flashMessageUserDetailsSaved  (ctxtemplates ctx))
      return LinkMain
    else do
      addFlashMsgText =<< (liftIO $ flashMessageMustAcceptTOS  (ctxtemplates ctx))
      return LinkAcceptTOS
      
activatePage::String->String -> Kontra Response                                    
activatePage sid mh=  do
                   ctx <- get
                   muser <- userFromExternalSessionData sid mh 
                   case muser of 
                    Just user -> do    
                                  tostext <- liftIO $ BS.readFile $ "html/termsofuse.html"
                                  content <- liftIO $ activatePageView (ctxtemplates ctx) $ BS.toString tostext
                                  renderFromBody ctx TopNone kontrakcja $ cdata content
                    Nothing -> do 
                                  content <- liftIO $ activatePageViewNotValidLink (ctxtemplates ctx)
                                  renderFromBody ctx TopNone kontrakcja $ cdata content

                                 
handleActivate::String->String -> Kontra KontraLink
handleActivate sid mh = do
                        ctx <- get
                        muser <- userFromExternalSessionData sid mh 
                        tos <- fmap ((==) $ Just "on") $ getField "tos"
                        name <- fmap (fromMaybe "") $ getField "name"
                        password <- fmap (fromMaybe "") $ getField "password"
                        password2 <- fmap (fromMaybe "") $ getField "password2"
                        case muser of 
                         Just user -> do    
                            case (checkPasswords password password2) of
                             Right () ->  if (tos)
                                           then do  
                                            passwordhash <- liftIO $ createPassword $ BS.fromString password
                                            update $ SetUserPassword user passwordhash
                                            update $ AcceptTermsOfService (userid user) (ctxtime ctx)
                                            update $ SetUserInfo (userid user) $ (userinfo user) {userfstname = BS.fromString name}
                                            dropExternalSession sid mh
                                            addFlashMsgHtmlFromTemplate =<< (liftIO $ flashMessageUserActivated (ctxtemplates ctx))
                                            logUserToContext $ Just user
                                            return LinkMain 
                                           else do
                                            addFlashMsgText =<< (liftIO $ flashMessageMustAcceptTOS (ctxtemplates ctx)) 
                                            return LoopBack
                             Left f ->  do
                                         addFlashMsgText =<< (liftIO $ f (ctxtemplates ctx))          
                                         return LoopBack  
                         Nothing -> do 
                                     memail <- fmap (fmap BS.fromString) $ getField "email"
                                     case memail of  
                                      Just email -> do
                                                     muser <- query $ GetUserByEmail $ Email email
                                                     case  muser of
                                                      Just user -> 
                                                          if (isNothing $ userhasacceptedtermsofservice user) 
                                                          then  
                                                               do  al <- liftIO $ activateLink $ userid user
                                                                   mail <-  liftIO $ newUserMail (ctxtemplates ctx) (ctxhostpart ctx) email email al
                                                                   liftIO $ sendMail (ctxmailsconfig ctx) $ mail { fullnameemails = [(email, email)]}
                                                                   addFlashMsgText =<< (liftIO $ flashMessageNewActivationLinkSend  (ctxtemplates ctx)) 
                                                                   return LinkMain
                                                          else do
                                                                addFlashMsgText =<< (liftIO $ flashMessageUserAlreadyActivated (ctxtemplates ctx)) 
                                                                return LinkMain
                                                      Nothing -> do
                                                                 addFlashMsgText =<< (liftIO $ flashMessageNoSuchUserExists (ctxtemplates ctx)) 
                                                                 return LoopBack
                                                              
                                      Nothing -> do
                                         addFlashMsgText =<< (liftIO $ flashMessageActivationLinkNotValid (ctxtemplates ctx)) 
                                         return LinkMain   
      
