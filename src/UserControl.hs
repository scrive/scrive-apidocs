{-# LANGUAGE CPP, OverloadedStrings #-}

module UserControl where
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Trans (liftIO,MonadIO,lift)
import "base" Control.Monad
import AppView
import Data.Maybe
import Data.Object
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
import qualified Data.Object.Json as Json
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
  renderFromBody ctx TopAccount kontrakcja $ showUser user ms friends

handleUserPost :: Kontra KontraLink
handleUserPost = do
  ctx@Context{ctxmaybeuser = Just user@User{userid},ctxtime} <- get
  fullname <- g "fullname"
  companyname <- g "companyname"
  companynumber <- g "companynumber"
  invoiceaddress <- g "invoiceaddress"
  defaultmainsignatoryemail <- g "defaultmainsignatory"
  newvieweremail <- g "newvieweremail"

  liftIO $ print "what's up?"

  when (BS8.length defaultmainsignatoryemail > 0) $ do
     dmsreturn <- update $ SetDefaultMainSignatoryByEmail userid $ Email defaultmainsignatoryemail
     case dmsreturn of
       Left msg -> addFlashMsgText msg
       Right _  -> return ()
     return ()

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
  viewSubaccounts ctx (Set.toList subaccounts)

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
  muser <- liftIO $ createUser ctx ctxhostpart fullname email Nothing (Just user)
  when (isNothing muser) $ addFlashMsgText =<< (liftIO $ flashMessageUserWithSameEmailExists ctxtemplates)
  return LinkSubaccount

handleRemoveSubaccounts :: Context -> Kontra KontraLink
handleRemoveSubaccounts ctx@Context { ctxmaybeuser = Just (user@User { userid }), ctxhostpart } = do
  subidstrings <- getDataFnM (lookInputList "doccheck")
  let Just subids = sequence $ map (readM . LS.toString) subidstrings
  removed <- liftIO $ removeSubaccounts userid subids
  return LinkSubaccount

removeSubaccounts :: UserID -> [UserID] -> IO ()
removeSubaccounts _ [] = return ()
removeSubaccounts userid (subid:xs) = do
    removeSubaccount userid subid
    removeSubaccounts userid xs

removeSubaccount userid subId = do
  takeover <- update $ FragileTakeOverDocuments userid subId
  maybeuser <- update $ FragileDeleteUser subId
  return ()

randomPassword :: IO BS.ByteString
randomPassword = do
    let letters =['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z']
    indexes <- liftIO $ replicateM 8 (randomRIO (0,length letters-1))
    return (BS.fromString $ map (letters!!) indexes)

createUser ::  Context -> String -> BS.ByteString -> BS.ByteString -> Maybe BS.ByteString -> Maybe User -> IO (Maybe User)
createUser ctx hostpart fullname email maybepassword maybesupervisor =
  case maybepassword of
    Nothing -> do
      password <- randomPassword
      createUser1 ctx hostpart fullname email password False maybesupervisor
    Just x ->
      createUser1 ctx hostpart fullname email x True maybesupervisor

createNewUserByAdmin :: Context -> BS.ByteString -> BS.ByteString -> IO (Maybe User)
createNewUserByAdmin ctx fullname email =
     do
      password <- randomPassword
      passwdhash <- createPassword password
      muser <- update $ AddUser fullname email passwdhash Nothing
      case muser of 
       Just user -> do
                    chpwdlink <- changePasswordLink (userid user)
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
                             else newUserMail (ctxtemplates ctx) hostpart email fullname
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
withDocumentAuthor document action= do
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
      pageAcceptTOS ctx tostext

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
                                    
      