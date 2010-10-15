{-# LANGUAGE CPP #-}

module UserControl where
import UserState
import UserView
import Session
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import KontraLink
import Happstack.Data.IxSet 
import Happstack.Server hiding (simpleHTTP)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.State (update,query)
import qualified Data.Object.Json as Json
import Control.Monad
import User
import AppView
import Control.Monad.Trans (liftIO,MonadIO,lift)
import Misc
import SendMail(Mail,sendMail,fullnameemails)
import System.Random
import System.Log.Logger
import System.Process
import Data.Object
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad.State
import DocView
import qualified HSP

addFlashMsgText :: BS.ByteString -> Kontra ()
addFlashMsgText msg = do
                       ctx@Context{ ctxflashmessages = flashmessages} <- get
                       put $ ctx{ ctxflashmessages =  (FlashMessage msg) : flashmessages}

  
addFlashMsgHtml :: HSP.HSP HSP.XML 
                -> Kontra ()
addFlashMsgHtml msg = do
                       ctx@Context{ ctxflashmessages = flashmessages} <- get
                       msg' <- liftM renderXMLAsBSHTML (webHSP1 msg)
                       put $ ctx {ctxflashmessages = (FlashMessage msg') :  flashmessages} 

logUserToContext user =  do
                          ctx<- get
                          put$ ctx { ctxmaybeuser =  user}

handleUser :: Context -> Kontra Response
handleUser ctx = 
    msum 
    [ methodM GET >> showUser ctx
    , methodM POST >> handleUserPost ctx
    , dir "password" $ handleUserPasswordPost ctx
    , dir "subaccount" $ msum [ methodM GET  >> handleGetSubaccount ctx 
                              , methodM POST >> handlePostSubaccount ctx
                              ]
    ]

handleUserPasswordPost :: Context -> Kontra Response
handleUserPasswordPost ctx@Context{ctxmaybeuser = Just user@User{userid}} = do
  oldpassword <- g "oldpassword"
  password <- g "password"
  password2 <- g "password2"
  
  if password == password2
    then 
      if verifyPassword (userpassword user) oldpassword
        then
          if isPasswordStrong password
            then do
              passwordhash <- liftIO $ createPassword password
              update $ SetUserPassword user passwordhash
              addFlashMsgHtml userDetailsSavedFlashMessage
            else
              addFlashMsgText $ BS.fromString "Det nya lösenordet ska vara minst 6 tecken"
        else
          addFlashMsgText $ BS.fromString "Du har skrivit in fel nuvarande lösenord"
    else
      addFlashMsgText $ BS.fromString "Nytt lösenord matchar inte med upprepa lösenord"
  backToAccount


handleUserPost :: Context -> Kontra Response
handleUserPost ctx@Context{ctxmaybeuser = Just user@User{userid}} = do
  fullname <- g "fullname"
  companyname <- g "companyname"
  companynumber <- g "companynumber"
  invoiceaddress <- g "invoiceaddress"
  
  newuser <- update $ SetUserDetails user fullname companyname companynumber invoiceaddress
  addFlashMsgHtml userDetailsSavedFlashMessage
  
  backToAccount
  {-
  -- Terms of Service logic
  tos <- getDataFn (look "tos")

  if isNothing (userhasacceptedtermsofservice user)
     then
         if isJust tos
            then do
              update $ AcceptTermsOfService userid minutestime
              -- update $ AddUserFlashMessage userid (FlashMessage $ BS.fromString "You have accepted the Terms of Service Agreement")
            else update $ AddUserFlashMessage userid (FlashMessage $ BS.fromString "För att kunna använda tjänsten måste du acceptera SkrivaPå Allmänna Villkor.")
     else
         return ()
  -}

backToAccount :: Kontra Response
backToAccount = do
  let link = show LinkAccount
  response <- webHSP (seeOtherXML link)
  seeOther link response


handleGetSubaccount :: Context -> Kontra Response
handleGetSubaccount ctx@Context { ctxmaybeuser = Just user@User { userid } }  = do
  subaccounts <- query $ GetUserSubaccounts userid
  viewSubaccounts ctx (Set.toList subaccounts)

handlePostSubaccount :: Context -> Kontra Response
handlePostSubaccount ctx@Context { ctxmaybeuser = Just (user@User { userid }), ctxhostpart } = do
  create <- g "create"   -- check if we are in proper action
  fullname <- g "fullname"
  email <- g "email"
  user <- liftIO $ createUser ctxhostpart fullname email Nothing (Just user)
  let link = show LinkSubaccount
  response <- webHSP (seeOtherXML link)
  seeOther link response

randomPassword :: IO BS.ByteString
randomPassword = do
    let letters =['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z']
    indexes <- liftIO $ replicateM 8 (randomRIO (0,length letters-1))
    return (BS.fromString $ map (letters!!) indexes)

createUser :: String -> BS.ByteString -> BS.ByteString -> Maybe BS.ByteString -> Maybe User -> IO User
createUser hostpart fullname email maybepassword maybesupervisor =
  case maybepassword of
    Nothing -> do
      password <- randomPassword
      createUser1 hostpart fullname email password False maybesupervisor
    Just x ->
      createUser1 hostpart fullname email x True maybesupervisor

createUser1 :: String -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Bool -> Maybe User -> IO User
createUser1 hostpart fullname email password isnewuser maybesupervisor = do
  passwdhash <- createPassword password
  user <- update $ AddUser fullname email passwdhash (fmap userid maybesupervisor)
  mail <- case maybesupervisor of
    Nothing ->
      if not isnewuser
       then passwordChangeMail hostpart email fullname password
       else newUserMail email fullname password
    Just supervisor -> inviteSubaccountMail hostpart (userfullname supervisor) (usercompanyname supervisor)
                       email fullname password
  sendMail $ mail { fullnameemails = [(fullname, email)]}
  return user
  
resetUserPassword :: String -> BS.ByteString -> IO ()
resetUserPassword hostpart email = do
  maybeuser <- query $ GetUserByEmail (Email{unEmail=email})
  case maybeuser of
    Just user -> do
      password <- randomPassword
      passwordhash <- createPassword password
      update $ SetUserPassword user passwordhash
      mail <- passwordChangeMail hostpart email (userfullname user) password
      sendMail $ mail { fullnameemails = [((userfullname user), email)]}
    Nothing ->
      return ()

-- Session
userLogin :: Kontra (Maybe User)
userLogin = userLogin1

-- RememberMe cookie
userLogin1 :: Kontra (Maybe User)
userLogin1 = do
    mayberememberme <- getRememberMeCookie
    case mayberememberme of
        Just (RememberMe userid _ _ _ _) -> do
            maybeuser <- query $ GetUserByUserID userid
            case maybeuser of
                Just user -> do
                    logUserToContext maybeuser
                    return maybeuser
                Nothing -> userLogin2
        Nothing -> userLogin2

-- OpenID
userLogin2 :: Kontra (Maybe User)
userLogin2 = do
    maybetoken <- getDataFn (look "token") 
#if MIN_VERSION_happstack_server(0,5,1)
    case maybetoken of
      Left _ -> return Nothing
      Right token -> do
#else
    case maybetoken of
      Nothing -> return Nothing
      Just token -> do
#endif
              let req = "https://rpxnow.com/api/v2/auth_info" ++ 
                        "?apiKey=" ++
                        -- "03bbfc36d54e523b2602af0f95aa173fb96caed9" ++
                        "a348dd93f1d78ae11c443574d73d974299007c00" ++
                        "&token=" ++ token
              
              let curlproc = CreateProcess { std_out = CreatePipe
                                           , std_err = CreatePipe
                                           , std_in = Inherit
                                           , cwd = Nothing
                                           , cmdspec = RawCommand "curl" [ "-q", "-k", req ]
                                           , close_fds = True
                                           , env = Nothing
                                           }
              (_, Just outhandle, Just errhandle, curlProcHandle) <- liftIO $ createProcess curlproc
              errcontent <- liftIO $ BS.hGetContents errhandle
              rpxdata <- liftIO $ BS.hGetContents outhandle
              
              liftIO $ noticeM rootLoggerName $ "RPXNow: " ++ BS.toString rpxdata

              let unJsonString (Json.JsonString x) = x
              let maybeProfileMapping = do
                    json <- Json.decode rpxdata
                    jsonMapping <- fromMapping json 
                    lookupMapping (BS.fromString "profile") jsonMapping
              let maybeVerifiedEmail = do
                    profileMapping <- maybeProfileMapping
                    verifiedEmail <- lookupScalar (BS.fromString "verifiedEmail") profileMapping
                    return (unJsonString verifiedEmail)
              let maybeNameFormatted = do
                    profileMapping <- maybeProfileMapping
                    nameMapping <- lookupMapping (BS.fromString "name") profileMapping
                    nameFormatted <- lookupScalar (BS.fromString "formatted") nameMapping
                    return (unJsonString nameFormatted)
              let verifiedEmail = maybe BS.empty id maybeVerifiedEmail
              let nameFormatted = maybe BS.empty id maybeNameFormatted

              when (verifiedEmail==BS.empty) $ do
                liftIO $ BS.putStrLn rpxdata
                error "SkrivaPa requires verifiedEmail in your OpenID login data, we cannot work without one"
                         

              maybeuser <- query $ GetUserByEmail (Email verifiedEmail)
    
              case maybeuser of
                Just user -> do
                  liftIO $ noticeM rootLoggerName $ "User: " ++ BS.toString nameFormatted ++ " <" ++ 
                         BS.toString verifiedEmail ++ "> logged in"
                  logUserToContext maybeuser
                  rq <- askRq
                  let link = rqUri rq
                  response <- webHSP (seeOtherXML link)
                  finishWith (redirect 303 link response)
                  return (Just user)

                Nothing -> do
                  let link = "/login"
                  response <- webHSP $ seeOtherXML link
                  finishWith (redirect 303 link response)
                  return Nothing

rememberMeCookieName = "remember_me"

setRememberMeCookie :: UserID -> Bool -> Kontra ()
setRememberMeCookie userid rememberMe = do
    cookie <- liftIO $ do
        cookieString <- createRememberMeCookie userid rememberMe
        return $ mkCookie rememberMeCookieName cookieString
    addCookie (sessionLength rememberMe) cookie
    
clearRememberMeCookie :: Kontra ()
clearRememberMeCookie =
    expireCookie rememberMeCookieName

instance FromData (Maybe RememberMe) where
    fromData = do
        cookie <- lookCookieValue rememberMeCookieName
        return $ readRememberMeCookie cookie

getRememberMeCookie :: (ServerMonad m, Control.Monad.MonadPlus m) => m (Maybe RememberMe)
getRememberMeCookie = do
    cookie <- getData
    return $ join cookie 


 
withTOS :: Kontra Response -> Kontra Response

-- to disable requiring TOS signing, comment out this definition (leaving the second case)
{-
withTOS ctx@(Context {ctxmaybeuser = (Just (User {userid = userid, userhasacceptedtermsofservice = Nothing }))}) _ = do
  update $ AddUserFlashMessage userid (FlashMessage (BS.fromString "You must accept the Terms of Service Agreement"))
  let link = "/account"
  response <- webHSP $ seeOtherXML link
  finishWith (redirect 303 link response)
-}
withTOS action = action
