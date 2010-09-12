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
import SendMail
import System.Random
import System.Log.Logger
import System.Process
import Data.Object
import qualified Data.Set as Set
import Data.Maybe

handleUser :: Context -> Kontra Response
handleUser ctx = 
    msum 
    [ methodM GET >> showUser ctx
    , methodM POST >> handleUserPost ctx
    , dir "subaccount" $ msum [ methodM GET  >> handleGetSubaccount ctx 
                              , methodM POST >> handlePostSubaccount ctx
                              ]
    ]

createMaybePassword :: BS.ByteString -> IO (Maybe Password)
createMaybePassword password
    | password == BS.empty = return Nothing
    | otherwise = do
        hash <- createPassword password
        return $ Just hash

handleUserPost :: Context -> Kontra Response
handleUserPost ctx@Context{ctxmaybeuser = Just user@User{userid}, ctxtime = minutestime} = do
  fullname <- g "fullname"
  companyname <- g "companyname"
  companynumber <- g "companynumber"
  invoiceaddress <- g "invoiceaddress"
  oldpassword <- g "oldpassword"
  password <- g "password"
  password2 <- g "password2"
  
  if (password) == (password2)
    then 
      if verifyPassword (userpassword user) oldpassword || password == BS.empty
        then
          if isPasswordStrong password || password == BS.empty
            then do
              passwordhash <- liftIO $ createMaybePassword password
              newuser <- update $ SetUserDetails user fullname companyname companynumber invoiceaddress passwordhash
              flashmsg <- userDetailsSavedFlashMessage
              update $ AddUserFlashMessage userid flashmsg
            else
              update $ AddUserFlashMessage userid (FlashMessage $ BS.fromString "The new password must be at least 6 characters long")
        else
          update $ AddUserFlashMessage userid (FlashMessage $ BS.fromString "Old password is incorrect")
    else
      update $ AddUserFlashMessage userid (FlashMessage $ BS.fromString "Passwords must match")

  -- Terms of Service logic
  tos <- getDataFn (look "tos")

  if isNothing (userhasacceptedtermsofservice user)
     then
         if isJust tos
            then do
              update $ AcceptTermsOfService userid minutestime
              update $ AddUserFlashMessage userid (FlashMessage $ BS.fromString "You have accepted the Terms of Service Agreement")
            else update $ AddUserFlashMessage userid (FlashMessage $ BS.fromString "You must accept the Terms of Service Agreement")
     else
         return ()

  let link = show LinkAccount
  response <- webHSP (seeOtherXML link)
  seeOther link response


handleGetSubaccount :: Context -> Kontra Response
handleGetSubaccount ctx@Context { ctxmaybeuser = Just user@User { userid } }  = do
  subaccounts <- query $ GetUserSubaccounts userid
  viewSubaccounts ctx (Set.toList subaccounts)

handlePostSubaccount :: Context -> Kontra Response
handlePostSubaccount ctx@Context { ctxmaybeuser = Just (user@User { userid })} = do
  create <- g "create"   -- check if we are in proper action
  fullname <- g "fullname"
  email <- g "email"
  user <- liftIO $ createUser fullname email Nothing (Just user)
  let link = show LinkSubaccount
  response <- webHSP (seeOtherXML link)
  seeOther link response

createUser :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString -> Maybe User -> IO User
createUser fullname email maybepassword maybesupervisor =
  case maybepassword of
    Nothing -> do
      let letters =['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z']
      indexes <- liftIO $ replicateM 8 (randomRIO (0,length letters-1))
      createUser1 fullname email (BS.fromString $ map (letters!!) indexes) False maybesupervisor
    Just x ->
      createUser1 fullname email x True maybesupervisor

createUser1 :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Bool -> Maybe User -> IO User
createUser1 fullname email password isnewuser maybesupervisor = do
  passwdhash <- createPassword password
  user <- update $ AddUser fullname email passwdhash (fmap userid maybesupervisor)
  content <- case maybesupervisor of
    Nothing ->
      if isnewuser
       then liftIO $ passwordChangeMail email fullname password
       else liftIO $ newUserMail email fullname
    Just supervisor -> liftIO $ inviteSubaccountMail (userfullname supervisor) (usercompanyname supervisor)
                       email fullname password
  liftIO $ sendMail [(fullname, email)]
               (BS.fromString "Välkommen!") content BS.empty
  return user

-- Session
userLogin :: (MonadIO m) => ServerPartT m (Maybe User)
userLogin = do
  maybeuser <- withMSessionDataSP2 $ \maybeuserid -> do 
    case maybeuserid of
      Just (sid,userid) -> do
                       -- prolong session
                       startSession sid
                       query $ GetUserByUserID userid
      Nothing -> return Nothing
  case maybeuser of
    Just user -> return maybeuser
    Nothing -> userLogin1

-- RememberMe cookie
userLogin1 :: (MonadIO m) => ServerPartT m (Maybe User)
userLogin1 = do
    mayberememberme <- getRememberMeCookie
    case mayberememberme of
        Just (RememberMe userid _ _ _ _) -> do
            maybeuser <- query $ GetUserByUserID userid
            case maybeuser of
                Just user -> do
                    sessionid <- update $ NewSession userid
                    startSession sessionid
                    return maybeuser
                Nothing -> userLogin2
        Nothing -> userLogin2

-- OpenID
userLogin2 :: (MonadIO m) => ServerPartT m (Maybe User)
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
                  sessionid <- update $ NewSession (userid user)
                  startSession sessionid
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
    return $ case cookie of
        Nothing -> Nothing
        Just Nothing -> Nothing
        Just (Just rememberMe) -> Just rememberMe


 
withTOS :: (MonadIO m) => Context -> ServerPartT m Response -> ServerPartT m Response

-- to disable requiring TOS signing, comment out this definition (leaving the second case)
{-
withTOS ctx@(Context {ctxmaybeuser = (Just (User {userid = userid, userhasacceptedtermsofservice = Nothing }))}) _ = do
  update $ AddUserFlashMessage userid (FlashMessage (BS.fromString "You must accept the Terms of Service Agreement"))
  let link = "/account"
  response <- webHSP $ seeOtherXML link
  finishWith (redirect 303 link response)
-}
withTOS _ action = action
