{-# LANGUAGE CPP, OverloadedStrings #-}

module UserControl where
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Trans (liftIO,MonadIO,lift)
import "base" Control.Monad
import AppView
import Data.Maybe
import Data.Object
import DocView
import Happstack.Data.IxSet 
import Happstack.Server hiding (simpleHTTP)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.State (update,query)
import KontraLink
import Misc
import SendMail(Mail,sendMail,fullnameemails)
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
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Object.Json as Json
import qualified Data.Set as Set
import qualified HSP


handleUser :: Context -> Kontra Response
handleUser ctx = withUserGet $
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
handleUserPost ctx@Context{ctxmaybeuser = Just user@User{userid},ctxtime} = do
  fullname <- g "fullname"
  companyname <- g "companyname"
  companynumber <- g "companynumber"
  invoiceaddress <- g "invoiceaddress"
  tos <- getDataFn' (look "tos")
  
  newuser <- update $ SetUserDetails user fullname companyname companynumber invoiceaddress
  if isNothing (userhasacceptedtermsofservice user)
     then
         if isJust tos
            then do
              update $ AcceptTermsOfService userid ctxtime
              addFlashMsgHtml userDetailsSavedFlashMessage
            else addFlashMsgText $ BS.fromString "För att kunna använda tjänsten måste du acceptera SkrivaPå Allmänna Villkor."
     else do
         addFlashMsgHtml userDetailsSavedFlashMessage
         return ()

  backToAccount


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
       else newUserMail hostpart email fullname password
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
   Guard against a GET with logged in users who have not signed the TOS agreement.
   If they have not, redirect to their account page.
-}
checkUserTOSGet :: Kontra Response -> Kontra Response
checkUserTOSGet action =
  withUserGet $ do
    ctx@(Context {ctxmaybeuser = (Just (User {userhasacceptedtermsofservice}))}) <- get
    case userhasacceptedtermsofservice of
      Nothing -> sendRedirect LinkAccount
      Just _  -> action
