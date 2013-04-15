module Login (
    signupPageGet
  , handleLoginGet
  , handleLoginPost
  , handleLogout
  ) where

import DB
import InputValidation hiding (Result)
import Happstack.Fields
import Kontra
import KontraLink
import Redirect
import User.Model
import IPAddress
import Company.Model
import qualified Log (debug)
import Util.HasSomeUserInfo
import Stats.Control
import User.History.Model

import Control.Applicative
import Data.Maybe
import Happstack.Server hiding (simpleHTTP, host, dir, path)
import Text.JSON.Gen as J
import Text.JSON
import qualified Text.StringTemplates.Fields as F
import Text.StringTemplates.Templates
import Routing
import Utils.HTTP
import ThirdPartyStats.Core

handleLoginGet :: Kontrakcja m => m (Either KontraLink ThinPage)
handleLoginGet = do
  ctx <- getContext
  case (ctxmaybeuser ctx) of
       Nothing -> do
          referer <- getField "referer"
          content <- renderTemplate "loginPage" $ do
                    F.value "referer" $ fromMaybe "/" referer
          return $ Right $ ThinPage content
       Just _ -> return $ Left LinkDesignView

signupPageGet :: Kontrakcja m => m ThinPage
signupPageGet = do
  memail <- getField "email"
  fmap ThinPage $ renderTemplate "signupPage" $ do
    F.value "email" memail

{- |
   Handles submission of a login form.  On failure will redirect back to referer, if there is one.
-}
handleLoginPost :: Kontrakcja m => m JSValue
handleLoginPost = do
    padlogin <- isFieldSet "pad"
    ctx <- getContext
    memail  <- getOptionalField asDirtyEmail    "email"
    mpasswd <- getOptionalField asDirtyPassword "password"
    case (memail, mpasswd) of
        (Just email, Just passwd) -> do
            -- check the user things here
            maybeuser <- dbQuery $ GetUserByEmail (Email email)
            ipIsOK <- case maybeuser of
                        Just x -> do
                             mcompany <- dbQuery $ GetCompanyByUserID (userid x)
                             case mcompany of
                               Just company -> do
                                 Log.debug $ "Company " ++ show (companyid company) ++ "(" ++ show (usercompany <$> maybeuser) ++ ") allows access from " ++ show ((companyinfo company))
                                 return $ null (companyipaddressmasklist (companyinfo company)) ||
                                               (any (ipAddressIsInNetwork (ctxipnumber ctx)) (companyipaddressmasklist (companyinfo company)))
                               Nothing -> return True
                        Nothing -> return True
            case maybeuser of
                Just user@User{userpassword}
                    | verifyPassword userpassword passwd
                    && ipIsOK -> do
                        Log.debug $ "User " ++ show email ++ " logged in"
                        _ <- dbUpdate $ SetUserSettings (userid user) $ (usersettings user) {
                          lang = ctxlang ctx
                        }
                        muuser <- dbQuery $ GetUserByID (userid user)
                        _ <- addUserLoginStatEvent (ctxtime ctx) (fromJust muuser)
                        case muuser of
                          Just User{userid = uid} -> do
                            asyncLogEvent "Login" [
                              UserIDProp uid,
                              IPProp $ ctxipnumber ctx,
                              TimeProp $ ctxtime ctx
                              ]
                            asyncLogEvent SetUserProps [
                              UserIDProp uid,
                              someProp "Last login" $ ctxtime ctx
                              ]
                          _ -> return ()
                        if padlogin
                          then do
                            _ <- dbUpdate $ LogHistoryPadLoginSuccess (userid user) (ctxipnumber ctx) (ctxtime ctx)
                            logPadUserToContext muuser
                          else do
                            _ <- dbUpdate $ LogHistoryLoginSuccess (userid user) (ctxipnumber ctx) (ctxtime ctx)
                            logUserToContext muuser
                        runJSONGenT $ value "logged" True
                Just u@User{userpassword} | not (verifyPassword userpassword passwd) -> do
                        Log.debug $ "User " ++ show email ++ " login failed (invalid password)"
                        _ <- if padlogin
                          then dbUpdate $ LogHistoryPadLoginAttempt (userid u) (ctxipnumber ctx) (ctxtime ctx)
                          else dbUpdate $ LogHistoryLoginAttempt (userid u) (ctxipnumber ctx) (ctxtime ctx)
                        runJSONGenT $ value "logged" False

                Just u -> do
                        Log.debug $ "User " ++ show email ++ " login failed (ip " ++ show (ctxipnumber ctx)
                                ++ " not on allowed list)"
                        _ <- if padlogin
                          then dbUpdate $ LogHistoryPadLoginAttempt (userid u) (ctxipnumber ctx) (ctxtime ctx)
                          else dbUpdate $ LogHistoryLoginAttempt (userid u) (ctxipnumber ctx) (ctxtime ctx)

                        mcompany <- dbQuery $ GetCompanyByUserID (userid u)
                        admins <- case mcompany of
                                    Just company -> dbQuery $ GetCompanyAdmins (companyid company)
                                    _ -> return []
                        case admins of
                          (admin:_) -> runJSONGenT $ do
                                         value "logged" False
                                         value "ipaddr" (show (ctxipnumber ctx))
                                         value "adminname" (getSmartName admin)
                          _ -> runJSONGenT $ do
                                         value "logged" False
                Nothing -> do
                    Log.debug $ "User " ++ show email ++ " login failed (user not found)"
                    runJSONGenT $ value "logged" False
        _ -> runJSONGenT $ value "logged" False

{- |
   Handles the logout, and sends user back to main page.
-}
handleLogout :: Kontrakcja m => m Response
handleLogout = do
    logUserToContext Nothing
    (sendRedirect . LinkExternal) =<< getHttpHostpart
