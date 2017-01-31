module Login (
    handleLoginGet
  , handleLoginPost
  , handleLogout
  , handleLogoutAJAX
  ) where

import Happstack.Server hiding (dir, host, path, simpleHTTP)
import Log
import Text.JSON
import Text.StringTemplates.Templates
import qualified Text.JSON.Gen as J
import qualified Text.StringTemplates.Fields as F

import Analytics.Include
import AppView
import Company.Model
import DB
import Happstack.Fields
import InputValidation hiding (Result)
import IPAddress
import Kontra
import KontraLink
import KontraPrelude
import Log.Identifier
import Redirect
import ThirdPartyStats.Core
import User.Email
import User.History.Model
import User.Model
import Util.HasSomeUserInfo
import Utils.HTTP

handleLoginGet :: Kontrakcja m => m (Either KontraLink Response)
handleLoginGet = do
  ctx <- getContext
  case (ctxmaybeuser ctx) of
       Nothing -> do
          referer <- getField "referer"
          ad <- getAnalyticsData
          content <- renderTemplate "loginPageWithBranding" $ do
            F.value "referer" $ fromMaybe "/" referer
            standardPageFields ctx Nothing ad
          Right <$> simpleHtmlResponse content
       Just _ -> return $ Left LinkDesignView

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
                        Just u -> do
                             (company :: Company) <- dbQuery $ GetCompanyByUserID (userid u)
                             return $ null (companyipaddressmasklist (companyinfo company)) ||
                                               (any (ipAddressIsInNetwork (ctxipnumber ctx)) (companyipaddressmasklist (companyinfo company)))
                        Nothing -> return True
            case maybeuser of
                Just user@User{userpassword}
                    | verifyPassword userpassword passwd
                    && ipIsOK -> do
                        logInfo "User logged in" $ logObject_ user
                        muuser <- dbQuery $ GetUserByID (userid user)

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
                        J.runJSONGenT $ J.value "logged" True
                Just u@User{userpassword} | not (verifyPassword userpassword passwd) -> do
                        logInfo "User login failed (invalid password)" $ logObject_ u
                        _ <- if padlogin
                          then dbUpdate $ LogHistoryPadLoginAttempt (userid u) (ctxipnumber ctx) (ctxtime ctx)
                          else dbUpdate $ LogHistoryLoginAttempt (userid u) (ctxipnumber ctx) (ctxtime ctx)
                        J.runJSONGenT $ J.value "logged" False

                Just u -> do
                        logInfo "User login failed (ip not on allowed list)" $ object [
                            logPair_ u
                          , "ip" .= show (ctxipnumber ctx)
                          ]
                        _ <- if padlogin
                          then dbUpdate $ LogHistoryPadLoginAttempt (userid u) (ctxipnumber ctx) (ctxtime ctx)
                          else dbUpdate $ LogHistoryLoginAttempt (userid u) (ctxipnumber ctx) (ctxtime ctx)

                        company <- dbQuery $ GetCompanyByUserID (userid u)
                        admins <-  dbQuery $ GetCompanyAdmins (companyid company)
                        case admins of
                          (admin:_) -> J.runJSONGenT $ do
                                         J.value "logged" False
                                         J.value "ipaddr" (show (ctxipnumber ctx))
                                         J.value "adminname" (getSmartName admin)
                          _ -> J.runJSONGenT $ do
                                         J.value "logged" False
                Nothing -> do
                    logInfo "User login failed (user not found)" $ object [
                        "email" .= email
                      ]
                    J.runJSONGenT $ J.value "logged" False
        _ -> J.runJSONGenT $ J.value "logged" False

{- |
   Handles the logout, and sends user back to main page.

   This should be removed when we rewrite header to React,
   and we should only use handleLogoutAJAX then
-}
handleLogout :: Kontrakcja m => m Response
handleLogout = do
    logUserToContext Nothing
    logPadUserToContext Nothing
    (sendRedirect . LinkExternal) =<< getHttpHostpart

handleLogoutAJAX :: Kontrakcja m => m JSValue
handleLogoutAJAX = do
    logUserToContext Nothing
    logPadUserToContext Nothing
    J.runJSONGenT $ J.value "success" True
