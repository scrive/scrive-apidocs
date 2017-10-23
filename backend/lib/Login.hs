module Login (
    handleLoginGet
  , handleLoginPost
  , handleLogout
  , handleLogoutAJAX
  , handleLoginWithRedirectGet
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
import InternalResponse
import IPAddress
import Kontra
import KontraLink
import KontraPrelude
import Log.Identifier
import Redirect
import ThirdPartyStats.Core
import ThirdPartyStats.Planhat
import User.Email
import User.History.Model
import User.Model
import Util.HasSomeUserInfo
import Util.MonadUtils
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
                Just user@User{userpassword,userid,useraccountsuspended}
                    | maybeVerifyPassword userpassword passwd
                    && ipIsOK && not useraccountsuspended -> do
                        failedAttemptCount <- dbQuery $ GetUserRecentAuthFailureCount userid
                        if failedAttemptCount <= 5
                          then do
                            logInfo "User logged in" $ logObject_ user
                            muuser <- dbQuery $ GetUserByID userid

                            case muuser of
                              Just User{userid = uid} -> do
                                now <- currentTime
                                asyncLogEvent
                                  SetUserProps
                                  (simplePlanhatAction "Login" user now)
                                  EventPlanhat
                                asyncLogEvent
                                  "Login"
                                  [ UserIDProp uid
                                  , IPProp $ ctxipnumber ctx
                                  , TimeProp $ ctxtime ctx ]
                                  EventMixpanel
                                asyncLogEvent
                                  SetUserProps
                                  [ UserIDProp uid
                                  , someProp "Last login" $ ctxtime ctx ]
                                  EventMixpanel
                              _ -> return ()
                            if padlogin
                              then do
                                _ <- dbUpdate $ LogHistoryPadLoginSuccess userid (ctxipnumber ctx) (ctxtime ctx)
                                logPadUserToContext muuser
                              else do
                                _ <- dbUpdate $ LogHistoryLoginSuccess userid (ctxipnumber ctx) (ctxtime ctx)
                                logUserToContext muuser
                            J.runJSONGenT $ J.value "logged" True
                          else do
                            logInfo "User login failed (too many attempts)" $ logObject_ user
                            J.runJSONGenT $ J.value "logged" False

                Just u@User{userpassword} | not (maybeVerifyPassword userpassword passwd) -> do
                        logInfo "User login failed (invalid password)" $ logObject_ u
                        _ <- if padlogin
                          then dbUpdate $ LogHistoryPadLoginFailure (userid u) (ctxipnumber ctx) (ctxtime ctx)
                          else dbUpdate $ LogHistoryLoginFailure (userid u) (ctxipnumber ctx) (ctxtime ctx)
                        J.runJSONGenT $ J.value "logged" False

                Just u | not ipIsOK -> do
                        logInfo "User login failed (ip not on allowed list)" $ object [
                            logPair_ u
                          , "ip" .= show (ctxipnumber ctx)
                          ]
                        _ <- if padlogin
                          then dbUpdate $ LogHistoryPadLoginFailure (userid u) (ctxipnumber ctx) (ctxtime ctx)
                          else dbUpdate $ LogHistoryLoginFailure (userid u) (ctxipnumber ctx) (ctxtime ctx)

                        company <- dbQuery $ GetCompanyByUserID (userid u)
                        admins <-  dbQuery $ GetCompanyAdmins (companyid company)
                        case admins of
                          (admin:_) -> J.runJSONGenT $ do
                                         J.value "logged" False
                                         J.value "ipaddr" (show (ctxipnumber ctx))
                                         J.value "adminname" (getSmartName admin)
                          _ -> J.runJSONGenT $ do
                                         J.value "logged" False
                Just u -> do
                  {- MR: useraccountsuspended must be true here. This is a hack for Hi3G. It will be removed in future -}
                        logInfo "User login failed (user account suspended)" $ object [logPair_ u]
                        _ <- if padlogin
                          then dbUpdate $ LogHistoryPadLoginFailure (userid u) (ctxipnumber ctx) (ctxtime ctx)
                          else dbUpdate $ LogHistoryLoginFailure (userid u) (ctxipnumber ctx) (ctxtime ctx)
                        J.runJSONGenT $ J.value "logged" False
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

handleLoginWithRedirectGet :: Kontrakcja m => m InternalKontraResponse
handleLoginWithRedirectGet = do
  sci <- guardJustM $ readField "session_id"
  url <- guardJustM $ getField "url"
  -- It may seems strange, that we do the same thing regardless whether the user
  -- session exists. The reason is, that session from link may become invalid, when
  -- user logs out from the original login link session. We want the original link
  -- to keep redirecting as intended. User may need to login again using his email and
  -- password depending on the redirect destination.
  _ <- unsafeSessionTakeover sci
  return $ internalResponse $ LinkExternal url
