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
import BrandedDomain.BrandedDomain
import DB
import Happstack.Fields
import InputValidation hiding (Result)
import InternalResponse
import IPAddress
import Kontra
import KontraLink
import Log.Identifier
import Redirect
import ThirdPartyStats.Core
import ThirdPartyStats.Planhat
import User.Email
import User.History.Model
import User.Model
import User.TwoFactor (verifyTOTPCode)
import UserGroup.Data
import UserGroup.Model
import Util.HasSomeUserInfo
import Util.MonadUtils
import Utils.HTTP

handleLoginGet :: Kontrakcja m => m (Either KontraLink Response)
handleLoginGet = do
  ctx <- getContext
  case (get ctxmaybeuser ctx) of
       Nothing -> do
          dirtyReferer <- getField "referer"
          let checkPrefixes s = s `isPrefixOf` fromMaybe "" dirtyReferer
              localReferer = any checkPrefixes ["/","%2F"]
              naughtyRef   = any checkPrefixes ["javascript:","data:"]
              referer = if localReferer then dirtyReferer else Nothing
          when naughtyRef $ logAttention_ "handleLoginGet: Somebody tried to XSS a referer"
          ad <- getAnalyticsData
          content <- renderTemplate "loginPageWithBranding" $ do
            F.value "referer" $ fromMaybe "/" referer
            F.value "nolinks" $ not $ get (bdMainDomain . ctxbrandeddomain) ctx
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
    -- we take TOTP asDirtyPassword here because we don't want to mis-report it
    -- as missing if it does not validate, we check it later
    mtotpcode <- getOptionalField asDirtyPassword "totp"
    case (memail, mpasswd) of
        (Just email, Just passwd) -> do
            -- check the user things here
            maybeuser <- dbQuery $ GetUserByEmail (Email email)
            ipIsOK <- case maybeuser of
                        Just u -> do
                             ug <- dbQuery $ UserGroupGetByUserID (userid u)
                             let masklist = get (ugsIPAddressMaskList . ugSettings) ug
                             return $ null masklist || (any (ipAddressIsInNetwork (get ctxipnumber ctx)) masklist)
                        Nothing -> return True
            case maybeuser of
                Just user@User{userpassword,userid,useraccountsuspended,usertotp,usertotpactive}
                    | maybeVerifyPassword userpassword passwd
                    && ipIsOK && not useraccountsuspended -> do
                        failedAttemptCount <- dbQuery $ GetUserRecentAuthFailureCount userid
                        if failedAttemptCount <= 5
                          then case (usertotp, usertotpactive, mtotpcode) of
                            (_, False, _) -> logTheUserIn ctx user padlogin
                            (Just totp, True, Just totpcode') -> do
                              now <- currentTime
                              let onBadTOTP = do
                                    logInfo "User login failed (invalid TOTP code provided)" $ logObject_ user
                                    _ <- dbUpdate $ LogHistoryLoginTOTPFailure userid (get ctxipnumber ctx) (get ctxtime ctx)
                                    J.runJSONGenT $ do
                                      J.value "logged" False
                                      J.value "totp_correct" False
                              case resultToMaybe $ asWord32 totpcode' of
                                Nothing -> onBadTOTP
                                Just totpcode ->
                                  if verifyTOTPCode totp now totpcode
                                     then logTheUserIn ctx user padlogin
                                     else do onBadTOTP
                            (_, True, Nothing) -> do
                              logInfo "User login failed: attempt without TOTP, expected, and next try should include it" $ logObject_ user
                              J.runJSONGenT $ do
                                J.value "logged" False
                                J.value "totp_missing" True
                            (Nothing, True, Just _) -> unexpectedError "TOTP condition should not happen"
                          else do
                            logInfo "User login failed (too many attempts)" $ logObject_ user
                            J.runJSONGenT $ J.value "logged" False

                Just u@User{userpassword} | not (maybeVerifyPassword userpassword passwd) -> do
                        logInfo "User login failed (invalid password)" $ logObject_ u
                        _ <- if padlogin
                          then dbUpdate $ LogHistoryPadLoginFailure (userid u) (get ctxipnumber ctx) (get ctxtime ctx)
                          else dbUpdate $ LogHistoryLoginFailure (userid u) (get ctxipnumber ctx) (get ctxtime ctx)
                        J.runJSONGenT $ J.value "logged" False

                Just u | not ipIsOK -> do
                        logInfo "User login failed (ip not on allowed list)" $ object [
                            logPair_ u
                          , "ip" .= show (get ctxipnumber ctx)
                          ]
                        _ <- if padlogin
                          then dbUpdate $ LogHistoryPadLoginFailure (userid u) (get ctxipnumber ctx) (get ctxtime ctx)
                          else dbUpdate $ LogHistoryLoginFailure (userid u) (get ctxipnumber ctx) (get ctxtime ctx)

                        ug <- dbQuery . UserGroupGetByUserID . userid $ u
                        admins <- dbQuery . GetUserGroupAdmins . get ugID $ ug
                        case admins of
                          (admin:_) -> J.runJSONGenT $ do
                                         J.value "logged" False
                                         J.value "ipaddr" (show (get ctxipnumber ctx))
                                         J.value "adminname" (getSmartName admin)
                          _ -> J.runJSONGenT $ do
                                         J.value "logged" False
                Just u -> do
                  {- MR: useraccountsuspended must be true here. This is a hack for Hi3G. It will be removed in future -}
                        logInfo "User login failed (user account suspended)" $ object [logPair_ u]
                        _ <- if padlogin
                          then dbUpdate $ LogHistoryPadLoginFailure (userid u) (get ctxipnumber ctx) (get ctxtime ctx)
                          else dbUpdate $ LogHistoryLoginFailure (userid u) (get ctxipnumber ctx) (get ctxtime ctx)
                        J.runJSONGenT $ J.value "logged" False
                Nothing -> do
                    logInfo "User login failed (user not found)" $ object [
                        "email" .= email
                      ]
                    J.runJSONGenT $ J.value "logged" False
        _ -> J.runJSONGenT $ J.value "logged" False

  where logTheUserIn ctx user padlogin = do
          logInfo "User logged in" $ logObject_ user
          muuser <- dbQuery $ GetUserByID (userid user)
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
                , IPProp $ get ctxipnumber ctx
                , TimeProp $ get ctxtime ctx ]
                EventMixpanel
              asyncLogEvent
                SetUserProps
                [ UserIDProp uid
                , someProp "Last login" $ get ctxtime ctx ]
                EventMixpanel
            _ -> return ()
          if padlogin
            then do
              _ <- dbUpdate $ LogHistoryPadLoginSuccess (userid user) (get ctxipnumber ctx) (get ctxtime ctx)
              logPadUserToContext muuser
            else do
              _ <- dbUpdate $ LogHistoryLoginSuccess (userid user) (get ctxipnumber ctx) (get ctxtime ctx)
              logUserToContext muuser
          J.runJSONGenT $ J.value "logged" True

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
  dirtyUrl <- guardJustM $ getField "url"
  let naughtyUrl = any (\s -> s `isPrefixOf` dirtyUrl) ["javascript:","data:"]
      url = if naughtyUrl then "" else dirtyUrl
  when naughtyUrl $ logAttention_ "handleLoginWithRedirectGet: someone tried to XSS url field"
  -- It may seems strange, that we do the same thing regardless whether the user
  -- session exists. The reason is, that session from link may become invalid, when
  -- user logs out from the original login link session. We want the original link
  -- to keep redirecting as intended. User may need to login again using his email and
  -- password depending on the redirect destination.
  _ <- unsafeSessionTakeover sci
  return $ internalResponse $ LinkExternal url
