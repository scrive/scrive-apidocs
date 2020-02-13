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
import qualified Data.Text as T
import qualified Text.JSON.Gen as J
import qualified Text.StringTemplates.Fields as F

import Analytics.Include
import AppView
import DB
import Happstack.Fields
import InputValidation hiding (Result)
import InternalResponse
import IPAddress
import Kontra
import KontraLink
import Log.Identifier
import LoginAuth.LoginAuthMethod
import Redirect
import Session.Cookies
import ThirdPartyStats.Core
import ThirdPartyStats.Planhat
import User.Email
import User.History.Model
import User.Model
import User.TwoFactor (verifyTOTPCode)
import UserGroup.Model
import UserGroup.Types
import Util.MonadUtils
import Utils.HTTP

handleLoginGet :: Kontrakcja m => m (Either KontraLink Response)
handleLoginGet = do
  ctx <- getContext
  case ctx ^. #maybeUser of
    Nothing -> do
      dirtyReferer <- getField "referer"
      let checkPrefixes s = s `T.isPrefixOf` fromMaybe "" dirtyReferer
          localReferer = any checkPrefixes ["/", "%2F"]
          naughtyRef   = any checkPrefixes ["javascript:", "data:"]
          referer      = if localReferer then dirtyReferer else Nothing
      when naughtyRef $ logAttention_ "handleLoginGet: Somebody tried to XSS a referer"
      ad      <- getAnalyticsData
      content <- renderTemplate "loginPageWithBranding" $ do
        F.value "referer" $ fromMaybe "/" referer
        F.value "nolinks" $ not $ ctx ^. #brandedDomain % #mainDomain
        standardPageFields ctx Nothing ad
      response <- simpleHtmlResponse $ T.pack content
      return $ Right response
    Just _ -> return $ Left LinkDesignView

-- | Handles the submission of a login form.  On failure will redirect
-- back to referer, if there is one.
handleLoginPost :: Kontrakcja m => m JSValue
handleLoginPost = do
  padlogin  <- isFieldSet "pad"
  ctx       <- getContext
  memail    <- getOptionalField asDirtyEmail "email"
  mpasswd   <- getOptionalField asDirtyPassword "password"
  -- we take TOTP asDirtyPassword here because we don't want to mis-report it
  -- as missing if it does not validate, we check it later
  mtotpcode <- getOptionalField asDirtyPassword "totp"
  case (memail, mpasswd) of
    (Just email, Just passwd) -> do
        -- check the user things here
      maybeuser <- dbQuery $ GetUserByEmail (Email email)
      ipIsOK    <- case maybeuser of
        Just u -> do
          ugwp <- dbQuery $ UserGroupGetWithParentsByUserID (u ^. #id)
          let masklist = ugwpSettings ugwp ^. #ipAddressMaskList
          return $ null masklist || (any (ipAddressIsInNetwork $ ctx ^. #ipAddr) masklist)
        Nothing -> return True
      case maybeuser of
        -- Is the user even allowed to login here? In order not to enable user discovery, we
        -- act as if it doesn't exist; cf. the `Nothing` case in `case maybeuser of` below.
        Just user | user ^. #sysAuth /= LoginAuthNative -> userNotFound email
        Just user
          | maybeVerifyPassword (user ^. #password) passwd && ipIsOK && not
            (user ^. #accountSuspended)
          -> do
            failedAttemptCount <- dbQuery $ GetUserRecentAuthFailureCount $ user ^. #id
            if failedAttemptCount <= 5
              then case (user ^. #totpKey, user ^. #totpActive, mtotpcode) of
                (_        , False, _             ) -> logTheUserIn ctx user padlogin
                (Just totp, True , Just totpcode') -> do
                  now <- currentTime
                  let onBadTOTP = do
                        logInfo "User login failed (invalid TOTP code provided)"
                          $ logObject_ user
                        void $ dbUpdate $ LogHistoryLoginTOTPFailure (user ^. #id)
                                                                     (ctx ^. #ipAddr)
                                                                     (ctx ^. #time)
                        J.runJSONGenT $ do
                          J.value "logged" False
                          J.value "totp_correct" False
                  case resultToMaybe $ asWord32 totpcode' of
                    Nothing       -> onBadTOTP
                    Just totpcode -> if verifyTOTPCode totp now totpcode
                      then logTheUserIn ctx user padlogin
                      else do
                        onBadTOTP
                (_, True, Nothing) -> do
                  logInfo
                      "User login failed: attempt without TOTP, expected, and next try should include it"
                    $ logObject_ user
                  J.runJSONGenT $ do
                    J.value "logged" False
                    J.value "totp_missing" True
                (Nothing, True, Just _) ->
                  unexpectedError "TOTP condition should not happen"
              else do
                logInfo "User login failed (too many attempts)" $ logObject_ user
                J.runJSONGenT $ J.value "logged" False

        Just u | not (maybeVerifyPassword (u ^. #password) passwd) -> do
          logInfo "User login failed (invalid password)" $ logObject_ u
          void $ if padlogin
            then dbUpdate
              $ LogHistoryPadLoginFailure (u ^. #id) (ctx ^. #ipAddr) (ctx ^. #time)
            else dbUpdate
              $ LogHistoryLoginFailure (u ^. #id) (ctx ^. #ipAddr) (ctx ^. #time)
          J.runJSONGenT $ J.value "logged" False

        Just u | not ipIsOK -> do
          logInfo "User login failed (ip not on allowed list)"
            $ object [logPair_ u, "ip" .= show (ctx ^. #ipAddr)]
          void $ if padlogin
            then dbUpdate
              $ LogHistoryPadLoginFailure (u ^. #id) (ctx ^. #ipAddr) (ctx ^. #time)
            else dbUpdate
              $ LogHistoryLoginFailure (u ^. #id) (ctx ^. #ipAddr) (ctx ^. #time)
          J.runJSONGenT $ J.value "logged" False
        Just u -> do
          {- MR: useraccountsuspended must be true here. This is a hack for Hi3G. It will be removed in future -}
          logInfo "User login failed (user account suspended)" $ object [logPair_ u]
          void $ if padlogin
            then dbUpdate
              $ LogHistoryPadLoginFailure (u ^. #id) (ctx ^. #ipAddr) (ctx ^. #time)
            else dbUpdate
              $ LogHistoryLoginFailure (u ^. #id) (ctx ^. #ipAddr) (ctx ^. #time)
          J.runJSONGenT $ J.value "logged" False
        Nothing -> userNotFound email
    _ -> J.runJSONGenT $ J.value "logged" False

  where
    userNotFound email = do
      logInfo "User login failed (user not found)" $ object ["email" .= email]
      J.runJSONGenT $ J.value "logged" False

    logTheUserIn ctx user padlogin = do
      logInfo "User logged in" $ logObject_ user
      muuser <- dbQuery $ GetUserByID (user ^. #id)
      case muuser of
        Just uuser -> do
          let uid = uuser ^. #id
          now <- currentTime
          asyncLogEvent SetUserProps (simplePlanhatAction "Login" user now) EventPlanhat
          asyncLogEvent
            "Login"
            [UserIDProp uid, IPProp $ ctx ^. #ipAddr, TimeProp $ ctx ^. #time]
            EventMixpanel
          asyncLogEvent SetUserProps
                        [UserIDProp uid, someProp "Last login" $ ctx ^. #time]
                        EventMixpanel
        _ -> return ()
      if padlogin
        then do
          void $ dbUpdate $ LogHistoryPadLoginSuccess (user ^. #id)
                                                      (ctx ^. #ipAddr)
                                                      (ctx ^. #time)
          logPadUserToContext muuser
        else do
          void $ dbUpdate $ LogHistoryLoginSuccess (user ^. #id)
                                                   (ctx ^. #ipAddr)
                                                   (ctx ^. #time)
          logUserToContext muuser
      J.runJSONGenT $ J.value "logged" True

-- | Handles logout and sends the user back to the main page.
--
-- This should be removed when we rewrite header to React, and we
-- should only use handleLogoutAJAX then
handleLogout :: Kontrakcja m => m Response
handleLogout = do
  logUserToContext Nothing
  logPadUserToContext Nothing
  -- Excessive stopSessionCookie here will clean all session related cookies,
  -- even if they hold broken values
  stopSessionCookie
  (sendRedirect . LinkExternal) =<< getHttpHostpart

handleLogoutAJAX :: Kontrakcja m => m JSValue
handleLogoutAJAX = do
  logUserToContext Nothing
  logPadUserToContext Nothing
  -- Excessive stopSessionCookie here will clean all session related cookies,
  -- even if they hold broken values
  stopSessionCookie
  J.runJSONGenT $ J.value "success" True

handleLoginWithRedirectGet :: Kontrakcja m => m InternalKontraResponse
handleLoginWithRedirectGet = do
  sci :: SessionCookieInfo <- guardJustM $ readField "session_id"
  dirtyUrl                 <- guardJustM $ getField "url"
  let naughtyUrl = any (\s -> s `T.isPrefixOf` dirtyUrl) ["javascript:", "data:"]
      url        = if naughtyUrl then "" else dirtyUrl
  when naughtyUrl
    $ logAttention_ "handleLoginWithRedirectGet: someone tried to XSS url field"
  -- It may seem strange that we do the same thing regardless of
  -- whether the user session exists. The reason is that the session
  -- from the link may become invalid when user logs out from the
  -- original login link session. We want the original link to keep
  -- redirecting as intended. The user may need to login again using
  -- their email and password depending on the redirect destination.
  void $ unsafeSessionTakeover sci
  return $ internalResponse $ LinkExternal url
