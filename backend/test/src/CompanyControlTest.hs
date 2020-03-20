module CompanyControlTest (companyControlTests) where

import Happstack.Server hiding (simpleHTTP)
import Test.Framework
import Text.JSON
import Text.JSON.FromJSValue
import qualified Data.Aeson as A
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Text as T

import Company.CompanyControl
import DB
import TestingUtil
import TestKontra as T
import Theme.Model
import User.Lang (defaultLang)
import User.Types.SignupMethod
import UserGroup.Model
import UserGroup.Types
import Util.MonadUtils

companyControlTests :: TestEnvSt -> Test
companyControlTests env = testGroup
  "CompanyControl"
  [ testThat "handleGetCompanyJSON works" env test_handleGetCompanyJSON
  , testThat "handleChangeCompanyBranding can be used to set the company ui"
             env
             test_settingUIWithHandleChangeCompanyBranding
  , testThat
    "handleChangeCompanyBranding can't connect any company ui with different company or domain themes"
    env
    test_settingUIWithHandleChangeCompanyBrandingRespectsThemeOwnership
  ]

test_handleGetCompanyJSON :: TestEnv ()
test_handleGetCompanyJSON = do
  ug   <- instantiateRandomUserGroup
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Andrzej"
                                               , lastName       = return "Rybczak"
                                               , email = return "andrzej@skrivapa.se"
                                               , groupID        = return $ ug ^. #id
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }

  ugwp <- dbQuery . UserGroupGetWithParentsByUG $ ug
  let ugui = ugwpUI ugwp

  ctx              <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  req              <- mkRequest GET []
  (avalue, _ctx')  <- runTestKontra req ctx $ handleGetCompanyBranding Nothing
  (jsv :: JSValue) <- case decode (BSL.toString $ A.encode avalue) of
    Ok js -> return $ js
    _     -> unexpectedError "Response from handleGetCompanyBranding is not a valid JSON"
  (jsonCompanyid :: String) <- guardJustM $ withJSValue jsv $ fromJSValueField "companyid"
  (jsonMailTheme :: Maybe String) <- withJSValue jsv $ fromJSValueField "mailTheme"
  (jsonSignviewTheme :: Maybe String) <- withJSValue jsv
    $ fromJSValueField "signviewTheme"
  (jsonServiceTheme :: Maybe String ) <- withJSValue jsv $ fromJSValueField "serviceTheme"
  (jsonBrowserTitle :: Maybe String ) <- withJSValue jsv $ fromJSValueField "browserTitle"
  (jsonSmsOriginator :: Maybe String) <- withJSValue jsv
    $ fromJSValueField "smsOriginator"
  (jsonFavicon :: Maybe String) <- withJSValue jsv $ fromJSValueField "favicon"

  assertEqual "JSON companyid matches company id" (show $ ug ^. #id) (jsonCompanyid)
  assertEqual "JSON companyMailTheme matches companyMailTheme"
              (show <$> ugui ^. #mailTheme)
              (jsonMailTheme)
  assertEqual "JSON companySignviewTheme matches companySignviewTheme"
              (show <$> ugui ^. #signviewTheme)
              (jsonSignviewTheme)
  assertEqual "JSON companyServiceTheme matches companyServiceTheme"
              (show <$> ugui ^. #serviceTheme)
              (jsonServiceTheme)
  assertEqual "JSON browserTitle matches browserTitle"
              (T.unpack <$> ugui ^. #browserTitle)
              (jsonBrowserTitle)
  assertEqual "JSON smsOriginator matches SmsOriginator"
              (T.unpack <$> ugui ^. #smsOriginator)
              (jsonSmsOriginator)
  assertEqual
    "JSON favicon matches favicon"
    (ugui ^. #favicon)
    (   B64.decodeLenient
    <$> BS.fromString
    <$> drop 1
    <$> dropWhile ((/=) ',')
    <$> jsonFavicon
    )



test_settingUIWithHandleChangeCompanyBranding :: TestEnv ()
test_settingUIWithHandleChangeCompanyBranding = do

  ug   <- instantiateRandomUserGroup
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Andrzej"
                                               , lastName       = return "Rybczak"
                                               , email = return "andrzej@skrivapa.se"
                                               , groupID        = return $ ug ^. #id
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }

  ctx                     <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  -- Try setting new themes
  mailThemeFromDomain     <- dbQuery $ GetTheme (ctx ^. #brandedDomain % #mailTheme)
  mailTheme <- dbUpdate $ InsertNewThemeForUserGroup (ug ^. #id) mailThemeFromDomain
  signviewThemeFromDomain <- dbQuery $ GetTheme (ctx ^. #brandedDomain % #signviewTheme)
  signviewTheme           <- dbUpdate
    $ InsertNewThemeForUserGroup (ug ^. #id) signviewThemeFromDomain
  serviceThemeFromDomain <- dbQuery $ GetTheme (ctx ^. #brandedDomain % #serviceTheme)
  serviceTheme <- dbUpdate $ InsertNewThemeForUserGroup (ug ^. #id) serviceThemeFromDomain
  let browserTitle  = "Super"
  let smsOriginator = "Super SMS"
  let favicon = "-almoust-binary-data-aaa-000000000-"
  let faviconBase64 =
        T.pack
          $ BS.toString
          $ BS.append (BS.fromString "data:image/png;base64,")
          $ B64.encode
          $ BS.fromString
          $ favicon
  req1 <- mkRequest
    POST
    [ ( "companyui"
      , inText
      $  "{\"companyid\":\""
      <> showt (ug ^. #id)
      <> "\",\"mailTheme\":\""
      <> showt (themeID mailTheme)
      <> "\",\"signviewTheme\":\""
      <> showt (themeID signviewTheme)
      <> "\",\"serviceTheme\":\""
      <> showt (themeID serviceTheme)
      <> "\",\"browserTitle\":\""
      <> browserTitle
      <> "\",\"smsOriginator\":\""
      <> smsOriginator
      <> "\",\"favicon\":\""
      <> faviconBase64
      <> "\"}"
      )
    ]
  (_, _)           <- runTestKontra req1 ctx $ handleChangeCompanyBranding Nothing
  req2             <- mkRequest GET []
  (avalue, _)      <- runTestKontra req2 ctx $ handleGetCompanyBranding Nothing
  (jsv :: JSValue) <- case decode (BSL.toString $ A.encode avalue) of
    Ok js -> return $ js
    _     -> unexpectedError "Response from handleGetCompanyBranding is not a valid JSON"
  (jsonMailTheme :: Maybe Text    ) <- withJSValue jsv $ fromJSValueField "mailTheme"
  (jsonSignviewTheme :: Maybe Text) <- withJSValue jsv $ fromJSValueField "signviewTheme"
  (jsonServiceTheme :: Maybe Text ) <- withJSValue jsv $ fromJSValueField "serviceTheme"
  (jsonBrowserTitle :: Maybe Text ) <- withJSValue jsv $ fromJSValueField "browserTitle"
  (jsonSmsOriginator :: Maybe Text) <- withJSValue jsv $ fromJSValueField "smsOriginator"
  (jsonFavicon :: Maybe Text      ) <- withJSValue jsv $ fromJSValueField "favicon"

  assertEqual "JSON companyMailTheme matches companyMailTheme after update"
              (Just $ showt $ themeID mailTheme)
              (jsonMailTheme)
  assertEqual "JSON companySignviewTheme matches companySignviewTheme after update"
              (Just $ showt $ themeID signviewTheme)
              (jsonSignviewTheme)
  assertEqual "JSON companyServiceTheme matches companyServiceTheme after update"
              (Just $ showt $ themeID serviceTheme)
              (jsonServiceTheme)
  assertEqual "JSON browserTitle matches browserTitle after update"
              (Just browserTitle)
              (jsonBrowserTitle)
  assertEqual "JSON smsOriginator matches SmsOriginator after update"
              (Just smsOriginator)
              (jsonSmsOriginator)
  assertEqual
    "JSON favicon matches favicon after update"
    (Just favicon)
    (   BS.toString
    <$> B64.decodeLenient
    <$> BS.fromString
    <$> drop 1
    <$> dropWhile ((/=) ',')
    <$> T.unpack
    <$> jsonFavicon
    )

  --Test removing all compoany ui settings
  req3 <- mkRequest
    POST
    [ ( "companyui"
      , inText
      $ "{\"companyid\":\""
      <> showt (ug ^. #id)
      <> "\",\"mailTheme\":null,\"signviewTheme\":null,\"serviceTheme\":null,\"browserTitle\": null ,\"smsOriginator\": null,\"favicon\":null}"
      )
    ]
  (_, _) <- runTestKontra req3 ctx $ handleChangeCompanyBranding Nothing
  ugwp   <- dbQuery . UserGroupGetWithParentsByUG $ ug
  let ugui = ugwpUI ugwp
  assertEqual "CompanyMailTheme  is empty"     (ugui ^. #mailTheme)     (Nothing)
  assertEqual "CompanySignviewTheme  is empty" (ugui ^. #signviewTheme) (Nothing)
  assertEqual "CompanyServiceTheme  is empty"  (ugui ^. #serviceTheme)  (Nothing)
  assertEqual "BrowserTitle is empty"          (ugui ^. #browserTitle)  (Nothing)
  assertEqual "SmsOriginator is empty"         (ugui ^. #smsOriginator) (Nothing)
  assertEqual "Favicon is empty"               (ugui ^. #favicon)       (Nothing)

test_settingUIWithHandleChangeCompanyBrandingRespectsThemeOwnership :: TestEnv ()
test_settingUIWithHandleChangeCompanyBrandingRespectsThemeOwnership = do

  ug   <- instantiateRandomUserGroup
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Andrzej"
                                               , lastName       = return "Rybczak"
                                               , email = return "andrzej@skrivapa.se"
                                               , groupID        = return $ ug ^. #id
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  ctx  <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  --Test we can't set mailTheme to domain theme
  req1 <- mkRequest
    POST
    [ ( "companyui"
      , inText
      $  "{\"companyid\":\""
      <> showt (ug ^. #id)
      <> "\",\"mailTheme\":\""
      <> showt (ctx ^. #brandedDomain % #mailTheme)
      <> "\",\"signviewTheme\":null,\"serviceTheme\":null,\"browserTitle\": null ,\"smsOriginator\": null,\"favicon\":null}"
      )
    ]
  assertRaisesKontra (\(_ :: ThemesNotOwnedByUserGroup) -> True)
    . void
    . runTestKontra req1 ctx
    $ handleChangeCompanyBranding Nothing

  ugwp' <- dbQuery . UserGroupGetWithParentsByUG $ ug
  assertEqual "Can't set domain theme as company theme"
              (ugwpUI ugwp' ^. #mailTheme)
              Nothing

  -- Create theme for other company
  otherUg      <- instantiateRandomUserGroup
  someTheme    <- dbQuery $ GetTheme (ctx ^. #brandedDomain % #mailTheme)
  otherUgTheme <- dbUpdate $ InsertNewThemeForUserGroup (otherUg ^. #id) someTheme

  --Test we can't set mailTheme to other company theme
  req2         <- mkRequest
    POST
    [ ( "companyui"
      , inText
      $  T.pack
      $  "{\"companyid\":\""
      <> show (ug ^. #id)
      <> "\",\"mailTheme\":\""
      <> show (themeID otherUgTheme)
      <> "\",\"signviewTheme\":null,\"serviceTheme\":null,\"browserTitle\": null ,\"smsOriginator\": null,\"favicon\":null}"
      )
    ]
  assertRaisesKontra (\(_ :: ThemesNotOwnedByUserGroup) -> True)
    . void
    . runTestKontra req2 ctx
    $ handleChangeCompanyBranding Nothing
  ugwp2 <- dbQuery . UserGroupGetWithParentsByUG $ ug
  assertEqual "Can't set other company theme as company theme"
              (ugwpUI ugwp2 ^. #mailTheme)
              Nothing
