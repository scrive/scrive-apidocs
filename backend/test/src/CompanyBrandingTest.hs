module CompanyBrandingTest(
    companyBrandingTests
) where

import Data.Unjson
import Happstack.Server hiding (simpleHTTP)
import Test.Framework
import Text.JSON
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.UTF8 as BSL

import BrandedDomain.Model
import Branding.Adler32
import Company.CompanyControl
import Company.JSON
import DB
import TestingUtil
import TestKontra
import Theme.Model
import Theme.View
import User.Lang (defaultLang)
import User.Model
import UserGroup.Model

companyBrandingTests :: TestEnvSt -> Test
companyBrandingTests env = testGroup
  "CompanyBranding"
  [ testThat "Test that user can fetch company branding" env testFetchCompanyBranding
  , testThat "Test that user can fetch domain themes (used by previews)"
             env
             testFetchDomainThemes
  , testThat "Test that admin can change company themes" env testUpdateCompanyTheme
  , testThat "Test that admin can delete company themes" env testDeleteCompanyTheme
  , testThat "Test that normal user cant delete or change company themes"
             env
             testNormalUserCantChangeOrDeleteTheme
  , testThat "Test that admin can change company branding additional details "
             env
             testChangeCompanyUI
  , testThat "Test that normal user can't deleted or change company UI"
             env
             testNormalUseCantChangeCompanyUI
  , testThat
    "Test that branding cache change if one of themes is set to default but still used"
    env
    testBrandingCacheChangesIfOneOfThemesIsSetToDefault
  ]


testFetchCompanyBranding :: TestEnv ()
testFetchCompanyBranding = do
  ugid         <- view #id <$> addNewUserGroup
  Just user    <- addNewUserToUserGroup "Mariusz" "Rak" "mariusz+ut@scrive.com" ugid
  ctx          <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  req1         <- mkRequest GET []
  (avalue1, _) <- runTestKontra req1 ctx $ handleGetCompanyBranding Nothing
  case decode (BSL.toString $ A.encode avalue1) of
    Ok (_ :: JSValue) -> return ()
    _ -> assertFailure "Response from handleGetCompanyBranding is not a valid JSON"

  req2         <- mkRequest GET []
  (avalue2, _) <- runTestKontra req2 ctx $ handleGetThemes Nothing
  case decode (BSL.toString $ A.encode avalue2) of
    Ok (_ :: JSValue) -> return ()
    _                 -> assertFailure "Response from handleGetThemes is not a valid JSON"

testFetchDomainThemes :: TestEnv ()
testFetchDomainThemes = do
  ctx         <- mkContext defaultLang
  req1        <- mkRequest GET []
  (avalue, _) <- runTestKontra req1 ctx $ handleGetDomainThemes
  case decode (BSL.toString $ A.encode avalue) of
    Ok (_ :: JSValue) -> return ()
    _ -> assertFailure "Response from handleGetDomainThemes is not a valid JSON"

testUpdateCompanyTheme :: TestEnv ()
testUpdateCompanyTheme = do
  ug        <- addNewUserGroup
  Just user <- addNewUserToUserGroup "Mariusz" "Rak" "mariusz+ut@scrive.com" (ug ^. #id)
  ctx       <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  mainbd    <- dbQuery $ GetMainBrandedDomain
  mailTheme <- dbQuery $ GetTheme (mainbd ^. #mailTheme)
  newTheme  <- dbUpdate $ InsertNewThemeForUserGroup (ug ^. #id) mailTheme
  let newChangedTheme1 = newTheme { themeBrandColor = "#12399a" }
  let newChangedThemeStr1 = unjsonToByteStringLazy'
        (Options { pretty = True, indent = 2, nulls = True })
        unjsonTheme
        newChangedTheme1
  req1 <- mkRequest POST [("theme", inTextBS $ newChangedThemeStr1)]
  ((), _) <- runTestKontra req1 ctx $ handleUpdateTheme Nothing (themeID newChangedTheme1)
  newThemeChangedFromDB <- dbQuery $ GetTheme (themeID newTheme)
  assertEqual "Theme color has been changed"
              "#12399a"
              (themeBrandColor newThemeChangedFromDB)


  --Check if invalid color wil raise an exception
  let newChangedTheme2 = newTheme { themeBrandColor = "bla bla" }
  let newChangedThemeStr2 = unjsonToByteStringLazy'
        (Options { pretty = True, indent = 2, nulls = True })
        unjsonTheme
        newChangedTheme2
  req2 <- mkRequest POST [("theme", inTextBS $ newChangedThemeStr2)]
  assertRaisesDBException $ do
    ((), _) <- runTestKontra req2 ctx
      $ handleUpdateTheme Nothing (themeID newChangedTheme2)
    return ()

  --Check if invalid font wil raise an exception
  let newChangedTheme3 = newTheme { themeFont = "bla bla" }
  let newChangedThemeStr3 = unjsonToByteStringLazy'
        (Options { pretty = True, indent = 2, nulls = True })
        unjsonTheme
        newChangedTheme3
  req3 <- mkRequest POST [("theme", inTextBS $ newChangedThemeStr3)]
  assertRaisesDBException $ do
    ((), _) <- runTestKontra req3 ctx
      $ handleUpdateTheme Nothing (themeID newChangedTheme3)
    return ()

  --Check if all valid fonts can be used
  let changeFontTest = changeFontTest' newTheme ctx
  mapM_ changeFontTest fonts

  where
    changeFontTest' newTheme ctx font = do
      let newChangedTheme1 = newTheme { themeFont = font }
      let newChangedThemeStr1 = unjsonToByteStringLazy'
            (Options { pretty = True, indent = 2, nulls = True })
            unjsonTheme
            newChangedTheme1
      req1    <- mkRequest POST [("theme", inTextBS $ newChangedThemeStr1)]
      ((), _) <- runTestKontra req1 ctx
        $ handleUpdateTheme Nothing (themeID newChangedTheme1)
      newThemeChangedFromDB <- dbQuery $ GetTheme (themeID newTheme)
      assertEqual "Theme color has been changed" font (themeFont newThemeChangedFromDB)
    fonts =
      [ "\"arial black\",sans-serif"
      , "\"arial narrow\",sans-serif"
      , "\"comic sans ms\",sans-serif"
      , "\"courier new\",monospace"
      , "\"Source Sans Pro\", \"Helvetica Neue\", Arial, sans-serif"
      , "garamond,serif"
      , "georgia,serif"
      , "\"times new roman\",serif"
      , "tahoma,sans-serif"
      , "\"trebuchet ms\",sans-serif"
      , "verdana,sans-serif"
      , "arial,helvetica,sans-serif"
      , "helvetica,sans-serif"
      ]

testDeleteCompanyTheme :: TestEnv ()
testDeleteCompanyTheme = do
  ug        <- addNewUserGroup
  Just user <- addNewUserToUserGroup "Mariusz" "Rak" "mariusz+ut@scrive.com" (ug ^. #id)
  ctx       <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  mainbd    <- dbQuery $ GetMainBrandedDomain
  mailTheme <- dbQuery $ GetTheme (mainbd ^. #mailTheme)
  newTheme  <- dbUpdate $ InsertNewThemeForUserGroup (ug ^. #id) mailTheme
  req1      <- mkRequest POST []
  ((), _)   <- runTestKontra req1 ctx $ handleDeleteTheme Nothing (themeID newTheme)
  assertRaisesDBException $ do
    void $ dbQuery $ GetTheme (themeID newTheme)
    return ()
  return ()


testNormalUserCantChangeOrDeleteTheme :: TestEnv ()
testNormalUserCantChangeOrDeleteTheme = do
  ug         <- addNewUserGroup
  Just user1 <- addNewUserToUserGroup "Mariusz" "Rak" "mariusz+ut@scrive.com" (ug ^. #id)
  True       <- dbUpdate $ SetUserCompanyAdmin (userid user1) False
  Just user2 <- dbQuery $ GetUserByID (userid user1)

  ctx        <- (set #maybeUser (Just user2)) <$> mkContext defaultLang

  mainbd     <- dbQuery $ GetMainBrandedDomain
  mailTheme  <- dbQuery $ GetTheme (mainbd ^. #mailTheme)
  newTheme   <- dbUpdate $ InsertNewThemeForUserGroup (ug ^. #id) mailTheme
  let newChangedTheme1 = newTheme { themeBrandColor = "#12399a" }
  let newChangedThemeStr1 = unjsonToByteStringLazy'
        (Options { pretty = True, indent = 2, nulls = True })
        unjsonTheme
        newChangedTheme1
  req1 <- mkRequest POST [("theme", inTextBS $ newChangedThemeStr1)]
  -- We should get exception when updating company theme, when not admin

  assertRaisesInternalError $ do
    ((), _) <- runTestKontra req1 ctx
      $ handleUpdateTheme Nothing (themeID newChangedTheme1)
    return ()

  -- Theme should also not be changed
  newThemeChangedFromDB <- dbQuery $ GetTheme (themeID newTheme)
  assertEqual "Theme color is same"
              (themeBrandColor mailTheme)
              (themeBrandColor newThemeChangedFromDB)

  -- We should also get expection when trying to delete theme
  assertRaisesInternalError $ do
    ((), _) <- runTestKontra req1 ctx $ handleDeleteTheme Nothing (themeID newTheme)
    return ()


testChangeCompanyUI :: TestEnv ()
testChangeCompanyUI = do
  ug        <- addNewUserGroup
  Just user <- addNewUserToUserGroup "Mariusz" "Rak" "mariusz+ut@scrive.com" (ug ^. #id)
  ctx       <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  mainbd    <- dbQuery $ GetMainBrandedDomain
  mailTheme <- dbQuery $ GetTheme (mainbd ^. #mailTheme)
  newTheme1 <- dbUpdate $ InsertNewThemeForUserGroup (ug ^. #id) mailTheme
  newTheme2 <- dbUpdate $ InsertNewThemeForUserGroup (ug ^. #id) mailTheme
  newTheme3 <- dbUpdate $ InsertNewThemeForUserGroup (ug ^. #id) mailTheme
  let ugui =
        (ug ^. #ui)
          & (#mailTheme ?~ themeID newTheme1)
          & (#signviewTheme ?~ themeID newTheme2)
          & (#serviceTheme ?~ themeID newTheme3)
          & (#browserTitle ?~ "Wow")

      newUgUIStr1 = unjsonToByteStringLazy'
        (Options { pretty = True, indent = 2, nulls = True })
        unjsonUserGroupUI
        ugui
  req1       <- mkRequest POST [("companyui", inTextBS newUgUIStr1)]
  ((), _)    <- runTestKontra req1 ctx $ handleChangeCompanyBranding Nothing
  mugUIAfter <- (view #ui <$>) <$> (dbQuery $ UserGroupGet $ ug ^. #id)
  assertEqual "User group UI has been changed" mugUIAfter (Just ugui)

testNormalUseCantChangeCompanyUI :: TestEnv ()
testNormalUseCantChangeCompanyUI = do
  ug         <- addNewUserGroup
  Just user1 <- addNewUserToUserGroup "Mariusz" "Rak" "mariusz+ut@scrive.com" (ug ^. #id)
  True       <- dbUpdate $ SetUserCompanyAdmin (userid user1) False
  Just user2 <- dbQuery $ GetUserByID (userid user1)
  ctx        <- (set #maybeUser (Just user2)) <$> mkContext defaultLang

  mainbd     <- dbQuery $ GetMainBrandedDomain
  mailTheme  <- dbQuery $ GetTheme (mainbd ^. #mailTheme)
  newTheme1  <- dbUpdate $ InsertNewThemeForUserGroup (ug ^. #id) mailTheme
  newTheme2  <- dbUpdate $ InsertNewThemeForUserGroup (ug ^. #id) mailTheme
  newTheme3  <- dbUpdate $ InsertNewThemeForUserGroup (ug ^. #id) mailTheme

  let oldUGUI = ug ^. #ui
      newUGUI =
        oldUGUI
          & (#mailTheme ?~ themeID newTheme1)
          & (#signviewTheme ?~ themeID newTheme2)
          & (#serviceTheme ?~ themeID newTheme3)
          & (#browserTitle ?~ "Wow")

      newUGUIStr1 = unjsonToByteStringLazy'
        (Options { pretty = True, indent = 2, nulls = True })
        unjsonUserGroupUI
        newUGUI
  req1 <- mkRequest POST [("companyui", inTextBS $ newUGUIStr1)]
  assertRaisesInternalError $ do
    ((), _) <- runTestKontra req1 ctx $ handleChangeCompanyBranding Nothing
    return ()
  mugUIAfter <- (view #ui <$>) <$> (dbQuery $ UserGroupGet $ ug ^. #id)
  assertEqual "User group UI has not been changed" mugUIAfter (Just oldUGUI)

testBrandingCacheChangesIfOneOfThemesIsSetToDefault :: TestEnv ()
testBrandingCacheChangesIfOneOfThemesIsSetToDefault = do
  ug            <- addNewUserGroup
  ctx           <- mkContext defaultLang

  mainbd        <- dbQuery $ GetMainBrandedDomain
  signviewTheme <- dbQuery $ GetTheme (mainbd ^. #signviewTheme)
  newTheme      <- dbUpdate
    $ InsertNewThemeForUserGroup (ug ^. #id) signviewTheme { themeBrandColor = "#669713" }
  let newUgUI =
        (ug ^. #ui)
          & (#mailTheme ?~ themeID newTheme)
          & (#signviewTheme ?~ themeID newTheme)
          & (#serviceTheme ?~ themeID newTheme)

  void $ dbUpdate $ UserGroupUpdate $ set #ui newUgUI ug
  (Just ugui1) <- (view #ui <$>) <$> (dbQuery $ UserGroupGet $ ug ^. #id)
  adlerSum1    <- brandingAdler32 ctx $ Just (ug ^. #id, ugui1)

  void $ dbUpdate $ UserGroupUpdate $ set #ui (set #serviceTheme Nothing ugui1) ug
  (Just ugui2) <- (view #ui <$>) <$> (dbQuery $ UserGroupGet $ ug ^. #id)
  adlerSum2    <- brandingAdler32 ctx $ Just (ug ^. #id, ugui2)

  assertBool "Branding Adler32 should change after we stoped using theme for service"
             (adlerSum1 /= adlerSum2)
