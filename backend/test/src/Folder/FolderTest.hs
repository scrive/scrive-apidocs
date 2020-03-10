module Folder.FolderTest (folderTests) where

import Control.Monad.Extra (concatForM)
import Data.Aeson hiding ((<?>))
import Data.Aeson.Types hiding ((<?>))
import Data.Foldable (foldlM)
import Data.Int
import Data.Unjson (field, fieldBy, fieldOptBy, objectOf)
import Happstack.Server
import Test.Framework
import Test.QuickCheck
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as AE
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Unjson as Unjson

import AccessControl.Model
import AccessControl.Types
import Context
import DB
import Doc.API.V2.AesonTestUtils
import Doc.API.V2.Calls.CallsTestUtils
import Doc.API.V2.JSONTest
import Doc.API.V2.Mock.TestUtils
import Doc.SignatoryLinkID ()
import Doc.Types.SignatoryLink
import Folder.API
import Folder.JSON
import Folder.Model
import Partner.API
import TestingUtil
import TestKontra as T
import User.API
import User.Email
import User.Model
import UserGroup
import UserGroupAccounts.UserGroupAccountsControl
import Util.MonadUtils
import qualified Folder.Internal as I

folderTests :: TestEnvSt -> Test
folderTests env = testGroup
  "Folder"
  [ testThat "Creating groups works in DB"                env testCreateFolders
  , testThat "Get children recursive works" env testFolderGetAllChildrenRecursive
  , testThat "New signup users have home Folders" env testSignuUsersHaveHomeFolders
  , testThat "New partner create users have home Folders" env testPartnerUsersWithFolders
  , testThat "New partner create users have home Folders, only if companies do"
             env
             testPartnerUsersWithoutFolders
  , testThat "Add new user in unsergroup"        env testNewCompanyAccount
  , testThat "Moving a folder works"             env testMoveFolder
  , testThat "Moving a folder cannot form cycle" env testMoveFolderCycleError
  , testThat "Cannot delete a Folder with subfolders"
             env
             testCannotDeleteFolderWithSubfolders
  , testThat "Test creating groups works in API "              env testFolderAPICreate
  , testThat "Test updating groups works in API"               env testFolderAPIUpdate
  , testThat "Test reading groups works in API"                env testFolderAPIGet
  , testThat "Test folder deletion endpoint works in API"      env testFolderAPIDelete
  , testThat "Test listing documents in a folder works in API" env testFolderAPIListDocs
  ]

folderToFolderWithChildren :: Folder -> FolderWithChildren
folderToFolderWithChildren folder =
  I.FolderWithChildren { folder = folder, children = [] }

testCreateFolders :: TestEnv ()
testCreateFolders = do
  let folderDepth = 5
  void $ createFolderTree folderDepth
  assertCountAllFolders "Created tree of groups" (2 ^ (folderDepth + 1) - 1)

testFolderGetAllChildrenRecursive :: TestEnv ()
testFolderGetAllChildrenRecursive = do
  let folderDepth = 5
  (fRootID, _) <- createFolderTree folderDepth
  allChildren  <- dbQuery $ FolderGetAllChildrenRecursive fRootID
  assertEqual "All children returned"
              (2 ^ (folderDepth + 1) - 2)
              (length $ concatMap fwcToList allChildren)

testSignuUsersHaveHomeFolders :: TestEnv ()
testSignuUsersHaveHomeFolders = do
  -- signup new user
  ctx <- mkContext defaultLang
  req <- mkRequest POST [("email", inText "jabberwocky@scrive.com")]
  void $ runTestKontra req ctx $ apiCallSignup

  -- 1 UserGroup Home Folder, 1 User Home folder
  assertCountAllFolders "Created tree of groups with folders" 2

testPartnerUsersWithFolders :: TestEnv ()
testPartnerUsersWithFolders = do
  partnerAdminUserGroup <- instantiateRandomUserGroup
  partnerAdminUser      <- instantiateUser $ randomUserTemplate
    { firstName      = return "Arthur"
    , lastName       = return "Dent"
    , email          = return "arthur.dent@scrive.com"
    , groupID        = return $ partnerAdminUserGroup ^. #id
    , isCompanyAdmin = True
    }
  void
    .  dbUpdate
    .  AccessControlCreateForUser (partnerAdminUser ^. #id)
    .  UserGroupAdminAR
    $  partnerAdminUserGroup
    ^. #id
  assertCountAllFolders "Partner and Partner admin have Folders" 2

  (ctx, cid) <- partnerCompanyCreate partnerAdminUser partnerAdminUserGroup
  assertCountAllFolders "Partner, Partner admin and UserGroup have Folders" 3

  partnerUserCreate ctx cid $ partnerAdminUserGroup ^. #id
  assertCountAllFolders "Partner, Partner admin, UserGroup and User have Folders" 4

  ug <- guardJustM . dbQuery $ UserGroupGet cid
  assertBool "UserGroup has home Folder" . isJust $ ug ^. #homeFolderID

  user       <- fmap head . dbQuery $ UserGroupGetUsers cid
  userFolder <- guardJustM . dbQuery . FolderGet . fromJust $ user ^. #homeFolderID
  assertEqual "User home folder is child of UserGroup home folder"
              (ug ^. #homeFolderID)
              (userFolder ^. #parentID)

testPartnerUsersWithoutFolders :: TestEnv ()
testPartnerUsersWithoutFolders = do
  partnerAdminUserGroup <- instantiateRandomUserGroup
  partnerAdminUser      <- instantiateUser $ randomUserTemplate
    { firstName      = return "Arthur"
    , lastName       = return "Dent"
    , email          = return "arthur.dent@scrive.com"
    , groupID        = return $ partnerAdminUserGroup ^. #id
    , isCompanyAdmin = True
    }
  void
    .  dbUpdate
    .  AccessControlCreateForUser (partnerAdminUser ^. #id)
    .  UserGroupAdminAR
    $  partnerAdminUserGroup
    ^. #id
  assertCountAllFolders "Partner and Partner admin have Folders" 2

  (ctx, cid) <- partnerCompanyCreate partnerAdminUser partnerAdminUserGroup
  assertCountAllFolders "Partner, Partner admin and UserGroup have Folders" 3

  -- unlink UserGroup from Folder
  ug <- guardJustM . dbQuery $ UserGroupGet cid
  dbUpdate . UserGroupUpdate . set #homeFolderID Nothing $ ug

  partnerUserCreate ctx cid $ partnerAdminUserGroup ^. #id
  assertCountAllFolders "User havs no Folder" 3

partnerCompanyCreate :: User -> UserGroup -> TestEnv (Context, UserGroupID)
partnerCompanyCreate partnerAdminUser partnerAdminUserGroup = do
  ctx <- (set #maybeUser (Just partnerAdminUser)) <$> mkContext defaultLang
  let partnerUgID = partnerAdminUserGroup ^. #id
  newCompanyJSON <- readTestFile "json/partner_api_v1/param-partnerCompanyCreate.json"
  let rq_newCompany_params = [("json", inTextBS newCompanyJSON)]
      rq_newCompany_resp_fp =
        inTestDir "json/partner_api_v1/resp-partnerCompanyCreate.json"
  respValue <- runApiJSONTest ctx
                              POST
                              (partnerApiCallV1CompanyCreate partnerUgID)
                              rq_newCompany_params
                              201
                              rq_newCompany_resp_fp
  let Object respObject      = snd respValue
      Just   (String cidStr) = H.lookup "id" respObject
      cid                    = read cidStr
  return (ctx, cid)

partnerUserCreate :: Context -> UserGroupID -> UserGroupID -> TestEnv ()
partnerUserCreate ctx cid partnerUgID = do
  newUserGoodJSON <- readTestFile
    "json/partner_api_v1/param-partnerCompanyUserNew-good.json"
  let rq_newUserGood_params = [("json", inTextBS newUserGoodJSON)]
      rq_newUserGood_resp_fp =
        inTestDir "json/partner_api_v1/resp-partnerCompanyUserNew-good.json"
  void $ runApiJSONTest ctx
                        POST
                        (partnerApiCallV1UserCreate partnerUgID cid)
                        rq_newUserGood_params
                        201
                        rq_newUserGood_resp_fp

testNewCompanyAccount :: TestEnv ()
testNewCompanyAccount = do
  ug   <- instantiateRandomUserGroup
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Andrzej"
                                               , lastName       = return "Rybczak"
                                               , email = return "andrzej@skrivapa.se"
                                               , groupID        = return $ ug ^. #id
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  assertBool "UserGroup has home Folder" . isJust $ ug ^. #homeFolderID

  ctx    <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  bobReq <- mkRequest
    POST
    [ ("add"    , inText "True")
    , ("email"  , inText "bob@blue.com")
    , ("fstname", inText "Bob")
    , ("sndname", inText "Blue")
    ]
  void . runTestKontra bobReq ctx $ handleAddUserGroupAccount
  Just userBob <- dbQuery $ GetUserByEmail (Email "bob@blue.com")
  userFolder   <- guardJustM . dbQuery . FolderGet . fromJust $ userBob ^. #homeFolderID
  assertEqual "User home folder is child of UserGroup home folder"
              (ug ^. #homeFolderID)
              (userFolder ^. #parentID)

  -- unlink UserGroup from Folder
  dbUpdate . UserGroupUpdate . set #homeFolderID Nothing $ ug

  -- create another user
  cliffReq <- mkRequest
    POST
    [ ("add"    , inText "True")
    , ("email"  , inText "cliff@cranberry.com")
    , ("fstname", inText "Cliff")
    , ("sndname", inText "Cranberry")
    ]
  void . runTestKontra cliffReq ctx $ handleAddUserGroupAccount
  Just userCliff <- dbQuery $ GetUserByEmail (Email "cliff@cranberry.com")
  assertBool "User has no home Folder" . not . isJust $ userCliff ^. #homeFolderID

assertCountAllFolders :: String -> Int64 -> TestEnv ()
assertCountAllFolders msg expectedCount =
  assertSQLCount msg expectedCount "SELECT COUNT(*) FROM folders"

createFolderTree :: Int -> TestEnv (FolderID, [FolderID])
createFolderTree folderDepth = do
  -- create a tree of depth 5 with each parent having 2 subgroups
  let createFolder parentID =
        view #id <$> dbUpdate (FolderCreate $ set #parentID (Just parentID) defaultFolder)
      createFolders (parentIDs, fids) _lvl = do
        newFids <- concatForM parentIDs
          $ \parentID -> replicateM 2 $ createFolder parentID
        return (newFids, fids ++ newFids)

  fRootID      <- view #id <$> dbUpdate (FolderCreate defaultFolder)
  (_, allFids) <- foldlM createFolders ([fRootID], [fRootID]) [1 .. folderDepth]
  return (fRootID, allFids)


-- test moving a folder to other part of the tree
--
-- before
--          0
--     1         2
--  3   4     5     6
-- 7 8 9 10 11 12 13 14
--
-- after
--         0
--               2
--            5     6
--          11 12 13 14
--                    1
--                 3   4
--                7 8 9 10

testMoveFolder :: TestEnv ()
testMoveFolder = do
  -- create a 4 level tree, each parent 2 subfolders
  (_, fids)    <- createFolderTree 3
  -- take Root.Left folder and move it to Root.Right....Right folder
  Just folder1 <- dbQuery . FolderGet $ fids !! 1
  void $ dbUpdate . FolderUpdate . set #parentID (Just $ fids !! 14) $ folder1
  -- parentpath of new leaf in moved group should be 6 items long
  parentFolders <- dbQuery . FolderGetParents $ fids !! 10
  assertEqual ("Fetched all parents of leaf folder") 6 (length parentFolders)
  -- root still has 14 children
  children <-
    fmap (concatMap fwcToList) . dbQuery . FolderGetAllChildrenRecursive $ fids !! 0
  assertEqual ("Fetched all children of top folder") 14 (length children)

testMoveFolderCycleError :: TestEnv ()
testMoveFolderCycleError = do
  folderA0 <- rand 10 arbitrary
  folderA  <- dbUpdate . FolderCreate . set #parentID Nothing $ folderA0

  -- make B child of A
  folderB0 <- rand 10 arbitrary
  folderB  <- dbUpdate . FolderCreate . set #parentID (Just $ folderA ^. #id) $ folderB0
  -- making A child of B fails
  assertRaisesKontra (\(FoldersFormCycle _) -> True) . dbUpdate . FolderUpdate $ set
    #parentID
    (Just $ folderB ^. #id)
    folderA

testCannotDeleteFolderWithSubfolders :: TestEnv ()
testCannotDeleteFolderWithSubfolders = do
  folderA0 <- rand 10 arbitrary
  folderA  <- dbUpdate . FolderCreate . set #parentID Nothing $ folderA0

  -- make B child of A
  folderB0 <- rand 10 arbitrary
  folderB  <- dbUpdate . FolderCreate . set #parentID (Just $ folderA ^. #id) $ folderB0
  commit -- so that raising exception will not take our data with it
  -- deleting folder A raises exception
  assertRaisesDBException . runSQL_ $ "DELETE from folders where id = " <?> folderA ^. #id
  commit
  -- deleting folder B works
  runSQL_ $ "DELETE from folders where id = " <?> folderB ^. #id

-- API tests
testFolderAPICreate :: TestEnv ()
testFolderAPICreate = do
  user      <- instantiateRandomUser
  ctx       <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  -- make root of folder struct
  fdrRootID <- view #id <$> do
    dbUpdate . FolderCreate $ defaultFolder

  folderNameTest <- rand 10 arbitraryName
  let fdrTest = set #name folderNameTest . set #parentID (Just fdrRootID) $ defaultFolder
      newFolderJSONBS =
        AE.encodingToLazyByteString
          . encodeFolderWithChildren
          $ folderToFolderWithChildren fdrTest
      reqNewFolderPrms = [("folder", inTextBS newFolderJSONBS)]
  -- should not be able to create a child since the user is not an admin of the
  -- above folder root
  void $ jsonTestRequestHelper ctx POST reqNewFolderPrms folderAPICreate 403

  -- make admin of folder
  let folderAdminR = FolderAdminAR fdrRootID
  void . dbUpdate $ AccessControlInsertRoleForUser (user ^. #id) folderAdminR
  -- should now be able to create a child
  newFdrFromAPI <- jsonToFolder
    <$> jsonTestRequestHelper ctx POST reqNewFolderPrms folderAPICreate 200

  -- check that API returns what it's supposed to
  Just newFdrFromDB <- dbQuery . FolderGet $ newFdrFromAPI ^. #id
  assertEqual ("New folder from API equals the one we sent in")
              newFdrFromAPI
              (set #id (newFdrFromAPI ^. #id) fdrTest)
              -- since id is 0 for default

  assertEqual ("New folder from API equals corresponding one in DB")
              newFdrFromAPI
              newFdrFromDB

testFolderAPIUpdate :: TestEnv ()
testFolderAPIUpdate = do
  grpAdmin <- instantiateRandomUser
  user     <- instantiateRandomUser
  ctxAdmin <- (set #maybeUser (Just grpAdmin)) <$> mkContext defaultLang
  ctxUser  <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  fdrRoot  <- dbUpdate $ FolderCreate (set #name "Folder root" defaultFolder)
  let folderAdminRoot = FolderAdminAR (fdrRoot ^. #id)
      admid           = grpAdmin ^. #id
  void . dbUpdate $ AccessControlInsertRoleForUser admid folderAdminRoot
  subs@[fdr1        , fdr2         ] <- createChildrenForParentByAPI ctxAdmin fdrRoot
  [     childrenFdr1, _childrenFdr2] <- forM subs $ createChildrenForParentByAPI ctxAdmin

  let folderAdminUserFdr1 = FolderAdminAR (fdr1 ^. #id)
  void . dbUpdate $ AccessControlInsertRoleForUser (user ^. #id) folderAdminUserFdr1

  -- `user` should be able to update fdr1-rooted tree
  forM_ childrenFdr1 (updateTestHelper ctxUser)

  fdr <-
    dbUpdate
    $ FolderCreate
    $ (set #parentID (Just $ fdr1 ^. #id) . set #name "Folder for move test")
        defaultFolder
  allChildrenFdr1 <-
    concatMap fwcToList <$> (dbQuery $ FolderGetAllChildrenRecursive $ fdr1 ^. #id)
  assertEqual "Children of folder are the same"
              (sortByFolderID allChildrenFdr1)
              (sortByFolderID $ [fdr] <> childrenFdr1)

  let fdrTryMoving = set #parentID (Just $ fdr2 ^. #id) fdr
  -- `user` should _not_ be able to change parent of fdrTryMoving to fdr2 before being
  -- an admin of both parents
  void $ fdrAPIUpdate ctxUser fdrTryMoving 403

  -- make `user` admin of `fdr2`
  void . dbUpdate $ AccessControlInsertRoleForUser (user ^. #id)
                                                   (FolderAdminAR (fdr2 ^. #id))

  -- move should now be successful
  movedFdr1'        <- jsonToFolder <$> fdrAPIUpdate ctxUser fdrTryMoving 200

  Just moveddbFdr1' <- dbQuery . FolderGet $ fdrTryMoving ^. #id
  assertEqual "Moved folder from API should equal the corresponding one in DB"
              moveddbFdr1'
              movedFdr1'
  assertEqual "Moved folder from API should equal the one specified"
              moveddbFdr1'
              fdrTryMoving

  fromDBchildrenFdr1' <- do
    dbQuery . FolderGetImmediateChildren $ fdr1 ^. #id
  assertEqual "Children of folder are the same"
              (sortByFolderID fromDBchildrenFdr1')
              (sortByFolderID childrenFdr1)

  parentsOfFdr <- dbQuery . FolderGetParents $ fdr ^. #id
  assertEqual "Parent path of moved folder is correct" parentsOfFdr [fdr2, fdrRoot]
  where
    sortByFolderID :: [Folder] -> [Folder]
    sortByFolderID = sortBy (\f1 f2 -> compare (f1 ^. #id) (f2 ^. #id))

    fdrUpdateParams :: Folder -> [(Text, Input)]
    fdrUpdateParams fdr =
      let fdrwc   = folderToFolderWithChildren fdr
          fdrJSON = AE.encodingToLazyByteString $ encodeFolderWithChildren fdrwc
      in  [("folder", inTextBS fdrJSON)]

    fdrAPIUpdate :: Context -> Folder -> Int -> TestEnv Aeson.Value
    fdrAPIUpdate ctx fdr code = do
      let apiCall = folderAPIUpdate $ fdr ^. #id
      jsonTestRequestHelper ctx POST (fdrUpdateParams fdr) apiCall code

    updateTestHelper :: Context -> Folder -> TestEnv ()
    updateTestHelper ctx fdr = do
      name <- rand 10 arbitraryName
      let fdr' = (set #name name fdr)
      updatedFdr <- jsonToFolder <$> fdrAPIUpdate ctx fdr' 200
      assertEqual ("New folder from API should equal the one specified") fdr' updatedFdr
      Just dbFdr <- dbQuery . FolderGet $ fdr ^. #id
      assertEqual ("New folder from API should equal the corresponding one in DB")
                  dbFdr
                  updatedFdr
      -- reset for easier testing in calling body
      void $ jsonToFolder <$> fdrAPIUpdate ctx fdr 200

testFolderAPIGet :: TestEnv ()
testFolderAPIGet = do
  grpAdmin <- instantiateRandomUser
  ctxAdmin <- set #maybeUser (Just grpAdmin) <$> mkContext defaultLang
  fdrRoot  <- dbUpdate $ FolderCreate (set #name "Folder root" defaultFolder)
  let fdrRootID    = fdrRoot ^. #id
      folderAdminR = FolderAdminAR fdrRootID
      admid        = grpAdmin ^. #id
  void . dbUpdate $ AccessControlInsertRoleForUser admid folderAdminR
  subs@[_fdr1          , _fdr2          ] <- createChildrenForParentByAPI ctxAdmin fdrRoot
  [     _childrenOfFDR1, _childrenOfFDR2] <- do
    forM subs $ createChildrenForParentByAPI ctxAdmin
  fdrwcfuFromAPI <- fdrAPIGet ctxAdmin fdrRoot 200
  fdrChildren    <- dbQuery $ FolderGetImmediateChildren (fdrRoot ^. #id)
  let
    fdrwc =
      I.FolderWithChildren fdrRoot $ map (\c -> I.FolderWithChildren c []) fdrChildren
    fdrwcbs =
      fromJust . Aeson.decode . AE.encodingToLazyByteString $ encodeFolderWithChildren
        fdrwc
  assertEqual "Folder data output corresponds to what was sent in" fdrwcfuFromAPI fdrwcbs
  -- signatories should have access to folders for documents they participate in siging in
  mockDoc <- testDocApiV2New' ctxAdmin
  let signatoryEmail = "jakub.janczak@scrive.com" :: String
  signatoryUser <- instantiateUser $ randomUserTemplate
    { firstName = return "Jakub"
    , lastName  = return "Janczak"
    , email     = return $ T.pack signatoryEmail
    }
  signatoryUserCtx <- set #maybeUser (Just signatoryUser) <$> mkContext defaultLang
  let signatorySigLink =
        setMockSigLinkStandardField "mobile" "+48666666666"
          $ setMockSigLinkStandardField "email" signatoryEmail
          $ defaultMockSigLink
  let approverEmail = "barbara.streisand@scrive.com" :: String
  approverUser <- instantiateUser $ randomUserTemplate
    { firstName = return "Jakub"
    , lastName  = return "Janczak"
    , email     = return $ T.pack approverEmail
    }
  approverUserCtx <- set #maybeUser (Just approverUser) <$> mkContext defaultLang
  let approverSigLink =
        setMockSigLinkStandardField "mobile" "+48666666666"
          $ setMockSigLinkStandardField "email" approverEmail
          $ setMockDocSigLinkSignatoryRole SignatoryRoleApprover
          $ defaultMockSigLink

  void $ testDocApiV2AddParties ctxAdmin
                                [signatorySigLink, approverSigLink]
                                (getMockDocId mockDoc)
  void $ testDocApiV2Start' ctxAdmin (getMockDocId mockDoc)
  void $ jsonTestRequestHelper signatoryUserCtx
                               GET
                               []
                               (folderAPIGet (fromJust $ grpAdmin ^. #homeFolderID))
                               200
  void $ jsonTestRequestHelper approverUserCtx
                               GET
                               []
                               (folderAPIGet (fromJust $ grpAdmin ^. #homeFolderID))
                               200
  return ()

  where
    fdrAPIGet :: Context -> Folder -> Int -> TestEnv Aeson.Value
    fdrAPIGet ctx fdr code = do
      let apiCall = folderAPIGet $ fdr ^. #id
      jsonTestRequestHelper ctx GET [] apiCall code

testFolderAPIDelete :: TestEnv ()
testFolderAPIDelete = do
  user       <- instantiateUser $ randomUserTemplate { isCompanyAdmin = True }  -- TODO: Fix properly!
  ctx        <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  userFolder <- fromJust <$> (dbQuery . FolderGet . fromJust $ user ^. #homeFolderID)
  -- I don't want to test deletion of the user home folder, so let's create a subfolder.
  let fdrTestDeleteChild = set #parentID (Just $ userFolder ^. #id) $ defaultFolder
      newFolderJSONBS =
        AE.encodingToLazyByteString
          . encodeFolderWithChildren
          $ folderToFolderWithChildren fdrTestDeleteChild
      reqNewFolderPrms = [("folder", inTextBS newFolderJSONBS)]
  newFdrFromAPI <- jsonToFolder
    <$> jsonTestRequestHelper ctx POST reqNewFolderPrms folderAPICreate 200
  void $ jsonTestRequestHelper ctx
                               POST
                               reqNewFolderPrms
                               (folderAPIDelete $ newFdrFromAPI ^. #id)
                               200

testFolderAPIListDocs :: TestEnv ()
testFolderAPIListDocs = do
  user       <- instantiateUser $ randomUserTemplate { isCompanyAdmin = True }  -- TODO: Fix properly!
  ctx        <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  userFolder <- fromJust <$> (dbQuery . FolderGet . fromJust $ user ^. #homeFolderID)
  let numDocsToStart = 5
      fdrid          = userFolder ^. #id
  mapM_ testDocApiV2New' $ take numDocsToStart . repeat $ ctx
  -- add a subfolder to org folder tree and start documents there to ensure that we do not
  -- list documents contained in children of a specified folder
  let newFolderBase = set #parentID (Just $ userFolder ^. #id) defaultFolder
      newFolderBaseJSONBS =
        AE.encodingToLazyByteString
          . encodeFolderWithChildren
          $ folderToFolderWithChildren newFolderBase
      reqNewFolderPrms = [("folder", inTextBS newFolderBaseJSONBS)]
  newFdr <-
    jsonToFolder <$> (jsonTestRequestHelper ctx POST reqNewFolderPrms folderAPICreate 200)
  -- HACK to enable user to start documents in another folder
  dbUpdate $ FolderSetUserHomeFolder (user ^. #id) (newFdr ^. #id)
  user' <- fromJust <$> (dbQuery $ GetUserByID (user ^. #id))
  let ctx'            = set #maybeUser (Just user') ctx
      numDocsToStart' = 3
  mapM_ testDocApiV2New' $ take numDocsToStart' . repeat $ ctx'
  -- switch back to original home folder for user
  dbUpdate $ FolderSetUserHomeFolder (user ^. #id) fdrid
  docsVal <- jsonTestRequestHelper ctx GET [] (folderAPIListDocs fdrid) 200
  assertEqual "Listing by folder id does not retrieve documents in child folders"
              numDocsToStart
              (getTotalReturned docsVal)
  -- list child folder
  docsVal' <- jsonTestRequestHelper ctx GET [] (folderAPIListDocs (newFdr ^. #id)) 200
  assertEqual "Listing works for child of user home folder"
              numDocsToStart'
              (getTotalReturned docsVal')
  where
    getTotalReturned val =
      fromJust $ parseMaybe (withObject "List call output" (.: "total_matching")) val


--  dbUpdate $ FolderSetUserHome (userid user) (fdrid)

--
-- local helpers
--

jsonToFolder :: Aeson.Value -> Folder
jsonToFolder val = let Unjson.Result fdr _ = Unjson.parse unjsonFolderReadAll val in fdr

-- jsonToFolderWithChildrenForUpdate :: Aeson.Value -> FolderWithChildrenForUpdate
-- jsonToFolderWithChildrenForUpdate val =
--   let Unjson.Result fdrwcfu _ =
--         Unjson.parse unjsonFolderWithChildrenForUpdate val
--   in fdrwcfu

createChildrenForParentByAPI :: Context -> Folder -> TestEnv [Folder]
createChildrenForParentByAPI ctx fdrParent = do
  forM [fdrParent, fdrParent] $ \fdrp -> do
    let newFolder = set #parentID (Just $ fdrp ^. #id) defaultFolder
        newFolderJSONBS =
          AE.encodingToLazyByteString
            . encodeFolderWithChildren
            $ folderToFolderWithChildren newFolder
        reqNewFolderPrms = [("folder", inTextBS newFolderJSONBS)]
    jsonToFolder <$> jsonTestRequestHelper ctx POST reqNewFolderPrms folderAPICreate 200

-- read _all_ fields of the presumed JSON serialisation of a folder
unjsonFolderReadAll :: Unjson.UnjsonDef Folder
unjsonFolderReadAll =
  objectOf
    $   pure I.Folder
    <*> (fieldBy "id" (^. #id) "The folder ID" Unjson.unjsonDef)
    <*> (fieldOptBy "parent_id" (^. #parentID) "Parent folder ID" Unjson.unjsonDef)
    <*> (field "name" (^. #name) "The folder name")
