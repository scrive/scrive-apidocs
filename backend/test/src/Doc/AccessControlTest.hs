module Doc.AccessControlTest (docAccessControlTests) where

import Control.Exception (SomeException)
import Control.Monad.Catch (catch)
import Happstack.Server
import Log
import Test.Framework

import AccessControl.Model
import AccessControl.Types
import Attachment.Model
import DB.Query (dbUpdate)
import Doc.AccessControl
import Doc.API.V2.Calls.DocumentGetCalls
import Doc.DocControl
import Doc.DocumentID (DocumentID)
import Doc.DocumentMonad (withDocumentID)
import Doc.Model.Update
import Doc.SignatoryLinkID
import Doc.Types.Document
import Doc.Types.DocumentStatus
import Doc.Types.SignatoryAccessToken
import Doc.Types.SignatoryLink
import File.FileID
import Folder.Model
import Kontra
import TestingUtil
import TestKontra
import User.Lang (defaultLang)
import User.Types.User
import Util.Actor
import Util.SignatoryLinkUtils

docAccessControlTests :: TestEnvSt -> Test
docAccessControlTests env = testGroup
  "Doc.AccessControl"
  [ testThat "Basic document access control with docAccessValidRoles"
             env
             testBasicValidRoles
  , testThat "Basic GET document access control"        env testBasicAccessControl
  , testThat "Basic GET document author access control" env testAuthorAccessControl
  , testThat "Basic GET shared document access control" env testSharedAccessControl
  , testThat "Group home folder access control"         env testGroupAccessControl
  , testThat "Folder inheritance access control"        env testFolderAccessControl
  , testThat "Folder inheritance shared access control" env testSharedFolderAccessControl
  , testThat "Document file access control"             env testDocumentFileAccessControl
  ]

createSignatoryContext :: DocumentID -> SignatoryLinkID -> TestEnv Context
createSignatoryContext docId signatoryId = do
  token <- dbUpdate
    $ NewSignatoryAccessToken signatoryId SignatoryAccessTokenForMailBeforeClosing Nothing

  getRequest <- mkRequestWithHeaders GET [] []
  ctx1       <- (set #maybeUser Nothing) <$> mkContext defaultLang
  (_, ctx2)  <- runTestKontra getRequest ctx1
    $ handleSignShowSaveMagicHash docId signatoryId token
  return ctx2

getDocumentValidRoles
  :: Document -> Maybe User -> Maybe SignatoryLink -> TestEnv [DocAccessRole]
getDocumentValidRoles doc mUser mSignatory = do
  ctx             <- mkContext defaultLang
  getRequest      <- mkRequestWithHeaders GET [] []
  (validRoles, _) <- runTestKontra getRequest ctx
    $ docAccessValidRoles doc mUser mSignatory
  return validRoles

assertHasDocumentPermission
  :: String -> Document -> Maybe User -> Maybe SignatoryLink -> TestEnv ()
assertHasDocumentPermission message doc mUser mSignatory = do
  validRoles <- getDocumentValidRoles doc mUser mSignatory
  case validRoles of
    [] ->
      assertFailure
        $  "Failure: expected user or signatory to have valid roles for document "
        <> show (documentid doc)
        <> ": "
        <> message
    _ -> return ()

assertNoDocumentPermission
  :: String -> Document -> Maybe User -> Maybe SignatoryLink -> TestEnv ()
assertNoDocumentPermission message doc mUser mSignatory = do
  validRoles <- getDocumentValidRoles doc mUser mSignatory
  case validRoles of
    (_ : _) ->
      assertFailure
        $  "Failure: expected user or signatory to NOT have valid roles for document "
        <> show (documentid doc)
        <> ": "
        <> message
    _ -> return ()

assertGetDocumentSucceed
  :: String -> DocumentID -> Context -> [(Text, Input)] -> TestEnv ()
assertGetDocumentSucceed message docId ctx params = do
  request       <- mkRequest GET params
  (response, _) <- runTestKontra request ctx $ docApiV2Get docId
  assertEqual message 200 (rsCode response)

assertGetDocumentFails :: String -> DocumentID -> Context -> [(Text, Input)] -> TestEnv ()
assertGetDocumentFails message docId ctx params = do
  request       <- mkRequest GET params
  (response, _) <- runTestKontra request ctx $ docApiV2Get docId
  assertEqual message 403 (rsCode response)

testBasicValidRoles :: TestEnv ()
testBasicValidRoles = do
  userGroup <- instantiateUserGroup $ randomUserGroupTemplate
  let userGroupId = userGroup ^. #id

  folderUser      <- instantiateRandomUser
  folderGuest     <- instantiateRandomUser
  folderAdmin     <- instantiateRandomUser
  otherUser       <- instantiateRandomUser
  otherFolderUser <- instantiateRandomUser

  groupUser       <- instantiateUser $ randomUserTemplate { groupID = return userGroupId }
  groupAdmin      <- instantiateUser
    $ randomUserTemplate { groupID = return userGroupId, isCompanyAdmin = True }

  folder <- dbUpdate . FolderCreate $ defaultFolder
  let folderId = folder ^. #id

  otherFolder <- dbUpdate . FolderCreate $ defaultFolder
  let otherFolderId = otherFolder ^. #id

  void $ dbUpdate $ AccessControlCreateForUser (folderUser ^. #id) (FolderUserAR folderId)
  void $ dbUpdate $ AccessControlCreateForUser (folderGuest ^. #id)
                                               (SharedTemplateUserAR folderId)
  void $ dbUpdate $ AccessControlCreateForUser (folderAdmin ^. #id)
                                               (FolderAdminAR folderId)

  void $ dbUpdate $ AccessControlCreateForUser (otherFolderUser ^. #id)
                                               (FolderUserAR otherFolderId)
  void $ dbUpdate $ AccessControlCreateForUser (otherFolderUser ^. #id)
                                               (FolderAdminAR otherFolderId)

  -- Assign group user all other possible roles that should not have permission to document
  void $ dbUpdate $ AccessControlCreateForUser (groupUser ^. #id)
                                               (UserAdminAR userGroupId)
  void $ dbUpdate $ AccessControlCreateForUser (groupUser ^. #id)
                                               (UserGroupAdminAR userGroupId)
  void $ dbUpdate $ AccessControlCreateForUser (groupUser ^. #id)
                                               (UserGroupMemberAR userGroupId)
  void $ dbUpdate $ AccessControlCreateForUser (groupUser ^. #id)
                                               (EidImpersonatorAR userGroupId)

  author <- instantiateUser $ randomUserTemplate { groupID = return userGroupId }

  void $ dbUpdate $ AccessControlCreateForUser (author ^. #id) (FolderUserAR folderId)

  do -- Document preparation phase
    doc <- addRandomDocument $ (rdaDefault author) { rdaTypes    = OneOf [Signable]
                                                   , rdaStatuses = OneOf [Preparation]
                                                   , rdaSharings = OneOf [Private]
                                                   , rdaFolderId = folderId
                                                   }

    let authorSigLink = fromJust $ getSigLinkFor (author ^. #id) doc
    let participantSigLink =
          fromJust $ find (\sl -> not $ signatoryisauthor sl) (documentsignatorylinks doc)

    assertHasDocumentPermission "author should have permission to draft document"
                                doc
                                (Just author)
                                Nothing

    -- Draft documents don't allow generation of magic hashes for signatory links,
    -- but since this is internal function call we can still test signatory
    -- access for draft documents if they can ever be authenticated.
    assertNoDocumentPermission
      "author signatory should not have permission to draft document"
      doc
      Nothing
      (Just authorSigLink)

    assertHasDocumentPermission "folder user should have permission to draft document"
                                doc
                                (Just folderUser)
                                Nothing

    assertNoDocumentPermission "signatory should not have permission to draft document"
                               doc
                               Nothing
                               (Just participantSigLink)

    assertNoDocumentPermission
      "folder admin should not have permission to draft document"
      doc
      (Just folderAdmin)
      Nothing

    assertNoDocumentPermission
      "shared template user should not have permission to draft document"
      doc
      (Just folderGuest)
      Nothing

    assertNoDocumentPermission "other user should not have permission to draft document"
                               doc
                               (Just otherUser)
                               Nothing

    assertNoDocumentPermission
      "other folder user should not have permission to draft document"
      doc
      (Just otherFolderUser)
      Nothing

    assertNoDocumentPermission "group user should not have permission to draft document"
                               doc
                               (Just groupUser)
                               Nothing

    assertNoDocumentPermission
      "group admin should not have permission to draft document"
      doc
      (Just groupAdmin)
      Nothing

    assertNoDocumentPermission
      "anonymous user should not have permission to draft document"
      doc
      Nothing
      Nothing

  do -- Document signing phase
    doc <- addRandomDocument $ (rdaDefault author) { rdaTypes    = OneOf [Signable]
                                                   , rdaStatuses = OneOf [Pending]
                                                   , rdaSharings = OneOf [Private]
                                                   , rdaFolderId = folderId
                                                   }

    let authorSigLink = fromJust $ getSigLinkFor (author ^. #id) doc
    let participantSigLink =
          fromJust $ find (\sl -> not $ signatoryisauthor sl) (documentsignatorylinks doc)

    assertHasDocumentPermission "Author should have permission to pending document"
                                doc
                                (Just author)
                                Nothing

    assertHasDocumentPermission
      "Author signatory should have permission to pending document"
      doc
      Nothing
      (Just authorSigLink)

    assertHasDocumentPermission
      "Author user or signatory should have permission to pending document"
      doc
      (Just author)
      (Just authorSigLink)

    assertHasDocumentPermission
      "Participant signatory should have permission to pending document"
      doc
      Nothing
      (Just participantSigLink)

    assertHasDocumentPermission
      "Participant signatory as other user should have permission to pending document"
      doc
      (Just otherUser)
      (Just participantSigLink)

    assertHasDocumentPermission "Folder user should have permission to pending document"
                                doc
                                (Just folderUser)
                                Nothing

    assertHasDocumentPermission
      "Folder admin should not have permission to pending document"
      doc
      (Just folderAdmin)
      Nothing

    assertNoDocumentPermission
      "Other folder user not should have permission to pending document"
      doc
      (Just otherFolderUser)
      Nothing

    assertNoDocumentPermission
      "Group user should not have permission to pending document"
      doc
      (Just groupUser)
      Nothing

    assertNoDocumentPermission
      "Group admin should not have permission to pending document"
      doc
      (Just groupAdmin)
      Nothing

    assertNoDocumentPermission
      "Anonymous should not have permission to pending document"
      doc
      Nothing
      Nothing

  do -- Cancelled document
    doc <- addRandomDocument $ (rdaDefault author) { rdaTypes    = OneOf [Signable]
                                                   , rdaStatuses = OneOf [Canceled]
                                                   , rdaSharings = OneOf [Private]
                                                   , rdaFolderId = folderId
                                                   }

    let authorSigLink = fromJust $ getSigLinkFor (author ^. #id) doc
    let participantSigLink =
          fromJust $ find (\sl -> not $ signatoryisauthor sl) (documentsignatorylinks doc)

    assertHasDocumentPermission "Author should have permission to cancelled document"
                                doc
                                (Just author)
                                Nothing

    assertNoDocumentPermission
      "Author signatory should not have permission to cancelled document"
      doc
      Nothing
      (Just authorSigLink)

    assertNoDocumentPermission
      "Participant signatory should not have permission to cancelled document"
      doc
      Nothing
      (Just participantSigLink)

    assertHasDocumentPermission
      "Folder user should have permission to cancelled document"
      doc
      (Just folderUser)
      Nothing

    assertHasDocumentPermission
      "Folder admin should have permission to cancelled document"
      doc
      (Just folderUser)
      Nothing

    assertNoDocumentPermission
      "Other folder user not should have permission to cancelled document"
      doc
      (Just otherFolderUser)
      Nothing

    assertNoDocumentPermission
      "Group user should not have permission to cancelled document"
      doc
      (Just groupUser)
      Nothing

    assertNoDocumentPermission
      "Group admin should not have permission to cancelled document"
      doc
      (Just groupAdmin)
      Nothing

    assertNoDocumentPermission
      "Anonymous should not have permission to cancelled document"
      doc
      Nothing
      Nothing

testBasicAccessControl :: TestEnv ()
testBasicAccessControl = do
  userGroup <- instantiateUserGroup $ randomUserGroupTemplate
  let userGroupId = userGroup ^. #id

  folderUser      <- instantiateRandomUser
  folderGuest     <- instantiateRandomUser
  folderAdmin     <- instantiateRandomUser
  otherUser       <- instantiateRandomUser
  otherFolderUser <- instantiateRandomUser

  groupUser       <- instantiateUser $ randomUserTemplate { groupID = return userGroupId }
  groupAdmin      <- instantiateUser
    $ randomUserTemplate { groupID = return userGroupId, isCompanyAdmin = True }

  anonCtx            <- (set #maybeUser Nothing) <$> mkContext defaultLang
  folderUserCtx      <- (set #maybeUser (Just folderUser)) <$> mkContext defaultLang
  folderGuestCtx     <- (set #maybeUser (Just folderGuest)) <$> mkContext defaultLang
  folderAdminCtx     <- (set #maybeUser (Just folderAdmin)) <$> mkContext defaultLang
  otherUserCtx       <- (set #maybeUser (Just otherUser)) <$> mkContext defaultLang
  otherFolderUserCtx <- (set #maybeUser (Just otherFolderUser)) <$> mkContext defaultLang
  groupUserCtx       <- (set #maybeUser (Just groupUser)) <$> mkContext defaultLang
  groupAdminCtx      <- (set #maybeUser (Just groupAdmin)) <$> mkContext defaultLang

  folder             <- dbUpdate . FolderCreate $ defaultFolder
  let folderId = folder ^. #id

  otherFolder <- dbUpdate . FolderCreate $ defaultFolder
  let otherFolderId = otherFolder ^. #id

  now <- currentTime

  void $ dbUpdate $ AccessControlCreateForUser (folderUser ^. #id) (FolderUserAR folderId)
  void $ dbUpdate $ AccessControlCreateForUser (folderGuest ^. #id)
                                               (SharedTemplateUserAR folderId)
  void $ dbUpdate $ AccessControlCreateForUser (folderAdmin ^. #id)
                                               (FolderAdminAR folderId)

  void $ dbUpdate $ AccessControlCreateForUser (otherFolderUser ^. #id)
                                               (FolderUserAR otherFolderId)
  void $ dbUpdate $ AccessControlCreateForUser (otherFolderUser ^. #id)
                                               (FolderAdminAR otherFolderId)

  -- Assign group user all other possible roles that should not have permission to document
  void $ dbUpdate $ AccessControlCreateForUser (groupUser ^. #id)
                                               (UserAdminAR userGroupId)
  void $ dbUpdate $ AccessControlCreateForUser (groupUser ^. #id)
                                               (UserGroupAdminAR userGroupId)
  void $ dbUpdate $ AccessControlCreateForUser (groupUser ^. #id)
                                               (UserGroupMemberAR userGroupId)
  void $ dbUpdate $ AccessControlCreateForUser (groupUser ^. #id)
                                               (EidImpersonatorAR userGroupId)

  author    <- instantiateUser $ randomUserTemplate { groupID = return userGroupId }
  authorCtx <- (set #maybeUser (Just author)) <$> mkContext defaultLang

  void $ dbUpdate $ AccessControlCreateForUser (author ^. #id) (FolderUserAR folderId)

  do -- Document preparation phase
    logInfo_ "Test access control for draft document"

    docId <- fmap documentid $ addRandomDocument $ (rdaDefault author)
      { rdaTypes    = OneOf [Signable]
      , rdaStatuses = OneOf [Preparation]
      , rdaSharings = OneOf [Private]
      , rdaFolderId = folderId
      }

    assertGetDocumentSucceed
      "Author should be able to get draft document with folder permission available"
      docId
      authorCtx
      []

    assertGetDocumentSucceed
      "Folder user should be able to get draft document with folder permission available"
      docId
      folderUserCtx
      []

    assertGetDocumentFails
      "Folder admin should not able to get draft document with no folder permission"
      docId
      folderAdminCtx
      []

    assertGetDocumentFails
      "Shared folder user should not able to get draft document with no folder permission"
      docId
      folderGuestCtx
      []

    assertGetDocumentFails
      "Other user should not able to get draft document with no folder permission"
      docId
      otherUserCtx
      []

    assertGetDocumentFails
      "Other folder user should not able to get draft document with no folder permission"
      docId
      otherFolderUserCtx
      []

    assertGetDocumentFails
      "Group user should not able to get draft document with no folder permission"
      docId
      groupUserCtx
      []

    assertGetDocumentFails
      "Group admin should not able to get draft document with no folder permission"
      docId
      groupAdminCtx
      []

    assertGetDocumentFails
      "Anonymous user should not able to get draft document with no folder permission"
      docId
      anonCtx
      []

  do -- Document signing phase
    logInfo_ "Test access control for started document"

    doc <- addRandomDocument $ (rdaDefault author) { rdaTypes    = OneOf [Signable]
                                                   , rdaStatuses = OneOf [Pending]
                                                   , rdaSharings = OneOf [Private]
                                                   , rdaFolderId = folderId
                                                   }

    let docId              = documentid doc
        authorSigLink      = fromJust $ getSigLinkFor (author ^. #id) doc
        participantSigLink = fromJust
          $ find (\sl -> not $ signatoryisauthor sl) (documentsignatorylinks doc)

    authorSignCtx      <- createSignatoryContext docId (signatorylinkid authorSigLink)
    participantSignCtx <- createSignatoryContext docId
                                                 (signatorylinkid participantSigLink)

    assertGetDocumentSucceed
      "Author should be able to access through signatory id without logging in"
      docId
      authorSignCtx
      [("signatory_id", inText $ showt $ signatorylinkid authorSigLink)]

    assertGetDocumentSucceed
      "Signatory user should be able to access through signatory id without logging in"
      docId
      participantSignCtx
      [("signatory_id", inText $ showt $ signatorylinkid participantSigLink)]

    assertGetDocumentSucceed
      "Signatory user should be able to access through signatory id when logging in as other user"
      docId
      (set #maybeUser (Just otherUser) participantSignCtx)
      [("signatory_id", inText $ showt $ signatorylinkid participantSigLink)]

    assertGetDocumentSucceed
      "Author should be able to get started document with folder permission available"
      docId
      authorCtx
      []

    assertGetDocumentSucceed
      "Folder user should be able to get started document with folder permission available"
      docId
      folderUserCtx
      []

    assertGetDocumentSucceed
      "Folder admin should be able to get started document with folder permission available"
      docId
      folderAdminCtx
      []

    assertGetDocumentFails
      "Other user should not able to access document with invalid signatory session"
      docId
      otherUserCtx
      [("signatory_id", inText $ showt $ signatorylinkid participantSigLink)]

    assertGetDocumentFails
      "Shared folder user should not able to get started document with no folder permission"
      docId
      folderGuestCtx
      []

    assertGetDocumentFails
      "Other user should not able to get started document with no folder permission"
      docId
      otherUserCtx
      []

    assertGetDocumentFails
      "Other folder user should not able to get started document with no folder permission"
      docId
      otherFolderUserCtx
      []

    assertGetDocumentFails
      "Group user should not able to get started document with no folder permission"
      docId
      groupUserCtx
      []

    assertGetDocumentFails
      "Group admin should not able to get started document with no folder permission"
      docId
      groupAdminCtx
      []

    assertGetDocumentFails
      "Anonymous user should not able to get started document with no folder permission"
      docId
      anonCtx
      []

  do -- Document cancelled
    logInfo_ "Test access control after document cancellation"

    doc <- addRandomDocument $ (rdaDefault author) { rdaTypes    = OneOf [Signable]
                                                   , rdaStatuses = OneOf [Pending]
                                                   , rdaSharings = OneOf [Private]
                                                   , rdaFolderId = folderId
                                                   }

    let docId              = documentid doc
        authorSigLink      = fromJust $ getSigLinkFor (author ^. #id) doc
        participantSigLink = fromJust
          $ find (\sl -> not $ signatoryisauthor sl) (documentsignatorylinks doc)

    authorSignCtx      <- createSignatoryContext docId (signatorylinkid authorSigLink)
    participantSignCtx <- createSignatoryContext docId
                                                 (signatorylinkid participantSigLink)

    withDocumentID docId $ dbUpdate $ CancelDocument (systemActor now)

    assertGetDocumentSucceed
      "Author should still able to get cancelled document logged in"
      docId
      authorCtx
      []

    assertGetDocumentFails
      "Author signatory should not be able to get cancelled document"
      docId
      authorSignCtx
      [("signatory_id", inText $ showt $ signatorylinkid authorSigLink)]

    assertGetDocumentFails
      "Participant signatory should not be able to get cancelled document"
      docId
      participantSignCtx
      [("signatory_id", inText $ showt $ signatorylinkid participantSigLink)]

    assertGetDocumentSucceed
      "Folder user should still be able to get cancelled document"
      docId
      folderUserCtx
      []

    assertGetDocumentSucceed
      "Folder admin should still be able to get cancelled document"
      docId
      folderAdminCtx
      []

    assertGetDocumentFails
      "Shared folder user should not able to get cancelled document"
      docId
      folderGuestCtx
      []

    assertGetDocumentFails "Other user should not able to get cancelled document"
                           docId
                           otherUserCtx
                           []

    assertGetDocumentFails "Other folder user should not able to get cancelled document"
                           docId
                           otherFolderUserCtx
                           []

    assertGetDocumentFails "Group user should not able to get cancelled document"
                           docId
                           groupUserCtx
                           []

    assertGetDocumentFails "Group admin should not able to get cancelled document"
                           docId
                           groupAdminCtx
                           []

    assertGetDocumentFails "Anonymous user should not able to get cancelled document"
                           docId
                           anonCtx
                           []

testAuthorAccessControl :: TestEnv ()
testAuthorAccessControl = do
  userGroup <- instantiateUserGroup $ randomUserGroupTemplate
  let userGroupId = userGroup ^. #id

  folder <- dbUpdate . FolderCreate $ defaultFolder
  let folderId = folder ^. #id

  now       <- currentTime

  author    <- instantiateUser $ randomUserTemplate { groupID = return userGroupId }
  authorCtx <- (set #maybeUser (Just author)) <$> mkContext defaultLang

  role <- dbUpdate $ AccessControlCreateForUser (author ^. #id) (FolderUserAR folderId)
  let roleId = fromJust $ accessRoleGetAccessRoleId $ fromJust role

  doc <- addRandomDocument $ (rdaDefault author) { rdaTypes    = OneOf [Signable]
                                                 , rdaStatuses = OneOf [Pending]
                                                 , rdaSharings = OneOf [Private]
                                                 , rdaFolderId = folderId
                                                 }

  let docId         = documentid doc
      authorSigLink = fromJust $ getSigLinkFor (author ^. #id) doc
      participantSigLink =
        fromJust $ find (\sl -> not $ signatoryisauthor sl) (documentsignatorylinks doc)

  authorSignCtx      <- createSignatoryContext docId (signatorylinkid authorSigLink)
  participantSignCtx <- createSignatoryContext docId (signatorylinkid participantSigLink)

  do -- Remove author's access to folder
    logInfo_ "Removing author's access to folder"

    void $ dbUpdate $ AccessControlRemoveRole roleId

    assertGetDocumentFails
      "Author should not able to get document with folder permission removed"
      docId
      authorCtx
      []

    assertGetDocumentSucceed
      "Author should still be able to access through signatory id without logging in"
      docId
      authorSignCtx
      [("signatory_id", inText $ showt $ signatorylinkid authorSigLink)]

    assertGetDocumentSucceed
      "Author should still be able to access through signatory id while logged in"
      docId
      (set #maybeUser (Just author) authorSignCtx)
      [("signatory_id", inText $ showt $ signatorylinkid authorSigLink)]

  do -- Document cancelled
    logInfo_ "Test access control after document cancellation"

    withDocumentID docId $ dbUpdate $ CancelDocument (systemActor now)

    assertGetDocumentFails "Author should not able to get cancelled document logged in"
                           docId
                           authorCtx
                           []

    assertGetDocumentFails
      "Author signatory should not able to get cancelled document"
      docId
      authorSignCtx
      [("signatory_id", inText $ showt $ signatorylinkid authorSigLink)]

    assertGetDocumentFails
      "Participant signatory should not be able to get cancelled document"
      docId
      participantSignCtx
      [("signatory_id", inText $ showt $ signatorylinkid participantSigLink)]

testSharedAccessControl :: TestEnv ()
testSharedAccessControl = do
  userGroup <- instantiateUserGroup $ randomUserGroupTemplate
  let userGroupId = userGroup ^. #id

  folderUser      <- instantiateRandomUser
  folderGuest     <- instantiateRandomUser
  folderAdmin     <- instantiateRandomUser
  otherUser       <- instantiateRandomUser
  otherFolderUser <- instantiateRandomUser

  groupUser       <- instantiateUser $ randomUserTemplate { groupID = return userGroupId }
  groupAdmin      <- instantiateUser
    $ randomUserTemplate { groupID = return userGroupId, isCompanyAdmin = True }

  anonCtx            <- (set #maybeUser Nothing) <$> mkContext defaultLang
  folderUserCtx      <- (set #maybeUser (Just folderUser)) <$> mkContext defaultLang
  folderGuestCtx     <- (set #maybeUser (Just folderGuest)) <$> mkContext defaultLang
  folderAdminCtx     <- (set #maybeUser (Just folderAdmin)) <$> mkContext defaultLang
  otherUserCtx       <- (set #maybeUser (Just otherUser)) <$> mkContext defaultLang
  otherFolderUserCtx <- (set #maybeUser (Just otherFolderUser)) <$> mkContext defaultLang
  groupUserCtx       <- (set #maybeUser (Just groupUser)) <$> mkContext defaultLang
  groupAdminCtx      <- (set #maybeUser (Just groupAdmin)) <$> mkContext defaultLang

  folder             <- dbUpdate . FolderCreate $ defaultFolder
  let folderId = folder ^. #id

  otherFolder <- dbUpdate . FolderCreate $ defaultFolder
  let otherFolderId = otherFolder ^. #id

  void $ dbUpdate $ AccessControlCreateForUser (folderUser ^. #id) (FolderUserAR folderId)
  void $ dbUpdate $ AccessControlCreateForUser (folderGuest ^. #id)
                                               (SharedTemplateUserAR folderId)
  void $ dbUpdate $ AccessControlCreateForUser (folderAdmin ^. #id)
                                               (FolderAdminAR folderId)

  void $ dbUpdate $ AccessControlCreateForUser (otherFolderUser ^. #id)
                                               (FolderUserAR otherFolderId)
  void $ dbUpdate $ AccessControlCreateForUser (otherFolderUser ^. #id)
                                               (FolderAdminAR otherFolderId)

  -- Assign group user all other possible roles that should not have permission to document
  void $ dbUpdate $ AccessControlCreateForUser (groupUser ^. #id)
                                               (UserAdminAR userGroupId)
  void $ dbUpdate $ AccessControlCreateForUser (groupUser ^. #id)
                                               (UserGroupAdminAR userGroupId)
  void $ dbUpdate $ AccessControlCreateForUser (groupUser ^. #id)
                                               (UserGroupMemberAR userGroupId)
  void $ dbUpdate $ AccessControlCreateForUser (groupUser ^. #id)
                                               (EidImpersonatorAR userGroupId)

  author <- instantiateUser $ randomUserTemplate { groupID = return userGroupId }
  authorCtx <- (set #maybeUser (Just author)) <$> mkContext defaultLang

  role <- dbUpdate $ AccessControlCreateForUser (author ^. #id) (FolderUserAR folderId)
  let roleId = fromJust $ accessRoleGetAccessRoleId $ fromJust role

  docId <- fmap documentid $ addRandomDocument $ (rdaDefault author)
    { rdaTypes    = OneOf [Template]
    , rdaStatuses = OneOf [Preparation]
    , rdaSharings = OneOf [Shared]
    , rdaFolderId = folderId
    }

  assertGetDocumentSucceed
    "Author should be able to get shared document with folder permission available"
    docId
    authorCtx
    []

  assertGetDocumentSucceed
    "Folder user should be able to get shared document with folder permission available"
    docId
    folderUserCtx
    []

  assertGetDocumentSucceed
    "Shared folder user should be able to get shared document with folder permission available"
    docId
    folderGuestCtx
    []

  assertGetDocumentFails
    "Folder admin should not able to get shared document with no folder permission"
    docId
    folderAdminCtx
    []

  assertGetDocumentFails
    "Other user should not able to get shared document with no folder permission"
    docId
    otherUserCtx
    []

  assertGetDocumentFails
    "Other folder user should not able to get shared document with no folder permission"
    docId
    otherFolderUserCtx
    []

  assertGetDocumentFails
    "Group user should not able to get shared document with no folder permission"
    docId
    groupUserCtx
    []

  assertGetDocumentFails "Group admin should not able to get shared document"
                         docId
                         groupAdminCtx
                         []

  assertGetDocumentFails
    "Anonymous user should not able to get shared document with no folder permission"
    docId
    anonCtx
    []

  do -- Remove author's access to folder
    logInfo_ "Removing author's access to shared folder"

    void $ dbUpdate $ AccessControlRemoveRole roleId

  assertGetDocumentFails
    "Author should not able to get shared document with folder permission removed"
    docId
    authorCtx
    []

  assertGetDocumentSucceed
    "Shared folder user should still be able to get shared document with folder permission available"
    docId
    folderGuestCtx
    []

testGroupAccessControl :: TestEnv ()
testGroupAccessControl = do
  userGroup <- instantiateUserGroup $ randomUserGroupTemplate
  let userGroupId = userGroup ^. #id
      folderId    = fromJust $ userGroup ^. #homeFolderID

  author     <- instantiateUser $ randomUserTemplate { groupID = return userGroupId }

  -- From derivedRoles, group user have SharedTemplateUserAR on group home folder
  groupUser  <- instantiateUser $ randomUserTemplate { groupID = return userGroupId }

  -- From derivedRoles, group admin have FolderAdminAR and SharedTemplateUserAR on group home folder
  groupAdmin <- instantiateUser
    $ randomUserTemplate { groupID = return userGroupId, isCompanyAdmin = True }

  authorCtx     <- (set #maybeUser (Just author)) <$> mkContext defaultLang
  groupUserCtx  <- (set #maybeUser (Just groupUser)) <$> mkContext defaultLang
  groupAdminCtx <- (set #maybeUser (Just groupAdmin)) <$> mkContext defaultLang

  -- Group users still require explicit FolderUserAR role to create documents and access created draft documents
  void $ dbUpdate $ AccessControlCreateForUser (author ^. #id) (FolderUserAR folderId)

  do -- Normal documents
    do -- Document preparation phase
      docId <- fmap documentid $ addRandomDocument $ (rdaDefault author)
        { rdaTypes       = OneOf [Signable]
        , rdaStatuses    = OneOf [Preparation]
        , rdaSharings    = OneOf [Private]
        , rdaFolderId    = folderId
        , rdaSignatories = let signatory =
                                 OneOf [AllOf [RSC_DeliveryMethodIs EmailDelivery]]
                           in  OneOf $ map (`replicate` signatory) [2 .. 10]
        }

      assertGetDocumentSucceed
        "Author should be able to get draft document with folder permission available"
        docId
        authorCtx
        []

      assertGetDocumentFails
        "Group user should not able to get draft document with implicit group home folder SharedTemplateUserAR"
        docId
        groupUserCtx
        []

      assertGetDocumentFails
        "Group admin should not able to get draft document with implicit group home folder FolderAdminAR"
        docId
        groupAdminCtx
        []

    do -- Document signing phase
      docId <- fmap documentid $ addRandomDocument $ (rdaDefault author)
        { rdaTypes    = OneOf [Signable]
        , rdaStatuses = OneOf [Pending]
        , rdaSharings = OneOf [Private]
        , rdaFolderId = folderId
        }

      assertGetDocumentSucceed
        "Author should be able to get started document with folder permission available"
        docId
        authorCtx
        []

      assertGetDocumentFails
        "Group user should not able to get started document with implicit group home folder SharedTemplateUserAR"
        docId
        groupUserCtx
        []

      assertGetDocumentSucceed
        "Group admin should be able to get started document with implicit group home folder FolderAdminAR"
        docId
        groupAdminCtx
        []

  do -- Shared documents
    docId <- fmap documentid $ addRandomDocument $ (rdaDefault author)
      { rdaTypes    = OneOf [Template]
      , rdaStatuses = OneOf [Preparation]
      , rdaSharings = OneOf [Shared]
      , rdaFolderId = folderId
      }

    assertGetDocumentSucceed
      "Author should be able to get shared document with folder permission available"
      docId
      authorCtx
      []

    assertGetDocumentSucceed
      "Group user should be able to get shared document with implicit group home folder SharedTemplateUserAR"
      docId
      groupUserCtx
      []

    assertGetDocumentSucceed
      "Group admin should be able to get shared document with implicit group home folder SharedTemplateUserAR"
      docId
      groupAdminCtx
      []

testFolderAccessControl :: TestEnv ()
testFolderAccessControl = do
  baseFolderUser         <- instantiateRandomUser
  parentFolderUser       <- instantiateRandomUser
  grandParentFolderUser  <- instantiateRandomUser

  baseFolderAdmin        <- instantiateRandomUser
  parentFolderAdmin      <- instantiateRandomUser
  grandParentFolderAdmin <- instantiateRandomUser

  baseFolderGuest        <- instantiateRandomUser
  parentFolderGuest      <- instantiateRandomUser
  grandParentFolderGuest <- instantiateRandomUser

  baseFolder             <- dbUpdate $ FolderCreate $ defaultFolder
  let baseFolderId = baseFolder ^. #id

  parentFolder <- dbUpdate $ FolderCreate $ set #parentID
                                                (Just $ baseFolderId)
                                                defaultFolder
  let parentFolderId = parentFolder ^. #id

  grandParentFolder <- dbUpdate $ FolderCreate $ set #parentID
                                                     (Just $ parentFolderId)
                                                     defaultFolder
  let grandParentFolderId = grandParentFolder ^. #id

  void $ dbUpdate $ AccessControlCreateForUser (baseFolderUser ^. #id)
                                               (FolderUserAR baseFolderId)

  void $ dbUpdate $ AccessControlCreateForUser (parentFolderUser ^. #id)
                                               (FolderUserAR parentFolderId)

  void $ dbUpdate $ AccessControlCreateForUser (grandParentFolderUser ^. #id)
                                               (FolderUserAR grandParentFolderId)

  void $ dbUpdate $ AccessControlCreateForUser (baseFolderAdmin ^. #id)
                                               (FolderAdminAR baseFolderId)

  void $ dbUpdate $ AccessControlCreateForUser (parentFolderAdmin ^. #id)
                                               (FolderAdminAR parentFolderId)

  void $ dbUpdate $ AccessControlCreateForUser (grandParentFolderAdmin ^. #id)
                                               (FolderAdminAR grandParentFolderId)

  void $ dbUpdate $ AccessControlCreateForUser (baseFolderGuest ^. #id)
                                               (SharedTemplateUserAR baseFolderId)

  void $ dbUpdate $ AccessControlCreateForUser (parentFolderGuest ^. #id)
                                               (SharedTemplateUserAR parentFolderId)

  void $ dbUpdate $ AccessControlCreateForUser
    (grandParentFolderGuest ^. #id)
    (SharedTemplateUserAR grandParentFolderId)

  baseFolderUserCtx   <- (set #maybeUser (Just baseFolderUser)) <$> mkContext defaultLang
  baseFolderAdminCtx  <- (set #maybeUser (Just baseFolderAdmin)) <$> mkContext defaultLang
  baseFolderGuestCtx  <- (set #maybeUser (Just baseFolderGuest)) <$> mkContext defaultLang

  parentFolderUserCtx <- (set #maybeUser (Just parentFolderUser))
    <$> mkContext defaultLang
  parentFolderAdminCtx <- (set #maybeUser (Just parentFolderAdmin))
    <$> mkContext defaultLang
  parentFolderGuestCtx <- (set #maybeUser (Just parentFolderGuest))
    <$> mkContext defaultLang

  grandParentFolderUserCtx <- (set #maybeUser (Just grandParentFolderUser))
    <$> mkContext defaultLang
  grandParentFolderAdminCtx <- (set #maybeUser (Just grandParentFolderAdmin))
    <$> mkContext defaultLang

  do -- Document in base folder
    logInfo_ "Test access control for document in base folder"

    do -- Document preparation phase
      logInfo_ "Test access control for draft document in base folder"

      docId <- fmap documentid $ addRandomDocument $ (rdaDefault baseFolderUser)
        { rdaTypes    = OneOf [Signable]
        , rdaStatuses = OneOf [Preparation]
        , rdaSharings = OneOf [Private]
        , rdaFolderId = baseFolderId
        }

      assertGetDocumentSucceed "Base folder user should be able to get draft document"
                               docId
                               baseFolderUserCtx
                               []

      assertGetDocumentFails "Parent folder user should not able to get draft document"
                             docId
                             parentFolderUserCtx
                             []

      assertGetDocumentFails
        "Grandparent folder user should not able to get draft document"
        docId
        grandParentFolderUserCtx
        []

      assertGetDocumentFails "Base folder admin should not able to get draft document"
                             docId
                             baseFolderAdminCtx
                             []

      assertGetDocumentFails "Parent Folder admin should not able to get draft document"
                             docId
                             parentFolderAdminCtx
                             []

      assertGetDocumentFails
        "Base folder shared template user should not able to get draft document"
        docId
        baseFolderGuestCtx
        []

      assertGetDocumentFails
        "Parent folder shared template user should not able to get draft document"
        docId
        parentFolderGuestCtx
        []

    do -- Pending document
      docId <- fmap documentid $ addRandomDocument $ (rdaDefault baseFolderUser)
        { rdaTypes    = OneOf [Signable]
        , rdaStatuses = OneOf [Pending]
        , rdaSharings = OneOf [Private]
        , rdaFolderId = baseFolderId
        }

      assertGetDocumentSucceed "Base folder user should be able to get started document"
                               docId
                               baseFolderUserCtx
                               []

      assertGetDocumentFails
        "Parent folder user should not able to get started document"
        docId
        parentFolderUserCtx
        []

      assertGetDocumentFails
        "Grandparent folder user should not able to get started document"
        docId
        grandParentFolderUserCtx
        []

      assertGetDocumentSucceed
        "Base folder admin should be able to get started document"
        docId
        baseFolderAdminCtx
        []

      assertGetDocumentFails
        "Parent Folder admin should not able to get started document"
        docId
        parentFolderAdminCtx
        []

      assertGetDocumentFails
        "Base folder shared template user should not able to get started document"
        docId
        baseFolderGuestCtx
        []

      assertGetDocumentFails
        "Base folder shared template user should not able to get started document"
        docId
        baseFolderGuestCtx
        []

      assertGetDocumentFails
        "Parent folder shared template user should not able to get started document"
        docId
        parentFolderGuestCtx
        []

  do -- Document in parent folder
    logInfo_ "Test access control for document in base folder"

    do -- Document preparation phase
      logInfo_ "Test access control for draft document in base folder"

      docId <- fmap documentid $ addRandomDocument $ (rdaDefault parentFolderUser)
        { rdaTypes    = OneOf [Signable]
        , rdaStatuses = OneOf [Preparation]
        , rdaSharings = OneOf [Private]
        , rdaFolderId = parentFolderId
        }


      assertGetDocumentSucceed "Base folder user should be able to get draft document"
                               docId
                               baseFolderUserCtx
                               []

      assertGetDocumentSucceed "Parent folder user should be able to get draft document"
                               docId
                               parentFolderUserCtx
                               []

      assertGetDocumentFails
        "Grandparent folder user should not able to get draft document"
        docId
        grandParentFolderUserCtx
        []

      assertGetDocumentFails "Base folder admin should not able to get draft document"
                             docId
                             baseFolderAdminCtx
                             []

      assertGetDocumentFails "Parent Folder admin should not able to get draft document"
                             docId
                             parentFolderAdminCtx
                             []

      assertGetDocumentFails
        "Base folder shared template user should not able to get draft document"
        docId
        baseFolderGuestCtx
        []

      assertGetDocumentFails
        "Parent folder shared template user should not able to get draft document"
        docId
        parentFolderGuestCtx
        []

    do -- Pending document
      docId <- fmap documentid $ addRandomDocument $ (rdaDefault parentFolderUser)
        { rdaTypes    = OneOf [Signable]
        , rdaStatuses = OneOf [Pending]
        , rdaSharings = OneOf [Private]
        , rdaFolderId = parentFolderId
        }

      assertGetDocumentSucceed "Base folder user should be able to get started document"
                               docId
                               baseFolderUserCtx
                               []

      assertGetDocumentSucceed
        "Parent folder user should be able to get started document"
        docId
        parentFolderUserCtx
        []

      assertGetDocumentFails
        "Grandparent folder user should not able to get started document"
        docId
        grandParentFolderUserCtx
        []

      assertGetDocumentSucceed
        "Base folder admin should be able to get started document"
        docId
        baseFolderAdminCtx
        []

      assertGetDocumentSucceed
        "Parent folder admin should be able to get started document"
        docId
        parentFolderAdminCtx
        []

      assertGetDocumentFails
        "Grandparent folder admin should not able to get started document"
        docId
        grandParentFolderAdminCtx
        []

      assertGetDocumentFails
        "Base folder shared template user should not able to get started document"
        docId
        baseFolderGuestCtx
        []

      assertGetDocumentFails
        "Parent folder shared template user should not able to get started document"
        docId
        parentFolderGuestCtx
        []

testSharedFolderAccessControl :: TestEnv ()
testSharedFolderAccessControl = do
  baseFolderUser         <- instantiateRandomUser
  parentFolderUser       <- instantiateRandomUser
  grandParentFolderUser  <- instantiateRandomUser

  baseFolderAdmin        <- instantiateRandomUser
  parentFolderAdmin      <- instantiateRandomUser
  grandParentFolderAdmin <- instantiateRandomUser

  baseFolderGuest        <- instantiateRandomUser
  parentFolderGuest      <- instantiateRandomUser
  grandParentFolderGuest <- instantiateRandomUser

  baseFolder             <- dbUpdate $ FolderCreate $ defaultFolder
  let baseFolderId = baseFolder ^. #id

  parentFolder <- dbUpdate $ FolderCreate $ set #parentID
                                                (Just $ baseFolderId)
                                                defaultFolder
  let parentFolderId = parentFolder ^. #id

  grandParentFolder <- dbUpdate $ FolderCreate $ set #parentID
                                                     (Just $ parentFolderId)
                                                     defaultFolder
  let grandParentFolderId = grandParentFolder ^. #id

  void $ dbUpdate $ AccessControlCreateForUser (baseFolderUser ^. #id)
                                               (FolderUserAR baseFolderId)

  void $ dbUpdate $ AccessControlCreateForUser (parentFolderUser ^. #id)
                                               (FolderUserAR parentFolderId)

  void $ dbUpdate $ AccessControlCreateForUser (grandParentFolderUser ^. #id)
                                               (FolderUserAR grandParentFolderId)

  void $ dbUpdate $ AccessControlCreateForUser (baseFolderAdmin ^. #id)
                                               (FolderAdminAR baseFolderId)

  void $ dbUpdate $ AccessControlCreateForUser (parentFolderAdmin ^. #id)
                                               (FolderAdminAR parentFolderId)

  void $ dbUpdate $ AccessControlCreateForUser (grandParentFolderAdmin ^. #id)
                                               (FolderAdminAR grandParentFolderId)

  void $ dbUpdate $ AccessControlCreateForUser (baseFolderGuest ^. #id)
                                               (SharedTemplateUserAR baseFolderId)

  void $ dbUpdate $ AccessControlCreateForUser (parentFolderGuest ^. #id)
                                               (SharedTemplateUserAR parentFolderId)

  void $ dbUpdate $ AccessControlCreateForUser
    (grandParentFolderGuest ^. #id)
    (SharedTemplateUserAR grandParentFolderId)

  baseFolderUserCtx   <- (set #maybeUser (Just baseFolderUser)) <$> mkContext defaultLang
  baseFolderAdminCtx  <- (set #maybeUser (Just baseFolderAdmin)) <$> mkContext defaultLang
  baseFolderGuestCtx  <- (set #maybeUser (Just baseFolderGuest)) <$> mkContext defaultLang

  parentFolderUserCtx <- (set #maybeUser (Just parentFolderUser))
    <$> mkContext defaultLang
  parentFolderAdminCtx <- (set #maybeUser (Just parentFolderAdmin))
    <$> mkContext defaultLang
  parentFolderGuestCtx <- (set #maybeUser (Just parentFolderGuest))
    <$> mkContext defaultLang

  grandParentFolderUserCtx <- (set #maybeUser (Just grandParentFolderUser))
    <$> mkContext defaultLang
  grandParentFolderGuestCtx <- (set #maybeUser (Just grandParentFolderGuest))
    <$> mkContext defaultLang

  do -- Shared document in base folder
    logInfo_ "Testing shared document access control for document in base folder"

    docId <- fmap documentid $ addRandomDocument $ (rdaDefault baseFolderUser)
      { rdaTypes    = OneOf [Template]
      , rdaStatuses = OneOf [Preparation]
      , rdaSharings = OneOf [Shared]
      , rdaFolderId = baseFolderId
      }

    assertGetDocumentSucceed "Base folder user should be able to get shared document"
                             docId
                             baseFolderUserCtx
                             []

    assertGetDocumentFails "Parent folder user should not able to get shared document"
                           docId
                           parentFolderUserCtx
                           []

    assertGetDocumentFails
      "Grandparent folder user should not able to get shared document"
      docId
      grandParentFolderUserCtx
      []

    assertGetDocumentFails "Base folder admin should not able to get shared document"
                           docId
                           baseFolderAdminCtx
                           []

    assertGetDocumentFails "Parent Folder admin should not able to get shared document"
                           docId
                           parentFolderAdminCtx
                           []

    assertGetDocumentSucceed
      "Base folder shared template user should be able to get shared document"
      docId
      baseFolderGuestCtx
      []

    assertGetDocumentFails
      "Parent folder shared template user should not able to get shared document"
      docId
      parentFolderGuestCtx
      []

    assertGetDocumentFails
      "Grandparent folder shared template user should not able to get shared document"
      docId
      grandParentFolderGuestCtx
      []

  do -- Shared document in parent folder
    logInfo_ "Testing shared document access control for document in parent folder"

    docId <- fmap documentid $ addRandomDocument $ (rdaDefault parentFolderUser)
      { rdaTypes    = OneOf [Template]
      , rdaStatuses = OneOf [Preparation]
      , rdaSharings = OneOf [Shared]
      , rdaFolderId = parentFolderId
      }

    assertGetDocumentSucceed "Base folder user should be able to get shared document"
                             docId
                             baseFolderUserCtx
                             []

    assertGetDocumentSucceed "Parent folder user should be able to get shared document"
                             docId
                             parentFolderUserCtx
                             []

    assertGetDocumentFails
      "Grandparent folder user should not able to get shared document"
      docId
      grandParentFolderUserCtx
      []

    assertGetDocumentFails "Base folder admin should not able to get shared document"
                           docId
                           baseFolderAdminCtx
                           []

    assertGetDocumentFails "Parent Folder admin should not able to get shared document"
                           docId
                           parentFolderAdminCtx
                           []

    assertGetDocumentSucceed
      "Base folder shared template user should be able to get shared document"
      docId
      baseFolderGuestCtx
      []

    assertGetDocumentSucceed
      "Parent folder shared template user should be able to get shared document"
      docId
      parentFolderGuestCtx
      []

    assertGetDocumentFails
      "Grandparent folder shared template user should not able to get shared document"
      docId
      grandParentFolderGuestCtx
      []

  do -- Shared document in grandparent folder
    logInfo_ "Testing shared document access control for document in grandparent folder"

    docId <- fmap documentid $ addRandomDocument $ (rdaDefault grandParentFolderUser)
      { rdaTypes    = OneOf [Template]
      , rdaStatuses = OneOf [Preparation]
      , rdaSharings = OneOf [Shared]
      , rdaFolderId = grandParentFolderId
      }

    assertGetDocumentSucceed "Base folder user should be able to get shared document"
                             docId
                             baseFolderUserCtx
                             []

    assertGetDocumentSucceed "Parent folder user should be able to get shared document"
                             docId
                             parentFolderUserCtx
                             []

    assertGetDocumentSucceed
      "Grandparent folder user should be able to get shared document"
      docId
      grandParentFolderUserCtx
      []

    assertGetDocumentFails "Base folder admin should not able to get shared document"
                           docId
                           baseFolderAdminCtx
                           []

    assertGetDocumentFails "Parent Folder admin should not able to get shared document"
                           docId
                           parentFolderAdminCtx
                           []

    assertGetDocumentSucceed
      "Base folder shared template user should be able to get shared document"
      docId
      baseFolderGuestCtx
      []

    assertGetDocumentSucceed
      "Parent folder shared template user should be able to get shared document"
      docId
      parentFolderGuestCtx
      []

    assertGetDocumentSucceed
      "Grandparent folder shared template user should be able to get shared document"
      docId
      grandParentFolderGuestCtx
      []

hasFileAccess :: [(Text, Input)] -> Context -> FileID -> TestEnv Bool
hasFileAccess params ctx fileId = do
  getRequest <- mkRequestWithHeaders GET params []
  (res, _)   <- runTestKontra getRequest ctx checkAccess
  return res
  where
    checkAccess = catch (const True <$> checkFileAccess fileId)
                        (\(_ :: SomeException) -> return False)

assertHasFileAccess :: String -> [(Text, Input)] -> Context -> FileID -> TestEnv ()
assertHasFileAccess message params ctx fileId = do
  res <- hasFileAccess params ctx fileId
  assertEqual message True res

assertNoFileAccess :: String -> [(Text, Input)] -> Context -> FileID -> TestEnv ()
assertNoFileAccess message params ctx fileId = do
  res <- hasFileAccess params ctx fileId
  assertEqual message False res

testDocumentFileAccessControl :: TestEnv ()
testDocumentFileAccessControl = do
  userGroup <- instantiateUserGroup $ randomUserGroupTemplate
  let userGroupId = userGroup ^. #id

  folder <- dbUpdate . FolderCreate $ defaultFolder
  let folderId = folder ^. #id

  author     <- instantiateUser $ randomUserTemplate { groupID = return userGroupId }
  otherUser  <- instantiateRandomUser
  folderUser <- instantiateRandomUser

  groupUser  <- instantiateUser $ randomUserTemplate { groupID = return userGroupId }

  fileId     <- addNewRandomFile

  void $ dbUpdate $ AccessControlCreateForUser (author ^. #id) (FolderUserAR folderId)
  void $ dbUpdate $ AccessControlCreateForUser (folderUser ^. #id) (FolderUserAR folderId)

  doc <- addRandomDocumentWithFile fileId $ (rdaDefault author) { rdaTypes    = OneOf
                                                                  [Signable]
                                                                , rdaStatuses = OneOf
                                                                  [Pending]
                                                                , rdaSharings = OneOf
                                                                  [Private]
                                                                , rdaFolderId = folderId
                                                                }

  anonCtx       <- mkContext defaultLang
  authorCtx     <- (set #maybeUser (Just author)) <$> mkContext defaultLang
  otherUserCtx  <- (set #maybeUser (Just otherUser)) <$> mkContext defaultLang
  folderUserCtx <- (set #maybeUser (Just folderUser)) <$> mkContext defaultLang
  groupUserCtx  <- (set #maybeUser (Just groupUser)) <$> mkContext defaultLang

  let docId         = documentid doc
      authorSigLink = fromJust $ getSigLinkFor (author ^. #id) doc
      participantSigLink =
        fromJust $ find (\sl -> not $ signatoryisauthor sl) (documentsignatorylinks doc)

  authorSignCtx      <- createSignatoryContext docId (signatorylinkid authorSigLink)
  participantSignCtx <- createSignatoryContext docId (signatorylinkid participantSigLink)

  let docIdParam = [("document_id", inText . showt $ docId)]
      authorSignatoryParam =
        [ ("document_id" , inText . showt $ docId)
        , ("signatory_id", inText . showt $ signatorylinkid authorSigLink)
        ]
      participantSignatoryParam =
        [ ("document_id" , inText . showt $ docId)
        , ("signatory_id", inText . showt $ signatorylinkid participantSigLink)
        ]

  assertHasFileAccess "Author should have access to document file"
                      docIdParam
                      authorCtx
                      fileId

  assertHasFileAccess "Author signatory should have access to document file"
                      authorSignatoryParam
                      authorSignCtx
                      fileId

  assertHasFileAccess "Participant signatory should have access to document file"
                      participantSignatoryParam
                      participantSignCtx
                      fileId

  assertHasFileAccess "Other folder user should have access to document file"
                      docIdParam
                      folderUserCtx
                      fileId

  assertNoFileAccess
    "Non validated signatory session should not have access to document file"
    authorSignatoryParam
    anonCtx
    fileId

  assertNoFileAccess "Group user should not have access to document file"
                     docIdParam
                     groupUserCtx
                     fileId

  assertNoFileAccess "Other user should not have access to document file"
                     docIdParam
                     otherUserCtx
                     fileId

  do
    docId2 <- fmap documentid $ addRandomDocument $ (rdaDefault author)
      { rdaTypes    = OneOf [Signable]
      , rdaStatuses = OneOf [Preparation]
      , rdaSharings = OneOf [Private]
      , rdaFolderId = folderId
      }

    assertNoFileAccess "Should not have access if file is not in document"
                       [("document_id", inText . showt $ docId2)]
                       authorCtx
                       fileId

  do -- Test attachment_id
    fileId2    <- addNewRandomFile
    attachment <- dbUpdate
      $ NewAttachment (author ^. #id) "shared" fileId2 (userActor authorCtx author)
    let attachmentId    = attachmentid attachment

    let attachmentParam = [("attachment_id", inText . showt $ attachmentId)]

    assertHasFileAccess "Author should have access to attachment with correct file ID"
                        attachmentParam
                        authorCtx
                        fileId2

    assertNoFileAccess
      "Author should not have access to attachment with incorrect file ID"
      attachmentParam
      authorCtx
      fileId

    assertNoFileAccess "Folder user should not have access to non-shared attachment"
                       attachmentParam
                       folderUserCtx
                       fileId2

    assertNoFileAccess "Other user should not have access to non-shared attachment"
                       attachmentParam
                       otherUserCtx
                       fileId2

    assertNoFileAccess "Anonymous should not have access to non-shared attachment"
                       attachmentParam
                       anonCtx
                       fileId2

    assertHasFileAccess
      "Folder user should have access to document file with attachment_id ignored"
      [ ("attachment_id", inText . showt $ attachmentId)
      , ("document_id"  , inText . showt $ docId)
      ]
      folderUserCtx
      fileId

    assertNoFileAccess "Group user should not have access to non-shared attachment"
                       attachmentParam
                       groupUserCtx
                       fileId2

    dbUpdate $ SetAttachmentsSharing (author ^. #id) [attachmentId] True

    assertNoFileAccess "Folder user should not have access to shared attachment"
                       attachmentParam
                       folderUserCtx
                       fileId2

    assertHasFileAccess "Group user should have access to shared attachment"
                        attachmentParam
                        groupUserCtx
                        fileId2

    assertNoFileAccess "Other user should not have access to shared attachment"
                       attachmentParam
                       otherUserCtx
                       fileId2

    assertNoFileAccess "Anonymous should not have access to shared attachment"
                       attachmentParam
                       anonCtx
                       fileId2
