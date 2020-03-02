module Doc.AccessControl
  ( DocAccessRole(..)
  , docAccessValidRoles
  , docAccessControl
  , docAccessMode
  , docAccessControlWithUserSignatory
  , docPermissionCondition
  )
where

import Control.Monad.Catch (MonadThrow(..))

import AccessControl.Check
import AccessControl.Model
import AccessControl.Types
import API.V2.Errors
import API.V2.MonadUtils
import API.V2.User
import DB
import Doc.API.V2.DocumentAccess
import Doc.API.V2.Guards
import Doc.DocInfo
import Doc.DocInfo (isAccessibleBySignatories, isDocumentShared)
import Doc.DocumentID (DocumentID)
import Doc.Model.Query
import Doc.SignatoryLinkID (SignatoryLinkID)
import Doc.Types.Document (Document(..))
import Doc.Types.SignatoryLink
import Folder.Internal (FolderID)
import Kontra
import OAuth.Model
import User.Types.User
import Util.SignatoryLinkUtils (getSigLinkFor)

-- Extra roles a user may have for accessing a document resource
data DocAccessRole
  -- role created for visiting through signatory link
  = SignatoryAR SignatoryLinkID
  -- signatory that is also author for a given document
  | SignatoryAuthorAR DocumentID
  -- the original access roles
  | OtherAR AccessRoleTarget
  deriving (Show, Eq)

docAccessValidRoles
  :: Kontrakcja m
  => Document
  -> Maybe AuthenticatedUser
  -> Maybe AuthenticatedSignatoryLink
  -> m [DocAccessRole]
docAccessValidRoles doc mAuthUser mAuthSignatory = do
  let mUser      = getAuthenticatedUser <$> mAuthUser
      mSignatory = getAuthenticatedSignatoryLink <$> mAuthSignatory

  -- Get signatory or signatory author roles if there is a signatory link
  let signatoryRoles = join $ maybeToList $ signatoryAccessRoles doc <$> mSignatory

  permissionCondition <- docPermissionCondition (canDo ReadA) doc $ documentfolderid doc
  userRoles           <- case mUser of
    Just user -> do
      roles <- dbQuery $ GetRoles user
      return $ fmap (OtherAR . accessRoleTarget) roles
    Nothing -> return []
  let availableRoles = userRoles <> signatoryRoles

  -- Filter out all the invalid roles that do not have access to the document.
  return $ filter
    (\role -> accessControlCheck (hasDocPermissions doc role) permissionCondition)
    availableRoles

docAccessControlWithUserSignatory
  :: Kontrakcja m
  => Document
  -> Maybe AuthenticatedUser
  -> Maybe AuthenticatedSignatoryLink
  -> m a
  -> m a
docAccessControlWithUserSignatory doc mAuthUser mSignatory onSuccess = do
  validRoles <- docAccessValidRoles doc mAuthUser mSignatory
  case validRoles of
    []      -> apiError insufficientPrivileges
    (_ : _) -> onSuccess

docAccessControlWithUser
  :: Kontrakcja m
  => DocumentID
  -> Maybe AuthenticatedUser
  -> Maybe SignatoryLinkID
  -> m a
  -> m a
docAccessControlWithUser docId mAuthUser mSignatoryId onSuccess = do
  doc        <- dbQuery $ GetDocumentByDocumentID docId
  mSignatory <- case mSignatoryId of
    Just signatoryId -> getMaybeAuthenticatedSignatory doc signatoryId
    Nothing          -> return Nothing

  docAccessControlWithUserSignatory doc mAuthUser mSignatory onSuccess

docAccessControl :: Kontrakcja m => DocumentID -> Maybe SignatoryLinkID -> m a -> m a
docAccessControl docId mSignatoryId onSuccess = do
  mAuthUser <- (fmap fst) <$> getMaybeAuthenticatedAPIUser APIDocCheck
  docAccessControlWithUser docId mAuthUser mSignatoryId onSuccess

-- Given a document and an access role, derive the permissions the access
-- role have to access the document resource. Currently we only derive
-- read permissions for proof of concept.
hasDocPermissions :: Document -> DocAccessRole -> [Permission]
hasDocPermissions doc role = case role of
  -- Signatory who is author of a document can read the document of any state
  SignatoryAuthorAR docId | docId == documentid doc -> [readPerm]

  -- Signatory of a document can read the document if it is in
  -- pending or closed state.
  SignatoryAR slid -> case getSigLinkFor slid doc of
    Just _ | isAccessibleBySignatories doc -> [readPerm]
    _ -> []

  -- Other roles just use access control's has permissions
  OtherAR otherRole -> hasPermissions otherRole

  -- otherwise no permission
  _                 -> []
  where
    -- Signatories have direct permission on the document resource.
    -- Other roles have indirect permission on the document's folder.
    readPerm :: Permission
    readPerm = Permission { permKind     = PermCanDo
                          , permAction   = ReadA
                          , permResource = DocumentR $ documentid doc
                          }

-- Determine what permission condition is required to access a document in a folder
-- - All documents allow signatories with the DocumentR permission to access
-- - If document is in draft mode, it alternatively requires DocumentInFolderR
-- - If document is shared template, it alternatively requires DocumentInFolderR or SharedTemplateR
-- - Otherwise it alternatively requires DocumentInFolderR or DocumentAfterPreparationR
docPermissionCondition
  :: forall m
   . (MonadThrow m, MonadDB m)
  => (AccessResource -> Permission)
  -> Document
  -> FolderID
  -> m PermissionCondition
docPermissionCondition mkPerm doc folderId = do
  folderCondition <- folderConditionM
  return $ OrCond [folderCondition, Cond $ mkPerm $ DocumentR docId]
  where
    docId            = documentid doc

    folderConditionM = if
      | isDocumentShared doc -> do
        folderPerm <- addAlternativePermissions $ mkPerm $ DocumentInFolderR folderId
        sharedPerm <- addAlternativePermissions $ mkPerm $ SharedTemplateR folderId
        return $ OrCond [folderPerm, sharedPerm]
      | isPreparation doc -> addAlternativePermissions $ mkPerm $ DocumentInFolderR
        folderId
      | otherwise -> do
        folderPerm <- addAlternativePermissions $ mkPerm $ DocumentInFolderR folderId
        prepPerm   <- addAlternativePermissions $ mkPerm $ DocumentAfterPreparationR
          folderId
        return $ OrCond [folderPerm, prepPerm]

-- Derive the access roles for a document from the signatory link
-- All signatories have the SignatoryAR role with their signatory
-- link ID. If the signatory is also the author of a document,
-- then they also have the SignatoryAuthorAR role.
signatoryAccessRoles :: Document -> SignatoryLink -> [DocAccessRole]
signatoryAccessRoles doc signatory = if signatoryisauthor signatory
  then [authorRole, signatoryRole]
  else [signatoryRole]
  where
    docId         = documentid doc
    signatoryRole = SignatoryAR $ signatorylinkid signatory
    authorRole    = SignatoryAuthorAR docId

-- Access a document based on what valid access roles the user has.
-- The provided acess roles must be filtered out with invalid access roles already
-- DocumentAccessMode depends on which role is granted access
docAccessMode :: Bool -> [DocAccessRole] -> Maybe DocumentAccessMode
docAccessMode isAuthor validRoles = case mSignatoryId of
  -- if there is any author role granted, use AuthorDocumentAccess
  _ | any isAuthorRole validRoles -> Just AuthorDocumentAccess
  -- else if there is any signatory role granted, use SignatoryDocumentAccess
  Just signatoryId                -> Just $ SignatoryDocumentAccess signatoryId
  -- If any non-admin role is granted, the document is folder access mode.
  -- Special case for author user to preserve legacy behavior when author
  -- accesses document. This is different from SignatoryAuthorAR as under new
  -- access control, author without signatory link still need folder permission
  -- to access the document.
  _ | any isNonAdminRole validRoles ->
    if isAuthor then Just AuthorDocumentAccess else Just $ FolderDocumentAccess Nothing
  -- Else if user group admin role is granted, use admin access mode
  _ | any isAdminRole validRoles -> Just $ CompanyAdminDocumentAccess Nothing
  -- If no valid role is found, return nothing indicating no permission granted.
  _ -> Nothing
  where
    isAuthorRole :: DocAccessRole -> Bool
    isAuthorRole (SignatoryAuthorAR _) = True
    isAuthorRole _                     = False

    getSignatoryAR :: DocAccessRole -> Maybe SignatoryLinkID
    getSignatoryAR (SignatoryAR slid) = Just slid
    getSignatoryAR _                  = Nothing

    mSignatoryId :: Maybe SignatoryLinkID
    mSignatoryId = join $ fmap listToMaybe $ traverse getSignatoryAR validRoles

    isAdminRole :: DocAccessRole -> Bool
    isAdminRole (OtherAR (UserAdminAR _)) = True
    isAdminRole (OtherAR (UserGroupAdminAR _)) = True
    isAdminRole (OtherAR (FolderAdminAR _)) = True
    isAdminRole _ = False

    isNonAdminRole :: DocAccessRole -> Bool
    isNonAdminRole (OtherAR (UserAdminAR _)) = False
    isNonAdminRole (OtherAR (UserGroupAdminAR _)) = False
    isNonAdminRole (OtherAR (FolderAdminAR _)) = False
    isNonAdminRole (OtherAR _) = True
    isNonAdminRole _ = False
