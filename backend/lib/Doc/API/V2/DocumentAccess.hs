module Doc.API.V2.DocumentAccess (
  DocumentAccess(..)
, DocumentAccessMode(..)
, canSeeSignlinks
, propertyForCurrentSignatory
, documentAccessForUser
, documentAccessModeForUser
, documentAccessForSlid
, documentAccessForAuthor
, documentAccessForAdminonly
, documentAccessByFolder
, documentAccess
) where

import AccessControl.Check
import AccessControl.Types
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocumentID
import Doc.SignatoryLinkID
import User.Model
import Util.SignatoryLinkUtils

data DocumentAccess = DocumentAccess {
      daDocumentID :: DocumentID
    , daAccessMode :: DocumentAccessMode
    , daStatus     :: DocumentStatus
  }

data DocumentAccessMode =
    SignatoryDocumentAccess SignatoryLinkID
    -- ^ Old list calls: Person looking at the document is this signatory.
    --   New list calls: Person looking at the document is signatory, but cannot access
    --                   the document by other means (Folders).
  | AuthorDocumentAccess
    -- ^ Old list calls: Person looking at the document is an author.
  | CompanyAdminDocumentAccess (Maybe SignatoryLinkID)
    -- ^ Old list calls: Person looking at the document is an admin of author.
  | CompanySharedDocumentAccess
    -- ^ Old list calls: Person looking at the document is in the author's company and the
    --                   document is shared.
    --   New list calls: Same
  | SystemAdminDocumentAccess
    -- ^ Old list calls: Person looking at the document has adminonly access.
  | FolderDocumentAccess (Maybe SignatoryLinkID)
    -- ^ New list calls: Person looking at the document has (Folder-based) admin access to the document.
    -- If we ever want to drop DocumentAccessMode and use Roles, Permissions and Resources:
    --   - signatory should have ReadA access to DocumentR
    --   - admin/folder user should have ReadA access to DocumentR and DocumentSecretsR
    deriving (Show)

canSeeSignlinks :: DocumentAccess -> Bool
canSeeSignlinks DocumentAccess { daAccessMode = AuthorDocumentAccess } = True
canSeeSignlinks DocumentAccess { daAccessMode = CompanyAdminDocumentAccess _ } = True
canSeeSignlinks DocumentAccess { daAccessMode = FolderDocumentAccess _ } = True
canSeeSignlinks _ = False

propertyForCurrentSignatory :: DocumentAccess -> (SignatoryLink -> a) -> Document -> a
propertyForCurrentSignatory da f doc = case slForAccess da doc of
  Just s  -> f s
  Nothing -> unexpectedError "Signatory that is looking at document is not in document"
  where
    slForAccess :: DocumentAccess -> Document -> Maybe SignatoryLink
    slForAccess DocumentAccess { daAccessMode = SignatoryDocumentAccess sid } =
      getSigLinkFor sid
    slForAccess DocumentAccess { daAccessMode = AuthorDocumentAccess } = getAuthorSigLink
    slForAccess DocumentAccess { daAccessMode = CompanySharedDocumentAccess } =
      getAuthorSigLink
    slForAccess DocumentAccess { daAccessMode = SystemAdminDocumentAccess } =
      getAuthorSigLink
    slForAccess DocumentAccess { daAccessMode = CompanyAdminDocumentAccess (Just sid) } =
      getSigLinkFor sid
    slForAccess DocumentAccess { daAccessMode = CompanyAdminDocumentAccess Nothing } =
      getAuthorSigLink
    slForAccess DocumentAccess { daAccessMode = FolderDocumentAccess (Just sid) } =
      getSigLinkFor sid
    slForAccess DocumentAccess { daAccessMode = FolderDocumentAccess Nothing } =
      getAuthorSigLink

documentAccessForUser :: User -> Document -> DocumentAccess
documentAccessForUser user document =
  documentAccess document $ documentAccessModeForUser user document

documentAccessByFolder :: User -> Document -> [AccessRole] -> DocumentAccess
documentAccessByFolder user document roles =
  documentAccess document $ documentAccessModeByFolder user document roles

documentAccess :: Document -> DocumentAccessMode -> DocumentAccess
documentAccess doc mode = DocumentAccess { daDocumentID = documentid doc
                                         , daAccessMode = mode
                                         , daStatus     = documentstatus doc
                                         }

documentAccessForAuthor :: Document -> DocumentAccess
documentAccessForAuthor document = documentAccess document AuthorDocumentAccess

documentAccessForAdminonly :: Document -> DocumentAccess
documentAccessForAdminonly document = documentAccess document SystemAdminDocumentAccess

documentAccessModeForUser :: User -> Document -> DocumentAccessMode
documentAccessModeForUser user document = case getSigLinkFor user document of
  Just sl -> if isAuthor sl
    then AuthorDocumentAccess
    else
      if documentauthorugid document
           == Just (user ^. #groupID)
           && (user ^. #isCompanyAdmin)
        then CompanyAdminDocumentAccess . Just $ signatorylinkid sl
        else SignatoryDocumentAccess $ signatorylinkid sl
  Nothing ->
    if documentauthorugid document == Just (user ^. #groupID) && user ^. #isCompanyAdmin
      then CompanyAdminDocumentAccess Nothing
      else
        if documentauthorugid document
           == Just (user ^. #groupID)
           && isDocumentShared document
        then
          CompanySharedDocumentAccess
        else
          unexpectedError
          $  "User "
          <> showt (user ^. #id)
          <> " accessing document "
          <> showt (documentid document)
          <> " without any permission. This should be cought earlier."

documentAccessModeByFolder :: User -> Document -> [AccessRole] -> DocumentAccessMode
documentAccessModeByFolder user document roles =
  case (getSigLinkFor user document, hasReadAccessForDocByFolder) of
    (Just sl, True) -> if signatoryisauthor sl
                       -- FIXME: Currently we use AuthorDocumentAccess to preserve legacy behavior of author.
                       -- For author accessing his own document, frontend expects to see the "signatory" viewer role.
      then AuthorDocumentAccess
      else FolderDocumentAccess . Just $ signatorylinkid sl
    (Nothing, True ) -> FolderDocumentAccess Nothing
    (Just sl, False) -> SignatoryDocumentAccess $ signatorylinkid sl
    (Nothing, False) ->
      if (documentauthorugid document == Just (user ^. #groupID))
         && isDocumentShared document
      then
        CompanySharedDocumentAccess
      else
        unexpectedError
        $   "User"
        <+> showt (user ^. #id)
        <+> "accessing document"
        <+> showt (documentid document)
        <+> "by folder without any permission. This should be caught earlier."
  where
    hasReadAccessForDocByFolder =
      accessControlCheckAny roles
        $ [ canDo ReadA resource | resource <- docResources document ]

documentAccessForSlid :: SignatoryLinkID -> Document -> DocumentAccess
documentAccessForSlid slid document =
  documentAccess document $ documentAccessModeForSlid slid document

documentAccessModeForSlid :: SignatoryLinkID -> Document -> DocumentAccessMode
documentAccessModeForSlid slid document = case getSigLinkFor slid document of
  Just sl -> if isAuthor sl
    then AuthorDocumentAccess
    else SignatoryDocumentAccess $ signatorylinkid sl
  Nothing ->
    unexpectedError
      $  "SignatoryLinkID "
      <> showt slid
      <> " accessing document "
      <> showt (documentid document)
      <> " without any permission. This should be caught earlier."
