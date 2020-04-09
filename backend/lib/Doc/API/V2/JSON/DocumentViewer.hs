module Doc.API.V2.JSON.DocumentViewer (
  DocumentViewer(..)
, unjsonDocumentViewer
, viewerForDocument
) where

import Data.Unjson

import Doc.API.V2.DocumentAccess
import Doc.API.V2.JSON.Misc ()
import Doc.API.V2.JSON.Utils
import Doc.DocStateData
import Doc.SignatoryLinkID
import Util.SignatoryLinkUtils

-- Document viewer is returned by API as a part of document json structure,
-- and describes relationship between document, and user authorizing current API call.
-- It is needed so frontend should present user with operations that are available to him.
data DocumentViewer =
    SignatoryDocumentViewer SignatoryLinkID
  | CompanyAdminDocumentViewer (Maybe SignatoryLinkID)
  | CompanySharedDocumentViewer
  | FolderDocumentViewer SignatoryLinkID

viewerForDocument :: DocumentAccess -> Document -> DocumentViewer
viewerForDocument DocumentAccess { daAccessMode = SignatoryDocumentAccess sid } _ =
  SignatoryDocumentViewer sid
viewerForDocument DocumentAccess { daAccessMode = CompanySharedDocumentAccess } _ =
  CompanySharedDocumentViewer
viewerForDocument DocumentAccess { daAccessMode = CompanyAdminDocumentAccess msid } _ =
  CompanyAdminDocumentViewer msid
viewerForDocument DocumentAccess { daAccessMode = AuthorDocumentAccess } doc =
  SignatoryDocumentViewer $ dvGetAuthorSigLink doc
viewerForDocument DocumentAccess { daAccessMode = SystemAdminDocumentAccess } _ =
  CompanyAdminDocumentViewer Nothing
viewerForDocument DocumentAccess { daAccessMode = FolderDocumentAccess msid } doc =
  FolderDocumentViewer . fromMaybe (dvGetAuthorSigLink doc) $ msid

dvGetAuthorSigLink :: Document -> SignatoryLinkID
dvGetAuthorSigLink doc = case getAuthorSigLink doc of
  Just sig -> signatorylinkid sig
  Nothing  -> unexpectedError "Could not find author for document for DocumentViewer"

dvSignatoryLinkID :: DocumentViewer -> Maybe SignatoryLinkID
dvSignatoryLinkID (SignatoryDocumentViewer sid) = Just sid
dvSignatoryLinkID (CompanyAdminDocumentViewer msid) = msid
dvSignatoryLinkID (FolderDocumentViewer sid) = Just sid
dvSignatoryLinkID _ = Nothing

dvRole :: DocumentViewer -> Text
dvRole (SignatoryDocumentViewer    _) = "signatory"
dvRole (CompanyAdminDocumentViewer _) = "company_admin"
dvRole (CompanySharedDocumentViewer ) = "company_shared"
dvRole (FolderDocumentViewer _      ) = "company_admin"

-- We should not introduce instance for DocumentViewer since this can't be really parsed. And instance for Maybe DocumentViewer would be missleading
unjsonDocumentViewer :: UnjsonDef (Maybe DocumentViewer)
unjsonDocumentViewer =
  nothingToNullDef
    .  objectOf
    $  Nothing
    <$ fieldOpt "role" (fmap dvRole) "Reason why current user is able to see document"
    <* fieldOpt "signatory_id" (dvSignatoryLinkID =<<) "Id of signatory if there is one"
