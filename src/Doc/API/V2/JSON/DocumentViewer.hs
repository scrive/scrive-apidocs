module Doc.API.V2.JSON.DocumentViewer (
  DocumentViewer(..)
, unjsonDocumentViewer
, viewerForDocument
) where

import Data.Unjson
import qualified Data.Text as T

import Doc.API.V2.DocumentAccess
import Doc.API.V2.JSON.Misc()
import Doc.API.V2.JSON.Utils
import Doc.DocStateData
import Doc.SignatoryLinkID
import KontraPrelude
import Util.SignatoryLinkUtils


-- Document viewer is returned by API as a part of document json structure,
-- and describes relationship between document, and user authorizing current API call.
-- It is needed so frontend should present user with operations that are available to him.
data DocumentViewer =
    SignatoryDocumentViewer SignatoryLinkID
  | AdminDocumentViewer

viewerForDocument :: DocumentAccess -> Document -> DocumentViewer
viewerForDocument (DocumentAccess { daAccessMode = SignatoryDocumentAccess sid}) _ = SignatoryDocumentViewer sid
viewerForDocument (DocumentAccess { daAccessMode = AdminDocumentAccess}) _ = AdminDocumentViewer
viewerForDocument (DocumentAccess { daAccessMode = AuthorDocumentAccess}) doc =
  case (getAuthorSigLink doc) of
    Just sig -> SignatoryDocumentViewer $ signatorylinkid sig
    Nothing  -> $unexpectedError "Could not find author for document for DocumentViewer"

dvSignatoryLinkID :: DocumentViewer -> Maybe SignatoryLinkID
dvSignatoryLinkID (SignatoryDocumentViewer sid) = Just sid
dvSignatoryLinkID _ = Nothing

dvRole ::DocumentViewer -> T.Text
dvRole (SignatoryDocumentViewer _) = "signatory"
dvRole (AdminDocumentViewer) = "company_admin"

-- We should not introduce instance for DocumentViewer since this can't be really parsed. And instance for Maybe DocumentViewer would be missleading
unjsonDocumentViewer :: UnjsonDef (Maybe DocumentViewer)
unjsonDocumentViewer = nothingToNullDef $ objectOf $ pure Nothing
  <* fieldOpt "role" (fmap dvRole)   "Reason why current user is able to see document"
  <* fieldOpt "signatory_id" (join . fmap dvSignatoryLinkID) "Id of signatory if there is one"
