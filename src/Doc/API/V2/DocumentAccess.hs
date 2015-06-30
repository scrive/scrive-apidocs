{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.API.V2.DocumentAccess (
  DocumentAccess(..)
, DocumentAccessMode(..)
, canSeeSignlinks
, propertyForCurrentSignatory
, documentAccessForUser
) where

import Doc.DocStateData
import Doc.DocumentID
import Doc.SignatoryLinkID
import KontraPrelude
import User.Model
import Util.SignatoryLinkUtils

data DocumentAccess = DocumentAccess {
      daDocumentID :: DocumentID
    , daAccessMode :: DocumentAccessMode
  }

data DocumentAccessMode =
    SignatoryDocumentAccess SignatoryLinkID -- Person looking at document is this signatory
  | AuthorDocumentAccess -- Person looking at document is an author
  | AdminDocumentAccess  -- Person looking at document is an admin of author

canSeeSignlinks :: DocumentAccess -> Bool
canSeeSignlinks (DocumentAccess { daAccessMode = AuthorDocumentAccess}) = True
canSeeSignlinks (DocumentAccess { daAccessMode = AdminDocumentAccess})  = True
canSeeSignlinks _ = False

propertyForCurrentSignatory :: DocumentAccess -> (SignatoryLink -> a) -> Document -> a
propertyForCurrentSignatory da f doc =
  case slForAccess da doc of
    Just s -> f s
    Nothing -> $unexpectedError "Signatory that is looking at document is not in document"
  where
    slForAccess :: DocumentAccess -> Document -> Maybe SignatoryLink
    slForAccess (DocumentAccess { daAccessMode = SignatoryDocumentAccess sid}) = getSigLinkFor sid
    slForAccess (DocumentAccess { daAccessMode = AdminDocumentAccess})         = getAuthorSigLink
    slForAccess (DocumentAccess { daAccessMode = AuthorDocumentAccess})        = getAuthorSigLink

documentAccessForUser :: User -> Document -> DocumentAccess
documentAccessForUser user document = DocumentAccess {
      daDocumentID = documentid document
    , daAccessMode = documentAccessModeForUser user document
  }

documentAccessModeForUser :: User -> Document -> DocumentAccessMode
documentAccessModeForUser user document =
  case (getSigLinkFor user document) of
    Just sl -> if (isAuthor sl)
                 then AuthorDocumentAccess
                 else SignatoryDocumentAccess $ signatorylinkid sl
    Nothing -> if (documentauthorcompanyid document == Just (usercompany user) && useriscompanyadmin user)
                 then AdminDocumentAccess
                 else $unexpectedError $ "User " ++ show (userid user) ++ " accessing document " ++ show (documentid document) ++ " without any permission. This should be cought earlier."
