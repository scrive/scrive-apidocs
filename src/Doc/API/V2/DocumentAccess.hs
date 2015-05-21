{-# OPTIONS_GHC -fno-warn-orphans #-}

module Doc.API.V2.DocumentAccess (DocumentAccess(..),DocumentAccessMode(..),canSeeSignlinks, propertyForCurrentSignatory) where

import Doc.SignatoryLinkID
import KontraPrelude
import Doc.DocumentID
import Util.SignatoryLinkUtils
import Doc.DocStateData

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