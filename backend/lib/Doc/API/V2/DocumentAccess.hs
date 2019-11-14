module Doc.API.V2.DocumentAccess (
  DocumentAccess(..)
, DocumentAccessMode(..)
, canSeeSignlinks
, propertyForCurrentSignatory
, documentAccessForUser
, documentAccessForSlid
, documentAccessForAuthor
, documentAccessForAdminonly
) where

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
    -- ^ Person looking at the document is this signatory.
  | AuthorDocumentAccess
    -- ^ Person looking at the document is an author.
  | CompanyAdminDocumentAccess (Maybe SignatoryLinkID)
    -- ^ Person looking at the document is an admin of author.
  | CompanySharedDocumentAccess
    -- ^ Person looking at the document is in the author's company and the
    -- document is shared.
  | SystemAdminDocumentAccess
    -- ^ Person looking at the document is an admin of author.

canSeeSignlinks :: DocumentAccess -> Bool
canSeeSignlinks (DocumentAccess { daAccessMode = AuthorDocumentAccess }) = True
canSeeSignlinks (DocumentAccess { daAccessMode = CompanyAdminDocumentAccess _ }) = True
canSeeSignlinks _ = False

propertyForCurrentSignatory :: DocumentAccess -> (SignatoryLink -> a) -> Document -> a
propertyForCurrentSignatory da f doc = case slForAccess da doc of
  Just s  -> f s
  Nothing -> unexpectedError "Signatory that is looking at document is not in document"
  where
    slForAccess :: DocumentAccess -> Document -> Maybe SignatoryLink
    slForAccess (DocumentAccess { daAccessMode = SignatoryDocumentAccess sid }) =
      getSigLinkFor sid
    slForAccess (DocumentAccess { daAccessMode = AuthorDocumentAccess }) =
      getAuthorSigLink
    slForAccess (DocumentAccess { daAccessMode = CompanySharedDocumentAccess }) =
      getAuthorSigLink
    slForAccess (DocumentAccess { daAccessMode = SystemAdminDocumentAccess }) =
      getAuthorSigLink
    slForAccess (DocumentAccess { daAccessMode = CompanyAdminDocumentAccess (Just sid) })
      = getSigLinkFor sid
    slForAccess (DocumentAccess { daAccessMode = CompanyAdminDocumentAccess Nothing }) =
      getAuthorSigLink

documentAccessForUser :: User -> Document -> DocumentAccess
documentAccessForUser user document = DocumentAccess
  { daDocumentID = documentid document
  , daAccessMode = documentAccessModeForUser user document
  , daStatus     = documentstatus document
  }

documentAccessForAuthor :: Document -> DocumentAccess
documentAccessForAuthor document = DocumentAccess { daDocumentID = documentid document
                                                  , daAccessMode = AuthorDocumentAccess
                                                  , daStatus     = documentstatus document
                                                  }

documentAccessForAdminonly :: Document -> DocumentAccess
documentAccessForAdminonly document = DocumentAccess
  { daDocumentID = documentid document
  , daAccessMode = SystemAdminDocumentAccess
  , daStatus     = documentstatus document
  }

documentAccessModeForUser :: User -> Document -> DocumentAccessMode
documentAccessModeForUser user document = case (getSigLinkFor user document) of
  Just sl -> if (isAuthor sl)
    then AuthorDocumentAccess
    else
      if (  documentauthorugid document
         == Just (user ^. #groupID)
         && (user ^. #isCompanyAdmin)
         )
      then
        CompanyAdminDocumentAccess $ Just $ signatorylinkid sl
      else
        SignatoryDocumentAccess $ signatorylinkid sl
  Nothing ->
    if (documentauthorugid document == Just (user ^. #groupID) && user ^. #isCompanyAdmin)
      then CompanyAdminDocumentAccess $ Nothing
      else
        if (  documentauthorugid document
           == Just (user ^. #groupID)
           && isDocumentShared document
           )
        then
          CompanySharedDocumentAccess
        else
          unexpectedError
          $  "User "
          <> showt (user ^. #id)
          <> " accessing document "
          <> showt (documentid document)
          <> " without any permission. This should be cought earlier."

documentAccessForSlid :: SignatoryLinkID -> Document -> DocumentAccess
documentAccessForSlid slid document = DocumentAccess
  { daDocumentID = documentid document
  , daAccessMode = documentAccessModeForSlid slid document
  , daStatus     = documentstatus document
  }

documentAccessModeForSlid :: SignatoryLinkID -> Document -> DocumentAccessMode
documentAccessModeForSlid slid document = case (getSigLinkFor slid document) of
  Just sl -> if (isAuthor sl)
    then AuthorDocumentAccess
    else SignatoryDocumentAccess $ signatorylinkid sl
  Nothing ->
    unexpectedError
      $  "SignatoryLinkID "
      <> showt slid
      <> " accessing document "
      <> showt (documentid document)
      <> " without any permission. This should be caught earlier."
