module Doc.DocStateInterface 
    ( getDocByDocID
    , getDocByDocIDSigLinkIDAndMagicHash
    , restartDocument
    , markDocumentSeen
    ) where
    
import DBError
import Doc.DocState
import Doc.DocStateData
import Doc.DocUtils
import Kontra
import Misc

import Control.Monad.State (get)
import Happstack.State     (query, update)
import Control.Monad       (mzero)

{- |
   Securely find a document by documentid for the author or his friends.
   User must be logged in (otherwise Left DBNotLoggedIn).
   Document must exist (otherwise Left DBNotAvailable).
   Logged in user is author OR logged in user is friend of author (otherwise LeftDBNotAvailable).
 -}
getDocByDocID :: DocumentID -> Kontra (Either DBError Document)
getDocByDocID docid = do
  Context { ctxmaybeuser } <- get
  case ctxmaybeuser of
    Nothing   -> return $ Left DBNotLoggedIn
    Just user -> do
      mdoc <- query $ GetDocumentByDocumentID docid
      case mdoc of
        Nothing  -> return $ Left DBResourceNotAvailable
        Just doc ->
          case isUserAuthor doc user of
            True  -> return $ Right doc
            False -> do
              usersImFriendsWith <- query $ GetUsersByFriendUserID (userid user)
              case any (isUserAuthor doc) usersImFriendsWith of
                True  -> return $ Right doc
                False -> return $ Left DBResourceNotAvailable

{- |
   Get all of the documents a user can view.
   User must be logged in.
   Logged in user is in the documentsignatorylinks or a friend of someone with the documentsignatorylinks
 -}
getDocsByLoggedInUser :: Kontra (Either DBError [Document])
getDocsByLoggedInUser = do
  Context { ctxmaybeuser } <- get
  case ctxmaybeuser of
    Nothing   -> return $ Left DBNotLoggedIn
    Just user -> do
      docs <- query $ GetDocuments
      usersImFriendsWith <- query $ GetUsersByFriendUserID (userid user)
      return $ Right $ [ doc | doc <- docs
                             , any (\u -> canUserViewDirectly u doc) (user : usersImFriendsWith) ]

{- |
   Get a document using docid, siglink, and magichash.
   ALWAYS FAILS THE SAME WAY FOR SECURITY PURPOSES.
   Document must exist.
   SignatoryLinkID must correspond to a siglink in document.
   MagicHash must match.
 -}
getDocByDocIDSigLinkIDAndMagicHash :: DocumentID 
                                   -> SignatoryLinkID 
                                   -> MagicHash 
                                   -> Kontra (Either DBError Document)
getDocByDocIDSigLinkIDAndMagicHash docid sigid mh = do
  mdoc <- query $ GetDocumentByDocumentID docid
  case mdoc of
    Nothing  -> return $ Left DBResourceNotAvailable
    Just doc ->
      case getSigLinkBySigLinkID sigid doc of
        Just siglink | signatorymagichash siglink == mh -> return $ Right doc
        _ -> return $ Left DBResourceNotAvailable


-- move this to an update file

{- |
   Mark document seen securely.
 -}
markDocumentSeen docid sigid mh time ipnum =
  update $ MarkDocumentSeen docid sigid mh time ipnum

{- |
   Securely 
 -}
restartDocument :: Document -> Kontra Document
restartDocument doc@Document { documentid } = do
  Context { ctxtime
          , ctxipnumber
          , ctxmaybeuser
          } <- get
  user <- returnJustOrMZero ctxmaybeuser
  case getAuthorSigLink doc of
    Just authorsiglink | isSigLinkForUser user authorsiglink -> do
      edoc <- update $ RestartDocument documentid user ctxtime ctxipnumber 
      case edoc of
        Right doc -> return doc
        _         -> mzero
    _ -> mzero
