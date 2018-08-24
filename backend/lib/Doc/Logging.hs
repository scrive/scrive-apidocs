module Doc.Logging (
    logDocument
  , logFile
  , logSignatory
  , logDocumentAndFile
  , logDocumentAndSignatory
  ) where

import Log

import Doc.DocumentID
import Doc.SignatoryLinkID
import File.FileID
import Log.Identifier

logDocument :: MonadLog m => DocumentID -> m r -> m r
logDocument did = localData [identifier did]

logFile :: MonadLog m => FileID -> m r -> m r
logFile fid = localData [identifier fid]

logSignatory :: MonadLog m => SignatoryLinkID -> m r -> m r
logSignatory slid = localData [identifier slid]

logDocumentAndFile :: MonadLog m => DocumentID -> FileID -> m r -> m r
logDocumentAndFile did fid = localData [identifier did, identifier fid]

logDocumentAndSignatory :: MonadLog m => DocumentID -> SignatoryLinkID -> m r -> m r
logDocumentAndSignatory did slid = localData [identifier did, identifier slid]
