{-# LANGUAGE NoMonomorphismRestriction #-}
module Attachment.Model
  ( defaultAttachment
  , NewAttachment(..)
  , Attachment(..)
  , DeleteAttachments(..)
  , SetAttachmentsSharing(..)
  , GetAttachments(..)
  , PurgeAttachments(..)
  , AttachmentDomain(..)
  , AttachmentFilter(..)
  , AttachmentOrderBy(..)
  )
where

import Control.Monad.Catch
import Control.Monad.State.Class
import Crypto.RNG

import Attachment.AttachmentID
import DB
import File.FileID
import MinutesTime
import User.UserID
import Util.Actor

data Attachment = Attachment
  { attachmentid      :: AttachmentID
  , attachmenttitle   :: Text
  , attachmentctime   :: UTCTime
  , attachmentmtime   :: UTCTime
  , attachmentfile    :: FileID
  , attachmentuser    :: UserID
  , attachmentshared  :: Bool
  , attachmentdeleted :: Bool
  } deriving (Eq, Show)

defaultAttachment :: Attachment
defaultAttachment = Attachment { attachmentid      = unsafeAttachmentID 0
                               , attachmenttitle   = ""
                               , attachmentctime   = unixEpoch
                               , attachmentmtime   = unixEpoch
                               , attachmentfile    = unsafeFileID 0
                               , attachmentuser    = unsafeUserID 0
                               , attachmentshared  = False
                               , attachmentdeleted = False
                               }

sqlAttachmentResults :: (MonadState v m, SqlResult v) => m ()
sqlAttachmentResults = do
  sqlResult "id"
  sqlResult "title"
  sqlResult "ctime"
  sqlResult "mtime"
  sqlResult "file_id"
  sqlResult "user_id"
  sqlResult "shared"
  sqlResult "deleted"

fetchAttachment
  :: (AttachmentID, Text, UTCTime, UTCTime, FileID, UserID, Bool, Bool) -> Attachment
fetchAttachment (aid, title, ctime, mtime, file_id, user_id, shared, deleted) =
  Attachment { attachmentid      = aid
             , attachmenttitle   = title
             , attachmentctime   = ctime
             , attachmentmtime   = mtime
             , attachmentfile    = file_id
             , attachmentuser    = user_id
             , attachmentshared  = shared
             , attachmentdeleted = deleted
             }

data NewAttachment = NewAttachment UserID Text FileID Actor
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m NewAttachment Attachment where
  dbUpdate (NewAttachment uid title fileid actor) = do
    let ctime = actorTime actor
    runQuery_ . sqlInsert "attachments" $ do
      sqlSet "user_id" uid
      sqlSet "title"   title
      sqlSet "ctime"   ctime
      sqlSet "mtime"   ctime
      sqlSet "shared"  False
      sqlSet "deleted" False
      sqlSet "file_id" fileid
      sqlAttachmentResults
    fetchOne fetchAttachment

data DeleteAttachments = DeleteAttachments UserID [AttachmentID] Actor
instance (CryptoRNG m, MonadDB m) => DBUpdate m DeleteAttachments () where
  dbUpdate (DeleteAttachments uid attids actor) = do
    let atime = actorTime actor
    runQuery_ . sqlUpdate "attachments" $ do
      sqlSet "mtime"   atime
      sqlSet "deleted" True
      sqlWhereIn "id" attids
      sqlWhereEq "user_id" uid
      sqlWhereEq "deleted" False

data PurgeAttachments = PurgeAttachments
instance (MonadDB m, MonadTime m) => DBUpdate m PurgeAttachments Int where
  dbUpdate PurgeAttachments = do
    now <- currentTime
    runQuery . sqlUpdate "attachments a" $ do
      sqlSet "mtime"   now
      sqlSet "deleted" True
      sqlWhereEq "a.deleted" False

      -- Keep attachments if
      -- 1) the company still exists and it is shared or
      -- 2) the user still exists.
      sqlWhereNotExists . sqlSelect "users u" $ do
        sqlJoinOn "user_groups ug" "u.user_group_id = ug.id"
        sqlWhere "u.id = a.user_id"
        sqlWhere "(ug.deleted IS NULL AND a.shared) OR u.deleted IS NULL"

data AttachmentFilter
  = AttachmentFilterByString Text           -- ^ Contains the string in title, list of people involved or anywhere
  | AttachmentFilterByID AttachmentID         -- ^ Attachments with IDs on the list
  | AttachmentFilterByFileID FileID           -- ^ Attachments with IDs on the list
  deriving Eq

sqlWhereAttachmentFilter :: (MonadState v m, SqlWhere v) => AttachmentFilter -> m ()
sqlWhereAttachmentFilter (AttachmentFilterByString string) =
  sqlWhereILike "attachments.title" ("%" <> string <> "%")
sqlWhereAttachmentFilter (AttachmentFilterByID aid) = sqlWhereEq "attachments.id" aid
sqlWhereAttachmentFilter (AttachmentFilterByFileID fileid) =
  sqlWhereEq "attachments.file_id" fileid

data AttachmentDomain
  = AttachmentsOfAuthorDeleteValue UserID Bool   -- ^ Attachments of user, with deleted flag
  | AttachmentsSharedInUsersUserGroup UserID       -- ^ Attachments shared in the user company

-- | These are possible order by clauses that make documents sorted by.
data AttachmentOrderBy
  = AttachmentOrderByTitle       -- ^ Order by title, alphabetically, case insensitive
  | AttachmentOrderByMTime       -- ^ Order by modification time
  deriving Eq

-- | GetAttachments is central switch for attachments list queries.
--
-- GetAttachments domains filters sorting
--
-- * domains are connected with OR, so attachments falling into ANY of domains will be returned
-- * filters weed out attachments from domains, are connected with AND so a attachments must pass through ALL filters
-- * sortings returns attachments in order
--
-- GetAttachments returns attachments in proper order, no reverse is needed.
--
data GetAttachments = GetAttachments [AttachmentDomain] [AttachmentFilter] [AscDesc AttachmentOrderBy]
instance MonadDB m => DBQuery m GetAttachments [Attachment] where
  dbQuery (GetAttachments domains filters orderbys) = do
    runQuery_ . sqlSelect "attachments" $ do
      sqlAttachmentResults
      sqlWhereAny (map (sqlWhere . domainToSQLCommand) domains)
      mapM_ sqlWhereAttachmentFilter         filters
      mapM_ (sqlOrderBy . orderToSQLCommand) orderbys
    fetchMany fetchAttachment
    where
      domainToSQLCommand (AttachmentsOfAuthorDeleteValue uid del) =
        "attachments.user_id =" <?> uid <+> "AND attachments.deleted =" <?> del
      domainToSQLCommand (AttachmentsSharedInUsersUserGroup uid) =
        "attachments.deleted = FALSE AND attachments.shared AND EXISTS (SELECT 1 FROM users, users AS users_2"
          <+> "WHERE attachments.user_id = users.id"
          <+> "AND users.user_group_id = users_2.user_group_id AND users_2.id ="
          <?> uid
          <+> ")"
      orderToSQLCommand (Asc  AttachmentOrderByTitle) = "attachments.title ASC"
      orderToSQLCommand (Desc AttachmentOrderByTitle) = "attachments.title DESC"
      orderToSQLCommand (Asc  AttachmentOrderByMTime) = "attachments.mtime ASC"
      orderToSQLCommand (Desc AttachmentOrderByMTime) = "attachments.mtime DESC"


data SetAttachmentsSharing = SetAttachmentsSharing UserID [AttachmentID] Bool
instance (MonadDB m) => DBUpdate m SetAttachmentsSharing () where
  dbUpdate (SetAttachmentsSharing uid atts flag) = do
    runQuery_ . sqlUpdate "attachments" $ do
      sqlSet "shared" flag
      sqlWhereIn "id" atts
      sqlWhereEq "user_id" uid
