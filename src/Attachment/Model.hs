
{-# LANGUAGE NoMonomorphismRestriction #-}
module Attachment.Model
  ( NewAttachment(..)
  , Attachment(..)
  , DeleteAttachments(..)
  , SetAttachmentTitle(..)
  , GetAttachments(..)
  , AttachmentPagination(..)
  , AttachmentDomain(..)
  , AttachmentFilter(..)
  , AttachmentOrderBy(..)
  )
where

import DB.SQL2
import DB
import Attachment.AttachmentID
import File.FileID
import File.File
import File.Model
import User.UserID
import MinutesTime
import Crypto.RNG
import Util.Actor
import Control.Applicative
import Control.Monad.State.Class

data Attachment = Attachment
  { attachmentid      :: AttachmentID
  , attachmenttitle   :: String
  , attachmentctime   :: MinutesTime
  , attachmentmtime   :: MinutesTime
  , attachmentfile    :: FileID
  , attachmentuser    :: UserID
  , attachmentshared  :: Bool
  , attachmentdeleted :: Bool
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

fetchAttachments :: MonadDB m => DBEnv m [Attachment]
fetchAttachments = foldDB decoder []
  where
    decoder acc aid title ctime mtime file_id user_id shared deleted =
      Attachment { attachmentid      = aid
                 , attachmenttitle   = title
                 , attachmentctime   = ctime
                 , attachmentmtime   = mtime
                 , attachmentfile    = file_id
                 , attachmentuser    = user_id
                 , attachmentshared  = shared
                 , attachmentdeleted = deleted
                 } : acc


data NewAttachment = NewAttachment UserID String String Binary Actor
instance (CryptoRNG m, MonadDB m, Applicative m) => DBUpdate m NewAttachment (Either String Attachment) where
  update (NewAttachment uid title filename filecontents actor) = do
  let ctime = actorTime actor
  file <- update $ NewFile filename filecontents
  kRun_ $ sqlInsert "attachments" $ do
    sqlSet "user_id" uid
    sqlSet "title" title
    sqlSet "ctime" ctime
    sqlSet "mtime" ctime
    sqlSet "shared" False
    sqlSet "deleted" False
    sqlSet "file_id" (fileid file)
    sqlAttachmentResults
  atts <- fetchAttachments
  case atts of
    [att] -> return (Right att)
    _ -> return (Left $ "NewAttachment of file " ++ title ++ " failed")

data DeleteAttachments = DeleteAttachments UserID [AttachmentID] Actor
instance (CryptoRNG m, MonadDB m, Applicative m) => DBUpdate m DeleteAttachments (Either String ()) where
  update (DeleteAttachments uid attids actor) = do
  let atime = actorTime actor
  kRun_ $ sqlUpdate "attachments" $ do
    sqlSet "mtime" atime
    sqlSet "deleted" True
    sqlWhereIn "id" attids
    sqlWhereEq "user_id" uid
    sqlWhereEq "deleted" False
  return $ Right ()

data SetAttachmentTitle = SetAttachmentTitle AttachmentID String Actor
instance (CryptoRNG m, MonadDB m, Applicative m) => DBUpdate m SetAttachmentTitle (Either String ()) where
  update (SetAttachmentTitle attid title actor) = do
  let atime = actorTime actor
  kRun_ $ sqlUpdate "attachments" $ do
    sqlSet "mtime" atime
    sqlSet "title" title
    sqlWhereEq "id" attid
  return $ Right ()

data AttachmentPagination =
  AttachmentPagination
  { attachmentOffset :: Int        -- ^ use for SQL OFFSET command
  , attachmentLimit  :: Int        -- ^ use for SQL LIMIT command
  }

data AttachmentFilter
  = AttachmentFilterByString String             -- ^ Contains the string in title, list of people involved or anywhere

instance IsSQL AttachmentFilter where
  toSQLCommand (AttachmentFilterByString string) =
    SQL "attachments.title ILIKE ?" [toSql ("%" ++ string ++ "%")]

data AttachmentDomain
  = AttachmentsOfAuthorDeleteValue UserID Bool   -- ^ Attachments of user, with deleted flag
  | AttachmentsSharedInUsersCompany UserID       -- ^ Attachments shared in the user company

instance IsSQL AttachmentDomain where
  toSQLCommand (AttachmentsOfAuthorDeleteValue uid del) =
    SQL "attachments.user_id = ? AND attachments.deleted = ?" [toSql uid, toSql del]
  toSQLCommand (AttachmentsSharedInUsersCompany uid) =
    SQL "attachments.deleted = FALSE AND EXISTS (SELECT 1 FROM users, users AS users_2 WHERE attachments.user_id = users.id AND users.company_id = users_2.company_id AND users_2.id = ?)" [toSql uid]

-- | These are possible order by clauses that make documents sorted by.
data AttachmentOrderBy
  = AttachmentOrderByTitle       -- ^ Order by title, alphabetically, case insensitive
  | AttachmentOrderByMTime       -- ^ Order by modification time
  | AttachmentOrderByCTime       -- ^ Order by creation time

instance IsSQL (AscDesc AttachmentOrderBy) where
  toSQLCommand (Asc AttachmentOrderByTitle) = SQL "attachments.title ASC" []
  toSQLCommand (Desc AttachmentOrderByTitle) = SQL "attachments.title DESC" []
  toSQLCommand (Asc AttachmentOrderByMTime) = SQL "attachments.mtime ASC" []
  toSQLCommand (Desc AttachmentOrderByMTime) = SQL "attachments.mtime DESC" []
  toSQLCommand (Asc AttachmentOrderByCTime) = SQL "attachments.ctime ASC" []
  toSQLCommand (Desc AttachmentOrderByCTime) = SQL "attachments.ctime DESC" []

-- | GetAttachments is central switch for attachments list queries.
--
-- GetAttachments domains filters sorting pagination
--
-- * domains are connected with OR, so attachments falling into ANY of domains will be returned
-- * filters weed out attachments from domains, are connected with AND so a attachments must pass through ALL filters
-- * sortings returns attachments in order
-- * pagination is a place to put OFFSET and LIMIT values
--
-- GetAttachments returns attachments in proper order, no reverse is needed.
--
data GetAttachments = GetAttachments [AttachmentDomain] [AttachmentFilter] [AscDesc AttachmentOrderBy] AttachmentPagination
instance MonadDB m => DBQuery m GetAttachments [Attachment] where
  query (GetAttachments domains filters orderbys _pagination) = do
    kRun_ $ sqlSelect "attachments" $ do
      sqlAttachmentResults
      sqlWhereOr domains
      mapM_ sqlWhere filters
      mapM_ sqlOrderBy orderbys
    fetchAttachments
