
{-# LANGUAGE NoMonomorphismRestriction #-}
module Attachment.Model
  ( NewAttachment(..)
  , Attachment(..)
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
