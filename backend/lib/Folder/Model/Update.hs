module Folder.Model.Update
  (
    AddFoldersToUserGroups(..) -- remove after initial migration of Folders
  , FolderCreate(..)
  , FolderSetUserHomeFolder(..)
  , FolderUpdate(..)
  , FoldersFormCycle(..)
  ) where

import Control.Monad.Catch
import Data.Typeable
import Text.JSON.Gen hiding (object)

import DB
import Folder.Internal
import Folder.Labels
import Folder.Model.Query
import User.UserID
import UserGroup.Model
import UserGroup.Types

data FolderCreate = FolderCreate Folder
instance (MonadDB m, MonadThrow m)
    => DBUpdate m FolderCreate Folder where
  update (FolderCreate folder) = do
    let name      = get folderName folder
        mParentID = get folderParentID folder
    newParentPath <- case mParentID of
      Nothing       -> return . Array1 $ ([] :: [FolderID])
      Just parentID -> do
        runQuery_ . sqlSelect "folders" $ do
          sqlWhereEq "id" parentID
          sqlResult "parent_path"
        Array1 parentPath <- fetchOne runIdentity
        return . Array1 $ parentID : parentPath
    runQuery_ . sqlInsert "folders" $ do
      sqlSet "name"        name
      sqlSet "parent_id"   mParentID
      sqlSet "parent_path" newParentPath
      sqlResult "id"
    fid <- fetchOne runIdentity
    (dbQuery . FolderGet $ fid) >>= \case
      Nothing      -> unexpectedError "Folder could not be read from DB"
      Just folder' -> return folder'

data FolderUpdate = FolderUpdate Folder
instance (MonadDB m, MonadThrow m) => DBUpdate m FolderUpdate () where
  update (FolderUpdate newFolder) = do
    let fid = get folderID newFolder
    -- updated group may have children already, these need to be adjusted
    Array1 (oldParentPath :: [FolderID]) <- do
      runQuery_ . sqlSelect "folders" $ do
        sqlResult "parent_path"
        sqlWhereEq "id" fid
      fetchOne runIdentity
    (Array1 newParentPath :: Array1 FolderID) <- do
      case get folderParentID newFolder of
        Nothing       -> return . Array1 $ []
        Just parentID -> do
          runQuery_ . sqlSelect "folders" $ do
            sqlResult "parent_path"
            sqlWhereEq "id" . Just $ parentID
          Array1 parentpath <- fetchOne runIdentity
          return . Array1 . (parentID :) $ parentpath
    -- verify, that groups will not form a cycle
    when (fid `elem` newParentPath)
      $ throwM
      . SomeDBExtraException
      . FoldersFormCycle
      $ fid
    -- update folder
    runQuery_ . sqlUpdate "folders" $ do
      sqlSet "parent_id" . get folderParentID $ newFolder
      sqlSet "parent_path" . Array1 $ newParentPath
      sqlSet "name" . get folderName $ newFolder
      sqlWhereEq "id" fid
    -- update all child groups' parentpaths
    runQuery_ . sqlUpdate "folders" $ do
      -- to remove multiple items at once from ARRAY, there is only slicing available
      -- inside slicing, we must specify index of the last item
      -- we cut old items from start and then prepend the new parent path
      sqlSetCmd "parent_path" (arraySliceCmd oldParentPath newParentPath)
      sqlWhere $ "parent_path @> " <?> (Array1 [fid])
    where
      arraySliceCmd oldParentPath newParentPath =
        (   "array_cat(parent_path[ 1:"
        <>  "(array_length(parent_path, 1) - "
        <?> length oldParentPath
        <+> ")]"
        <>  ","
        <?> Array1 newParentPath
        <+> ")"
        )

-- Create a folder and link it to a user group.
data FolderCreateByUserGroup = FolderCreateByUserGroup UserGroupID
instance (MonadDB m, MonadThrow m) =>
  DBUpdate m FolderCreateByUserGroup (Maybe Folder) where
  update (FolderCreateByUserGroup ugid) = do
    mfdr <- query . FolderGetUserGroupHome $ ugid
    mug  <- query . UserGroupGet $ ugid

    case (mug, mfdr) of
      (Nothing , _        ) -> noUserGroupErr
      (Just _ug, Nothing  ) -> linkNewFolder ugid defaultFolder
      (Just _ug, Just _fdr) -> return Nothing

    where
      noUserGroupErr =
        unexpectedError $ "No user group corresponding to id" <+> showt ugid

      linkNewFolder ugid' fdr' = do
        fdr <- update . FolderCreate $ fdr'
        _   <- setFolderID ugid' $ get folderID fdr
        return . Just $ fdr

      -- local functionality to avoid import loop
      setFolderID ugid' fdrid' = runQuery_ . sqlUpdate "user_groups" $ do
        sqlSet "home_folder_id" fdrid'
        sqlWhereEq "id" ugid'

data FolderCreateForUser = FolderCreateForUser UserID (Maybe FolderID)
instance (MonadDB m, MonadThrow m) =>
  DBUpdate m FolderCreateForUser (Maybe Folder)where
    -- make sure there isn't one already set for this user
  update (FolderCreateForUser uid mfdrparentid) = do
    (get folderID <$>) <$> (query . FolderGetUserHome $ uid) >>= \case
      (Just _) -> return Nothing
      Nothing  -> do
        let fdr' = set folderParentID mfdrparentid defaultFolder
        fdr <- update . FolderCreate $ fdr'

        runQuery_ . sqlUpdate "users" $ do
          sqlSet "home_folder_id" (get folderID fdr)
          sqlWhereEq "id" uid
        return . Just $ fdr


data FolderCreateForUsersInUserGroup =
    FolderCreateForUsersInUserGroup UserGroupID
instance (MonadDB m, MonadThrow m) =>
  DBUpdate m FolderCreateForUsersInUserGroup Int where
  update (FolderCreateForUsersInUserGroup ugid) = do
    ((get folderID <$>) <$> (query . FolderGetUserGroupHome $ ugid)) >>= \case
      Nothing -> do
        unexpectedError "how did we get this far???"
      homeFdrID -> do
        uids <- do
          runQuery_ . sqlSelect "users" $ do
            sqlResult "id"
            sqlWhereEq "user_group_id" ugid
          fetchMany runIdentity
        length
          .   catMaybes
          <$> (forM uids $ \uid -> update (FolderCreateForUser uid homeFdrID))

data AddFoldersToUserGroups = AddFoldersToUserGroups [UserGroupID]
instance (MonadDB m, MonadThrow m) =>
  DBUpdate m AddFoldersToUserGroups Int where
  update (AddFoldersToUserGroups ugids) = do
    numActuallyCreatedFolders <- do
      length . catMaybes <$> (forM ugids $ update . FolderCreateByUserGroup)
    _ <- (forM ugids $ update . FolderCreateForUsersInUserGroup)
    return numActuallyCreatedFolders

data FolderSetUserHomeFolder = FolderSetUserHomeFolder UserID FolderID
instance (MonadDB m, MonadThrow m) => DBUpdate m FolderSetUserHomeFolder () where
  update (FolderSetUserHomeFolder uid fdrid) = do
    runQuery01_ . sqlUpdate "users" $ do
      sqlWhereEq "id" uid
      sqlSet "home_folder_id" (Just fdrid)

data FoldersFormCycle = FoldersFormCycle FolderID
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue FoldersFormCycle where
  toJSValue (FoldersFormCycle dgid) = runJSONGen $ do
    value "message"   ("Folders Form Cycle" :: String)
    value "folder_id" (show dgid)

instance DBExtraException FoldersFormCycle
