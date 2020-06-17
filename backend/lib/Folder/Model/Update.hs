-- brittany --exactprint-only
module Folder.Model.Update
  (
    AddFoldersToUserGroups(..) -- remove after initial migration of Folders
  , FolderCreate(..)
  , FolderUpdate(..)
  , FoldersFormCycle(..)
  , FolderMovingBetweenDifferentsUserGroupTrees(..)
  ) where

import Control.Monad.Catch
import Data.Typeable
import Text.JSON.Gen hiding (object)

import DB
import Folder.Model.Query
import Folder.Types
import User.UserID
import UserGroup.Model
import UserGroup.Types

newtype FolderCreate = FolderCreate Folder
instance (MonadDB m, MonadThrow m)
    => DBUpdate m FolderCreate Folder where
  dbUpdate (FolderCreate folder) = do
    let name      = folder ^. #name
        mParentID = folder ^. #parentID
    newParentPath <- case mParentID of
      Nothing       -> return $ Array1 ([] :: [FolderID])
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

newtype FolderUpdate = FolderUpdate Folder
instance (MonadDB m, MonadThrow m) => DBUpdate m FolderUpdate () where
  dbUpdate (FolderUpdate newFolder) = do
    let fid = newFolder ^. #id
    -- updated group may have children already, these need to be adjusted
    Array1 (oldParentPath :: [FolderID]) <- do
      runQuery_ . sqlSelect "folders" $ do
        sqlResult "parent_path"
        sqlWhereEq "id" fid
      fetchOne runIdentity
    (Array1 newParentPath :: Array1 FolderID) <- do
      case newFolder ^. #parentID of
        Nothing       -> return . Array1 $ []
        Just parentID -> do
          runQuery_ . sqlSelect "folders" $ do
            sqlResult "parent_path"
            sqlWhereEq "id" . Just $ parentID
          Array1 parentpath <- fetchOne runIdentity
          return . Array1 . (parentID :) $ parentpath
    -- verify, that folder is not moving between dirrented UserGroup trees
    oldRootUGID <- folderUGIDRoot $ fid : oldParentPath
    newRootUGID <- folderUGIDRoot $ fid : newParentPath
    when (oldRootUGID /= newRootUGID)
      . throwM
      . SomeDBExtraException
      $ FolderMovingBetweenDifferentsUserGroupTrees fid oldRootUGID newRootUGID
    -- verify, that groups will not form a cycle
    when (fid `elem` newParentPath)
      . throwM
      . SomeDBExtraException
      . FoldersFormCycle
      $ fid
    -- update folder
    runQuery_ . sqlUpdate "folders" $ do
      sqlSet "parent_id" $ newFolder ^. #parentID
      sqlSet "parent_path" . Array1 $ newParentPath
      sqlSet "name" $ newFolder ^. #name
      sqlWhereEq "id" fid
    -- update all child groups' parentpaths
    runQuery_ . sqlUpdate "folders" $ do
      -- to remove multiple items at once from ARRAY, there is only slicing available
      -- inside slicing, we must specify index of the last item
      -- we cut old items from start and then prepend the new parent path
      sqlSetCmd "parent_path" (arraySliceCmd oldParentPath newParentPath)
      sqlWhere $ "parent_path @> " <?> Array1 [fid]
    where
      arraySliceCmd oldParentPath newParentPath =
        "array_cat(parent_path[ 1:"
          <>  "(array_length(parent_path, 1) - "
          <?> length oldParentPath
          <+> ")]"
          <>  ","
          <?> Array1 newParentPath
          <+> ")"



-- Create a folder and link it to a user group.
newtype FolderCreateByUserGroup = FolderCreateByUserGroup UserGroupID
instance (MonadDB m, MonadThrow m) =>
  DBUpdate m FolderCreateByUserGroup (Maybe Folder) where
  dbUpdate (FolderCreateByUserGroup ugid) = do
    mfdr <- dbQuery . FolderGetUserGroupHome $ ugid
    mug  <- dbQuery . UserGroupGet $ ugid

    case (mug, mfdr) of
      (Nothing , _        ) -> noUserGroupErr
      (Just _ug, Nothing  ) -> linkNewFolder ugid defaultFolder
      (Just _ug, Just _fdr) -> return Nothing

    where
      noUserGroupErr =
        unexpectedError $ "No user group corresponding to id" <+> showt ugid

      linkNewFolder ugid' fdr' = do
        fdr <- dbUpdate . FolderCreate $ fdr'
        _   <- setFolderID ugid' $ fdr ^. #id
        return . Just $ fdr

      -- local functionality to avoid import loop
      setFolderID ugid' fdrid' = runQuery_ . sqlUpdate "user_groups" $ do
        sqlSet "home_folder_id" fdrid'
        sqlWhereEq "id" ugid'

data FolderCreateForUser = FolderCreateForUser UserID (Maybe FolderID)
instance (MonadDB m, MonadThrow m) =>
  DBUpdate m FolderCreateForUser (Maybe Folder)where
    -- make sure there isn't one already set for this user
  dbUpdate (FolderCreateForUser uid mfdrparentid) = do
    (view #id <$>) <$> (dbQuery . FolderGetUserHome $ uid) >>= \case
      (Just _) -> return Nothing
      Nothing  -> do
        let fdr' = set #parentID mfdrparentid defaultFolder
        fdr <- dbUpdate . FolderCreate $ fdr'

        runQuery_ . sqlUpdate "users" $ do
          sqlSet "home_folder_id" (fdr ^. #id)
          sqlWhereEq "id" uid
        return . Just $ fdr


newtype FolderCreateForUsersInUserGroup =
    FolderCreateForUsersInUserGroup UserGroupID
instance (MonadDB m, MonadThrow m) =>
  DBUpdate m FolderCreateForUsersInUserGroup Int where
  dbUpdate (FolderCreateForUsersInUserGroup ugid) = do
    ((view #id <$>) <$> (dbQuery . FolderGetUserGroupHome $ ugid)) >>= \case
      Nothing -> do
        unexpectedError "how did we get this far???"
      homeFdrID -> do
        uids <- do
          runQuery_ . sqlSelect "users" $ do
            sqlResult "id"
            sqlWhereEq "user_group_id" ugid
          fetchMany runIdentity
        length . catMaybes <$> forM
          uids
          (\uid -> dbUpdate (FolderCreateForUser uid homeFdrID))

newtype AddFoldersToUserGroups = AddFoldersToUserGroups [UserGroupID]
instance (MonadDB m, MonadThrow m) =>
  DBUpdate m AddFoldersToUserGroups Int where
  dbUpdate (AddFoldersToUserGroups ugids) = do
    numActuallyCreatedFolders <- do
      length . catMaybes <$> forM ugids (dbUpdate . FolderCreateByUserGroup)
    forM_ ugids $ dbUpdate . FolderCreateForUsersInUserGroup
    return numActuallyCreatedFolders

newtype FoldersFormCycle = FoldersFormCycle FolderID
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue FoldersFormCycle where
  toJSValue (FoldersFormCycle dgid) = runJSONGen $ do
    value "message"   ("Folders Form Cycle" :: String)
    value "folder_id" (show dgid)

instance DBExtraException FoldersFormCycle

data FolderMovingBetweenDifferentsUserGroupTrees =
  FolderMovingBetweenDifferentsUserGroupTrees {
    fmFid :: FolderID,
    fmOldRootUgid :: UserGroupID,
    fmNewRootUgid :: UserGroupID }
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue FolderMovingBetweenDifferentsUserGroupTrees where
  toJSValue FolderMovingBetweenDifferentsUserGroupTrees {..} = runJSONGen $ do
    -- FIXME in a better way; FC did this only to fix this for users and quickly move on
    -- since this is going to Elm FE we mimic APIError
    value "error_type" ("folder_action_forbidden" :: String)
    value "error_message" ("Folder is moving between 2 different UserGroup trees" :: String)
    value "http_code" (403 :: Int)
    value "message" ("Folder is moving between 2 different UserGroup trees" :: String)
    value "folder_id" $ show fmFid
    value "old_root_user_group_id" $ show fmOldRootUgid
    value "new_root_user_group_id" $ show fmNewRootUgid

instance DBExtraException FolderMovingBetweenDifferentsUserGroupTrees

folderUGIDRoot :: (MonadDB m, MonadThrow m) => [FolderID] -> m UserGroupID
folderUGIDRoot fidAndParentPath = do
  mUGs      <- mapM (dbQuery . UserGroupGetByHomeFolderID) fidAndParentPath
  closestUG <- case listToMaybe . catMaybes $ mUGs of
    Nothing ->
      unexpectedError
        $  "None of Folder + parents is home Folder of some UserGroup"
        <> showt fidAndParentPath
    Just closestUG -> return closestUG
  closestUgwp <- dbQuery $ UserGroupGetWithParentsByUG closestUG
  return . view #id . ugwpRoot $ closestUgwp
