module Folder.Model.Query
  (
    FolderDelete(..)
  , FolderGet(..)
  , FolderGetUserGroupHome(..)
  , FolderGetUserHome(..)
  , FolderIsAHomeFolder(..)
  , FolderHasDocuments(..)
  , FolderGetParents(..)
  , FolderGetAllChildrenRecursive(..)
  , FolderGetImmediateChildren(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Time

import DB
import Doc.Conditions
import Folder.Types
import User.UserID
import UserGroup.Types

newtype FolderGet = FolderGet FolderID
instance (MonadDB m, MonadThrow m) => DBQuery m FolderGet (Maybe Folder) where
  dbQuery (FolderGet dgid) = do
    runQuery_ . sqlSelect "folders" $ do
      mapM_ sqlResult folderSelectors
      sqlWhereEq "id" dgid
      sqlWhereIsNULL "deleted"
    fetchMaybe fetchFolder

newtype FolderGetUserGroupHome =
    FolderGetUserGroupHome UserGroupID
instance (MonadDB m, MonadThrow m)
  => DBQuery m FolderGetUserGroupHome (Maybe Folder) where
  dbQuery (FolderGetUserGroupHome ugid) = do
    mfolderId <- do
      runQuery_ . sqlSelect "user_groups" $ do
        sqlResult "home_folder_id"
        sqlWhereIsNotNULL "home_folder_id"
        sqlWhereEq "id" ugid
      fetchMaybe runIdentity
    case mfolderId of
      Nothing       -> return Nothing
      Just folderId -> dbQuery . FolderGet $ folderId

newtype FolderGetUserHome = FolderGetUserHome UserID
instance (MonadDB m, MonadThrow m)
  => DBQuery m FolderGetUserHome (Maybe Folder) where
  dbQuery (FolderGetUserHome uid) = do
    mfolderId <- do
      runQuery_ . sqlSelect "users" $ do
        sqlResult "home_folder_id"
        sqlWhereIsNotNULL "home_folder_id"
        sqlWhereEq "id" uid
      fetchMaybe runIdentity
    case mfolderId of
      Nothing       -> return Nothing
      Just folderId -> dbQuery . FolderGet $ folderId

newtype FolderIsAHomeFolder = FolderIsAHomeFolder FolderID
instance (MonadDB m, MonadThrow m)
  => DBQuery m FolderIsAHomeFolder Bool where
  dbQuery (FolderIsAHomeFolder fid) = do
    userRows <- runQuery01 . sqlSelect "users" $ do
      sqlWhereEq "home_folder_id" fid
      sqlLimit 1
    ugRows <- runQuery01 . sqlSelect "user_groups" $ do
      sqlWhereEq "home_folder_id" fid
      sqlLimit 1
    return $ userRows || ugRows

newtype FolderHasDocuments = FolderHasDocuments FolderID
instance (MonadDB m, MonadThrow m)
  => DBQuery m FolderHasDocuments Bool where
  dbQuery (FolderHasDocuments fid) = do
    runQuery01 . sqlSelect "documents" $ do
      sqlWhereEq "folder_id" fid
      sqlWhereDocumentWasNotPurged
      sqlWhereDocumentIsNotReallyDeletedByAuthor
      sqlLimit 1

-- Get all children recursively
newtype FolderGetAllChildrenRecursive = FolderGetAllChildrenRecursive FolderID
instance (MonadDB m, MonadThrow m)
  => DBQuery m FolderGetAllChildrenRecursive [FolderWithChildren] where
  dbQuery (FolderGetAllChildrenRecursive fid) = do
    runQuery_ . sqlSelect "folders" $ do
      mapM_ sqlResult folderSelectors
      sqlWhere $ "parent_path @> " <?> Array1 [fid]
    allChildren <- fetchMany fetchFolder
    let directChildren parentID =
          filter ((== Just parentID) . view #parentID) allChildren
        mkChildren parentID = mkChild <$> directChildren parentID
        mkChild folder = FolderWithChildren folder . mkChildren $ folder ^. #id
    return $ mkChildren fid

newtype FolderGetParents = FolderGetParents FolderID
instance (MonadDB m, MonadThrow m)
    => DBQuery m FolderGetParents [Folder] where
  dbQuery (FolderGetParents fid) = do
    (dbQuery . FolderGet $ fid) >>= \case
      Nothing -> return []
      Just _  -> do
        -- JOIN does not necessarily preserve order of rows, so we add
        -- ORDINALITY and ORDER BY it.
        --
        -- WITH ORDINALITY can only appear inside a FROM clause after
        -- a function call:
        --   <https://www.postgresql.org/docs/current/static/queries-table-expressions.html#QUERIES-TABLEFUNCTIONS>
        -- The LATERAL is optional for function calls:
        --   <https://www.postgresql.org/docs/current/static/queries-table-expressions.html#QUERIES-LATERAL>
        let lateralJoin =
              "folders as fdr cross join lateral"
                <+> "unnest(fdr.parent_path) with ordinality"
        runQuery_ . sqlSelect "folders" $ do
          sqlWith "parentids" . sqlSelect lateralJoin $ do
            sqlResult "unnest as id" -- `unnest` is default column name
            sqlResult "ordinality"   -- `ordinality` is default column name
            sqlWhereEq "fdr.id" fid
          sqlJoinOn "parentids" "parentids.id = folders.id"
          mapM_ sqlResult folderSelectors
          sqlOrderBy "ordinality"
        fetchMany fetchFolder

newtype FolderGetImmediateChildren = FolderGetImmediateChildren FolderID
instance (MonadDB m, MonadThrow m) => DBQuery m FolderGetImmediateChildren [Folder] where
  dbQuery (FolderGetImmediateChildren ugid) = do
    runQuery_ . sqlSelect "folders" $ do
      mapM_ sqlResult folderSelectors
      sqlWhereEq "parent_id" ugid
      sqlWhereIsNULL "deleted"
    fetchMany fetchFolder

newtype FolderDelete = FolderDelete FolderID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m FolderDelete () where
  dbUpdate (FolderDelete fdrid) = do
    now <- currentTime
    void . runQuery . sqlUpdate "folders" $ do
      sqlSet "deleted" now
      sqlWhereEq "id" $ Just fdrid

folderSelectors :: [SQL]
folderSelectors = ("folders." <>) <$> ["id", "parent_id", "name"]
