module UserGroup.FreeDocumentTokens.Model (
    UserGroupFreeDocumentTokensGet(..)
  , UserGroupFreeDocumentTokensUpdate(..)
  , UserGroupFreeDocumentTokensUseOneIfIfPossible(..)
  , module UserGroup.FreeDocumentTokens.Types
  ) where

import Control.Monad.Catch
import Data.Int
import Log

import DB
import UserGroup.FreeDocumentTokens.Types
import UserGroup.Types

fetchFreeDocumentTokens :: (Int32, UTCTime) -> FreeDocumentTokens
fetchFreeDocumentTokens (count, validity) = freeDocumentTokensFromValues count validity

userGroupFreeDocumentTokensSelectors :: [SQL]
userGroupFreeDocumentTokensSelectors =
  [ "user_group_free_document_tokens.tokens_count"
  , "user_group_free_document_tokens.tokens_validity"
  ]

data UserGroupFreeDocumentTokensUseOneIfIfPossible = UserGroupFreeDocumentTokensUseOneIfIfPossible UserGroupID
instance (MonadDB m, MonadThrow m) => DBUpdate m UserGroupFreeDocumentTokensUseOneIfIfPossible Bool where
  update (UserGroupFreeDocumentTokensUseOneIfIfPossible ugid) = do
    count <- runQuery . sqlUpdate "user_group_free_document_tokens" $ do
      sqlSetCmd "tokens_count" "tokens_count - 1"
      sqlWhere "tokens_count > 0"
      sqlWhereEq "user_group_id" ugid
    return $ count == 1

data UserGroupFreeDocumentTokensUpdate = UserGroupFreeDocumentTokensUpdate UserGroupID FreeDocumentTokens
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m UserGroupFreeDocumentTokensUpdate () where
  update (UserGroupFreeDocumentTokensUpdate ugid fdts) = do
    runQuery_ . sqlDelete "user_group_free_document_tokens" $ do
      sqlWhereEq "user_group_id" ugid
    tc <- numberOfValidTokens fdts
    tv <- validityOfTokens fdts
    runQuery_ . sqlInsert "user_group_free_document_tokens" $ do
      sqlSet "tokens_count"    tc
      sqlSet "tokens_validity" tv
      sqlSet "user_group_id"   ugid

data UserGroupFreeDocumentTokensGet = UserGroupFreeDocumentTokensGet UserGroupID
instance (MonadDB m, MonadThrow m) => DBQuery m UserGroupFreeDocumentTokensGet FreeDocumentTokens where
  query (UserGroupFreeDocumentTokensGet ugid) = do
    mfdt <- query $ UserGroupFreeDocumentTokensGetInternal ugid
    case mfdt of
      Just fdt -> return fdt
      Nothing  -> return noFreeDocumentTokens

data UserGroupFreeDocumentTokensGetInternal = UserGroupFreeDocumentTokensGetInternal UserGroupID
instance (MonadDB m, MonadThrow m) => DBQuery m UserGroupFreeDocumentTokensGetInternal (Maybe FreeDocumentTokens) where
  query (UserGroupFreeDocumentTokensGetInternal ugid) = do
    runQuery_ . sqlSelect "user_group_free_document_tokens" $ do
      mapM_ sqlResult userGroupFreeDocumentTokensSelectors
      sqlWhereEq "user_group_id" ugid
    fetchMaybe fetchFreeDocumentTokens
