module UserGroup.FreeDocumentTokens.Model (
    UserGroupFreeDocumentTokensGet(..)
  , UserGroupFreeDocumentTokensUpdate(..)
  , UserGroupFreeDocumentTokensUseOneIfOnFreePlan(..)
  , module UserGroup.FreeDocumentTokens.Types
  ) where

import Control.Monad.Catch
import Data.Int
import Log

import DB
import UserGroup.FreeDocumentTokens.Types
import UserGroup.Types
import UserGroup.Types.PaymentPlan

fetchFreeDocumentTokens :: (Int32, UTCTime) -> FreeDocumentTokens
fetchFreeDocumentTokens (count, validity) = freeDocumentTokensFromValues count validity

userGroupFreeDocumentTokensSelectors :: [SQL]
userGroupFreeDocumentTokensSelectors =
  [ "user_group_free_document_tokens.tokens_count"
  , "user_group_free_document_tokens.tokens_validity"
  ]

data UserGroupFreeDocumentTokensUseOneIfOnFreePlan = UserGroupFreeDocumentTokensUseOneIfOnFreePlan UserGroupID
instance (MonadDB m, MonadThrow m) => DBUpdate m UserGroupFreeDocumentTokensUseOneIfOnFreePlan () where
  update (UserGroupFreeDocumentTokensUseOneIfOnFreePlan ugid) = do
    runQuery_ . sqlUpdate "user_group_free_document_tokens" $ do
      sqlSetCmd "tokens_count" "tokens_count - 1"
      sqlWhereEq "user_group_id" ugid
      sqlWhereExists . sqlSelect "user_group_invoicings" $ do
        sqlResult "TRUE"
        sqlWhereEq "user_group_id" $ ugid
        sqlWhereEq "payment_plan" $ FreePlan

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
