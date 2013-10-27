module ActionQueue.Migrations where

import ActionQueue.Tables
import DB

removeDuplicateIndexFromAccessNewAccounts :: MonadDB m => Migration m
removeDuplicateIndexFromAccessNewAccounts = Migration {
  mgrTable = tableAccessNewAccounts
, mgrFrom = 1
, mgrDo = do
  let Table{..} = tableAccessNewAccounts
  kRun_ . sqlDropIndex tblName $ indexOnColumn "user_id"
}

removeDuplicateIndexFromPasswordReminders :: MonadDB m => Migration m
removeDuplicateIndexFromPasswordReminders = Migration {
  mgrTable = tablePasswordReminders
, mgrFrom = 1
, mgrDo = do
  let Table{..} = tablePasswordReminders
  kRun_ . sqlDropIndex tblName $ indexOnColumn "user_id"
}

removeDuplicateIndexFromEmailChangeRequests :: MonadDB m => Migration m
removeDuplicateIndexFromEmailChangeRequests = Migration {
  mgrTable = tableEmailChangeRequests
, mgrFrom = 1
, mgrDo = do
  let Table{..} = tableEmailChangeRequests
  kRun_ . sqlDropIndex tblName $ indexOnColumn "user_id"
}

removeDuplicateIndexFromUserAccountRequests :: MonadDB m => Migration m
removeDuplicateIndexFromUserAccountRequests = Migration {
  mgrTable = tableUserAccountRequests
, mgrFrom = 1
, mgrDo = do
  let Table{..} = tableUserAccountRequests
  kRun_ . sqlDropIndex tblName $ indexOnColumn "user_id"
}
