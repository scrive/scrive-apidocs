module User.Model.Query (
    GetUserGroupAccountsCountActive(..)
  , GetUserGroupAccountsCountMainDomainBranding(..)
  , GetUserGroupAccountsCountTotal(..)
  , GetUserGroupAdmins(..)
  , GetUsageStats(..)
  , GetUserByID(..)
  , GetUserByIDIncludeDeleted(..)
  , GetUserByEmail(..)
  , GetUserWherePasswordAlgorithmIsEarlierThan(..)
  , GetUsersWithUserGroupNames(..)
  , IsUserDeletable(..)
  , UserGroupGetAllUsersFromThisAndSubgroups(..)
  , UserGroupGetUsers(..)
  , UserGroupGetUsersIncludeDeleted(..)
  , UserNotDeletableReason(..)
  , userNotDeletableReasonToString
  ) where

import Control.Monad.Catch
import Control.Monad.Except (lift, runExceptT, throwError)
import Control.Monad.State (MonadState)
import Data.Char
import Data.Int
import Data.String (IsString(..))
import Text.JSON
import Text.JSON.ToJSValue
import qualified Data.Text as T

import Chargeable.Model
import DB
import Doc.DocStateData (DocumentStatus(..))
import MinutesTime
import User.Data.Stats
import User.Data.User
import User.Email
import User.History.Model
import User.Model.Filter
import User.Model.OrderBy
import User.Password
import User.UserID
import UserGroup.Data (UserGroupID)
import UserGroup.Data.PaymentPlan

data GetUserWherePasswordAlgorithmIsEarlierThan =
  GetUserWherePasswordAlgorithmIsEarlierThan PasswordAlgorithm

instance (MonadDB m, MonadThrow m) =>
  DBQuery m GetUserWherePasswordAlgorithmIsEarlierThan (Maybe User) where
  query (GetUserWherePasswordAlgorithmIsEarlierThan strength) = do
    runQuery_ $ selectUsersSQL
      -- We include deleted users, we want to strengthen those too if
      -- they have a password
      <+> "WHERE password IS NOT NULL"
      <+> "AND (password_algorithm IS NULL OR password_algorithm < "
      <?> pwdAlgorithmToInt16 strength <+> ")"
      <+> "LIMIT 1"
    fetchMaybe fetchUser

data GetUserByID = GetUserByID UserID
instance (MonadDB m, MonadThrow m) => DBQuery m GetUserByID (Maybe User) where
  query (GetUserByID uid) = do
    runQuery_ $ selectUsersSQL <+> "WHERE id =" <?> uid <+> "AND deleted IS NULL"
    fetchMaybe fetchUser

data GetUserByIDIncludeDeleted = GetUserByIDIncludeDeleted UserID
instance (MonadDB m, MonadThrow m) => DBQuery m GetUserByIDIncludeDeleted (Maybe User) where
  query (GetUserByIDIncludeDeleted uid) = do
    runQuery_ $ selectUsersSQL <+> "WHERE id =" <?> uid
    fetchMaybe fetchUser

data GetUserByEmail = GetUserByEmail Email
instance (MonadDB m, MonadThrow m) => DBQuery m GetUserByEmail (Maybe User) where
  query (GetUserByEmail email) = do
    runQuery_ $ selectUsersSQL <+> "WHERE deleted IS NULL AND email =" <?> map toLower (unEmail email)
    fetchMaybe fetchUser

data UserGroupGetUsers = UserGroupGetUsers UserGroupID
instance MonadDB m => DBQuery m UserGroupGetUsers [User] where
  query (UserGroupGetUsers ugid) = do
    runQuery_ $ selectUsersSQL <+> "WHERE user_group_id =" <?> ugid <+> "AND deleted IS NULL ORDER BY email"
    fetchMany fetchUser

data UserGroupGetUsersIncludeDeleted = UserGroupGetUsersIncludeDeleted UserGroupID
instance MonadDB m => DBQuery m UserGroupGetUsersIncludeDeleted [User] where
  query (UserGroupGetUsersIncludeDeleted ugid) = do
    runQuery_ $ selectUsersSQL <+> "WHERE User_group_id =" <?> ugid <+> "ORDER BY email"
    fetchMany fetchUser

data GetUserGroupAccountsCountTotal = GetUserGroupAccountsCountTotal
instance (MonadDB m, MonadThrow m) => DBQuery m GetUserGroupAccountsCountTotal [(UserGroupID, Int64)] where
  query GetUserGroupAccountsCountTotal = do
    runQuery_ $ sqlSelect "users u" $ do
      sqlJoinOn "user_group_invoicings uginv" "u.user_group_id = uginv.user_group_id"
      sqlResult "u.user_group_id"
      sqlResult "count(u.id)"
      sqlWhereIsNotScriveEmail "u.email"
      sqlWhereIsNULL "u.deleted"
      sqlWhereNotEq "uginv.payment_plan" FreePlan
      sqlGroupBy "u.user_group_id"
    fetchMany id

data GetUserGroupAccountsCountMainDomainBranding = GetUserGroupAccountsCountMainDomainBranding
instance (MonadDB m, MonadThrow m) => DBQuery m GetUserGroupAccountsCountMainDomainBranding [(UserGroupID, Int64)] where
  query GetUserGroupAccountsCountMainDomainBranding = do
    runQuery_ $ sqlSelect "users u" $ do
      sqlJoinOn "user_group_invoicings uginv" "u.user_group_id = uginv.user_group_id"
      sqlResult "u.user_group_id"
      sqlResult "count(u.id)"
      sqlWhereIsNotScriveEmail "u.email"
      sqlWhereIsNULL "u.deleted"
      sqlWhereNotEq "uginv.payment_plan" FreePlan
      sqlWhere ("u.associated_domain_id = (SELECT b.id FROM branded_domains b " <>
                "WHERE b.main_domain=TRUE LIMIT 1)")
      sqlGroupBy "u.user_group_id"
    fetchMany id

data GetUserGroupAccountsCountActive = GetUserGroupAccountsCountActive
instance (MonadDB m, MonadThrow m, MonadTime m) => DBQuery m GetUserGroupAccountsCountActive [(UserGroupID, Int64)] where
  query GetUserGroupAccountsCountActive = do
    now <- currentTime
    runQuery_ $ activeUsersQuery now
    fetchMany id

      where

        activeUsersQuery :: UTCTime -> SQL
        activeUsersQuery now = ("SELECT user_group_id, count(user_id) FROM") <+>
                           (
                            "(" <+>
                            loggedInRecently now <+>
                            "UNION" <+>
                            docSentRecently now <+>
                            ")"
                           ) <+>
                           "as active_users GROUP BY user_group_id;"

        loggedInRecently :: UTCTime -> SQL
        loggedInRecently now = toSQLCommand $
          sqlSelect "users u" $ do
            sqlJoinOn "users_history h" "u.id = h.user_id"
            sqlJoinOn "user_groups ug" "ug.id = u.user_group_id"
            sqlResult "u.user_group_id AS user_group_id"
            sqlResult "u.id AS user_id"
            sqlWhereEq "h.event_type" UserLoginSuccess
            sqlWhereNotEq "c.payment_plan" FreePlan
            sqlWhereIsNotScriveEmail "u.email"
            sqlGroupBy "u.id"
            sqlGroupBy "ug.id"
            sqlHaving $ "max(h.time) > (" <?> now <+> " - interval '4 weeks')"

        docSentRecently :: UTCTime -> SQL
        docSentRecently now = toSQLCommand $
          sqlSelect "chargeable_items i" $ do
            sqlJoinOn "user_groups ug" "ug.id = i.user_group_id"
            sqlJoinOn "users u" "u.id = i.user_id"
            sqlResult "i.user_group_id AS user_group_id"
            sqlResult "i.user_id AS user_id"
            sqlWhereEq "i.\"type\"" CIStartingDocument
            sqlWhereNotEq "ug.payment_plan" FreePlan
            sqlWhereIsNotScriveEmail "u.email"
            sqlGroupBy "i.user_id"
            sqlGroupBy "i.user_group_id"
            sqlHaving $ "max(i.time) > (" <?> now <+> " - interval '4 weeks')"

data GetUserGroupAdmins = GetUserGroupAdmins UserGroupID
instance MonadDB m => DBQuery m GetUserGroupAdmins [User] where
  query (GetUserGroupAdmins ugid) = do
    runQuery_ $ selectUsersSQL <+> "WHERE is_company_admin AND user_group_id =" <?> ugid <+> "AND deleted IS NULL ORDER BY email"
    fetchMany fetchUser

data UserNotDeletableReason
  = UserNotDeletableDueToPendingDocuments
  | UserNotDeletableDueToLastAdminWithUsers
  | UserNotDeletableDueToPendingUserInvitations
  deriving Show

instance ToJSValue UserNotDeletableReason where
  toJSValue reason =
    let code = case reason of
          UserNotDeletableDueToPendingDocuments       -> "pending_documents"
          UserNotDeletableDueToLastAdminWithUsers     -> "last_admin_with_users"
          UserNotDeletableDueToPendingUserInvitations -> "pending_user_invitations"
        msg = userNotDeletableReasonToString reason
    in JSObject $ toJSObject
         [ ("code",    JSString $ toJSString code)
         , ("message", JSString $ toJSString msg)
         ]

userNotDeletableReasonToString :: IsString s => UserNotDeletableReason -> s
userNotDeletableReasonToString = fromString . \case
  UserNotDeletableDueToPendingDocuments ->
    "Can't delete a user with pending documents."
  UserNotDeletableDueToLastAdminWithUsers ->
    "Can't delete a user if it would leave the company without an admin."
  UserNotDeletableDueToPendingUserInvitations ->
    "Can't delete last admin user of a company with pending user invitations."

-- | Check if a user can be deleted giving the reason if it can't and returning
-- whether it is the last company user otherwise.
data IsUserDeletable = IsUserDeletable User
instance (MonadDB m, MonadThrow m)
    => DBQuery m IsUserDeletable (Either UserNotDeletableReason Bool) where
  query (IsUserDeletable user) = runExceptT $ do
    accounts <- lift $
      dbQuery $ UserGroupGetAllUsersFromThisAndSubgroups $ usergroupid user
    let (activeAccounts, userInvitations) =
          partition (isJust . userhasacceptedtermsofservice) accounts

    let lastAdmin = case filter useriscompanyadmin activeAccounts of
          [admin] -> userid user == userid admin
          _ -> False
        lastUser = case activeAccounts of
          [_] -> True
          _ -> False

    when (lastAdmin && not (null userInvitations)) $
      throwError UserNotDeletableDueToPendingUserInvitations

    when (lastAdmin && not lastUser) $
      throwError UserNotDeletableDueToLastAdminWithUsers

    n <- lift $ runQuery $ sqlSelect "users" $ do
      sqlWhere "users.deleted IS NULL"
      sqlWhereEq "users.id" $ userid user
      sqlJoinOn "signatory_links" "users.id = signatory_links.user_id"
      sqlWhere "signatory_links.deleted IS NULL"
      sqlJoinOn "documents" "signatory_links.id = documents.author_id"
      sqlWhereEq "documents.status" Pending
      sqlResult "documents.id"
      sqlLimit 1
    when (n /= 0) $ throwError UserNotDeletableDueToPendingDocuments

    return lastUser

{-
  @note: There's some shared functionality between `GetUsageStatsOld` and
  `GetUsageStatsNew` that could be factored out, but the old version _shall_
  disappear within short (December 2017), so I see no point in doing this.
-}

data GetUsageStats = GetUsageStatsOld (Either UserID UserGroupID) StatsPartition Interval
                   | GetUsageStatsNew (Either UserID UserGroupID) StatsPartition Interval
instance (MonadDB m, MonadTime m) => DBQuery m GetUsageStats [UserUsageStats] where
  query (GetUsageStatsOld eid statsPartition interval) = do
    now <- currentTime
    -- Fetches relevant documents and then groups them by the
    -- timestamps (trimmed to the precision we want) and users to
    -- achieve desired partitioning. It is also worth noting that it
    -- doesn't return time windows where all numbers would equal 0.
    runQuery_ . sqlSelect "docs_sent FULL JOIN docs_closed USING (time_window, uid) FULL JOIN sigs_closed USING (time_window, uid) JOIN users u ON (uid = u.id)" $ do
      -- Use intermediate CTE to fetch all the relevant documents up
      -- front as the majority of them will be both sent and
      -- closed. We also save time by traversing the table only once.
      sqlWith "stats_data" $ selectStatsData now
      -- Get the number of sent documents per time window / user.
      sqlWith "docs_sent" . sqlSelect "stats_data" $ do
        sqlResult "sent_time_window AS time_window"
        sqlResult "uid"
        sqlResult "COUNT(*) AS docs_sent"
        sqlWhere "document_sent"
        sqlGroupBy "time_window"
        sqlGroupBy "uid"
      -- Get the number of closed documents per time window / user.
      sqlWith "docs_closed" . sqlSelect "stats_data" $ do
        sqlResult "closed_time_window AS time_window"
        sqlResult "uid"
        sqlResult "COUNT(*) AS docs_closed"
        sqlWhere "document_closed"
        sqlGroupBy "time_window"
        sqlGroupBy "uid"
      -- Get the number of closed signatures per time window / user.
      sqlWith "sigs_closed" . sqlSelect "stats_data sd" $ do
        sqlJoinOn "signatory_links sl" "sd.did = sl.document_id"
        sqlResult "sd.closed_time_window AS time_window"
        sqlResult "sd.uid"
        sqlResult "COUNT(*) AS sigs_closed"
        sqlWhere "document_closed"
        sqlWhereIsNotNULL "sl.sign_time"
        sqlGroupBy "time_window"
        sqlGroupBy "sd.uid"
      -- Fetch joined data and sort it appropriately.
      sqlResult "time_window"
      sqlResult "u.email"
      sqlResult "u.first_name || ' ' || u.last_name AS name"
      sqlResult "COALESCE(docs_sent, 0) AS docs_sent"
      sqlResult "COALESCE(docs_closed, 0) AS docs_closed"
      sqlResult "COALESCE(sigs_closed, 0) AS sigs_closed"
      sqlOrderBy "time_window DESC"
      sqlOrderBy "docs_sent DESC"
      sqlOrderBy "docs_closed DESC"
      sqlOrderBy "sigs_closed DESC"
      sqlOrderBy "u.email"
    fetchMany fetchUserUsageStats
    where
      maxSignTime :: SQL
      maxSignTime = parenthesize . toSQLCommand . sqlSelect "signatory_links osl" $ do
        sqlResult "max(osl.sign_time)"
        sqlWhere "osl.document_id = d.id"
        sqlWhere "osl.is_partner"

      selectStatsData :: UTCTime -> SqlSelect
      selectStatsData now = sqlSelect "documents d" $ do
        sqlJoinOn "users u" "d.author_user_id = u.id"
        sqlResult $ dateTrunc "d.invite_time" <+> "AS sent_time_window"
        sqlResult $ dateTrunc maxSignTime     <+> "AS closed_time_window"
        sqlResult "d.id AS did"
        sqlResult "u.id AS uid"
        sqlResult $ documentSent   <+> "AS document_sent"
        sqlResult $ documentClosed <+> "AS document_closed"
        sqlWhere $ documentSent `sqlOR` documentClosed
        case eid of
          Left  uid  -> sqlWhereEq "u.id" uid
          Right ugid -> sqlWhereEq "u.user_group_id" ugid
        where
          documentSent = dateTrunc "d.invite_time" <+> ">=" <+> startingDate

          documentClosed = sqlConcatAND [
              "d.status =" <?> Closed
            , dateTrunc maxSignTime <+> ">=" <+> startingDate
            ]

          startingDate = dateTrunc (sqlParam now <+> "-" <?> interval)

      dateTrunc :: SQL -> SQL
      dateTrunc time = "date_trunc('" <> granularity <> "', " <> time <> ")"
        where
          granularity = case statsPartition of
            PartitionByDay   -> "day"
            PartitionByMonth -> "month"

      fetchUserUsageStats :: (UTCTime, String, String, Int64, Int64, Int64) -> UserUsageStats
      fetchUserUsageStats (time_window_start, user_email, user_name, docs_sent, docs_closed, sigs_closed) = UserUsageStats {
          uusTimeWindowStart = time_window_start
        , uusUserEmail       = user_email
        , uusUserName        = user_name
        , uusDocumentStats   = DocumentStats {
            dsDocumentsSent    = docs_sent
          , dsDocumentsClosed  = docs_closed
          , dsSignaturesClosed = sigs_closed
          }
        }

  -- @note: use new way: chargeable_items
  query (GetUsageStatsNew eid statsPartition interval) = do
    now <- currentTime
    -- Fetches relevant documents and then groups them by the
    -- timestamps (trimmed to the precision we want) and users to
    -- achieve desired partitioning. It is also worth noting that it
    -- doesn't return time windows where all numbers would equal 0.
    runQuery_ . sqlSelect ("docs_sent FULL JOIN" <+>
                           "docs_closed USING (time_window, uid) FULL JOIN" <+>
                           "sigs_closed USING (time_window, uid) JOIN" <+>
                           "users u ON (uid = u.id)") $ do
      -- define the CTEs for the appropriate quantities
      forM_ [ ("docs_sent",   CIStartingDocument)
            , ("docs_closed", CIClosingDocument)
            , ("sigs_closed", CIClosingSignature)] $ \(qName, chItem) -> do
            sqlWith qName (sqlSelectChargeableItem now qName chItem)
      -- Fetch joined data and sort it appropriately.
      sqlResult "time_window"
      sqlResult "u.email"
      sqlResult "u.first_name || ' ' || u.last_name AS name"
      sqlResult "COALESCE(docs_sent, 0) AS docs_sent"
      sqlResult "COALESCE(docs_closed, 0) AS docs_closed"
      sqlResult "COALESCE(sigs_closed, 0) AS sigs_closed"
      sqlOrderBy "time_window DESC"
      sqlOrderBy "docs_sent DESC"
      sqlOrderBy "docs_closed DESC"
      sqlOrderBy "sigs_closed DESC"
      sqlOrderBy "u.email"
    fetchMany fetchUserUsageStats

    where

      sqlSelectChargeableItem :: UTCTime -> SQL -> ChargeableItem -> SqlSelect
      sqlSelectChargeableItem now quantityName chItem =
        sqlSelect "chargeable_items chi" $ do
          sqlResult $ dateTrunc "chi.time" <+> "AS time_window"
          sqlResult "chi.user_id AS uid"
          sqlResult $ "sum(chi.quantity) AS" <+> quantityName
          sqlWhereEq "chi.type" chItem
          sqlWhere $ "chi.time" <+> ">=" <+> startingDate now
          sqlGroupBy "time_window"
          sqlGroupBy "chi.user_id"
          case eid of
            Left  uid  -> sqlWhereEq "chi.user_id" uid
            Right ugid -> sqlWhereEq "chi.user_group_id" ugid

      startingDate :: UTCTime -> SQL
      startingDate now = dateTrunc (sqlParam now <+> "-" <?> interval)

      dateTrunc :: SQL -> SQL
      dateTrunc time = "date_trunc('" <> granularity <> "', " <> time <> ")"

      granularity :: SQL
      granularity = case statsPartition of
        PartitionByDay   -> "day"
        PartitionByMonth -> "month"

      fetchUserUsageStats :: (UTCTime, String, String, Int64, Int64, Int64) -> UserUsageStats
      fetchUserUsageStats (time_window_start, user_email, user_name, docs_sent, docs_closed, sigs_closed) = UserUsageStats {
          uusTimeWindowStart = time_window_start
        , uusUserEmail       = user_email
        , uusUserName        = user_name
        , uusDocumentStats   = DocumentStats {
            dsDocumentsSent    = docs_sent
          , dsDocumentsClosed  = docs_closed
          , dsSignaturesClosed = sigs_closed
          }
      }

data GetUsersWithUserGroupNames = GetUsersWithUserGroupNames [UserFilter] [AscDesc UserOrderBy] (Int, Int)
instance MonadDB m => DBQuery m GetUsersWithUserGroupNames [(User, T.Text)] where
  query (GetUsersWithUserGroupNames filters sorting (offset, limit)) = do
    runQuery_ $ smconcat [
        selectUsersWithUserGroupNamesSQL
      , if null filters
          then mempty
          else "AND" <+> sqlConcatAND (map userFilterToSQL filters)
      , if null sorting
          then mempty
          else "ORDER BY" <+> sqlConcatComma (map userOrderByAscDescToSQL sorting)
      , " OFFSET" <?> (fromIntegral offset :: Int32) <+> "LIMIT" <?> (fromIntegral limit :: Int32)
      ]
    fetchMany fetchUserWithUserGroupName

data UserGroupGetAllUsersFromThisAndSubgroups = UserGroupGetAllUsersFromThisAndSubgroups UserGroupID
instance (MonadDB m, MonadThrow m) => DBQuery m UserGroupGetAllUsersFromThisAndSubgroups [User] where
  query (UserGroupGetAllUsersFromThisAndSubgroups ugid) = do
    runQuery_ . sqlSelect "users" $ do
      mapM_ sqlResult selectUsersSelectorsList
      sqlWhereAny [
          sqlWhereInSql "user_group_id" $ sqlSelect "user_groups" $ do
            sqlResult "id"
            sqlWhere $ "parent_group_path @> " <?> (Array1 [ugid])
        , sqlWhereEq "user_group_id" ugid
        ]
      sqlWhereIsNULL "deleted"
    fetchMany fetchUser

-- helpers
sqlWhereIsNotScriveEmail :: (MonadState v m, SqlWhere v) => SQL -> m ()
sqlWhereIsNotScriveEmail field = do
  sqlWhere $ field <+> "NOT LIKE '%@scrive.%'"
  sqlWhere $ field <+> "NOT LIKE '%@skrivapa.%'"
