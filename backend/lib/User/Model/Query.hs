module User.Model.Query (
    GetCompanyAccounts(..)
  , GetCompanyAccountsCountActive(..)
  , GetCompanyAccountsCountMainDomainBranding(..)
  , GetCompanyAccountsCountTotal(..)
  , GetCompanyAdmins(..)
  , GetUsageStats(..)
  , GetUserByID(..)
  , GetUserByIDIncludeDeleted(..)
  , GetUserByEmail(..)
  , GetUsers(..)
  , GetUserWherePasswordAlgorithmIsEarlierThan(..)
  , GetUsersWithCompanies(..)
  , IsUserDeletable(..)
  ) where

import Control.Monad.Catch
import Control.Monad.State (MonadState)
import Data.Char
import Data.Int

import Chargeable.Model
import Company.Model
import DB
import Doc.DocStateData (DocumentStatus(..))
import KontraPrelude
import MinutesTime
import User.Data.Stats
import User.Data.User
import User.Email
import User.History.Model
import User.Model.Filter
import User.Model.OrderBy
import User.Password
import User.UserID

data GetUsers = GetUsers
instance MonadDB m => DBQuery m GetUsers [User] where
  query GetUsers = do
    runQuery_ $ selectUsersSQL <+> "WHERE deleted IS NULL ORDER BY first_name || ' ' || last_name"
    fetchMany fetchUser

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

data GetCompanyAccounts = GetCompanyAccounts CompanyID
instance MonadDB m => DBQuery m GetCompanyAccounts [User] where
  query (GetCompanyAccounts cid) = do
    runQuery_ $ selectUsersSQL <+> "WHERE company_id =" <?> cid <+> "AND deleted IS NULL ORDER BY email"
    fetchMany fetchUser

data GetCompanyAccountsCountTotal = GetCompanyAccountsCountTotal
instance (MonadDB m, MonadThrow m) => DBQuery m GetCompanyAccountsCountTotal [(CompanyID, Int64)] where
  query GetCompanyAccountsCountTotal = do
    runQuery_ $ sqlSelect "users u" $ do
      sqlJoinOn "companies c" "u.company_id = c.id"
      sqlResult "u.company_id"
      sqlResult "count(u.id)"
      sqlWhereIsNotScriveEmail "u.email"
      sqlWhereIsNULL "u.deleted"
      sqlWhereNotEq "c.payment_plan" FreePlan
      sqlGroupBy "u.company_id"
    fetchMany id

data GetCompanyAccountsCountMainDomainBranding = GetCompanyAccountsCountMainDomainBranding
instance (MonadDB m, MonadThrow m) => DBQuery m GetCompanyAccountsCountMainDomainBranding [(CompanyID, Int64)] where
  query GetCompanyAccountsCountMainDomainBranding = do
    runQuery_ $ sqlSelect "users u" $ do
      sqlJoinOn "companies c" "u.company_id = c.id"
      sqlResult "u.company_id"
      sqlResult "count(u.id)"
      sqlWhereIsNotScriveEmail "u.email"
      sqlWhereIsNULL "u.deleted"
      sqlWhereNotEq "c.payment_plan" FreePlan
      sqlWhere ("u.associated_domain_id = (SELECT b.id FROM branded_domains b " <>
                "WHERE b.main_domain=TRUE LIMIT 1)")
      sqlGroupBy "u.company_id"
    fetchMany id

data GetCompanyAccountsCountActive = GetCompanyAccountsCountActive
instance (MonadDB m, MonadThrow m, MonadTime m) => DBQuery m GetCompanyAccountsCountActive [(CompanyID, Int64)] where
  query GetCompanyAccountsCountActive = do
    now <- currentTime
    runQuery_ $ activeUsersQuery now
    fetchMany id

      where

        activeUsersQuery :: UTCTime -> SQL
        activeUsersQuery now = ("SELECT company_id, count(user_id) FROM") <+>
                           (
                            "(" <+>
                            loggedInRecently now <+>
                            "UNION" <+>
                            docSentRecently now <+>
                            ")"
                           ) <+>
                           "as active_users GROUP BY company_id;"

        loggedInRecently :: UTCTime -> SQL
        loggedInRecently now = toSQLCommand $
          sqlSelect "users u" $ do
            sqlJoinOn "users_history h" "u.id = h.user_id"
            sqlJoinOn "companies c" "c.id = u.company_id"
            sqlResult "u.company_id AS company_id"
            sqlResult "u.id AS user_id"
            sqlWhereEq "h.event_type" UserLoginSuccess
            sqlWhereNotEq "c.payment_plan" FreePlan
            sqlWhereIsNotScriveEmail "u.email"
            sqlGroupBy "u.id"
            sqlGroupBy "c.id"
            sqlHaving $ "max(h.time) > (" <?> now <+> " - interval '4 weeks')"

        docSentRecently :: UTCTime -> SQL
        docSentRecently now = toSQLCommand $
          sqlSelect "chargeable_items i" $ do
            sqlJoinOn "companies c" "c.id = i.company_id"
            sqlJoinOn "users u" "u.id = i.user_id"
            sqlResult "i.company_id AS company_id"
            sqlResult "i.user_id AS user_id"
            sqlWhereEq "i.\"type\"" CIStartingDocument
            sqlWhereNotEq "c.payment_plan" FreePlan
            sqlWhereIsNotScriveEmail "u.email"
            sqlGroupBy "i.user_id"
            sqlGroupBy "i.company_id"
            sqlHaving $ "max(i.time) > (" <?> now <+> " - interval '4 weeks')"

data GetCompanyAdmins = GetCompanyAdmins CompanyID
instance MonadDB m => DBQuery m GetCompanyAdmins [User] where
  query (GetCompanyAdmins cid) = do
    runQuery_ $ selectUsersSQL <+> "WHERE is_company_admin AND company_id =" <?> cid <+> "AND deleted IS NULL ORDER BY email"
    fetchMany fetchUser

data IsUserDeletable = IsUserDeletable UserID
instance MonadDB m => DBQuery m IsUserDeletable Bool where
  query (IsUserDeletable uid) = do
    n <- runQuery $ sqlSelect "users" $ do
      sqlWhere "users.deleted IS NULL"
      sqlWhereEq "users.id" uid
      sqlJoinOn "signatory_links" "users.id = signatory_links.user_id"
      sqlWhere "signatory_links.deleted IS NULL"
      sqlJoinOn "documents" "signatory_links.id = documents.author_id"
      sqlWhereEq "documents.status" Pending
      sqlResult "documents.id"
      sqlLimit 1
    return (n == 0)

{-
  @note: There's some shared functionality between `GetUsageStatsOld` and
  `GetUsageStatsNew` that could be factored out, but the old version _shall_
  disappear within short (December 2017), so I see no point in doing this.
-}

data GetUsageStats = GetUsageStatsOld (Either UserID CompanyID) StatsPartition Interval
                   | GetUsageStatsNew (Either UserID CompanyID) StatsPartition Interval
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
        sqlJoinOn "signatory_links sl" "d.id = sl.document_id"
        sqlJoinOn "users u" "sl.user_id = u.id"
        sqlResult $ dateTrunc "d.invite_time" <+> "AS sent_time_window"
        sqlResult $ dateTrunc maxSignTime     <+> "AS closed_time_window"
        sqlResult "d.id AS did"
        sqlResult "u.id AS uid"
        sqlResult $ documentSent   <+> "AS document_sent"
        sqlResult $ documentClosed <+> "AS document_closed"
        sqlWhere "d.author_id = sl.id"
        sqlWhere $ documentSent `sqlOR` documentClosed
        case eid of
          Left  uid -> sqlWhereEq "u.id" uid
          Right cid -> sqlWhereEq "u.company_id" cid
        where
          documentSent = dateTrunc "d.invite_time" <+> ">=" <+> startingDate

          documentClosed = sqlConcatAND [
              "d.status =" <?> Closed
            , dateTrunc maxSignTime <+> ">=" <+> startingDate
            ]

          startingDate = dateTrunc (sqlParam now <+> " -" <?> interval)

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
            Left  uid -> sqlWhereEq "chi.user_id" uid
            Right cid -> sqlWhereEq "chi.company_id" cid

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

data GetUsersWithCompanies = GetUsersWithCompanies [UserFilter] [AscDesc UserOrderBy] (Int, Int)
instance MonadDB m => DBQuery m GetUsersWithCompanies [(User, Company)] where
  query (GetUsersWithCompanies filters sorting (offset, limit)) = do
    runQuery_ $ smconcat [
        selectUsersWithCompaniesSQL
      , if null filters
          then mempty
          else "AND" <+> sqlConcatAND (map userFilterToSQL filters)
      , if null sorting
          then mempty
          else "ORDER BY" <+> sqlConcatComma (map userOrderByAscDescToSQL sorting)
      , " OFFSET" <?> (fromIntegral offset :: Int32) <+> "LIMIT" <?> (fromIntegral limit :: Int32)
      ]
    fetchMany fetchUserWithCompany

-- helpers
sqlWhereIsNotScriveEmail :: (MonadState v m, SqlWhere v) => SQL -> m ()
sqlWhereIsNotScriveEmail field = do
  sqlWhere $ field <+> "NOT LIKE '%@scrive.%'"
  sqlWhere $ field <+> "NOT LIKE '%@skrivapa.%'"
