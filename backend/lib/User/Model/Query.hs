module User.Model.Query (
    GetUserGroupAccountsCountActive(..)
  , GetUserGroupAccountsCountTotal(..)
  , GetUserGroupAdmins(..)
  , GetUsageStats(..)
  , GetUsageStatsOnShareableLinks(..)
  , UsageStatsFor(..)
  , GetUserByID(..)
  , GetUserByIDIncludeDeleted(..)
  , GetUserByEmail(..)
  , GetUserByTempLoginToken(..)
  , GetUsers(..)
  , GetUsersWithUserGroupNames(..)
  , IsUserDeletable(..)
  , UserGroupGetAllUsersFromThisAndSubgroups(..)
  , UserGroupGetUsers(..)
  , UserGroupGetUsersIncludeDeleted(..)
  , UserNotDeletableReason(..)
  ) where

import Control.Monad.Catch
import Control.Monad.State (MonadState)
import Data.Int
import qualified Data.Text as T

import Chargeable.Model
import DB
import Doc.DocStateData (DocumentStatus(..))
import MagicHash
import MinutesTime
import User.Email
import User.History.Model
import User.Model.Filter
import User.Model.OrderBy
import User.Types.Stats
import User.Types.User
import User.UserID
import UserGroup.Types
import UserGroup.Types.PaymentPlan

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
    runQuery_ $ selectUsersSQL <+> "WHERE deleted IS NULL AND email =" <?> T.toLower (unEmail email)
    fetchMaybe fetchUser

data GetUserByTempLoginToken = GetUserByTempLoginToken UTCTime MagicHash
instance (MonadDB m, MonadThrow m) => DBQuery m GetUserByTempLoginToken (Maybe (User, Bool)) where
  query (GetUserByTempLoginToken now logintoken) = do
    runQuery_ $ sqlSelect "temporary_login_tokens" $ do
      sqlResult "user_id"
      sqlResult $ "expiration_time <=" <?> now
      sqlWhere $ "hash =" <?> logintoken
    mrow <- fetchMaybe id
    case mrow of
      Nothing -> return Nothing
      Just (uid, expired) -> do
        muser <- dbQuery $ GetUserByID uid
        case muser of
          Nothing -> return Nothing -- In theory, this state shouldn't occur
          Just user -> return $ Just (user, expired)

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
instance (MonadDB m, MonadThrow m)
  => DBQuery m GetUserGroupAccountsCountTotal [(UserGroupID, UserGroupID, Int64)] where
  query GetUserGroupAccountsCountTotal = do
    runQuery_ $ sqlSelect "users u" $ do
      sqlWithClosestInvoicingID "closest_invoicing_parent"

      sqlJoinOn "user_groups ug" "ug.id = u.user_group_id"
      sqlJoinOn "user_group_invoicings ugi" "ugi.user_group_id = u.user_group_id"
      sqlJoinOn "closest_invoicing_parent cip" "cip.user_group_id = u.user_group_id"

      sqlResult "cip.parent_user_group_id"
      sqlResult "ug.id"
      sqlResult "count(u.id)"

      sqlGroupBy "cip.parent_user_group_id"
      sqlGroupBy "ug.id"

      sqlWhereIsNotScriveEmail "u.email"
      sqlWhereIsNULL "u.deleted"
      sqlWhere . sqlConcatOR $ [isNotFree, hasParent]

    fetchMany id
      where hasParent = "ug.parent_group_id IS NOT NULL"
            isNotFree = "ugi.payment_plan <>" <?> FreePlan

data GetUserGroupAccountsCountActive = GetUserGroupAccountsCountActive
instance (MonadDB m, MonadThrow m, MonadTime m) => DBQuery m GetUserGroupAccountsCountActive [(UserGroupID, UserGroupID, Int64)] where
  query GetUserGroupAccountsCountActive = do
    now <- currentTime
    runQuery_ $ activeUsersQuery now
    fetchMany id
      where
        activeUsersQuery :: UTCTime -> SQL
        activeUsersQuery now =
          "SELECT parent_invoicing, user_group_id, count(user_id) FROM" <+>
          parenthesize (parenthesize (loggedInRecently now) <+>
                        "UNION" <+>
                        parenthesize (docSentRecently now)) <+>
          "AS active_users GROUP BY parent_invoicing, user_group_id;"

        -- This query would benefit by having more `work_mem` since it's merge
        -- sorting on disc with a mere 4MB; 8MB was not enough, but 20
        -- _was_ at the time of checking.
        loggedInRecently :: UTCTime -> SQL
        loggedInRecently now = toSQLCommand $
          sqlSelect "users u" $ do
            -- set up CTEs to get closest invoicing parent
            sqlWithClosestInvoicingID "closest_invoicing_parent"

            sqlJoinOn "user_groups ug" "u.user_group_id = ug.id"
            sqlJoinOn "user_group_invoicings ugi" "ugi.user_group_id = u.user_group_id"
            sqlJoinOn "closest_invoicing_parent cip" "cip.user_group_id = u.user_group_id"
            sqlJoinOn "users_history h" "u.id = h.user_id"

            sqlResult "cip.parent_user_group_id parent_invoicing"
            sqlResult "u.user_group_id AS user_group_id"
            sqlResult "u.id AS user_id"

            sqlWhereEq "h.event_type" UserLoginSuccess
            sqlWhere . sqlConcatOR $ [isNotFree, hasParent]
            sqlWhereIsNotScriveEmail "u.email"

            -- No need to group by "u.user_group_id" since we do that on the PK
            -- in the same table;
            -- cf. https://www.postgresql.org/docs/current/sql-select.html#SQL-GROUPBY
            sqlGroupBy "cip.parent_user_group_id"
            sqlGroupBy "u.id"

            sqlHaving $ "max(h.time) > (" <?> now <+> " - interval '4 weeks')"

        docSentRecently :: UTCTime -> SQL
        docSentRecently now = toSQLCommand $
          sqlSelect "users as u" $ do
            -- set up CTEs to get closest invoicing parent
            sqlWithClosestInvoicingID "closest_invoicing_parent"

            sqlJoinOn "chargeable_items as ci" "u.id = ci.user_id"
            sqlJoinOn "user_groups as ug" "ug.id = u.user_group_id"
            sqlJoinOn "user_group_invoicings as ugi" "ugi.user_group_id = ci.user_group_id"
            sqlJoinOn "closest_invoicing_parent as cip" "cip.user_group_id = u.user_group_id"

            sqlResult "cip.parent_user_group_id as parent_invoicing"
            sqlResult "u.user_group_id as user_group_id"
            sqlResult "u.id as user_id"

            sqlWhereEq "ci.\"type\"" CIStartingDocument
            sqlWhere . sqlConcatOR $ [isNotFree, hasParent]
            sqlWhereIsNotScriveEmail "u.email"

            -- No need to group by "u.user_group_id" since we do that on the PK
            -- in the same table; cf. above.
            sqlGroupBy "cip.parent_user_group_id "
            sqlGroupBy "u.id"

            sqlHaving $ "max(ci.time) > (" <?> now <+> " - interval '4 weeks')"

        hasParent :: SQL
        hasParent = "ug.parent_group_id IS NOT NULL"

        isNotFree :: SQL
        isNotFree = "ugi.payment_plan <>" <?> FreePlan

data GetUserGroupAdmins = GetUserGroupAdmins UserGroupID
instance MonadDB m => DBQuery m GetUserGroupAdmins [User] where
  query (GetUserGroupAdmins ugid) = do
    runQuery_ $ selectUsersSQL <+> "WHERE is_company_admin AND user_group_id =" <?> ugid <+> "AND deleted IS NULL ORDER BY email"
    fetchMany fetchUser

data UserNotDeletableReason
  = UserNotDeletableDueToPendingDocuments
  deriving Show

-- | Check if a user can be deleted giving the reason if it can't.
data IsUserDeletable = IsUserDeletable User
instance (MonadDB m, MonadThrow m)
    => DBQuery m IsUserDeletable (Maybe UserNotDeletableReason) where
  query (IsUserDeletable user) = do
    n <- runQuery $ sqlSelect "users" $ do
      sqlWhere "users.deleted IS NULL"
      sqlWhereEq "users.id" $ userid user
      sqlJoinOn "signatory_links" "users.id = signatory_links.user_id"
      sqlWhere "signatory_links.deleted IS NULL"
      sqlJoinOn "documents" "signatory_links.id = documents.author_id"
      sqlWhereEq "documents.status" Pending
      sqlResult "documents.id"
      sqlLimit 1
    if n == 0
      then return Nothing
      else return $ Just UserNotDeletableDueToPendingDocuments

data UsageStatsFor = UsageStatsForUser !UserID
                   | UsageStatsForUserGroup !UserGroupID

data GetUsageStats =
  GetUsageStats UsageStatsFor StatsPartition Interval

instance (MonadDB m, MonadTime m) => DBQuery m GetUsageStats [UserUsageStats] where
  query (GetUsageStats forWhom statsPartition interval) = do
    now <- currentTime
    -- Fetches relevant documents and then groups them by the
    -- timestamps (trimmed to the precision we want) and users to
    -- achieve desired partitioning. It is also worth noting that it
    -- doesn't return time windows where all numbers would equal 0.
    let statsRelation = case chargeableItemKeys of
          hd:tl ->
            foldl' (\acc (qName, _) ->
              acc <+> "FULL JOIN" <+> qName <+> "USING (time_window, uid)"
            ) (fst hd) tl
          [] -> unexpectedError "Empty chargeableItemKeys"
    runQuery_ . sqlSelect (statsRelation <+> "JOIN users u ON (uid = u.id)") $ do
      -- define the CTEs for the appropriate quantities
      forM_ chargeableItemKeys $ \(qName, chItem) -> do
        sqlWith qName (sqlSelectChargeableItem now qName chItem)
      -- Fetch joined data and sort it appropriately.
      sqlResult "time_window"
      sqlResult "u.email"
      sqlResult "u.first_name || ' ' || u.last_name AS name"
      forM_ chargeableItemKeys $ \(qName, _) ->
        sqlResult $ "COALESCE(" <> qName <> ", 0) AS" <+> qName
      sqlOrderBy "time_window DESC"
      forM_ chargeableItemKeys $ \(qName, _) ->
        sqlOrderBy $ qName <+> "DESC"
      sqlOrderBy "u.email"
    fetchMany fetchUserUsageStats

    where

      sqlSelectChargeableItem :: UTCTime -> SQL -> ChargeableItem -> SqlSelect
      sqlSelectChargeableItem now quantityName chItem =
        sqlSelect "chargeable_items chi" $ do
          sqlResult $ dateTrunc "chi.time" statsPartition <+> "AS time_window"
          sqlResult "chi.user_id AS uid"
          sqlResult $ "sum(chi.quantity) AS" <+> quantityName
          sqlWhereEq "chi.type" chItem
          sqlWhere $ "chi.time" <+> ">=" <+> startingDate now interval statsPartition
          sqlGroupBy "time_window"
          sqlGroupBy "chi.user_id"
          case forWhom of
            UsageStatsForUser      uid  -> sqlWhereEq "chi.user_id" uid
            UsageStatsForUserGroup ugid -> sqlWhereEq "chi.user_group_id" ugid

      fetchUserUsageStats
        :: ( UTCTime, String, String, Int64, Int64, Int64, Int64, Int64, Int64
           , Int64, Int64, Int64, Int64, Int64, Int64, Int64 ) -> UserUsageStats
      fetchUserUsageStats ( time_window_start, user_email
                          , user_name, docs_sent, docs_closed, sigs_closed
                          , sms_sent, sms_sent_via_telia, se_bankid_sigs
                          , se_bankid_auths, no_bankid_auths, nemid_auths
                          , no_bankid_sigs, nemid_sigs, tupas_auths
                          , shareable_links ) =
        UserUsageStats {
          uusTimeWindowStart = time_window_start
        , uusUserEmail       = user_email
        , uusUserName        = user_name
        , uusDocumentStats   = DocumentStats {
            dsDocumentsSent           = docs_sent
          , dsDocumentsClosed         = docs_closed
          , dsSignaturesClosed        = sigs_closed
          , dsSMSSent                 = sms_sent
          , dsSMSSentViaTelia         = sms_sent_via_telia
          , dsSEBankIDSignatures      = se_bankid_sigs
          , dsSEBankIDAuthentications = se_bankid_auths
          , dsNOBankIDSignatures      = no_bankid_sigs
          , dsNOBankIDAuthentications = no_bankid_auths
          , dsNemIDSignatures         = nemid_sigs
          , dsNemIDAuthentications    = nemid_auths
          , dsTupasAuthentications    = tupas_auths
          , dsShareableLinks          = shareable_links
          }
        }

chargeableItemKeys :: [(SQL, ChargeableItem)]
chargeableItemKeys =
  [ ("docs_sent", CIStartingDocument)
  , ("docs_closed", CIClosingDocument)
  , ("sigs_closed", CIClosingSignature)
  , ("sms_sent", CISMS)
  , ("sms_sent_via_telia", CISMSTelia)
  , ("se_bankid_sigs", CISEBankIDSignature)
  , ("se_bankid_auths", CISEBankIDAuthentication)
  , ("no_bankid_auths", CINOBankIDAuthentication)
  , ("nemid_auths", CIDKNemIDAuthentication)
  , ("no_bankid_sigs", CINOBankIDSignature)
  , ("nemid_sigs", CIDKNemIDSignature)
  , ("tupas_auths", CIFITupasAuthentication)
  , ("shareable_links", CIShareableLink)
  ]

data GetUsageStatsOnShareableLinks =
  GetUsageStatsOnShareableLinks UsageStatsFor StatsPartition Interval

instance (MonadDB m, MonadTime m) => DBQuery m GetUsageStatsOnShareableLinks [ShareableLinkUsageStats] where
  query (GetUsageStatsOnShareableLinks forWhom statsPartition interval) = do
    now <- currentTime
    -- Fetches relevant documents and then groups them by the
    -- timestamps (trimmed to the precision we want) and by template ids to
    -- achieve desired partitioning. It is also worth noting that it
    -- doesn't return time windows where all numbers would equal 0.
    let statsRelation = case chargeableItemKeys of
          hd:tl ->
            foldl' (\acc (qName, _) ->
              acc <+> "FULL JOIN" <+> qName <+> "USING (time_window, tid)"
            ) (fst hd) tl
          [] -> unexpectedError "Empty chargeableItemKeys"
    runQuery_ . sqlSelect (statsRelation <+> "JOIN documents d ON (tid = d.id)") $ do
      -- define the CTEs for the appropriate quantities
      forM_ chargeableItemKeys $ \(qName, chItem) -> do
        sqlWith qName (sqlSelectChargeableItem now qName chItem)
      -- Fetch joined data and sort it appropriately.
      sqlResult "time_window"
      sqlResult "tid"
      sqlResult "d.title"
      forM_ chargeableItemKeys $ \(qName, _) ->
        sqlResult $ "COALESCE(" <> qName <> ", 0) AS" <+> qName
      sqlOrderBy "time_window DESC"
      forM_ chargeableItemKeys $ \(qName, _) ->
        sqlOrderBy $ qName <+> "DESC"
      sqlOrderBy "tid"
    fetchMany fetchShareableLinkUsageStats

    where

      sqlSelectChargeableItem :: UTCTime -> SQL -> ChargeableItem -> SqlSelect
      sqlSelectChargeableItem now quantityName chItem =
        sqlSelect "chargeable_items chi JOIN documents d ON (chi.document_id = d.id)" $ do
          sqlResult $ dateTrunc "chi.time" statsPartition <+> "AS time_window"
          sqlResult "d.template_id AS tid"
          sqlResult $ "sum(chi.quantity) AS" <+> quantityName
          sqlWhereEq "chi.type" chItem
          sqlWhere $ "chi.time" <+> ">=" <+> startingDate now interval statsPartition
          sqlWhereEq "d.from_shareable_link" True
          sqlGroupBy "time_window"
          sqlGroupBy "d.template_id"
          case forWhom of
            UsageStatsForUser      uid  -> sqlWhereEq "chi.user_id" uid
            UsageStatsForUserGroup ugid -> sqlWhereEq "chi.user_group_id" ugid

      fetchShareableLinkUsageStats
        :: ( UTCTime, Int64, String, Int64, Int64, Int64, Int64, Int64, Int64
           , Int64, Int64, Int64, Int64, Int64, Int64, Int64 )
        -> ShareableLinkUsageStats
      fetchShareableLinkUsageStats ( time_window_start, template_id
                                   , template_title, docs_sent, docs_closed
                                   , sigs_closed, sms_sent, sms_sent_via_telia
                                   , se_bankid_sigs, se_bankid_auths, no_bankid_auths
                                   , nemid_auths, no_bankid_sigs, nemid_sigs
                                   , tupas_auths, shareable_links ) =
        ShareableLinkUsageStats {
          slusTimeWindowStart = time_window_start
        , slusTemplateId      = template_id
        , slusTemplateTitle   = template_title
        , slusDocumentStats   = DocumentStats {
            dsDocumentsSent           = docs_sent
          , dsDocumentsClosed         = docs_closed
          , dsSignaturesClosed        = sigs_closed
          , dsSMSSent                 = sms_sent
          , dsSMSSentViaTelia         = sms_sent_via_telia
          , dsSEBankIDSignatures      = se_bankid_sigs
          , dsSEBankIDAuthentications = se_bankid_auths
          , dsNOBankIDSignatures      = no_bankid_sigs
          , dsNOBankIDAuthentications = no_bankid_auths
          , dsNemIDSignatures         = nemid_sigs
          , dsNemIDAuthentications    = nemid_auths
          , dsTupasAuthentications    = tupas_auths
          , dsShareableLinks          = shareable_links
          }
        }

startingDate :: UTCTime -> Interval -> StatsPartition -> SQL
startingDate now interval statsPartition =
  dateTrunc (sqlParam now <+> "-" <?> interval) statsPartition

dateTrunc :: SQL -> StatsPartition -> SQL
dateTrunc time statsPartition = "date_trunc('" <> granularity <> "', " <> time <> ")"
  where
    granularity :: SQL
    granularity = case statsPartition of
      PartitionByDay   -> "day"
      PartitionByMonth -> "month"

data GetUsersWithUserGroupNames = GetUsersWithUserGroupNames [UserFilter] [AscDesc UserOrderBy] (Int, Int)
instance MonadDB m => DBQuery m GetUsersWithUserGroupNames [(User, Text)] where
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

data GetUsers = GetUsers [UserFilter]
instance MonadDB m => DBQuery m GetUsers [User] where
  query (GetUsers filters) = do
    runQuery_ . sqlSelect "users" $ do
      mapM_ sqlResult selectUsersSelectorsList
      sqlWhereIsNULL "deleted"
      sqlWhere $ sqlConcatAND (map userFilterToSQL filters)
    fetchMany fetchUser

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

-- | Get the user group id of a group and that of the closest parent that has
-- invoicing set. The SQL param is for convenient naming at point of use to keep
-- the names locally in the sources.
sqlWithClosestInvoicingID :: (MonadState SqlSelect m) => SQL -> m ()
sqlWithClosestInvoicingID publicName = do
  sqlWith "invoicing_parents" . sqlSelect ("user_groups ug," <+>
               "unnest(array_prepend(ug.id, ug.parent_group_path)) WITH ORDINALITY") $ do
    sqlJoinOn "user_group_invoicings ugi" "ugi.user_group_id = unnest"
    sqlResult "ug.id user_group_id"
    sqlResult "unnest parent_user_group_id"
    sqlResult "ordinality parent_order"
    sqlWhereEq "ugi.invoicing_type" InvoicingTypeInvoice
  sqlWith "closest_invoicing_parent_order_id" . sqlSelect "invoicing_parents ip" $ do
    sqlResult "ip.user_group_id user_group_id"
    sqlResult "min(ip.parent_order) parent_order"
    sqlGroupBy "ip.user_group_id"
  sqlWith publicName . sqlSelect "invoicing_parents ip" $ do
    sqlJoinOn "closest_invoicing_parent_order_id cipoid" "ip.user_group_id = cipoid.user_group_id"
    sqlResult "ip.user_group_id user_group_id"
    sqlResult "ip.parent_user_group_id parent_user_group_id"
    sqlWhere "ip.parent_order = cipoid.parent_order"
