{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fcontext-stack=50 #-}
module Stats.Model
       (
         AddDocStatEvent(..),
         DocStatEvent(..),
         DocStatQuantity(..),
         FlushDocStats(..),
         GetDocStatEvents(..),
         GetDocStatEventsByCompanyID(..),
         GetDocStatEventsByUserID(..),
         GetDocStatCSV(..),

         UserStatEvent(..),
         AddUserStatEvent(..),
         GetUserStatEvents(..),
         UserStatQuantity(..),
         RemoveInactiveUserLoginEvents(..),

         SignStatQuantity(..),
         SignStatEvent(..),
         AddSignStatEvent(..),
         GetSignStatEvents(..),

         GetUsersAndStatsAndInviteInfo(..)
       )

       where

import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Data.Monoid
import Data.List
import Database.HDBC

import DB
import MinutesTime
import User.Model
import Doc.DocStateData
import Stats.Tables
import Company.Model
import API.Service.Model
import OurPrelude
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS

{------ Doc Stats ------}

-- | A named quantity in the statistics events
-- please maintain the order on this (only add to the bottom)
data DocStatQuantity = DocStatClose       -- ^ A Close Document event
                     | DocStatEmailSignatures  -- ^ The number of email signatories in a closed document
                     | DocStatSend        -- ^ A Send Document event
                     | DocStatElegSignatures
                     | DocStatCreate
                     | DocStatEmailSignaturePending
                     | DocStatElegSignaturePending
                     | DocStatCancel
                     | DocStatEmailSignatureCancel
                     | DocStatElegSignatureCancel
                     | DocStatReject
                     | DocStatEmailSignatureReject
                     | DocStatElegSignatureReject
                     | DocStatTimeout
                     | DocStatEmailSignatureTimeout
                     | DocStatElegSignatureTimeout
                     | DocStatPadSignatures
                     | DocStatPadSignaturePending
                     | DocStatPadSignatureCancel
                     | DocStatPadSignatureReject
                     | DocStatPadSignatureTimeout
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''DocStatQuantity)

-- | An even to be logged for statistical purposes
data DocStatEvent = DocStatEvent {
    seUserID     :: UserID          -- ^ User who generated the event
  , seTime       :: MinutesTime     -- ^ The time of the event
  , seQuantity   :: DocStatQuantity -- ^ The type of event
  , seAmount     :: Int             -- ^ The value of the event
  , seDocumentID :: DocumentID      -- ^ If the event is related to a document, it's this doc
  , seServiceID :: Maybe ServiceID
  , seCompanyID :: Maybe CompanyID
  , seDocumentType :: DocumentType
  , seAPIString :: String
  }

sqlOR :: SQL -> SQL -> SQL
sqlOR sql1 sql2 = mconcat [parenthesize sql1, SQL " OR " [], parenthesize sql2]

sqlAND :: SQL -> SQL -> SQL
sqlAND sql1 sql2 = mconcat [parenthesize sql1, SQL " AND " [], parenthesize sql2]

sqlJoinWith :: SQL -> [SQL] -> SQL
sqlJoinWith comm list = mconcat $ intersperse comm $ map parenthesize list


sqlJoinWithOR :: [SQL] -> SQL
sqlJoinWithOR = sqlJoinWith (SQL " OR " [])

sqlJoinWithAND :: [SQL] -> SQL
sqlJoinWithAND = sqlJoinWith (SQL " AND " [])

sqlConcatComma :: [SQL] -> SQL
sqlConcatComma sqls =
  mconcat $ intersperse (SQL ", " []) sqls

sqlConcatAND :: [SQL] -> SQL
sqlConcatAND sqls =
  mconcat $ intercalate [SQL " AND " []] (map (\s -> [SQL "(" [], s, SQL ")" [] ]) sqls)

sqlConcatOR :: [SQL] -> SQL
sqlConcatOR sqls =
  mconcat $ intercalate [SQL " OR " []] (map (\s -> [SQL "(" [], s, SQL ")" [] ]) sqls)

parenthesize :: SQL -> SQL
parenthesize (SQL command values) = SQL ("(" ++ command ++ ")") values

{-------- Doc Stat Queries ---}

selectDocStatEventsSQL :: SQL
selectDocStatEventsSQL = SQL ("SELECT "
 ++ "  e.user_id"
 ++ ", e.time"
 ++ ", e.quantity"
 ++ ", e.amount"
 ++ ", e.document_id"
 ++ ", e.service_id"
 ++ ", e.company_id"
 ++ ", e.document_type"
 ++ ", e.api_string"
 ++ "  FROM doc_stat_events e"
 ++ " ") []

fetchDocStats :: MonadDB m => DBEnv m [DocStatEvent]
fetchDocStats = foldDB decoder []
  where
    decoder acc uid time quantity amount documentid serviceid
     companyid documenttype apistring = DocStatEvent {
         seUserID       = uid
       , seTime         = time
       , seQuantity     = quantity
       , seAmount       = amount
       , seDocumentID   = documentid
       , seServiceID    = serviceid
       , seCompanyID    = companyid
       , seDocumentType = doctypeFromString documenttype
       , seAPIString    = apistring
       } : acc

data GetDocStatEvents = GetDocStatEvents
instance MonadDB m => DBQuery m GetDocStatEvents [DocStatEvent] where
  query GetDocStatEvents = do
    _ <- kRun selectDocStatEventsSQL
    fetchDocStats

data GetDocStatEventsByUserID = GetDocStatEventsByUserID UserID
instance MonadDB m => DBQuery m GetDocStatEventsByUserID [DocStatEvent] where
  query (GetDocStatEventsByUserID userid) = do
    _ <- kRun $ selectDocStatEventsSQL
      <> SQL "WHERE e.user_id = ?" [toSql userid]
    fetchDocStats

data GetDocStatEventsByCompanyID = GetDocStatEventsByCompanyID CompanyID
instance MonadDB m => DBQuery m GetDocStatEventsByCompanyID [DocStatEvent] where
  query (GetDocStatEventsByCompanyID companyid) = do
    _ <- kRun $ selectDocStatEventsSQL
      <> SQL "WHERE e.company_id = ?" [toSql companyid]
    fetchDocStats

data GetDocStatCSV = GetDocStatCSV MinutesTime MinutesTime
instance MonadDB m => DBQuery m GetDocStatCSV [[BS.ByteString]] where
  query (GetDocStatCSV start end) = do
    _ <- kRun $ SQL ("SELECT doc_stat_events.user_id, trim(trim(users.first_name) || ' ' || trim(users.last_name)), trim(users.email), " ++
                    "       doc_stat_events.time, doc_stat_events.quantity, doc_stat_events.amount,  " ++
                    "       doc_stat_events.document_id, doc_stat_events.service_id, trim(companies.name), " ++
                    "       doc_stat_events.company_id, doc_stat_events.document_type, doc_stat_events.api_string " ++
                    "FROM doc_stat_events " ++
                    "LEFT JOIN users ON doc_stat_events.user_id = users.id " ++
                    "LEFT JOIN companies ON doc_stat_events.company_id = companies.id " ++
                    "WHERE doc_stat_events.time > ? AND doc_stat_events.time <= ?" ++
                    "ORDER BY doc_stat_events.time DESC") [toSql start, toSql end]
    foldDB f []
      where f :: [[BS.ByteString]] -> UserID -> BS.ByteString -> BS.ByteString -> MinutesTime -> DocStatQuantity -> Int -> DocumentID -> Maybe BS.ByteString -> Maybe BS.ByteString -> Maybe CompanyID -> BS.ByteString -> BS.ByteString -> [[BS.ByteString]]
            f acc uid n em t q a did sid cn cid tp api =
              let smartname = if BS.null n then em else n
              in [BS.fromString $ show uid, smartname, BS.fromString $ showDateYMD t, BS.fromString $ show q, 
                  BS.fromString $ show a, BS.fromString $ show did, fromMaybe (BS.fromString "scrive") sid, fromMaybe (BS.fromString "none") cn, BS.fromString $ maybe "" show cid, tp, api] : acc
                    

selectUsersAndCompaniesAndInviteInfoSQL :: SQL
selectUsersAndCompaniesAndInviteInfoSQL = SQL ("SELECT "
  -- User:
  ++ "  users.id AS user_id"
  ++ ", users.password"
  ++ ", users.salt"
  ++ ", users.is_company_admin"
  ++ ", users.account_suspended"
  ++ ", users.has_accepted_terms_of_service"
  ++ ", users.signup_method"
  ++ ", users.service_id AS user_service_id"
  ++ ", users.company_id AS user_company_id"
  ++ ", users.first_name"
  ++ ", users.last_name"
  ++ ", users.personal_number"
  ++ ", users.company_position"
  ++ ", users.phone"
  ++ ", users.mobile"
  ++ ", users.email"
  ++ ", users.lang"
  ++ ", users.region"
  ++ ", users.customfooter"
  ++ ", users.company_name"
  ++ ", users.company_number"
  ++ ", users.is_free"
  -- Company:
  ++ ", c.id AS company_id"
  ++ ", c.external_id"
  ++ ", c.service_id AS company_service_id"
  ++ ", c.name"
  ++ ", c.number"
  ++ ", c.address"
  ++ ", c.zip"
  ++ ", c.city"
  ++ ", c.country"
  ++ ", c.bars_background"
  ++ ", c.bars_textcolour"
  ++ ", c.logo"
  ++ ", email_domain"
  -- InviteInfo:
  ++ ", user_invite_infos.inviter_id"
  ++ ", user_invite_infos.invite_time"
  ++ ", user_invite_infos.invite_type"
  ++ "  FROM users"
  ++ "  LEFT JOIN companies c ON users.company_id = c.id"
  ++ "  LEFT JOIN user_invite_infos ON users.id = user_invite_infos.user_id"
  ++ "  WHERE users.deleted = FALSE")
  []


fetchUsersAndCompaniesAndInviteInfo :: MonadDB m => DBEnv m [(User, Maybe Company, Maybe InviteInfo)]
fetchUsersAndCompaniesAndInviteInfo = reverse `liftM` foldDB decoder []
  where
    decoder acc uid password salt is_company_admin account_suspended
     has_accepted_terms_of_service signup_method service_id company_id
     first_name last_name personal_number company_position phone mobile
     email lang region customfooter company_name company_number cid eid sid
     name number address zip' city country bars_background bars_textcolour logo email_domain
     inviter_id invite_time invite_type is_free
     = (
       User {
           userid = uid
         , userpassword = maybePassword (password, salt)
         , useriscompanyadmin = is_company_admin
         , useraccountsuspended = account_suspended
         , userhasacceptedtermsofservice = has_accepted_terms_of_service
         , usersignupmethod = signup_method
         , userinfo = UserInfo {
             userfstname = first_name
           , usersndname = last_name
           , userpersonalnumber = personal_number
           , usercompanyposition = company_position
           , userphone = phone
           , usermobile = mobile
           , useremail = email
           , usercompanyname = company_name
           , usercompanynumber = company_number
           }
         , usersettings = UserSettings {
             locale = mkLocale region lang
           , customfooter = customfooter
           }
         , userservice = service_id
         , usercompany = company_id
         , userisfree = is_free
         }
        , case cid of
            (Just _) -> Just Company {
                companyid = $(fromJust) cid
              , companyexternalid = eid
              , companyservice = sid
              , companyinfo = CompanyInfo {
                  companyname = $(fromJust) name
                , companynumber = $(fromJust) number
                , companyaddress = $(fromJust) address
                , companyzip = $(fromJust) zip'
                , companycity = $(fromJust) city
                , companycountry = $(fromJust) country
                , companyemaildomain = email_domain
                }
              , companyui = CompanyUI {
                  companybarsbackground = bars_background
                , companybarstextcolour = bars_textcolour
                , companylogo = logo
                }
              }
            _ -> Nothing
        , InviteInfo <$> inviter_id <*> invite_time <*> invite_type
        ) : acc

selectUserIDAndStatsSQL :: (DocStatQuantity, DocStatQuantity) -> SQL
selectUserIDAndStatsSQL (q1, q2) = SQL ("SELECT "
  ++ " e.user_id"
  ++ ", e.time"
  ++ ", e.quantity"
  ++ ", e.amount"
  ++ "  FROM doc_stat_events e"
  ++ "    WHERE e.quantity IN (?, ?)")
  [toSql q1, toSql q2]


fetchUserIDAndStats :: MonadDB m => DBEnv m (M.Map UserID [(MinutesTime, DocStatQuantity, Int)])
fetchUserIDAndStats = foldDB decoder M.empty
  where
    decoder acc uid time quantity amount =
      M.insertWith' (++) uid [(time,quantity,amount)] acc

data GetUsersAndStatsAndInviteInfo = GetUsersAndStatsAndInviteInfo [UserFilter] [AscDesc UserOrderBy] UserPagination
instance MonadDB m => DBQuery m GetUsersAndStatsAndInviteInfo
  [(User, Maybe Company, [(MinutesTime, DocStatQuantity, Int)], Maybe InviteInfo)] where
  query (GetUsersAndStatsAndInviteInfo filters sorting pagination) = do
    _ <- kRun $ mconcat
         [ SQL "CREATE TEMP TABLE users_and_companies_temp AS " []
         , selectUsersAndCompaniesAndInviteInfoSQL
         , if null filters
             then SQL "" []
             else SQL " AND " [] `mappend` sqlConcatAND (map userFilterToSQL filters)
         , if null sorting
           then mempty
           else SQL " ORDER BY " [] <> sqlConcatComma (map userOrderByAscDescToSQL sorting)
         , SQL (" OFFSET " ++ show (userOffset pagination) ++ " LIMIT " ++ show (userLimit pagination)) []
         ]

    _ <- kRun $ SQL "SELECT * FROM users_and_companies_temp" []
    usersWithCompaniesAndInviteInfo <- fetchUsersAndCompaniesAndInviteInfo

    _ <- kRun $ selectUserIDAndStatsSQL (DocStatCreate, DocStatClose) <>
         SQL " AND EXISTS (SELECT 1 FROM users_and_companies_temp WHERE users_and_companies_temp.user_id = e.user_id)" []

    stats <- fetchUserIDAndStats

    _ <- kRunRaw "DROP TABLE users_and_companies_temp"

    let findStats (user,mcompany,minviteinfo) = (user, mcompany, M.findWithDefault [] (userid user) stats,minviteinfo)

    return $ map findStats usersWithCompaniesAndInviteInfo

{-------- Doc Stat Updates --}

data AddDocStatEvent = AddDocStatEvent DocStatEvent
instance MonadDB m => DBUpdate m AddDocStatEvent Bool where
  update (AddDocStatEvent DocStatEvent{..}) =
    kRun01 $ mkSQL INSERT tableDocStatEvents [
        sql "user_id" seUserID
      , sql "time" seTime
      , sql "quantity" seQuantity
      , sql "amount" seAmount
      , sql "document_id" seDocumentID
      , sql "service_id" seServiceID
      , sql "company_id" seCompanyID
      , sql "document_type" $ show seDocumentType
      , sql "api_string" seAPIString
      ] <> SQL "WHERE NOT EXISTS (SELECT 1 FROM doc_stat_events WHERE document_id = ? AND quantity = ?)" [
        toSql seDocumentID
      , toSql seQuantity
      ]

data FlushDocStats = FlushDocStats
instance MonadDB m => DBUpdate m FlushDocStats () where
  update FlushDocStats = kRunRaw "DELETE FROM doc_stat_events"

{------ User Stats ------}

data UserStatQuantity = UserSignTOS             -- When user signs TOS
                      | UserSaveAfterSign       -- when user accepts the save option after signing
                      | UserPhoneAfterTOS       -- when a user requests a phone call after accepting the TOS
                      | UserCreateCompany       -- when a user creates a company
                      | UserLogin               -- when a user logs in
                      | UserAPIGrantAccess      -- when a user requests an access token
                      | UserAPINewUser          -- when they create a new user because of api
                      deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''UserStatQuantity)

data UserStatEvent = UserStatEvent {
    usUserID    :: UserID
  , usTime      :: MinutesTime
  , usQuantity  :: UserStatQuantity
  , usAmount    :: Int
  , usServiceID :: Maybe ServiceID
  , usCompanyID :: Maybe CompanyID
  }

selectUserStatEventsSQL :: SQL
selectUserStatEventsSQL = SQL ("SELECT"
 ++ "  e.user_id"
 ++ ", e.time"
 ++ ", e.quantity"
 ++ ", e.amount"
 ++ ", e.service_id"
 ++ ", e.company_id"
 ++ "  FROM user_stat_events e"
 ++ " ") []

fetchUserStats :: MonadDB m => DBEnv m [UserStatEvent]
fetchUserStats = foldDB decoder []
  where
    decoder acc uid time quantity amount serviceid companyid = UserStatEvent {
        usUserID       = uid
      , usTime         = time
      , usQuantity     = quantity
      , usAmount       = amount
      , usServiceID    = serviceid
      , usCompanyID    = companyid
      } : acc

data GetUserStatEvents = GetUserStatEvents
instance MonadDB m => DBQuery m GetUserStatEvents [UserStatEvent] where
  query GetUserStatEvents = do
    _ <- kRun selectUserStatEventsSQL
    fetchUserStats

data AddUserStatEvent = AddUserStatEvent UserStatEvent
instance MonadDB m => DBUpdate m AddUserStatEvent Bool where
  update (AddUserStatEvent UserStatEvent{..}) =
    kRun01 $ mkSQL INSERT tableUserStatEvents [
        sql "user_id" usUserID
      , sql "time" usTime
      , sql "quantity" usQuantity
      , sql "amount" usAmount
      , sql "service_id" usServiceID
      , sql "company_id" usCompanyID
      ]

data RemoveInactiveUserLoginEvents = RemoveInactiveUserLoginEvents UserID
instance MonadDB m => DBUpdate m RemoveInactiveUserLoginEvents Bool where
  update (RemoveInactiveUserLoginEvents uid) = do
    n <- kRun $ SQL "DELETE FROM user_stat_events WHERE quantity = ? AND EXISTS (SELECT 1 FROM users WHERE deleted = FALSE AND id = ? AND has_accepted_terms_of_service IS NULL)" [toSql UserLogin, toSql uid]
    return $ n > 0

{------ Signatory Stats ------}

data SignStatQuantity = SignStatInvite     -- Invitation Sent
                      | SignStatReceive    -- Invitation Received
                      | SignStatOpen       -- Invitation Opened
                      | SignStatLink       -- Secret Link Clicked
                      | SignStatSign       -- Document signed
                      | SignStatReject     -- Document rejected
                      | SignStatDelete     -- Signatory deletes
                      | SignStatPurge      -- Signatory really deletes
                      deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''SignStatQuantity)

data SignStatEvent = SignStatEvent {
    ssDocumentID      :: DocumentID
  , ssSignatoryLinkID :: SignatoryLinkID
  , ssTime            :: MinutesTime
  , ssQuantity        :: SignStatQuantity
  , ssServiceID       :: Maybe ServiceID
  , ssCompanyID       :: Maybe CompanyID
  , ssDocumentProcess :: DocumentProcess
  }

selectSignStatEventsSQL :: SQL
selectSignStatEventsSQL = SQL ("SELECT"
 ++ "  e.document_id"
 ++ ", e.signatory_link_id"
 ++ ", e.time"
 ++ ", e.quantity"
 ++ ", e.service_id"
 ++ ", e.company_id"
 ++ ", e.document_process"
 ++ "  FROM sign_stat_events e"
 ++ " ") []

fetchSignStats :: MonadDB m => DBEnv m [SignStatEvent]
fetchSignStats = foldDB decoder []
  where
    decoder acc docid slid time quantity serviceid
     companyid documentprocess = SignStatEvent {
         ssDocumentID      = docid
       , ssSignatoryLinkID = slid
       , ssTime            = time
       , ssQuantity        = quantity
       , ssServiceID       = serviceid
       , ssCompanyID       = companyid
       , ssDocumentProcess = documentprocess
       } : acc

data GetSignStatEvents = GetSignStatEvents
instance MonadDB m => DBQuery m GetSignStatEvents [SignStatEvent] where
  query GetSignStatEvents = do
    _ <- kRun selectSignStatEventsSQL
    fetchSignStats

data AddSignStatEvent = AddSignStatEvent SignStatEvent
instance MonadDB m => DBUpdate m AddSignStatEvent Bool where
  update (AddSignStatEvent SignStatEvent{..}) =
    kRun01 $ mkSQL INSERT tableSignStatEvents [
        sql "document_id" ssDocumentID
      , sql "signatory_link_id" ssSignatoryLinkID
      , sql "time" ssTime
      , sql "quantity" ssQuantity
      , sql "service_id" ssServiceID
      , sql "company_id" ssCompanyID
      , sql "document_process" ssDocumentProcess
      ] <> SQL "WHERE NOT EXISTS (SELECT 1 FROM sign_stat_events WHERE document_id = ? AND quantity = ? AND signatory_link_id = ?)" [
        toSql ssDocumentID
      , toSql ssQuantity
      , toSql ssSignatoryLinkID
      ]

