{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fcontext-stack=50 #-}
module Stats.Model
       (
         DocStatEvent(..),
         DocStatQuantity(..),
         FlushDocStats(..),
         GetDocStatEvents(..),
         GetDocStatEventsByCompanyID(..),
         GetDocStatEventsByUserID(..),
         GetDocStatCSV(..),
         GetDocHistCSV(..),

         UserStatEvent(..),
         AddUserStatEvent(..),
         GetUserStatEvents(..),
         UserStatQuantity(..),
         RemoveInactiveUserLoginEvents(..),

         SignStatQuantity(..),
         SignStatEvent(..),
         AddSignStatEvent(..),
         GetSignStatEvents(..),
         GetSignHistCSV(..),

         GetUsersAndStatsAndInviteInfo(..),
         StatUpdate,
         StatEvt (..),
         statUpdate,
         docStatClose,
         docStatSignMethod,
         docStatSend,
         docStatCancel,
         docStatReject,
         docStatCreate,
         docStatTimeout
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
  , seCompanyID :: Maybe CompanyID
  , seDocumentType :: DocumentType
  , seAPIString :: String
  }

{-------- Doc Stat Queries ---}

selectDocStatEventsSQL :: SQL
selectDocStatEventsSQL = SQL ("SELECT "
 <> "  e.user_id"
 <> ", e.time"
 <> ", e.quantity"
 <> ", e.amount"
 <> ", e.document_id"
 <> ", e.company_id"
 <> ", e.document_type"
 <> ", e.api_string"
 <> "  FROM doc_stat_events e"
 <> " ") []

fetchDocStats :: MonadDB m => DBEnv m [DocStatEvent]
fetchDocStats = foldDB decoder []
  where
    decoder acc uid time quantity amount documentid
     companyid documenttype apistring =
       case doctypeFromString documenttype of
         Nothing -> acc -- Skip Attachment doctypes
         Just doctype -> DocStatEvent {
            seUserID       = uid
          , seTime         = time
          , seQuantity     = quantity
          , seAmount       = amount
          , seDocumentID   = documentid
          , seCompanyID    = companyid
          , seDocumentType = doctype
          , seAPIString    = apistring
          } : acc

data GetDocStatEvents = GetDocStatEvents MinutesTime
instance MonadDB m => DBQuery m GetDocStatEvents [DocStatEvent] where
  query (GetDocStatEvents since) = do
    _ <- kRun $ selectDocStatEventsSQL
      <> SQL "WHERE e.time >= ?" [toSql since]
    fetchDocStats

data GetDocStatEventsByUserID = GetDocStatEventsByUserID UserID MinutesTime
instance MonadDB m => DBQuery m GetDocStatEventsByUserID [DocStatEvent] where
  query (GetDocStatEventsByUserID userid since) = do
    _ <- kRun $ selectDocStatEventsSQL
      <> SQL "WHERE e.user_id = ?" [toSql userid]
      <> SQL "AND e.time >= ?" [toSql since]
    fetchDocStats

data GetDocStatEventsByCompanyID = GetDocStatEventsByCompanyID CompanyID MinutesTime
instance MonadDB m => DBQuery m GetDocStatEventsByCompanyID [DocStatEvent] where
  query (GetDocStatEventsByCompanyID companyid since) = do
    _ <- kRun $ selectDocStatEventsSQL
      <> SQL "WHERE e.company_id = ?" [toSql companyid]
      <> SQL "AND e.time >= ?" [toSql since]
    fetchDocStats

data GetDocStatCSV = GetDocStatCSV MinutesTime MinutesTime
instance MonadDB m => DBQuery m GetDocStatCSV [[BS.ByteString]] where
  query (GetDocStatCSV start end) = do
    _ <- kRun $ SQL ("SELECT doc_stat_events.user_id, trim(trim(users.first_name) || ' ' || trim(users.last_name)), trim(users.email), " <>
                    "       doc_stat_events.time, doc_stat_events.quantity, doc_stat_events.amount,  " <>
                    "       doc_stat_events.document_id, trim(companies.name), " <>
                    "       doc_stat_events.company_id, doc_stat_events.document_type, doc_stat_events.api_string " <>
                    "FROM doc_stat_events " <>
                    "LEFT JOIN users ON doc_stat_events.user_id = users.id " <>
                    "LEFT JOIN companies ON doc_stat_events.company_id = companies.id " <>
                    "WHERE doc_stat_events.time > ? AND doc_stat_events.time <= ?" <>
                    "ORDER BY doc_stat_events.time DESC") [toSql start, toSql end]
    foldDB f []
      where f :: [[BS.ByteString]] -> UserID -> BS.ByteString -> BS.ByteString -> MinutesTime -> DocStatQuantity -> Int -> DocumentID -> Maybe BS.ByteString -> Maybe CompanyID -> BS.ByteString -> BS.ByteString -> [[BS.ByteString]]
            f acc uid n em t q a did cn cid tp api =
              let smartname = if BS.null n then em else n
              in [BS.fromString $ show uid, smartname, BS.fromString $ showDateYMD t, BS.fromString $ show q,
                  BS.fromString $ show a, BS.fromString $ show did, fromMaybe (BS.fromString "none") cn, BS.fromString $ maybe "" show cid, tp, api] : acc

data GetDocHistCSV = GetDocHistCSV MinutesTime MinutesTime
instance MonadDB m => DBQuery m GetDocHistCSV [[String]] where
  query (GetDocHistCSV start end) = do
    _ <- kRun $ SQL ("SELECT ds.document_id, ds.company_id, ds.document_type, " <>
                     "       creat.time, " <>
                     "       send.time, " <>
                     "       close.time, " <>
                     "       reject.time, " <>
                     "       cancel.time, " <>
                     "       timeout.time " <>
                     "FROM (SELECT DISTINCT document_id, company_id, document_type FROM doc_stat_events WHERE doc_stat_events.time > ? AND doc_stat_events.time <= ?) AS ds " <>
                     "LEFT JOIN doc_stat_events AS creat   ON (ds.document_id = creat.document_id   AND creat.quantity = ?)" <>                     
                     "LEFT JOIN doc_stat_events AS send    ON (ds.document_id = send.document_id    AND send.quantity = ?)" <>
                     "LEFT JOIN doc_stat_events AS close   ON (ds.document_id = close.document_id   AND close.quantity = ?)" <>
                     "LEFT JOIN doc_stat_events AS reject  ON (ds.document_id = reject.document_id  AND reject.quantity = ?)" <>
                     "LEFT JOIN doc_stat_events AS cancel  ON (ds.document_id = cancel.document_id  AND cancel.quantity = ?)" <>
                     "LEFT JOIN doc_stat_events AS timeout ON (ds.document_id = timeout.document_id AND timeout.quantity = ?)" <>
                     "ORDER BY ds.document_id DESC") [toSql start, 
                                                      toSql end,
                                                      toSql DocStatCreate,
                                                      toSql DocStatSend,
                                                      toSql DocStatClose,
                                                      toSql DocStatReject,
                                                      toSql DocStatCancel,
                                                      toSql DocStatTimeout]
    foldDB f []
      where f :: [[String]] -> DocumentID -> Maybe CompanyID -> String -> Maybe MinutesTime -> Maybe MinutesTime -> Maybe MinutesTime -> Maybe MinutesTime -> Maybe MinutesTime -> Maybe MinutesTime -> [[String]]
            f acc did co t cr se cl re ca ti =
              [show did, maybe "" show co, t, maybe "" formatMinutesTimeISO cr, maybe "" formatMinutesTimeISO se, maybe "" formatMinutesTimeISO cl, maybe "" formatMinutesTimeISO re, maybe "" formatMinutesTimeISO ca, maybe "" formatMinutesTimeISO ti] : acc

selectUsersAndCompaniesAndInviteInfoSQL :: SQL
selectUsersAndCompaniesAndInviteInfoSQL = SQL ("SELECT "
  -- User:
  <> "  users.id AS user_id"
  <> ", users.password"
  <> ", users.salt"
  <> ", users.is_company_admin"
  <> ", users.account_suspended"
  <> ", users.has_accepted_terms_of_service"
  <> ", users.signup_method"
  <> ", users.company_id AS user_company_id"
  <> ", users.first_name"
  <> ", users.last_name"
  <> ", users.personal_number"
  <> ", users.company_position"
  <> ", users.phone"
  <> ", users.mobile"
  <> ", users.email"
  <> ", users.lang"
  <> ", users.customfooter"
  <> ", users.company_name"
  <> ", users.company_number"
  <> ", users.is_free"
  -- Company:
  <> ", c.id AS company_id"
  <> ", c.external_id"
  <> ", c.name"
  <> ", c.number"
  <> ", c.address"
  <> ", c.zip"
  <> ", c.city"
  <> ", c.country"
  <> ", c.bars_background"
  <> ", c.bars_textcolour"
  <> ", c.logo"
  <> ", email_domain"
  -- InviteInfo:
  <> ", user_invite_infos.inviter_id"
  <> ", user_invite_infos.invite_time"
  <> ", user_invite_infos.invite_type"
  <> "  FROM users"
  <> "  LEFT JOIN companies c ON users.company_id = c.id"
  <> "  LEFT JOIN user_invite_infos ON users.id = user_invite_infos.user_id"
  <> "  WHERE users.deleted = FALSE")
  []


fetchUsersAndCompaniesAndInviteInfo :: MonadDB m => DBEnv m [(User, Maybe Company, Maybe InviteInfo)]
fetchUsersAndCompaniesAndInviteInfo = reverse `liftM` foldDB decoder []
  where
    decoder acc uid password salt is_company_admin account_suspended
     has_accepted_terms_of_service signup_method company_id
     first_name last_name personal_number company_position phone mobile
     email lang customfooter company_name company_number is_free cid eid
     name number address zip' city country bars_background bars_textcolour logo email_domain
     inviter_id invite_time invite_type
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
             lang = lang
           , customfooter = customfooter
           }
         , usercompany = company_id
         , userisfree = is_free
         }
        , case cid of
            (Just _) -> Just Company {
                companyid = $(fromJust) cid
              , companyexternalid = eid
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
  <> " e.user_id"
  <> ", e.time"
  <> ", e.quantity"
  <> ", e.amount"
  <> "  FROM doc_stat_events e"
  <> "    WHERE e.quantity IN (?, ?)")
  [toSql q1, toSql q2]


fetchUserIDAndStats :: MonadDB m => DBEnv m (M.Map UserID [(MinutesTime, DocStatQuantity, Int)])
fetchUserIDAndStats = foldDB decoder M.empty
  where
    decoder acc uid time quantity amount =
      M.insertWith' (++) uid [(time,quantity,amount)] acc

data GetUsersAndStatsAndInviteInfo = GetUsersAndStatsAndInviteInfo [UserFilter] [AscDesc UserOrderBy] (Int,Int)
instance MonadDB m => DBQuery m GetUsersAndStatsAndInviteInfo
  [(User, Maybe Company, [(MinutesTime, DocStatQuantity, Int)], Maybe InviteInfo)] where
  query (GetUsersAndStatsAndInviteInfo filters sorting (offset,limit)) = do
    _ <- kRun $ mconcat
         [ SQL "CREATE TEMP TABLE users_and_companies_temp AS " []
         , selectUsersAndCompaniesAndInviteInfoSQL
         , if null filters
             then SQL "" []
             else SQL " AND " [] `mappend` sqlConcatAND (map userFilterToSQL filters)
         , if null sorting
           then mempty
           else SQL " ORDER BY " [] <> sqlConcatComma (map userOrderByAscDescToSQL sorting)
         , " OFFSET" <?> offset <+> "LIMIT" <?> limit
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
  , usCompanyID :: Maybe CompanyID
  }

selectUserStatEventsSQL :: SQL
selectUserStatEventsSQL = SQL ("SELECT"
 <> "  e.user_id"
 <> ", e.time"
 <> ", e.quantity"
 <> ", e.amount"
 <> ", e.company_id"
 <> "  FROM user_stat_events e"
 <> " ") []

fetchUserStats :: MonadDB m => DBEnv m [UserStatEvent]
fetchUserStats = foldDB decoder []
  where
    decoder acc uid time quantity amount companyid = UserStatEvent {
        usUserID       = uid
      , usTime         = time
      , usQuantity     = quantity
      , usAmount       = amount
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
  , ssCompanyID       :: Maybe CompanyID
  , ssDocumentProcess :: DocumentProcess
  }

selectSignStatEventsSQL :: SQL
selectSignStatEventsSQL = SQL ("SELECT"
 <> "  e.document_id"
 <> ", e.signatory_link_id"
 <> ", e.time"
 <> ", e.quantity"
 <> ", e.company_id"
 <> ", e.document_process"
 <> "  FROM sign_stat_events e"
 <> " ") []

fetchSignStats :: MonadDB m => DBEnv m [SignStatEvent]
fetchSignStats = foldDB decoder []
  where
    decoder acc docid slid time quantity
     companyid documentprocess = SignStatEvent {
         ssDocumentID      = docid
       , ssSignatoryLinkID = slid
       , ssTime            = time
       , ssQuantity        = quantity
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
      , sql "company_id" ssCompanyID
      , sql "document_process" ssDocumentProcess
      ] <> SQL "WHERE NOT EXISTS (SELECT 1 FROM sign_stat_events WHERE document_id = ? AND quantity = ? AND signatory_link_id = ?)" [
        toSql ssDocumentID
      , toSql ssQuantity
      , toSql ssSignatoryLinkID
      ]

data GetSignHistCSV = GetSignHistCSV MinutesTime MinutesTime
instance MonadDB m => DBQuery m GetSignHistCSV [[String]] where
  query (GetSignHistCSV start end) = do
    _ <- kRun $ SQL ("SELECT ss.document_id, ss.signatory_link_id, ss.company_id, ss.document_process, " <>
                     "       invite.time, " <>
                     "       receive.time, " <>
                     "       open.time, " <>
                     "       link.time, " <>
                     "       sign.time, " <>
                     "       reject.time, " <>
                     "       del.time, " <>                     
                     "       purge.time " <>                                          
                     "FROM (SELECT DISTINCT document_id, signatory_link_id, company_id, document_process FROM sign_stat_events WHERE sign_stat_events.time > ? AND sign_stat_events.time <= ?) AS ss " <>
                     "LEFT JOIN sign_stat_events AS invite   ON (ss.document_id = invite.document_id   AND invite.quantity  = ? AND ss.signatory_link_id = invite.signatory_link_id )" <>                     
                     "LEFT JOIN sign_stat_events AS receive  ON (ss.document_id = receive.document_id  AND receive.quantity = ? AND ss.signatory_link_id = receive.signatory_link_id )" <>                                          
                     "LEFT JOIN sign_stat_events AS open     ON (ss.document_id = open.document_id     AND open.quantity    = ? AND ss.signatory_link_id = open.signatory_link_id )" <>
                     "LEFT JOIN sign_stat_events AS link     ON (ss.document_id = link.document_id     AND link.quantity    = ? AND ss.signatory_link_id = link.signatory_link_id )" <>
                     "LEFT JOIN sign_stat_events AS sign     ON (ss.document_id = sign.document_id     AND sign.quantity    = ? AND ss.signatory_link_id = sign.signatory_link_id )" <>
                     "LEFT JOIN sign_stat_events AS reject   ON (ss.document_id = reject.document_id   AND reject.quantity  = ? AND ss.signatory_link_id = reject.signatory_link_id )" <>
                     "LEFT JOIN sign_stat_events AS del      ON (ss.document_id = del.document_id      AND del.quantity     = ? AND ss.signatory_link_id = del.signatory_link_id )" <>
                     "LEFT JOIN sign_stat_events AS purge    ON (ss.document_id = purge.document_id    AND purge.quantity   = ? AND ss.signatory_link_id = purge.signatory_link_id )" <>                     
                     "ORDER BY ss.document_id DESC, ss.signatory_link_id DESC") [toSql start, 
                                                      toSql end,
                                                      toSql SignStatInvite,
                                                      toSql SignStatReceive,
                                                      toSql SignStatOpen,
                                                      toSql SignStatLink,
                                                      toSql SignStatSign,
                                                      toSql SignStatReject,
                                                      toSql SignStatDelete,
                                                      toSql SignStatPurge]
    foldDB f []
      where f :: [[String]] -> DocumentID -> SignatoryLinkID -> Maybe CompanyID -> String -> Maybe MinutesTime -> Maybe MinutesTime -> Maybe MinutesTime -> Maybe MinutesTime -> Maybe MinutesTime -> Maybe MinutesTime -> Maybe MinutesTime -> Maybe MinutesTime -> [[String]]
            f acc did sid co t inv rec op li si rej del pur =
              [show did, show sid, maybe "" show co, t, maybe "" formatMinutesTimeISO inv, maybe "" formatMinutesTimeISO rec, maybe "" formatMinutesTimeISO op, maybe "" formatMinutesTimeISO li, maybe "" formatMinutesTimeISO si, maybe "" formatMinutesTimeISO rej, maybe "" formatMinutesTimeISO del, maybe "" formatMinutesTimeISO pur] : acc

-- More readable names for types used in stat logging.
type APIString = String
type StatQuery = DocumentID -> APIString -> SQL
data StatUpdate = StatUpdate StatQuery DocumentID APIString

-- | Some functions are very similar, differing only in whether they're
--   recording a stat for a closed, pending, etc. document; there is already
--   a document status type, but that type has alternatives that aren't
--   involved in the stat logging and thus creates the possibility of trying
--   to log a "bad" stat. Better to use this type instead.
data StatEvt
  = DocClosed
  | DocPending
  | DocCancel
  | DocReject 
  | DocTimeout
    deriving Eq

instance MonadDB m => DBUpdate m StatUpdate Bool where
  update (StatUpdate doIt did apistring) =
      kRun01 $ doIt did apistring

-- | SQL for turning a (document.type, document.process) pair into a doctype
--   that's suitable for the doc_stat_events table. Its sole argument is the
--   name of the documents table.
--   Example:
-- > "SELECT id, " ++ docType "doc" ++ " FROM documents AS doc"
docType :: SQL -> SQL
docType doc = mconcat [
    "  CONCAT(",
    "    CASE",
    "      WHEN " <+> doc <+> ".type = 1 THEN 'Signable'",
    "      ELSE 'Template'",
    "    END, ' ',",
    "    CASE",
    "      WHEN " <+> doc <+> ".process = 1 THEN 'Contract'",
    "      WHEN " <+> doc <+> ".process = 2 THEN 'Offer'",
    "      ELSE 'Order'",
    "    END)"]


-- | Create a stat database update using the given event, ID and API string.
statUpdate :: StatQuery -> DocumentID -> APIString -> StatUpdate
statUpdate = StatUpdate

-- | Register a stat event for whatever signing method was used.
docStatSignMethod :: StatEvt -> StatQuery
docStatSignMethod status did apistring =
    mkSQL INSERT tableDocStatEvents [
        sqlGeneric "user_id"       "sl.user_id",
        sqlGeneric "time"          time,
        sqlGeneric "amount"        "sigs",
        sqlGeneric "document_id"   "doc.id",
        sqlGeneric "company_id"    "(SELECT users.company_id FROM users WHERE users.id = sl.user_id)",
        sql        "api_string"    apistring,
        sqlGeneric "quantity"      qty,
        sqlGeneric "document_type" (docType "doc")
      ] <+> fromPart
        <+> joinSigLinks
        <+> joinSigCount
        <+> joinQty
        <+> selectCondition
  where
    qty =
      foldl1' (<+>) [
        "(CASE",
        "  WHEN doc.authentication_method=1 AND delivery_method=2 THEN padqty",
        "  WHEN doc.authentication_method=1 THEN emailqty",
        "  ELSE elegqty ",
        "END)"]
    fromPart =
      "FROM documents AS doc"
    joinSigLinks =
      "JOIN signatory_links AS sl ON sl.document_id = doc.id AND sl.is_author"
    joinSigCount =
      foldl1' (<+>) [
        "JOIN (SELECT MAX(sign_time) AS last_sign, ",
        countSigs, " AS sigs ",
        SQL "FROM signatory_links WHERE document_id = ?)" [did'],
        " AS sls ON TRUE"]
    -- I have no idea why, but unless we use these casts, postgres whines about
    -- not being able to cast UNKNOWN to TEXT, which is weird because
    -- toSql :: DocStatQuantity -> SQLValue returns an SQLInteger and the
    -- column being written to is a SMALLINT. Anyway, this works.
    joinQty =
      SQL "JOIN (SELECT CAST(? AS SMALLINT) AS padqty) AS padtbl ON TRUE"
          [toSql padQty] <+>
      SQL "JOIN (SELECT CAST(? AS SMALLINT) AS emailqty) AS mailtbl ON TRUE"
          [toSql emailQty] <+>
      SQL "JOIN (SELECT CAST(? AS SMALLINT) AS elegqty) AS elegtbl ON TRUE"
          [toSql elegQty]
    
    selectCondition =
      SQL "WHERE doc.id = ? AND " [did'] <+> docStatusCondition
    did' = toSql did
    -- If status == Closed, then we want to count signatures and only update
    -- if the document is closed. If the document is instead pending or
    -- cancelled, we want to count _signatories_ and _always_ insert.
    -- Also, the event IDs differ, as does the timestamp to use.
    (padQty, emailQty, elegQty, countSigs, docStatusCondition, time) =
      case status of
        DocClosed ->  (DocStatPadSignatures,
                       DocStatEmailSignatures,
                       DocStatElegSignatures,
                       "COUNT (sign_time)",
                       SQL "doc.status = ?" [toSql Closed],
                       "sls.last_sign")
        DocPending -> (DocStatPadSignaturePending,
                       DocStatEmailSignaturePending,
                       DocStatElegSignaturePending,
                       signatoryCount,
                       "doc.invite_time IS NOT NULL",
                       "doc.invite_time")
        DocCancel ->  (DocStatPadSignatureCancel,
                       DocStatEmailSignatureCancel,
                       DocStatElegSignatureCancel,
                       signatoryCount,
                       cancelCondition,
                       "doc.mtime")
        DocReject ->  (DocStatPadSignatureReject,
                       DocStatEmailSignatureReject,
                       DocStatElegSignatureReject,
                       signatoryCount,
                       "doc.rejection_time IS NOT NULL",
                       "doc.rejection_time")
        DocTimeout -> (DocStatPadSignatureTimeout,
                       DocStatEmailSignatureTimeout,
                       DocStatElegSignatureTimeout,
                       signatoryCount,
                       timeoutCondition,
                       "doc.timeout_time")
    signatoryCount =
        "COUNT (CASE WHEN is_partner THEN TRUE ELSE NULL END)"
    cancelCondition =
        SQL "doc.status = ?" [toSql Canceled]
    timeoutCondition =
        foldl1' (<+>) [
          SQL "doc.status = ?" [toSql Timedout],
              " AND sl.user_id IS NOT NULL",
              " AND doc.timeout_time IS NOT NULL"]

-- | Register an event for "another document closed".
docStatClose :: StatQuery
docStatClose did =
    genericStatEvent DocStatClose
                     ("(SELECT MAX(sign_time) FROM signatory_links" <+>
                      SQL " WHERE document_id = ?)" [toSql did])
                     (SQL "doc.status = ?" [toSql Closed])
                     did

-- | Record a "doc sent" stat.
docStatSend :: StatQuery
docStatSend =
  genericStatEvent DocStatSend
                   "doc.invite_time"
                   "doc.invite_time IS NOT NULL"

-- | Record a "doc cancelled" stat.
docStatCancel :: StatQuery
docStatCancel =
    genericStatEvent DocStatCancel "doc.mtime" "TRUE"

-- | Record a "doc cancelled" stat.
docStatReject :: StatQuery
docStatReject =
    genericStatEvent DocStatReject
                     "doc.rejection_time"
                     "doc.rejection_time IS NOT NULL"

-- | Record a "doc created" stat.
docStatCreate :: StatQuery
docStatCreate =
    genericStatEvent DocStatCreate
                     "doc.ctime"
                     "TRUE"

-- | Record a "doc timed out" event.
docStatTimeout :: StatQuery
docStatTimeout =
    genericStatEvent DocStatTimeout
                     "doc.timeout_time"
                     (foldl1' (<+>) [
                         SQL "doc.status = ?" [toSql Timedout],
                         " AND sl.user_id IS NOT NULL",
                         " AND doc.timeout_time IS NOT NULL"])

-- | Generic document stat event. An "event" is a log item which:
--   * always has amount = 1;
--   * has a timestamp that can be determined by looking only at the document;
--   * can be discarded or accepted by looking only at the document; and
--   * has a static DocStatQuantity.
--   In the SQL arguments, the document is referred to as doc and the
--   signatory link of the document's author as sl.
--   See 'docStatReject' for an example of usage.
--   This is a convenience function, and it should absolutely NOT be exported
--   sicne the interface is horribly unsafe for non-internal use.
genericStatEvent :: DocStatQuantity -- ^ Event to log.
                 -> SQL             -- ^ SQL expression for the event timestamp.
                                    --   This expression may bind AT MOST
                                    --   one variable; preferrably none!
                 -> SQL             -- ^ SQL expression that determines whether
                                    --   to log the stat or not.
                 -> StatQuery
genericStatEvent qty time condition did apistring =
    mkSQL INSERT tableDocStatEvents [
        sqlGeneric "user_id"       "sl.user_id",
        sqlGeneric "time"          time,
        sql        "amount"        (1 :: Int),
        sqlGeneric "document_id"   "doc.id",
        sqlGeneric "company_id"    "(SELECT users.company_id FROM users WHERE users.id = sl.user_id)",
        sql        "api_string"    apistring,
        sql        "quantity"      qty,
        sqlGeneric "document_type" (docType "doc")
      ] <+> fromPart
  where
    fromPart =
      foldl1' (<+>) [
         " FROM documents AS doc",
         " JOIN signatory_links",
         " AS sl ON sl.document_id = doc.id",
         SQL " WHERE doc.id = ?" [toSql did],
         " AND sl.is_author"] <+> "AND" <+> condition
