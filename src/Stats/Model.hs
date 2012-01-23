module Stats.Model
       (
         AddDocStatEvent(..),
         DocStatEvent(..),
         DocStatQuantity(..),
         FlushDocStats(..),
         GetDocStatEvents(..),
         GetDocStatEventsByCompanyID(..),
         GetDocStatEventsByUserID(..),

         UserStatEvent(..),
         AddUserStatEvent(..),
         GetUserStatEvents(..),
         UserStatQuantity(..),

         SignStatQuantity(..),
         SignStatEvent(..),
         AddSignStatEvent(..),
         GetSignStatEvents(..),

         GetUsersAndStats(..)
       )

       where

import Database.HDBC

import DB.Classes
import DB.Derive
import DB.Utils
import DB.Fetcher
import DB.Types
import MinutesTime
import User.Model
import Doc.DocStateData
import Company.Model
import API.Service.Model
import Data.Maybe
import qualified Data.ByteString as BS

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
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''DocStatQuantity)

-- | An even to be logged for statistical purposes
data DocStatEvent = DocStatEvent { seUserID     :: UserID          -- ^ User who generated the event
                                 , seTime       :: MinutesTime     -- ^ The time of the event
                                 , seQuantity   :: DocStatQuantity -- ^ The type of event
                                 , seAmount     :: Int             -- ^ The value of the event
                                 , seDocumentID :: DocumentID      -- ^ If the event is related to a document, it's this doc
                                 , seServiceID :: Maybe ServiceID
                                 , seCompanyID :: Maybe CompanyID
                                 , seDocumentType :: DocumentType
                                 }

{-------- Doc Stat Queries ---}

selectDocStatEventsSQL :: String
selectDocStatEventsSQL = "SELECT "
 ++ "  e.user_id"
 ++ ", e.time"
 ++ ", e.quantity"
 ++ ", e.amount"
 ++ ", e.document_id"
 ++ ", e.service_id"
 ++ ", e.company_id"
 ++ ", e.document_type"
 ++ "  FROM doc_stat_events e"
 ++ " " -- always end in space to avoid problems

fetchDocStats :: Statement -> [DocStatEvent] -> IO [DocStatEvent]
fetchDocStats st acc = fetchRow st >>= maybe (return acc) f
  where f [uid, time, quantity, amount, documentid, serviceid, companyid, documenttype] =
          fetchDocStats st $ DocStatEvent { seUserID       = fromSql uid
                                          , seTime         = fromSql time
                                          , seQuantity     = fromSql quantity
                                          , seAmount       = fromSql amount
                                          , seDocumentID   = DocumentID (fromSql documentid)
                                          , seServiceID    = fromSql serviceid
                                          , seCompanyID    = fromSql companyid
                                          , seDocumentType = doctypeFromString $ fromSql documenttype
                                          } : acc
        f l = error $ "fetchDocStats: unexpected row: "++show l

data GetDocStatEvents = GetDocStatEvents
instance DBQuery GetDocStatEvents [DocStatEvent] where
  dbQuery GetDocStatEvents = wrapDB $ \conn -> do
    st <- prepare conn $ selectDocStatEventsSQL
    _ <- execute st []
    fetchDocStats st []

data GetDocStatEventsByUserID = GetDocStatEventsByUserID UserID
instance DBQuery GetDocStatEventsByUserID [DocStatEvent] where
  dbQuery (GetDocStatEventsByUserID userid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectDocStatEventsSQL
      ++ " WHERE e.user_id = ?"
    _ <- execute st [toSql userid]
    fetchDocStats st []

data GetDocStatEventsByCompanyID = GetDocStatEventsByCompanyID CompanyID
instance DBQuery GetDocStatEventsByCompanyID [DocStatEvent] where
  dbQuery (GetDocStatEventsByCompanyID companyid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectDocStatEventsSQL
      ++ " WHERE e.company_id = ?"
    _ <- execute st [toSql companyid]
    fetchDocStats st []

selectUsersAndStatsSQL :: String
selectUsersAndStatsSQL = "SELECT "
  -- User:
  ++ "  u.id AS userid"
  ++ ", encode(u.password, 'base64')"
  ++ ", encode(u.salt, 'base64')"
  ++ ", u.is_company_admin"
  ++ ", u.account_suspended"
  ++ ", u.has_accepted_terms_of_service"
  ++ ", u.signup_method"
  ++ ", u.service_id"
  ++ ", u.company_id"
  ++ ", u.first_name"
  ++ ", u.last_name"
  ++ ", u.personal_number"
  ++ ", u.company_position"
  ++ ", u.phone"
  ++ ", u.mobile"
  ++ ", u.email"
  ++ ", u.preferred_design_mode"
  ++ ", u.lang"
  ++ ", u.region"
  ++ ", u.customfooter"
  -- Company:
  ++ ", c.id AS company_id"
  ++ ", c.external_id"
  ++ ", c.service_id"
  ++ ", c.name"
  ++ ", c.number"
  ++ ", c.address"
  ++ ", c.zip"
  ++ ", c.city"
  ++ ", c.country"
  -- Events:
  ++ ", e.time"
  ++ ", e.quantity"
  ++ ", e.amount"
  ++ "  FROM users u"
  ++ "  LEFT JOIN companies c ON u.company_id = c.id"
  ++ "  LEFT JOIN doc_stat_events e ON u.id = e.user_id"
  ++ "      AND e.quantity IN (?,?)"
  ++ "  WHERE u.deleted = FALSE"
  ++ "  ORDER BY u.first_name || ' ' || u.last_name ASC, u.email ASC, userid ASC"

fetchUsersAndStats :: Statement 
                   -> IO [(User, Maybe Company, ( Maybe MinutesTime
                                                , Maybe DocStatQuantity
                                                , Maybe Int))]
fetchUsersAndStats st = fetchValues st decoder
  where
    decoder :: UserID                   -- u.id 
            -> Maybe Binary             -- encode(u.password, 'base64')
            -> Maybe Binary             -- encode(u.salt, 'base64')
            -> Bool                     -- u.is_company_admin
            -> Bool                     -- u.account_suspended
            -> Maybe MinutesTime        -- u.has_accepted_terms_of_service
            -> SignupMethod             -- u.signup_method
            -> Maybe ServiceID          -- u.service_id
            -> Maybe CompanyID          -- u.company_id
            -> BS.ByteString            -- u.first_name
            -> BS.ByteString            -- u.last_name
            -> BS.ByteString            -- u.personal_number
            -> BS.ByteString            -- u.company_position
            -> BS.ByteString            -- u.phone
            -> BS.ByteString            -- u.mobile
            -> Email                    -- u.email
            -> Maybe DesignMode         -- u.preferred_design_mode
            -> Lang                     -- u.lang
            -> Region                   -- u.region
            -> Maybe String             -- u.customfooter
            -> Maybe CompanyID          -- c.id AS company_id
            -> Maybe ExternalCompanyID  -- c.external_id
            -> Maybe ServiceID          -- c.service_id
            -> Maybe BS.ByteString      -- c.name
            -> Maybe BS.ByteString      -- c.number
            -> Maybe BS.ByteString      -- c.address
            -> Maybe BS.ByteString      -- c.zip
            -> Maybe BS.ByteString      -- c.city
            -> Maybe BS.ByteString      -- c.country
            -> Maybe MinutesTime        -- e.time
            -> Maybe DocStatQuantity    -- e.quantity
            -> Maybe Int                -- e.amount
            -> Either DBException (User, Maybe Company, ( Maybe MinutesTime
                                                        , Maybe DocStatQuantity
                                                        , Maybe Int))
    decoder uid 
            password 
            salt 
            is_company_admin 
            account_suspended
            has_accepted_terms_of_service 
            signup_method 
            service_id 
            company_id
            first_name 
            last_name 
            personal_number 
            company_position 
            phone 
            mobile
            email 
            preferred_design_mode 
            lang 
            region 
            customfooter
            cid 
            eid 
            sid 
            name 
            number 
            address 
            zip' 
            city 
            country
            time
            quantity
            amount = return (
                User {
                       userid = uid
                     , userpassword = case (password, salt) of
                         (Just pwd, Just salt') -> Just Password {
                                                       pwdHash = pwd
                                                     , pwdSalt = salt'
                                                     }
                         _                      -> Nothing
                     , useriscompanyadmin = is_company_admin
                     , useraccountsuspended = account_suspended
                     , userhasacceptedtermsofservice = has_accepted_terms_of_service
                     , usersignupmethod = signup_method
                     , userinfo = UserInfo { userfstname = first_name
                                           , usersndname = last_name
                                           , userpersonalnumber = personal_number
                                           , usercompanyposition = company_position
                                           , userphone = phone
                                           , usermobile = mobile
                                           , useremail = email
                                           }
                     , usersettings = UserSettings { preferreddesignmode = preferred_design_mode
                                                   , locale = mkLocale region lang
                                                   , customfooter = customfooter
                                                   }
                     , userservice = service_id
                     , usercompany = company_id
                     }
        , case cid of 
            (Just _) -> Just Company { companyid = fromJust cid
                                     , companyexternalid = eid
                                     , companyservice = sid
                                     , companyinfo = CompanyInfo {
                                           companyname = fromJust name
                                         , companynumber = fromJust number
                                         , companyaddress = fromJust address
                                         , companyzip = fromJust zip'
                                         , companycity = fromJust city
                                         , companycountry = fromJust country
                                         }
                                     }
            _        -> Nothing
        , case time of
            (Just _) -> (time, quantity, amount)
            _        -> (Nothing, Nothing, Nothing)
        )

data GetUsersAndStats = GetUsersAndStats
instance DBQuery GetUsersAndStats [(User, Maybe Company, ( Maybe MinutesTime
                                                         , Maybe DocStatQuantity
                                                         , Maybe Int))] where
  dbQuery GetUsersAndStats = wrapDB $ \conn -> do
    st  <- prepare conn $ selectUsersAndStatsSQL 
    _   <- execute st [toSql DocStatCreate, toSql DocStatClose]
    fetchUsersAndStats st

{-------- Doc Stat Updates --}

data AddDocStatEvent = AddDocStatEvent DocStatEvent
instance DBUpdate AddDocStatEvent Bool where
  dbUpdate (AddDocStatEvent event) = wrapDB $ \conn -> do
    st <- prepare conn $ "INSERT INTO doc_stat_events ("
          ++ "  user_id"
          ++ ", time"
          ++ ", quantity"
          ++ ", amount"
          ++ ", document_id"
          ++ ", service_id"
          ++ ", company_id"
          ++ ", document_type"
          ++ ") SELECT ?, ?, ?, ?, ?, ?, ?, ? "
          -- want to avoid an error, so check if exists
          ++ " WHERE NOT EXISTS (SELECT 1 FROM doc_stat_events WHERE"
          ++ " document_id = ? AND quantity = ?)"
    r <- execute st [toSql $ seUserID event
                    ,toSql $ seTime event
                    ,toSql $ seQuantity event
                    ,toSql $ seAmount event
                    ,toSql $ unDocumentID $ seDocumentID event
                    ,toSql $ seServiceID event
                    ,toSql $ seCompanyID event
                    ,toSql $ show $ seDocumentType event

                    ,toSql $ unDocumentID $ seDocumentID event
                    ,toSql $ seQuantity event]
    oneRowAffectedGuard r


data FlushDocStats = FlushDocStats
instance DBUpdate FlushDocStats Bool where
  dbUpdate FlushDocStats = wrapDB $ \conn -> do
    st <- prepare conn $ "DELETE FROM doc_stat_events"
    _ <- execute st []
    return True



{------ User Stats ------}

data UserStatQuantity = UserSignTOS             -- When user signs TOS
                      | UserSaveAfterSign       -- when user accepts the save option after signing
                      | UserRefuseSaveAfterSign -- when user refuses the save option after signing
                      | UserPhoneAfterTOS       -- when a user requests a phone call after accepting the TOS
                      | UserCreateCompany       -- when a user creates a company
                      | UserLogin               -- when a user logs in
                      deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''UserStatQuantity)

data UserStatEvent = UserStatEvent { usUserID    :: UserID
                                   , usTime      :: MinutesTime
                                   , usQuantity  :: UserStatQuantity
                                   , usAmount    :: Int
                                   , usServiceID :: Maybe ServiceID
                                   , usCompanyID :: Maybe CompanyID
                                   }

selectUserStatEventsSQL :: String
selectUserStatEventsSQL = "SELECT "
 ++ "  e.user_id"
 ++ ", e.time"
 ++ ", e.quantity"
 ++ ", e.amount"
 ++ ", e.service_id"
 ++ ", e.company_id"
 ++ "  FROM user_stat_events e"
 ++ " " -- always end in space to avoid problems

fetchUserStats :: Statement -> [UserStatEvent] -> IO [UserStatEvent]
fetchUserStats st acc = fetchRow st >>= maybe (return acc) f
  where f [uid, time, quantity, amount, serviceid, companyid] =
          fetchUserStats st $ UserStatEvent { usUserID       = fromSql uid
                                            , usTime         = fromSql time
                                            , usQuantity     = fromSql quantity
                                            , usAmount       = fromSql amount
                                            , usServiceID    = fromSql serviceid
                                            , usCompanyID    = fromSql companyid
                                            } : acc
        f l = error $ "fetchUserStats: unexpected row: "++show l

data GetUserStatEvents = GetUserStatEvents
instance DBQuery GetUserStatEvents [UserStatEvent] where
  dbQuery GetUserStatEvents = wrapDB $ \conn -> do
    st <- prepare conn $ selectUserStatEventsSQL
    _ <- execute st []
    fetchUserStats st []

data AddUserStatEvent = AddUserStatEvent UserStatEvent
instance DBUpdate AddUserStatEvent Bool where
  dbUpdate (AddUserStatEvent event) = wrapDB $ \conn -> do
    st <- prepare conn $ "INSERT INTO user_stat_events ("
          ++ "  user_id"
          ++ ", time"
          ++ ", quantity"
          ++ ", amount"
          ++ ", service_id"
          ++ ", company_id"
          ++ ") SELECT ?, ?, ?, ?, ?, ? "
          -- want to avoid an error, so check if exists
          ++ " WHERE NOT EXISTS (SELECT 1 FROM user_stat_events WHERE"
          ++ " user_id = ? AND quantity = ?)"
    r <- execute st [toSql $ usUserID event
                    ,toSql $ usTime event
                    ,toSql $ usQuantity event
                    ,toSql $ usAmount event
                    ,toSql $ usServiceID event
                    ,toSql $ usCompanyID event

                    ,toSql $ usUserID event
                    ,toSql $ usQuantity event]
    oneRowAffectedGuard r

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

data SignStatEvent = SignStatEvent { ssDocumentID      :: DocumentID
                                   , ssSignatoryLinkID :: SignatoryLinkID
                                   , ssTime            :: MinutesTime
                                   , ssQuantity        :: SignStatQuantity
                                   , ssServiceID       :: Maybe ServiceID
                                   , ssCompanyID       :: Maybe CompanyID
                                   , ssDocumentProcess :: DocumentProcess
                                   }

selectSignStatEventsSQL :: String
selectSignStatEventsSQL = "SELECT "
 ++ "  e.document_id"
 ++ ", e.signatory_link_id"
 ++ ", e.time"
 ++ ", e.quantity"
 ++ ", e.service_id"
 ++ ", e.company_id"
 ++ ", e.document_process"
 ++ "  FROM sign_stat_events e"
 ++ " " -- always end in space to avoid problems

fetchSignStats :: Statement -> [SignStatEvent] -> IO [SignStatEvent]
fetchSignStats st acc = fetchRow st >>= maybe (return acc) f
  where f [docid, slid, time, quantity, serviceid, companyid, documentprocess] =
          fetchSignStats st $ SignStatEvent { ssDocumentID      = fromSql docid
                                            , ssSignatoryLinkID = fromSql slid
                                            , ssTime            = fromSql time
                                            , ssQuantity        = fromSql quantity
                                            , ssServiceID       = fromSql serviceid
                                            , ssCompanyID       = fromSql companyid
                                            , ssDocumentProcess = fromSql documentprocess
                                            } : acc
        f l = error $ "fetchSignStats: unexpected row: "++show l

data GetSignStatEvents = GetSignStatEvents
instance DBQuery GetSignStatEvents [SignStatEvent] where
  dbQuery GetSignStatEvents = wrapDB $ \conn -> do
    st <- prepare conn $ selectSignStatEventsSQL
    _ <- execute st []
    fetchSignStats st []

data AddSignStatEvent = AddSignStatEvent SignStatEvent
instance DBUpdate AddSignStatEvent Bool where
  dbUpdate (AddSignStatEvent event) = wrapDB $ \conn -> do
    st <- prepare conn $ "INSERT INTO sign_stat_events ("
          ++ "  document_id"
          ++ ", signatory_link_id"
          ++ ", time"
          ++ ", quantity"
          ++ ", service_id"
          ++ ", company_id"
          ++ ", document_process"
          ++ ") SELECT ?, ?, ?, ?, ?, ?, ? "
          -- want to avoid an error, so check if exists
          ++ " WHERE NOT EXISTS (SELECT 1 FROM sign_stat_events WHERE"
          ++ " document_id = ? AND quantity = ? AND signatory_link_id = ?)"
    r <- execute st [toSql $ ssDocumentID      event
                    ,toSql $ ssSignatoryLinkID event
                    ,toSql $ ssTime            event
                    ,toSql $ ssQuantity        event
                    ,toSql $ ssServiceID       event
                    ,toSql $ ssCompanyID       event
                    ,toSql $ ssDocumentProcess event

                    ,toSql $ ssDocumentID      event
                    ,toSql $ ssQuantity        event
                    ,toSql $ ssSignatoryLinkID event]
    oneRowAffectedGuard r
