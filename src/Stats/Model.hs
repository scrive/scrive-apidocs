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

import Control.Monad
import Database.HDBC

import DB
import Misc
import MinutesTime
import User.Model
import Doc.DocStateData
import Stats.Tables
import Company.Model
import API.Service.Model
import OurPrelude

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
data DocStatEvent = DocStatEvent {
    seUserID     :: UserID          -- ^ User who generated the event
  , seTime       :: MinutesTime     -- ^ The time of the event
  , seQuantity   :: DocStatQuantity -- ^ The type of event
  , seAmount     :: Int             -- ^ The value of the event
  , seDocumentID :: DocumentID      -- ^ If the event is related to a document, it's this doc
  , seServiceID :: Maybe ServiceID
  , seCompanyID :: Maybe CompanyID
  , seDocumentType :: DocumentType
  }

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
 ++ "  FROM doc_stat_events e"
 ++ " ") []

fetchDocStats :: MonadDB m => DBEnv m [DocStatEvent]
fetchDocStats = foldDB decoder []
  where
    decoder acc uid time quantity amount documentid serviceid
     companyid documenttype = DocStatEvent {
         seUserID       = uid
       , seTime         = time
       , seQuantity     = quantity
       , seAmount       = amount
       , seDocumentID   = documentid
       , seServiceID    = serviceid
       , seCompanyID    = companyid
       , seDocumentType = doctypeFromString documenttype
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
      <++> SQL "WHERE e.user_id = ?" [toSql userid]
    fetchDocStats

data GetDocStatEventsByCompanyID = GetDocStatEventsByCompanyID CompanyID
instance MonadDB m => DBQuery m GetDocStatEventsByCompanyID [DocStatEvent] where
  query (GetDocStatEventsByCompanyID companyid) = do
    _ <- kRun $ selectDocStatEventsSQL
      <++> SQL "WHERE e.company_id = ?" [toSql companyid]
    fetchDocStats

selectUsersAndStatsSQL :: (DocStatQuantity, DocStatQuantity) -> SQL
selectUsersAndStatsSQL (q1, q2) = SQL ("SELECT "
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
  ++ ", u.company_name"
  ++ ", u.company_number"
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
  ++ ", c.bars_background"
  ++ ", c.bars_textcolour"
  ++ ", encode(c.logo, 'base64')"
  ++ ", email_domain"
  -- Events:
  ++ ", e.time"
  ++ ", e.quantity"
  ++ ", e.amount"
  ++ "  FROM users u"
  ++ "  LEFT JOIN companies c ON u.company_id = c.id"
  ++ "  LEFT JOIN doc_stat_events e ON u.id = e.user_id"
  ++ "    AND e.quantity IN (?, ?)"
  ++ "  WHERE u.deleted = FALSE"
  ++ "  ORDER BY u.first_name || ' ' || u.last_name ASC, u.email ASC, userid ASC")
  [toSql q1, toSql q2]

fetchUsersAndStats :: MonadDB m => DBEnv m 
  [(User, Maybe Company, ( Maybe MinutesTime
                         , Maybe DocStatQuantity
                         , Maybe Int))]
fetchUsersAndStats = reverse `liftM` foldDB decoder []
  where
    decoder acc uid password salt is_company_admin account_suspended
     has_accepted_terms_of_service signup_method service_id company_id
     first_name last_name personal_number company_position phone mobile
     email preferred_design_mode lang region customfooter company_name company_number cid eid sid
     name number address zip' city country bars_background bars_textcolour logo email_domain time quantity amount = (
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
             preferreddesignmode = preferred_design_mode
           , locale = mkLocale region lang
           , customfooter = customfooter
           }
         , userservice = service_id
         , usercompany = company_id
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
        , case time of
            (Just _) -> (time, quantity, amount)
            _        -> (Nothing, Nothing, Nothing)
        ) : acc

data GetUsersAndStats = GetUsersAndStats
instance MonadDB m => DBQuery m GetUsersAndStats
  [(User, Maybe Company, ( Maybe MinutesTime
                         , Maybe DocStatQuantity
                         , Maybe Int))] where
  query GetUsersAndStats = do
    _ <- kRun $ selectUsersAndStatsSQL (DocStatCreate, DocStatClose)
    fetchUsersAndStats

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
      ] <++> SQL "WHERE NOT EXISTS (SELECT 1 FROM doc_stat_events WHERE document_id = ? AND quantity = ?)" [
        toSql seDocumentID
      , toSql seQuantity
      ]

data FlushDocStats = FlushDocStats
instance MonadDB m => DBUpdate m FlushDocStats () where
  update FlushDocStats = kRunRaw "DELETE FROM doc_stat_events"

{------ User Stats ------}

data UserStatQuantity = UserSignTOS             -- When user signs TOS
                      | UserSaveAfterSign       -- when user accepts the save option after signing
                      | UserRefuseSaveAfterSign -- deprecated: when user refuses the save option after signing
                      | UserPhoneAfterTOS       -- when a user requests a phone call after accepting the TOS
                      | UserCreateCompany       -- when a user creates a company
                      | UserLogin               -- when a user logs in
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
      ] <++> SQL "WHERE NOT EXISTS (SELECT 1 FROM sign_stat_events WHERE document_id = ? AND quantity = ? AND signatory_link_id = ?)" [
        toSql ssDocumentID
      , toSql ssQuantity
      , toSql ssSignatoryLinkID
      ]
