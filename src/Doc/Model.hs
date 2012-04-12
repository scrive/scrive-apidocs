{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fcontext-stack=50  #-}
module Doc.Model
  ( module File.File
  , isTemplate -- fromUtils
  , isShared -- fromUtils
  , isDeletableDocument -- fromUtils
  , anyInvitationUndelivered
  , undeliveredSignatoryLinks
  , insertDocumentAsIs
  , toDocumentProcess

  , migrateDocumentTagsFromJSONToTable

  , DocumentFilter(..)
  , DocumentDomain(..)
  , DocumentPagination(..)
  , AscDesc(..)
  , DocumentOrderBy(..)

  , AddDocumentAttachment(..)
  , AddInvitationEvidence(..)
  , AdminOnlySaveForUser(..)
  , ArchiveDocument(..)
  , AttachCSVUpload(..)
  , AttachFile(..)
  , AttachSealedFile(..)
  , CancelDocument(..)
  , ChangeMainfile(..)
  , ChangeSignatoryEmailWhenUndelivered(..)
  , CloseDocument(..)
  , DeleteSigAttachment(..)
  , DocumentFromSignatoryData(..)
  , ErrorDocument(..)
  , GetDeletedDocumentsByUser(..)
  , GetDocuments(..)
  , GetDocumentsCount(..)
  , GetDocumentByDocumentID(..)
  , GetDocumentsByService(..)
  , GetDocumentsByCompanyWithFiltering(..)
  , GetDocumentsByAuthor(..)
  , GetTemplatesByAuthor(..)
  , GetAvailableTemplates(..)
  , GetAttachmentsByAuthor(..)
  , GetDocumentsBySignatory(..)
  , GetTimeoutedButPendingDocuments(..)
  , MarkDocumentSeen(..)
  , MarkInvitationRead(..)
  , NewDocument(..)
  , PreparationToPending(..)
  , ReallyDeleteDocument(..)
  , RejectDocument(..)
  , RemoveDocumentAttachment(..)
  , ResetSignatoryDetails(..)
  , ResetSignatoryDetails2(..)
  , RestartDocument(..)
  , RestoreArchivedDocument(..)
  , SaveDocumentForUser(..)
  , SaveSigAttachment(..)
  , SetDaysToSign(..)
  , SetDocumentFunctionality(..)
  , SetDocumentInviteTime(..)
  , SetDocumentLocale(..)
  , SetDocumentSharing(..)
  , SetDocumentTags(..)
  , SetDocumentTimeoutTime(..)
  , SetDocumentTitle(..)
  , SetDocumentUI(..)
  , SetDocumentIdentification(..)
  , SetInvitationDeliveryStatus(..)
  , SetInviteText(..)
  , SignDocument(..)
  , SignLinkFromDetailsForTest(..)
  , SignableFromDocumentIDWithUpdatedAuthor(..)
  , StoreDocumentForTesting(..)
  , TemplateFromDocument(..)
  , TimeoutDocument(..)
  , UpdateFields(..)
  , SetSigAttachments(..)
  , UpdateFieldsNoStatusCheck(..)
  , UpdateDraft(..)
  , SetDocumentModificationData(..)
  ) where

import API.Service.Model
import DB.Classes
import DB.Fetcher2
import MagicHash (MagicHash)
import Crypto.RNG(random)
import DB.Utils
import File.File
import File.FileID
import Doc.DocUtils
import User.UserID
import User.Model
import Company.Model
import MinutesTime
import OurPrelude
import Control.Logic
import Doc.DocStateData
import Doc.Invariants
import Database.HDBC
import Data.Maybe hiding (fromJust)
import Misc
import IPAddress
import Data.List hiding (tail, head)
import Data.Monoid
import qualified Data.Map as M
import Doc.Tables
import Control.Applicative
import Util.SignatoryLinkUtils
import Doc.DocProcess
import Doc.DocStateCommon
import qualified Log
import Control.Monad.IO.Class
import Control.Monad
import qualified Control.Exception as E
import Util.MonadUtils

import EvidenceLog.Model
import Util.HasSomeUserInfo

data DocumentPagination =
  DocumentPagination
  { documentOffset :: Int        -- ^ use for SQL OFFSET command
  , documentLimit  :: Int        -- ^ use for SQL LIMIT command
  }

data DocumentFilter
  = DocumentFilterStatuses [DocumentStatus]   -- ^ Any of listed statuses
  | DocumentFilterByTags [DocumentTag]        -- ^ All of listed tags
  | DocumentFilterMinChangeTime MinutesTime   -- ^ Minimal mtime
  | DocumentFilterMaxChangeTime MinutesTime   -- ^ Maximum mtime
  | DocumentFilterByService (Maybe ServiceID) -- ^ Only documents belonging to a service
  | DocumentFilterByRole SignatoryRole        -- ^ Signatory must have role
  | DocumentFilterByProcess [DocumentProcess] -- ^ Any of listed processes
  | DocumentFilterByString String             -- ^ Contains the string in title, list of people involved or anywhere
  | DocumentFilterByIdentification IdentificationType -- ^ Only documents that use selected identification type

data DocumentDomain
  = DocumentsOfAuthor UserID                     -- ^ Documents by author, not deleted
  | DocumentsOfAuthorDeleted UserID              -- ^ Documents by author, deleted
  | DocumentsOfAuthorDeleteValue UserID Bool     -- ^ Documents by author, with delete flag
  | DocumentsForSignatory UserID                 -- ^ Documents by signatory, not deleted
  | DocumentsForSignatoryDeleted UserID          -- ^ Documents by signatory, deleted
  | DocumentsForSignatoryDeleteValue UserID Bool -- ^ Documents by signatory, with delete flag
  | TemplatesOfAuthor UserID                     -- ^ Templates by author, not deleted
  | TemplatesOfAuthorDeleted UserID              -- ^ Templates by author, deleted
  | TemplatesOfAuthorDeleteValue UserID Bool     -- ^ Templates by author, with deleted flag
  | TemplatesSharedInUsersCompany UserID         -- ^ Templates shared in company
  | DocumentsOfService (Maybe ServiceID)         -- ^ All documents of service
  | DocumentsOfCompany CompanyID                 -- ^ All documents of a company, not deleted
  | AttachmentsOfAuthorDeleteValue UserID Bool   -- ^ Attachments of user, with deleted flag

-- | These are possible order by clauses that make documents sorted by.
data DocumentOrderBy
  = DocumentOrderByTitle       -- ^ Order by title, alphabetically, case insensitive
  | DocumentOrderByMTime       -- ^ Order by modification time
  | DocumentOrderByStatusClass -- ^ Order by status class.
  | DocumentOrderByType        -- ^ Order by document type.
  | DocumentOrderByProcess     -- ^ Order by process

-- | 'AscDesc' marks ORDER BY order as ascending or descending.
-- Conversion to SQL adds DESC marker to descending and no marker
-- to ascending order.
data AscDesc a = Asc a | Desc a

-- | Convert DocumentOrderBy enumeration into proper SQL order by statement
documentOrderByToSQL :: DocumentOrderBy -> SQL
documentOrderByToSQL DocumentOrderByTitle = SQL "documents.title" []
documentOrderByToSQL DocumentOrderByMTime = SQL "documents.mtime" []
documentOrderByToSQL DocumentOrderByStatusClass = 
  SQL (documentStatusClassExpression) []
documentOrderByToSQL DocumentOrderByType = SQL "documents.type" []
documentOrderByToSQL DocumentOrderByProcess = SQL "documents.process" []

documentOrderByAscDescToSQL :: AscDesc DocumentOrderBy -> SQL
documentOrderByAscDescToSQL (Asc x) = documentOrderByToSQL x
documentOrderByAscDescToSQL (Desc x) = documentOrderByToSQL x <++> SQL " DESC" []

documentDomainToSQL :: DocumentDomain -> SQL
documentDomainToSQL (DocumentsOfAuthorDeleteValue uid deleted) =
  SQL ("(signatory_links.roles & ?) <> 0"
       ++ " AND signatory_links.user_id = ?"
       ++ " AND signatory_links.deleted = ?"
       ++ " AND signatory_links.really_deleted = FALSE"
       ++ " AND documents.type = 1")
        [toSql [SignatoryAuthor], toSql uid, toSql deleted]
documentDomainToSQL (DocumentsOfAuthor uid) =
  documentDomainToSQL (DocumentsOfAuthorDeleteValue uid False)
documentDomainToSQL (DocumentsOfAuthorDeleted uid) =
  documentDomainToSQL (DocumentsOfAuthorDeleteValue uid True)
documentDomainToSQL (DocumentsForSignatory uid) =
  documentDomainToSQL (DocumentsForSignatoryDeleteValue uid False)
documentDomainToSQL (DocumentsForSignatoryDeleted uid) =
  documentDomainToSQL (DocumentsForSignatoryDeleteValue uid True)
documentDomainToSQL (DocumentsForSignatoryDeleteValue uid deleted) =
  SQL ("signatory_links.user_id = ?"
       ++ " AND signatory_links.deleted = ?"
       ++ " AND signatory_links.really_deleted = FALSE"
       ++ " AND documents.type = 1"
       ++ " AND ((signatory_links.roles & ?) <> 0"
       ++ "      OR ((signatory_links.roles & ?) <> 0"
       ++ "          AND NOT EXISTS (SELECT 1 FROM signatory_links AS sl2"
       ++ "                           WHERE signatory_links.document_id = sl2.document_id"
       ++ "                             AND ((sl2.roles & ?) <> 0)"
       ++ "                             AND sl2.sign_time IS NULL"
       ++ "                             AND sl2.sign_order < signatory_links.sign_order)))")
        [toSql uid, toSql deleted, toSql [SignatoryAuthor], toSql [SignatoryPartner], toSql [SignatoryPartner]]
documentDomainToSQL (TemplatesOfAuthorDeleteValue uid deleted) =
  SQL ("signatory_links.user_id = ?"
       ++ " AND signatory_links.deleted = ?"
       ++ " AND signatory_links.really_deleted = FALSE"
       ++ " AND documents.type = 2")
        [toSql uid, toSql deleted]
documentDomainToSQL (TemplatesOfAuthor uid) =
  documentDomainToSQL (TemplatesOfAuthorDeleteValue uid False)
documentDomainToSQL (TemplatesOfAuthorDeleted uid) =
  documentDomainToSQL (TemplatesOfAuthorDeleteValue uid True)
documentDomainToSQL (TemplatesSharedInUsersCompany uid) =
  SQL ("signatory_links.deleted = FALSE"
       ++ " AND documents.type = 2"
       ++ " AND documents.sharing = ?"
       ++ " AND signatory_links.really_deleted = FALSE"
       ++ " AND EXISTS (SELECT 1 FROM users AS usr1, users AS usr2 "
       ++ "                WHERE signatory_links.user_id = usr2.id "
       ++ "                  AND usr2.company_id = usr1.company_id "
       ++ "                  AND usr1.id = ?)")
        [toSql Shared, toSql uid]
documentDomainToSQL (DocumentsOfService sid) =
  SQL "documents.service_id IS NOT DISTINCT FROM ? AND documents.type = 1"
        [toSql sid]
documentDomainToSQL (DocumentsOfCompany cid) =
  SQL "signatory_links.company_id = ? AND signatory_links.deleted = FALSE"
        [toSql cid]
documentDomainToSQL (AttachmentsOfAuthorDeleteValue uid deleted) =
  SQL ("signatory_links.user_id = ?"
       ++ " AND signatory_links.deleted = ?"
       ++ " AND signatory_links.really_deleted = FALSE"
       ++ " AND documents.type = 3")
        [toSql uid, toSql deleted]



maxselect :: String
maxselect = "(SELECT max(greatest(signatory_links.sign_time"
            ++ ", signatory_links.seen_time"
            ++ ", signatory_links.read_invitation"
            ++ ", documents.invite_time"
            ++ ", documents.rejection_time"
            ++ ", documents.mtime"
            ++ ", documents.ctime"
            ++ ")) FROM signatory_links WHERE signatory_links.document_id = documents.id)"

documentFilterToSQL :: DocumentFilter -> SQL
documentFilterToSQL (DocumentFilterStatuses []) =
  SQL "FALSE" []
documentFilterToSQL (DocumentFilterStatuses statuses) =
  SQL ("documents.status IN (" ++ intercalate "," (map (const "?") statuses) ++ ")")
                               (map toSql statuses)
documentFilterToSQL (DocumentFilterMinChangeTime ctime) =
  SQL (maxselect ++ " >= ?") [toSql ctime]
documentFilterToSQL (DocumentFilterMaxChangeTime ctime) =
  SQL (maxselect ++ " <= ?") [toSql ctime]
documentFilterToSQL (DocumentFilterByService mservice) =
  SQL "documents.service_id IS NOT DISTINCT FROM ?" [toSql mservice]
documentFilterToSQL (DocumentFilterByProcess processes) =
  sqlConcatOR $ map (\process -> SQL "documents.process = ?" [toSql process]) processes
documentFilterToSQL (DocumentFilterByRole role) =
  SQL "(signatory_links.roles & ?) <> 0" [toSql [role]]
documentFilterToSQL (DocumentFilterByTags []) =
  SQL "TRUE" []
documentFilterToSQL (DocumentFilterByTags tags) =
  sqlConcatAND $ map (\tag -> SQL "EXISTS (SELECT 1 FROM document_tags WHERE name = ? AND value = ? AND document_id = documents.id)"
                              [toSql $ tagname tag, toSql $ tagvalue tag]) tags
documentFilterToSQL (DocumentFilterByString string) =
  SQL "documents.title ILIKE ?" [sqlpat] `sqlOR` 
     sqlJoinWithAND (map (\wordpat -> SQL "signatory_links.fields ILIKE ?" [wordpat]) sqlwordpat)
  where
      sqlpat = toSql $ "%" ++ concatMap escape string ++ "%"
      sqlwordpat = map (\word -> toSql $ "%" ++ concatMap escape word ++ "%") (words string)
      escape '\\' = "\\\\"
      escape '%' = "\\%"
      escape '_' = "\\_"
      escape c = [c]
documentFilterToSQL (DocumentFilterByIdentification identification) =
  SQL ("(documents.allowed_id_types & ?) <> 0") [toSql [identification]]

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


sqlLog :: MinutesTime -> String -> (String, String, SqlValue)
sqlLog time text = sql' "log" "log || ?" logmsg
  where logmsg = unlines [show $ DocumentLogEntry time text]

getOneDocumentAffected :: String -> Integer -> DocumentID -> DB (Either String Document)
getOneDocumentAffected text r did =
  case r of
    0 -> do
      return (Left (text ++ " did not affect any rows"))
    1 -> do
      mnewdoc <- dbQuery $ GetDocumentByDocumentID did
      case mnewdoc of
        Nothing -> return (Left ("Document #" ++ show did ++ " diappeared after " ++ text))
        Just newdoc -> return (Right newdoc)
    _ -> do
      -- here we really want to abort transaction, as we have affected more rows that we wanted
      -- something is seriously wrong!
      liftIO $ E.throwIO TooManyObjects {
          originalQuery = mempty
        , tmoExpected = 1
        , tmoGiven = fromIntegral r
        }

checkEqualBy :: (Eq b, Show b) => String -> (a -> b) -> a -> a -> Maybe (String, String, String)
checkEqualBy name func obj1 obj2
  | func obj1 /= func obj2 = Just (name, show (func obj1), show (func obj2))
  | otherwise              = Nothing

checkEqualByAllowSecondNothing :: (Eq b, Show b) => String -> (a -> Maybe b) -> a -> a -> Maybe (String, String, String)
checkEqualByAllowSecondNothing name func obj1 obj2
  | func obj1 /= func obj2 && (not (isNothing (func obj2))) = Just (name, show (func obj1), show (func obj2))
  | otherwise              = Nothing

assertEqualDocuments :: (Monad m, MonadIO m) => Document -> Document -> m ()
assertEqualDocuments d1 d2 | null inequalities = return ()
                           | otherwise = do
                                Log.debug message
                                error message
  where
    message = "Documents aren't equal in " ++ concat (map showInequality inequalities)
    showInequality (name,obj1,obj2) = name ++ ": \n" ++ obj1 ++ "\n" ++ obj2 ++ "\n"
    sl1 = documentsignatorylinks d1
    sl2 = documentsignatorylinks d2
    checkSigLink s1 s2 = map (\f -> f s1 s2)
                         [ checkEqualBy "signatorydetails" signatorydetails
                         , checkEqualBy "signatorymagichash" signatorymagichash
                         , checkEqualByAllowSecondNothing "maybesignatory" maybesignatory
                         , checkEqualBy "maybecompany" maybecompany
                         , checkEqualBy "maybesigninfo" maybesigninfo
                         , checkEqualBy "maybeseeninfo" maybeseeninfo
                         , checkEqualBy "maybereadinvite" maybereadinvite
                         , checkEqualBy "invitationdeliverystatus" invitationdeliverystatus
                         , checkEqualBy "signatorysignatureinfo" signatorysignatureinfo
                         , checkEqualBy "signatoryroles" (sort . signatoryroles)
                         , checkEqualBy "signatorylinkdeleted" signatorylinkdeleted
                         , checkEqualBy "signatorylinkreallydeleted" signatorylinkreallydeleted
                         , checkEqualBy "signatorylinkcsvupload" signatorylinkcsvupload
                         ]

    inequalities = catMaybes $ map (\f -> f d1 d2)
                   [ checkEqualBy "documenttitle" documenttitle
                   , checkEqualBy "documentfiles" documentfiles
                   , checkEqualBy "documentsealedfiles" documentsealedfiles
                   , checkEqualBy "documentstatus" documentstatus
                   , checkEqualBy "documenttype" documenttype
                   , checkEqualBy "documentfunctionality" documentfunctionality
                   , checkEqualBy "documentctime" documentctime
                   , checkEqualBy "documentmtime" documentmtime
                   , checkEqualBy "documentdaystosign" documentdaystosign
                   , checkEqualBy "documenttimeouttime" documenttimeouttime
                   , checkEqualBy "documentinvitetime" documentinvitetime
                   , checkEqualBy "documentlog" documentlog
                   , checkEqualBy "documentinvitetext" documentinvitetext
                   , checkEqualBy "documentallowedidtypes" (nub . documentallowedidtypes)
                   , checkEqualBy "documentcancelationreason" documentcancelationreason
                   , checkEqualBy "documentsharing" documentsharing
                   , checkEqualBy "documentrejectioninfo" documentrejectioninfo
                   , checkEqualBy "documenttags" (sort . documenttags)
                   , checkEqualBy "documentservice" documentservice
                   , checkEqualBy "documentdeleted" documentdeleted
                   , checkEqualBy "documentauthorattachments" documentauthorattachments
                   , checkEqualBy "documentui" documentui
                   , checkEqualBy "documentregion" documentregion
                   , checkEqualBy "documentsignatorylinks count" (length . documentsignatorylinks)
                   ] ++
                   concat (zipWith checkSigLink sl1 sl2)


documentsSelectors :: String
documentsSelectors = intercalate ", " [
    "id"
  , "title"
  , "file_id"
  , "sealed_file_id"
  , "status"
  , "error_text"
  , "type"
  , "process"
  , "functionality"
  , "ctime"
  , "mtime"
  , "days_to_sign"
  , "timeout_time"
  , "invite_time"
  , "invite_ip"
  , "log"
  , "invite_text"
  , "allowed_id_types"
  , "cancelation_reason"
  , "rejection_time"
  , "rejection_signatory_link_id"
  , "rejection_reason"
  , "service_id"
  , "deleted"
  , "mail_footer"
  , "region"
  , "sharing"
  , documentStatusClassExpression
  ]

selectDocumentsSQL :: SQL
selectDocumentsSQL = SQL ("SELECT "
  ++ documentsSelectors
  ++ " FROM documents ") []

fetchDocuments :: DB [Document]
fetchDocuments = foldDB decoder []
  where
    -- Note: this function gets documents in reversed order, but all queries
    -- use reversed order too, so in the end everything is properly ordered.
    decoder acc did title file_id sealed_file_id status error_text simple_type
     process functionality ctime mtime days_to_sign timeout_time invite_time
     invite_ip dlog invite_text allowed_id_types cancelationreason rejection_time
     rejection_signatory_link_id rejection_reason service deleted mail_footer
     region sharing status_class = Document {
         documentid = did
       , documenttitle = title
       , documentsignatorylinks = []
       , documentfiles = maybeToList file_id
       , documentsealedfiles = maybeToList sealed_file_id
       , documentstatus = case (status, error_text) of
           (DocumentError{}, Just text) -> DocumentError text
           (DocumentError{}, Nothing) -> DocumentError "document error"
           _ -> status
       , documenttype = documentType (simple_type, process)
       , documentfunctionality = functionality
       , documentctime = ctime
       , documentmtime = mtime
       , documentdaystosign = days_to_sign
       , documenttimeouttime = timeout_time
       , documentinvitetime = case invite_time of
           Nothing -> Nothing
           Just t -> Just (SignInfo t $ fromMaybe noIP invite_ip)
       , documentlog = dlog
       , documentinvitetext = invite_text
       , documentallowedidtypes = allowed_id_types
       , documentcancelationreason = cancelationreason
       , documentsharing = sharing
       , documentrejectioninfo = case (rejection_time, rejection_signatory_link_id, rejection_reason) of
           (Just t, Just sl, mr) -> Just (t, sl, fromMaybe "" mr)
           _ -> Nothing
       , documenttags = []
       , documentservice = service
       , documentdeleted = deleted
       , documentauthorattachments = []
       , documentui = DocumentUI mail_footer
       , documentregion = region
       , documentstatusclass = toEnum (status_class :: Int)
       } : acc

documentStatusClassExpression :: String
documentStatusClassExpression =
  "(COALESCE((SELECT min(" ++ statusClassCaseExpression ++ ")"
  ++         "  FROM signatory_links"
  ++         " WHERE signatory_links.document_id = documents.id AND ((signatory_links.roles&1)<>0)), 0))"

statusClassCaseExpression :: String
statusClassCaseExpression =
  "CASE "
  ++ " WHEN documents.status IN (1) THEN 0"                        -- (documentstatus==Preparation, SCDraft)
  ++ " WHEN documents.status IN (4,5,6,7,8) THEN 1"                -- (documentstatus==Canceled, SCCancelled)
  ++ " WHEN signatory_links.sign_time IS NOT NULL THEN 6"          -- (isJust maybesigninfo, SCSigned)
  ++ " WHEN signatory_links.seen_time IS NOT NULL THEN 5"          -- (isJust maybeseeninfo, SCOpened)
  ++ " WHEN signatory_links.read_invitation IS NOT NULL THEN 4"    -- (isJust maybereadinvite, SCRead)
  ++ " WHEN signatory_links.invitation_delivery_status = 2 THEN 1" -- (invitationdeliverystatus==Undelivered, SCCancelled)
  ++ " WHEN signatory_links.invitation_delivery_status = 1 THEN 3" -- (invitationdeliverystatus==Delivered, SCDelivered)
  ++ " ELSE 2"                                                     -- SCSent
  ++ " END"

signatoryLinksSelectors :: String
signatoryLinksSelectors = intercalate ", " 
  [ "signatory_links.id"
  , "signatory_links.document_id"
  , "signatory_links.user_id"
  , "signatory_links.company_id"
  , "signatory_links.fields"
  , "signatory_links.sign_order"
  , "signatory_links.token"
  , "signatory_links.sign_time"
  , "signatory_links.sign_ip"
  , "signatory_links.seen_time"
  , "signatory_links.seen_ip"
  , "signatory_links.read_invitation"
  , "signatory_links.invitation_delivery_status"
  , "signatory_links.signinfo_text"
  , "signatory_links.signinfo_signature"
  , "signatory_links.signinfo_certificate"
  , "signatory_links.signinfo_provider"
  , "signatory_links.signinfo_first_name_verified"
  , "signatory_links.signinfo_last_name_verified"
  , "signatory_links.signinfo_personal_number_verified"
  , "signatory_links.roles"
  , "signatory_links.csv_title"
  , "signatory_links.csv_contents"
  , "signatory_links.csv_signatory_index"
  , "signatory_links.deleted"
  , "signatory_links.really_deleted"

  , -- this is to fetch status class, so we can do sorting according to that class
    --  0 Draft - 1 Cancel - 2 Fall due - 3 Sent - 4 Opened - 5 Signed
    -- FIXME: we should really be using constants from Haskell, but this after some refactoring
    -- this has to stay a single string for now
    statusClassCaseExpression ++ " AS status_class"
  ]

selectSignatoryLinksSQL :: SQL
selectSignatoryLinksSQL = SQL ("SELECT "
  ++ signatoryLinksSelectors
  ++ ", signatory_attachments.file_id as sigfileid "
  ++ ", signatory_attachments.name as signame "
  ++ ", signatory_attachments.description as sigdesc "
  ++ " FROM (signatory_links "
  ++ " LEFT JOIN signatory_attachments "
  ++ " ON signatory_attachments.document_id = signatory_links.document_id "
  ++ " AND signatory_attachments.signatory_link_id = signatory_links.id) "
  ++ " JOIN documents "
  ++ " ON signatory_links.document_id = documents.id ") []

fetchSignatoryLinks :: DB (M.Map DocumentID [SignatoryLink])
fetchSignatoryLinks = do
  sigs <- foldDB decoder (nulldocid, [], M.empty)
  return $ (\(d, l, m) -> M.insertWith' (++) d l m) sigs
  where
    nulldocid = unsafeDocumentID $ -1
    decoder (docid, links, linksmap) slid document_id user_id company_id fields
     sign_order token sign_time sign_ip seen_time seen_ip read_invitation
     invitation_delivery_status signinfo_text signinfo_signature signinfo_certificate
     signinfo_provider signinfo_first_name_verified signinfo_last_name_verified
     signinfo_personal_number_verified roles csv_title csv_contents csv_signatory_index
     deleted really_deleted status_class
     safileid saname sadesc
      | docid == nulldocid                      = (document_id, [link], linksmap)
      | docid /= document_id                    = (document_id, [link], M.insertWith' (++) docid links linksmap)
      | signatorylinkid ($(head) links) == slid = (docid, addSigAtt ($(head) links) : $(tail) links, linksmap)
      | otherwise                               = (docid, link : links, linksmap)
      where
        addSigAtt l = l { signatoryattachments = sigAtt ++ signatoryattachments l }
        sigAtt = maybe [] (\name -> [SignatoryAttachment {
            signatoryattachmentfile = safileid
          , signatoryattachmentname = name
          , signatoryattachmentdescription = fromMaybe "" sadesc
          }]) saname
        link = SignatoryLink {
            signatorylinkid = slid
          , signatorydetails = SignatoryDetails {
              signatorysignorder = sign_order
            , signatoryfields = fields
          }
          , signatorymagichash = token
          , maybesignatory = user_id
          , maybecompany = company_id
          , maybesigninfo = SignInfo <$> sign_time <*> sign_ip
          , maybeseeninfo = SignInfo <$> seen_time <*> seen_ip
          , maybereadinvite = read_invitation
          , invitationdeliverystatus = invitation_delivery_status
          , signatorysignatureinfo = do -- Maybe Monad
              signinfo_text' <- signinfo_text
              signinfo_signature' <- signinfo_signature
              signinfo_certificate' <- signinfo_certificate
              signinfo_provider' <- signinfo_provider
              signinfo_first_name_verified' <- signinfo_first_name_verified
              signinfo_last_name_verified' <- signinfo_last_name_verified
              signinfo_personal_number_verified' <- signinfo_personal_number_verified
              return $ SignatureInfo {
                  signatureinfotext        = signinfo_text'
                , signatureinfosignature   = signinfo_signature'
                , signatureinfocertificate = signinfo_certificate'
                , signatureinfoprovider    = signinfo_provider'
                , signaturefstnameverified = signinfo_first_name_verified'
                , signaturelstnameverified = signinfo_last_name_verified'
                , signaturepersnumverified = signinfo_personal_number_verified'
                }
          , signatoryroles = roles
          , signatorylinkdeleted = deleted
          , signatorylinkreallydeleted = really_deleted
          , signatorylinkcsvupload =
              CSVUpload <$> csv_title <*> csv_contents <*> csv_signatory_index
          , signatoryattachments = sigAtt
          , signatorylinkstatusclass = toEnum (status_class :: Int)
          }

insertSignatoryLinkAsIs :: DocumentID -> SignatoryLink -> DB (Maybe SignatoryLink)
insertSignatoryLinkAsIs documentid link = do

  _ <- kRun $ mkSQL INSERT tableSignatoryLinks
           [ sql "document_id" documentid
           , sql "user_id" $ maybesignatory link
           , sql "roles" $ signatoryroles link
           , sql "company_id" $ maybecompany link
           , sql "token" $ signatorymagichash link
           , sql "fields" $ signatoryfields $ signatorydetails link
           , sql "sign_order"$ signatorysignorder $ signatorydetails link
           , sql "sign_time" $ signtime `fmap` maybesigninfo link
           , sql "sign_ip" $ signipnumber `fmap` maybesigninfo link
           , sql "seen_time" $ signtime `fmap` maybeseeninfo link
           , sql "seen_ip" $ signipnumber `fmap` maybeseeninfo link
           , sql "read_invitation" $ maybereadinvite link
           , sql "invitation_delivery_status" $ invitationdeliverystatus link
           , sql "signinfo_text" $ signatureinfotext `fmap` signatorysignatureinfo link
           , sql "signinfo_signature" $ signatureinfosignature `fmap` signatorysignatureinfo link
           , sql "signinfo_certificate" $ signatureinfocertificate `fmap` signatorysignatureinfo link
           , sql "signinfo_provider" $ signatureinfoprovider `fmap` signatorysignatureinfo link
           , sql "signinfo_first_name_verified" $ signaturefstnameverified `fmap` signatorysignatureinfo link
           , sql "signinfo_last_name_verified" $ signaturelstnameverified `fmap` signatorysignatureinfo link
           , sql "signinfo_personal_number_verified" $ signaturepersnumverified `fmap` signatorysignatureinfo link
           , sql "csv_title" $ csvtitle `fmap` signatorylinkcsvupload link
           , sql "csv_contents" $ csvcontents `fmap` signatorylinkcsvupload link
           , sql "csv_signatory_index" $ csvsignatoryindex `fmap` signatorylinkcsvupload link
           , sql "deleted" $ signatorylinkdeleted link
           , sql "really_deleted" $ signatorylinkreallydeleted link
           ] <++> SQL " RETURNING id" []

  slids <- foldDB (\acc slid -> slid : acc) []

  _ <- kRun $ selectSignatoryLinksSQL <++> SQL "WHERE signatory_links.id = ? AND signatory_links.document_id = ? ORDER BY internal_insert_order DESC" 
       [$(head) slids, toSql documentid]

  msiglink <- fetchSignatoryLinks
              >>= oneObjectReturnedGuard . concatMap snd . M.toList

  case msiglink of
    Nothing -> return Nothing
    Just siglink -> do
      msigattaches <- mapM (insertSignatoryAttachmentAsIs documentid (signatorylinkid siglink)) (signatoryattachments link)
      if any isNothing msigattaches
        then return Nothing
        else do
          let newsiglink = link { signatoryattachments = catMaybes msigattaches }
          return (Just newsiglink)

signatoryAttachmentsSelectors :: String
signatoryAttachmentsSelectors = intercalate ", " [
    "document_id"
  , "signatory_link_id"
  , "file_id"
  , "name"
  , "description"
  ]

fetchSignatoryAttachments :: DB (M.Map (DocumentID, SignatoryLinkID) [SignatoryAttachment])
fetchSignatoryAttachments = foldDB decoder M.empty
  where decoder acc document_id signatory_link_id file_id name description =
            M.insertWith' (++) (document_id, signatory_link_id) [SignatoryAttachment {
                                                                   signatoryattachmentfile = file_id
                                                                 , signatoryattachmentname = name
                                                                 , signatoryattachmentdescription = description
                                                                 }] acc




migrateDocumentTagsFromJSONToTable :: DB ()
migrateDocumentTagsFromJSONToTable = do
  _ <- kRun $ SQL "SELECT id, tags FROM documents WHERE tags <> '' AND tags <> '[]'" [];
  let decoder :: [(DocumentID,[DocumentTag])] -> DocumentID -> [DocumentTag] -> [(DocumentID,[DocumentTag])]
      decoder acc docid tags = (docid,tags) : acc
  tagsmap <- foldDB decoder []
  flip mapM_ tagsmap $ \(docid, tags) -> do
    mapM_ (insertDocumentTagAsIs docid) tags
    _ <- kRun $ selectDocumentTagsSQL <++> SQL "WHERE document_tags.document_id = ?" [toSql docid]
    newtagsmap <- fetchDocumentTags
    let newtags = snd $ $(head) $ M.toList newtagsmap
    when (sort newtags /= sort tags) $
         error $ "migrateDocumentTagsFromJSONToTable failed to preserve tags:\nold tags =\n" ++ show (sort tags) ++ "\nnew tags =\n" ++ show (sort newtags)

    _ <- kRun $ SQL "UPDATE documents SET tags = '[]' WHERE id = ?" [toSql docid]
    return ()
  return ()

documentTagsSelectors :: String
documentTagsSelectors = intercalate ", " [
    "document_id"
  , "name"
  , "value"
  ]

selectDocumentTagsSQL :: SQL
selectDocumentTagsSQL = SQL ("SELECT "
  ++ documentTagsSelectors
  ++ " FROM document_tags ") []

fetchDocumentTags :: DB (M.Map DocumentID [DocumentTag])
fetchDocumentTags = foldDB decoder M.empty
  where
    decoder acc document_id name value =
      M.insertWith' (++) document_id
         [DocumentTag name value] acc

insertDocumentTagAsIs :: DocumentID -> DocumentTag -> DB (Maybe DocumentTag)
insertDocumentTagAsIs documentid tag = do
  _ <- kRun $ mkSQL INSERT tableDocumentTags
       [ sql "name" $ tagname tag
       , sql "value" $ tagvalue tag
       , sql "document_id" documentid
       ] <++> SQL ("RETURNING " ++ documentTagsSelectors) []

  fetchDocumentTags
    >>= oneObjectReturnedGuard . concatMap snd . M.toList












authorAttachmentsSelectors :: String
authorAttachmentsSelectors = intercalate ", " [
    "document_id"
  , "file_id"
  ]

selectAuthorAttachmentsSQL :: SQL
selectAuthorAttachmentsSQL = SQL ("SELECT "
  ++ authorAttachmentsSelectors
  ++ " FROM author_attachments ") []

fetchAuthorAttachments :: DB (M.Map DocumentID [AuthorAttachment])
fetchAuthorAttachments = foldDB decoder M.empty
  where
    decoder acc document_id file_id =
      M.insertWith' (++) document_id [AuthorAttachment {
        authorattachmentfile = file_id
      }] acc

insertAuthorAttachmentAsIs :: DocumentID -> AuthorAttachment -> DB (Maybe AuthorAttachment)
insertAuthorAttachmentAsIs documentid attach = do
  _ <- kRun $ mkSQL INSERT tableAuthorAttachments [
      sql "file_id" $ authorattachmentfile attach
    , sql "document_id" documentid
    ] <++> SQL ("RETURNING " ++ authorAttachmentsSelectors) []

  fetchAuthorAttachments
    >>= oneObjectReturnedGuard . concatMap snd . M.toList

insertSignatoryAttachmentAsIs :: DocumentID -> SignatoryLinkID -> SignatoryAttachment -> DB (Maybe SignatoryAttachment)
insertSignatoryAttachmentAsIs did slid SignatoryAttachment {..} = do
  _ <- kRun $ mkSQL INSERT tableSignatoryAttachments [
        sql "file_id" signatoryattachmentfile
       , sql "name" signatoryattachmentname
       , sql "description" signatoryattachmentdescription
       , sql "document_id" did
       , sql "signatory_link_id" slid
       ] <++> SQL ("RETURNING " ++ signatoryAttachmentsSelectors) []

  fetchSignatoryAttachments
    >>= oneObjectReturnedGuard . concatMap snd . M.toList

insertDocumentAsIs :: Document -> DB (Maybe Document)
insertDocumentAsIs document = do
    let Document { documenttitle
                 , documentsignatorylinks
                 , documentfiles
                 , documentsealedfiles
                 , documentstatus
                 , documenttype
                 , documentfunctionality
                 , documentctime
                 , documentmtime
                 , documentdaystosign
                 , documenttimeouttime
                 , documentinvitetime
                 , documentlog
                 , documentinvitetext
                 , documentallowedidtypes
                 , documentcancelationreason
                 , documentsharing
                 , documentrejectioninfo
                 , documenttags
                 , documentservice
                 , documentdeleted
                 , documentauthorattachments
                 , documentui
                 , documentregion
                 } = document
        process = toDocumentProcess documenttype

    _ <- kRun $ mkSQL INSERT tableDocuments [
        sql "title" documenttitle
      , sql "file_id" $ listToMaybe documentfiles
      , sql "sealed_file_id" $ listToMaybe documentsealedfiles
      , sql "status" documentstatus
      , sql "error_text" $ case documentstatus of
          DocumentError msg -> toSql msg
          _ -> SqlNull
      , sql "type" documenttype
      , sql "process" process
      , sql "functionality" documentfunctionality
      , sql "ctime" documentctime
      , sql "mtime" documentmtime
      , sql "days_to_sign" documentdaystosign
      , sql "timeout_time" documenttimeouttime
      , sql "invite_time" $ signtime `fmap` documentinvitetime
      , sql "invite_ip" (fmap signipnumber documentinvitetime)
      , sql "invite_text" documentinvitetext
      , sql "log" documentlog
      , sql "allowed_id_types" documentallowedidtypes
      , sql "cancelation_reason" documentcancelationreason
      , sql "rejection_time" $ fst3 `fmap` documentrejectioninfo
      , sql "rejection_signatory_link_id" $ snd3 `fmap` documentrejectioninfo
      , sql "rejection_reason" $ thd3 `fmap` documentrejectioninfo
      , sql "service_id" documentservice
      , sql "deleted" documentdeleted
      , sql "mail_footer" $ documentmailfooter $ documentui -- should go into separate table?
      , sql "region" documentregion
      , sql "sharing" documentsharing
      ] <++> SQL ("RETURNING " ++ documentsSelectors) []

    mdoc <- fetchDocuments >>= oneObjectReturnedGuard
    case mdoc of
      Nothing -> return Nothing
      Just doc -> do
        mlinks <- mapM (insertSignatoryLinkAsIs (documentid doc)) documentsignatorylinks
        mauthorattachments <- mapM (insertAuthorAttachmentAsIs (documentid doc)) documentauthorattachments
        mtags <- mapM (insertDocumentTagAsIs (documentid doc)) documenttags
        if any isNothing mlinks || any isNothing mauthorattachments || any isNothing mtags
         then return Nothing
         else do
          let newdocument = doc { documentsignatorylinks    = catMaybes mlinks
                                , documentauthorattachments = catMaybes mauthorattachments
                                , documenttags              = catMaybes mtags
                                }
          assertEqualDocuments document newdocument
          return (Just newdocument)

insertNewDocument :: Document -> DB Document
insertNewDocument doc = do
  now <- getMinutesTime
  let docWithTime = doc {documentmtime  = now, documentctime = now}
  newdoc <- insertDocumentAsIs docWithTime
  case newdoc of
    Just d -> return d
    Nothing -> error "insertNewDocument failed for some reason"


-- Create new document based on existing one
newFromDocument :: (Document -> Document) -> DocumentID -> DB (Either String Document)
newFromDocument f docid = do
  mdoc <- dbQuery $ GetDocumentByDocumentID docid
  case mdoc of
      Just doc -> fmap Right $ insertNewDocument $ f doc
      Nothing -> return $ Left $ "Document " ++ show docid ++ " does not exist"

{- |
    The existance of this function is wrong.  What it means is that storing
    maybesignatory and maybecompany on the signatory links is the wrong way of doing it,
    and there should be something else for hooking accounts to sig links that doesn't
    involve editing all the docs as a user moves between private and company accounts.
-}
data Actor a => AdminOnlySaveForUser a = AdminOnlySaveForUser DocumentID User a

instance Actor a => DBUpdate (AdminOnlySaveForUser a) (Either String Document) where
  dbUpdate (AdminOnlySaveForUser did user actor) = do
    r <- kRun $ mkSQL UPDATE tableSignatoryLinks [sql "company_id" $ usercompany user]
      <++> SQL "WHERE document_id = ? AND user_id = ? " [
        toSql did
      , toSql $ userid user
      ]
    when_ (r == 1) $
      dbUpdate $ InsertEvidenceEvent
      AdminOnlySaveForUserEvidence
      ("Document saved for user with email \"" ++ getEmail user ++ "\" by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "AdminOnlySaveForUser" r did

data Actor a => ArchiveDocument a = ArchiveDocument User DocumentID a
instance Actor a => DBUpdate (ArchiveDocument a) (Either String Document) where
  dbUpdate (ArchiveDocument user did actor) = do
    r <- case (usercompany user, useriscompanyadmin user) of
      (Just cid, True) -> updateArchivableDoc $ SQL "WHERE company_id = ?" [toSql cid]
      _ -> updateArchivableDoc $ SQL "WHERE user_id = ?" [toSql $ userid user]
    -- a supervisor could delete both their own and another subaccount's links
    -- on the same document, so this would mean the sig link count affected
    -- is more than 1. see bug 1195.
    let fudgedr = if r==0 then 0 else 1

    let forstr = case (usercompany user, useriscompanyadmin user) of
          (Just _, True) -> "company with admin email \"" ++ getEmail user ++ "\""
          _ -> "user with email \"" ++ getEmail user ++ "\""
    when_ (fudgedr == 1) $
      dbUpdate $ InsertEvidenceEvent
      ArchiveDocumentEvidence
      ("Moved document to rubbish bin for " ++ forstr ++ " by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "ArchiveDocument" fudgedr did

    where
      updateArchivableDoc whereClause = kRun $ mconcat [
          mkSQL UPDATE tableSignatoryLinks [sql "deleted" True]
        , whereClause
        , SQL " AND document_id = ? AND EXISTS (SELECT 1 FROM documents WHERE id = ? AND status <> ?)" [
            toSql did
          , toSql did
          , toSql Pending
          ]
        ]

data Actor a => AttachCSVUpload a = AttachCSVUpload DocumentID SignatoryLinkID CSVUpload a
instance Actor a => DBUpdate (AttachCSVUpload a) (Either String Document) where
  dbUpdate (AttachCSVUpload did slid csvupload actor) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID did
    case mdocument of
      Nothing -> return $ Left $ "Cannot AttachCSVUpload document " ++ show did ++ " because it does not exist"
      Just document -> do
        case documentstatus document of
          Preparation -> do
            r <- kRun $ mkSQL UPDATE tableSignatoryLinks [
                sql "csv_title" $ csvtitle csvupload
              , sql "csv_signatory_index" $ csvsignatoryindex csvupload
              , sql "csv_contents" $ csvcontents csvupload
              ] <++> SQL "WHERE document_id = ? AND signatory_links.id = ? AND deleted = FALSE AND ((roles & ?) = 0)" [
                toSql did
              , toSql slid
              , toSql [SignatoryAuthor]
              ]                     
            when_ (r == 1) $
              dbUpdate $ InsertEvidenceEvent
              AttachCSVUploadEvidence
              ("Attached CSV (" ++ csvtitle csvupload ++ ") to document by " ++ actorWho actor ++ ".")
              (Just did)
              actor
            getOneDocumentAffected "AttachCSVUpload" r did
          _ -> return $ Left $ "Document #" ++ show documentid ++ " is in " ++ show (documentstatus document) ++ " state, must be Preparation"

data Actor a => AttachFile a = AttachFile DocumentID FileID a
instance Actor a => DBUpdate (AttachFile a) (Either String Document) where
  dbUpdate (AttachFile did fid a) = do
    let time = actorTime a
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "mtime" time
      , sql "file_id" $ fid
      , sqlLog time $ "Attached main file " ++ show fid
      ] <++> SQL "WHERE id = ? AND status = ?" [toSql did, toSql Preparation]
    when_ (r == 1) $
      dbUpdate $ InsertEvidenceEvent
        AttachFileEvidence
        ("Uploaded main PDF by " ++ actorWho a ++ ".")
        (Just did)
        a
    getOneDocumentAffected "AttachFile" r did

data Actor a => AttachSealedFile a = AttachSealedFile DocumentID FileID a
instance Actor a => DBUpdate (AttachSealedFile a) (Either String Document) where
  dbUpdate (AttachSealedFile did fid actor) = do
    let time = actorTime actor
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "mtime" time
      , sql "sealed_file_id" fid
      , sqlLog time $ "Attached sealed file " ++ show fid
      ] <++> SQL "WHERE id = ? AND status = ?" [toSql did, toSql Closed]
    when_ (r == 1) $ 
      dbUpdate $ InsertEvidenceEvent
      AttachSealedFileEvidence
      ("Sealed file attached by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "AttachSealedFile" r did

data Actor a => CancelDocument a = CancelDocument DocumentID CancelationReason a
instance Actor a => DBUpdate (CancelDocument a) (Either String Document) where
  dbUpdate (CancelDocument did reason actor) = do
    let mtime = actorTime actor
    mdocument <- dbQuery $ GetDocumentByDocumentID did
    case mdocument of
      Nothing -> return $ Left $ "Cannot CancelDocument document " ++ show did ++ " because it does not exist"
      Just document ->
        case checkCancelDocument document of
          [] -> do
            let ipaddress = fromMaybe noIP $ actorIP actor
            r <- kRun $ mkSQL UPDATE tableDocuments [
                sql "status" Canceled
              , sql "mtime" mtime
              , sql "cancelation_reason" $ reason
              , sqlLog mtime $ "Document canceled from " ++ show ipaddress
              ] <++> SQL "WHERE id = ? AND type = ?" [
                toSql did
              , toSql $ Signable undefined
              ]
            when_ (r == 1) $ 
              case reason of
                ManualCancel -> 
                  dbUpdate $ InsertEvidenceEvent
                  CancelDocumentEvidence
                  ("The document was canceled by " ++ actorWho actor ++ ".")
                  (Just did)
                  actor

                ELegDataMismatch _ sid fn ln num -> do
                  let Just sl = getSigLinkFor document sid
                      trips = [("first name",      getFirstName      sl, fn)
                              ,("last name",       getLastName       sl, ln)
                              ,("personal number", getPersonalNumber sl, num)]
                      uneql = filter (\(_,a,b)->a/=b) trips
                      msg = intercalate "; " $ map (\(f,s,e)->f ++ " from document was \"" ++ s ++ "\" but from e-legitimation was \"" ++ e ++ "\"") uneql
                  dbUpdate $ InsertEvidenceEvent
                    CancelDocumenElegEvidence
                    ("The document was canceled due to a mismatch with e-legitimation data by " ++ actorWho actor ++ ". Reason: " ++ msg ++ ".")
                    (Just did)
                    actor

            getOneDocumentAffected "CancelDocument" r did
          s -> return $ Left $ "Cannot CancelDocument document " ++ show did ++ " because " ++ concat s

data Actor a => ChangeMainfile a = ChangeMainfile DocumentID FileID a
instance Actor a=> DBUpdate (ChangeMainfile a) (Either String Document) where
  dbUpdate (ChangeMainfile did fid actor) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID did
    case mdocument of
      Nothing -> return $ Left $ "Cannot ChangeMainfile document " ++ show did ++ " because it does not exist"
      Just document -> do
        let fieldname = if (documentstatus document == Closed || allHadSigned document)
                        then "sealed_file_id"
                        else "file_id"
        r <- kRun $ mkSQL UPDATE tableDocuments [sql fieldname $ fid]
          <++> SQL "WHERE id = ?" [toSql did]
        when_ (r == 1) $
          dbUpdate $ InsertEvidenceEvent
            ChangeMainfileEvidence
            ("Main file was changed by " ++ actorWho actor ++ ".")
            (Just did)
            actor
        getOneDocumentAffected "ChangeMainfile" r did
    where
        allHadSigned doc = all (hasSigned ||^ (not . isSignatory)) $ documentsignatorylinks doc

data Actor a => ChangeSignatoryEmailWhenUndelivered a = ChangeSignatoryEmailWhenUndelivered DocumentID SignatoryLinkID (Maybe User) String a
instance Actor a => DBUpdate (ChangeSignatoryEmailWhenUndelivered a) (Either String Document) where
  dbUpdate (ChangeSignatoryEmailWhenUndelivered did slid muser email actor) = do
    Just doc <- dbQuery $ GetDocumentByDocumentID did
    let setEmail signatoryfields =
         map (\sf -> case sfType sf of
                 EmailFT -> sf { sfValue = email }
                 _       -> sf) signatoryfields

    let Just sl = getSigLinkFor doc slid
        oldemail = getEmail sl
    r <- kRun $ mkSQL UPDATE tableSignatoryLinks [
        sql "invitation_delivery_status" Unknown
      , sql "fields" $ setEmail $ signatoryfields $ signatorydetails sl
      , sql "user_id" $ fmap userid muser
      , sql "company_id" $ muser >>= usercompany
      ] <++> SQL "WHERE EXISTS (SELECT 1 FROM documents WHERE documents.id = signatory_links.document_id AND documents.status = ?) AND id = ?" [
        toSql Pending
      , toSql slid
      ]
    when_ (r == 1) $ 
      dbUpdate $ InsertEvidenceEvent
      ChangeSignatoryEmailWhenUndeliveredEvidence
      ("Changed the email address for signatory from \"" ++ oldemail ++ "\" to \"" ++ email ++ "\" by " ++ actorWho actor ++ ".")
      (Just did)
      actor
   
    getOneDocumentAffected "ChangeSignatoryEmailWhenUndelivered" r did

data Actor a => PreparationToPending a = PreparationToPending DocumentID a
instance Actor a => DBUpdate (PreparationToPending a) (Either String Document) where
  dbUpdate (PreparationToPending docid actor) = do
    let time = actorTime actor
    mdocument <- dbQuery $ GetDocumentByDocumentID docid
    case mdocument of
      Nothing -> return $ Left $ "Cannot PreparationToPending document " ++ show docid ++ " because it does not exist"
      Just document ->
        case checkPreparationToPending document of
          [] -> do
            let mtt = (\days -> (days * 24 *60) `minutesAfter` time) <$> documentdaystosign document
                t = case mtt of
                  Just tt -> " Timeout time set to " ++ formatMinutesTimeUTC tt ++ " UTC."
                  _ -> ""
            r <- kRun $ mkSQL UPDATE tableDocuments [
                sql "status" Pending
              , sql "mtime" time
              , sql "timeout_time" $ (\days -> (days * 24 * 60) `minutesAfter` time)
                  <$> documentdaystosign document
              , sqlLog time "Document put into Pending state"
              ] <++> SQL "WHERE id = ? AND type = ?" [
                toSql docid
              , toSql $ Signable undefined
              ]
            when_ (r == 1) $
              dbUpdate $ InsertEvidenceEvent
              PreparationToPendingEvidence
              ("Document was put into Pending state by " ++ actorWho actor ++ "." ++ t)
              (Just docid)
              actor
            getOneDocumentAffected "PreparationToPending" r docid
          s -> return $ Left $ "Cannot PreparationToPending document " ++ show docid ++ " because " ++ concat s

data Actor a => CloseDocument a = CloseDocument DocumentID a
instance Actor a => DBUpdate (CloseDocument a) (Either String Document) where
  dbUpdate (CloseDocument docid actor) = do
    let time = actorTime actor
        ipaddress = fromMaybe noIP $ actorIP actor
    mdocument <- dbQuery $ GetDocumentByDocumentID docid
    case mdocument of
      Nothing -> return $ Left $ "Cannot Close document " ++ show docid ++ " because it does not exist"
      Just document ->
        case checkCloseDocument document of
          [] -> do
            r <- kRun $ mkSQL UPDATE tableDocuments [
                sql "status" Closed
              , sql "mtime" time
              , sqlLog time $ "Document closed from " ++ show ipaddress
              ] <++> SQL "WHERE id = ? AND type = ?" [
                toSql docid
              , toSql $ Signable undefined
              ]
            when_ (r == 1) $
              dbUpdate $ InsertEvidenceEvent 
              CloseDocumentEvidence
              ("The document was closed by " ++ actorWho actor ++ " after all signatories had signed.")
              (Just docid)
              actor
            getOneDocumentAffected "CloseDocument" r docid
          s -> return $ Left $ "Cannot CloseDocument " ++ show docid ++ " because " ++ concat s

data Actor a => DeleteSigAttachment a = DeleteSigAttachment DocumentID SignatoryLinkID FileID a
instance Actor a => DBUpdate (DeleteSigAttachment a) (Either String Document) where
  dbUpdate (DeleteSigAttachment did slid fid actor) = do
    r <- kRun $ mkSQL UPDATE tableSignatoryAttachments [sql "file_id" SqlNull]
      <++> SQL "WHERE document_id = ? AND file_id = ? AND signatory_link_id = ?" [
        toSql did
      , toSql fid
      , toSql slid
      ]
    when_ (r == 1) $
      dbUpdate $ InsertEvidenceEvent
      DeleteSigAttachmentEvidence
      ("Signatory attachment for signatory was deleted by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "DeleteSigAttachment" r did

data Actor a => DocumentFromSignatoryData a = DocumentFromSignatoryData DocumentID String String String String String String [String] a
instance Actor a => DBUpdate (DocumentFromSignatoryData a) (Either String Document) where
  dbUpdate (DocumentFromSignatoryData docid fstname sndname email company personalnumber companynumber fieldvalues actor) = do
    mdoc <- dbQuery $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Left $ "In DocumentFromSignatoryData: Document does not exist for id: " ++ show docid
      Just doc -> do
        mhs <- mapM (\_ -> random) (documentsignatorylinks doc)
        ed <- newFromDocument (toNewDoc mhs) docid    
        when_ (isRight ed) $ 
          let Right d = ed 
          in do
            copyEvidenceLogToNewDocument docid (documentid d)
            _ <- dbUpdate $ InsertEvidenceEvent
                 AuthorUsesCSVEvidence
                 ("Document created from CSV file by " ++ actorWho actor ++ ".")
                 (Just $ documentid d)
                 actor
            dbUpdate $ InsertEvidenceEvent
              AuthorUsesCSVEvidence
              ("Documents created from this document using CSV file by " ++ actorWho actor ++ ".")
              (Just $ docid)
              actor
        return ed
   where
     now = actorTime actor 
     toNewDoc :: [MagicHash] -> Document -> Document
     toNewDoc mhs d = d { documentsignatorylinks = zipWith toNewSigLink mhs (documentsignatorylinks d)
                       , documenttype = newDocType $ documenttype d
                       , documentctime = now
                       , documentmtime = now
                       }
     newDocType :: DocumentType -> DocumentType
     newDocType (Signable p) = Signable p
     newDocType (Template p) = Signable p
     newDocType dt = dt
     toNewSigLink :: MagicHash -> SignatoryLink -> SignatoryLink
     toNewSigLink mh sl
         | isJust (signatorylinkcsvupload sl) = (pumpData sl) { signatorylinkcsvupload = Nothing, signatorymagichash = mh }
         | otherwise = sl { signatorymagichash = mh }
     pumpData :: SignatoryLink -> SignatoryLink
     pumpData siglink = replaceSignatoryData siglink fstname sndname email company personalnumber companynumber fieldvalues

data Actor a => ErrorDocument a = ErrorDocument DocumentID String a
instance Actor a => DBUpdate (ErrorDocument a) (Either String Document) where
  dbUpdate (ErrorDocument docid errmsg actor) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID docid
    case mdocument of
      Nothing -> return $ Left $ "Cannot ErrorDocument document " ++ show docid ++ " because it does not exist"
      Just _ ->
        case [] of
          [] -> do
            r <- kRun $ mkSQL UPDATE tableDocuments [
                sql "status" $ DocumentError errmsg
              , sql "error_text" errmsg
              ] <++> SQL "WHERE id = ?" [toSql docid]
            when_ (r == 1) $
              dbUpdate $ InsertEvidenceEvent
              ErrorDocumentEvidence
              ("The following error occured on the document: " ++ errmsg)
              (Just docid)
              actor
            getOneDocumentAffected "ErrorDocument" r docid
          s -> return $ Left $ "Cannot ErrorDocument document " ++ show docid ++ " because " ++ concat s

selectDocuments :: SQL -> DB [Document]
selectDocuments query = do
    _ <- kRun $ SQL "CREATE TEMP TABLE docs AS " [] <++> query

    _ <- kRun $ SQL "SELECT * FROM docs" []
    docs <- reverse <$> fetchDocuments

    _ <- kRun $ selectSignatoryLinksSQL <++> SQL "WHERE EXISTS (SELECT 1 FROM docs WHERE signatory_links.document_id = docs.id) ORDER BY document_id DESC, internal_insert_order DESC" []
    sls <- fetchSignatoryLinks

    _ <- kRun $ selectAuthorAttachmentsSQL <++> SQL "WHERE EXISTS (SELECT 1 FROM docs WHERE author_attachments.document_id = docs.id) ORDER BY document_id DESC" []
    ats <- fetchAuthorAttachments

    _ <- kRun $ selectDocumentTagsSQL <++> SQL "WHERE EXISTS (SELECT 1 FROM docs WHERE document_tags.document_id = docs.id)" []
    tags <- fetchDocumentTags

    kRunRaw "DROP TABLE docs"

    let fill doc = doc
                   { documentsignatorylinks       = M.findWithDefault [] (documentid doc) sls
                   , documentauthorattachments    = M.findWithDefault [] (documentid doc) ats
                   , documenttags                 = M.findWithDefault [] (documentid doc) tags
                   }

    return $ map fill docs

data GetDocumentByDocumentID = GetDocumentByDocumentID DocumentID
instance DBQuery GetDocumentByDocumentID (Maybe Document) where
  dbQuery (GetDocumentByDocumentID did) = do
    selectDocuments (selectDocumentsSQL
      <++> SQL "WHERE id = ? AND deleted = FALSE" [toSql did])
      >>= oneObjectReturnedGuard

data GetDocumentsByService = GetDocumentsByService (Maybe ServiceID)
instance DBQuery GetDocumentsByService [Document] where
  dbQuery (GetDocumentsByService msid) =
    dbQuery (GetDocuments [DocumentsOfService msid] [] [Asc DocumentOrderByMTime] (DocumentPagination 0 maxBound))

-- | GetDocuments is central switch for documents list queries.
--
-- GetDocuments domains filters sorting pagination
--
-- * domains are connected with OR, so documents falling into ANY of domains will be returned
-- * filters weed out documents from domains, are connected with AND so a document must pass through ALL filters
-- * sortings returns documents in order
-- * pagination is a place to put OFFSET and LIMIT values
--
-- GetDocuments returns documents in proper order, no reverse is needed.
--
data GetDocuments = GetDocuments [DocumentDomain] [DocumentFilter] [AscDesc DocumentOrderBy] DocumentPagination
instance DBQuery GetDocuments [Document] where
  dbQuery (GetDocuments domains filters orderbys pagination) = do
    selectDocuments $ mconcat
      [ selectDocumentsSQL
      , SQL "WHERE EXISTS (SELECT 1 FROM signatory_links WHERE documents.id = signatory_links.document_id AND " []
      , SQL "(" []
      , sqlConcatOR (map documentDomainToSQL domains)
      , SQL ")" []
      , if not (null filters)
        then SQL " AND " [] `mappend` sqlConcatAND (map documentFilterToSQL filters)
        else SQL "" []
      , SQL ")" []
      , if not (null orderbys)
        then SQL " ORDER BY " [] `mappend` sqlConcatComma (map documentOrderByAscDescToSQL orderbys)
        else SQL "" []
      , SQL (" OFFSET " ++ show (documentOffset pagination) ++ " LIMIT " ++ show (documentLimit pagination)) []
      ]

data GetDocumentsCount = GetDocumentsCount [DocumentDomain] [DocumentFilter]
instance DBQuery GetDocumentsCount Int where
  dbQuery (GetDocumentsCount domains filters) = $(fromJust) <$> getOne (mconcat
    [ SQL "SELECT count(*) FROM documents " []
    , SQL "WHERE EXISTS (SELECT 1 FROM signatory_links WHERE documents.id = signatory_links.document_id AND " []
    , SQL "(" []
    , sqlConcatOR (map documentDomainToSQL domains)
    , SQL ")" []
    , if not (null filters)
      then SQL " AND " [] `mappend` sqlConcatAND (map documentFilterToSQL filters)
      else SQL "" []
    , SQL ")" []
    ])

{- |
    Fetches documents by company with filtering by tags, edate, and status.
    this won't return documents that have been deleted (so ones
    that would appear in the recycle bin//trash can.)  It also makes sure to respect the sign order in
    cases where the company is linked via a signatory that hasn't yet been activated.

    Filters
    ----------------------------
    Service must match
    CompanyID must match Author
    Author must not be deleted
    All DocumentTags must be present and match (currently still done in Haskell)
    If isJust stime, the last change on the document must be greater than or equal to stime
    if isJust ftime, the last change on the document must be less than or equal to ftime
    if isJust statuses, the document status must be element of statuses
-}
data GetDocumentsByCompanyWithFiltering = GetDocumentsByCompanyWithFiltering CompanyID [DocumentFilter]
instance DBQuery GetDocumentsByCompanyWithFiltering [Document] where
  dbQuery (GetDocumentsByCompanyWithFiltering companyid filters) =
    dbQuery (GetDocuments [DocumentsOfCompany companyid] filters [Asc DocumentOrderByMTime] (DocumentPagination 0 maxBound))

data GetDeletedDocumentsByUser = GetDeletedDocumentsByUser UserID
instance DBQuery GetDeletedDocumentsByUser [Document] where
  dbQuery (GetDeletedDocumentsByUser uid) =
    dbQuery (GetDocuments [DocumentsForSignatoryDeleteValue uid True] [] [Asc DocumentOrderByMTime] (DocumentPagination 0 maxBound))

{- |
    All documents authored by the user that have never been deleted.
-}
data GetDocumentsByAuthor = GetDocumentsByAuthor UserID
instance DBQuery GetDocumentsByAuthor [Document] where
  dbQuery (GetDocumentsByAuthor uid) =
    dbQuery (GetDocuments [DocumentsOfAuthor uid, TemplatesOfAuthor uid] [] [Asc DocumentOrderByMTime] (DocumentPagination 0 maxBound))

data GetAttachmentsByAuthor = GetAttachmentsByAuthor UserID
instance DBQuery GetAttachmentsByAuthor [Document] where
  dbQuery (GetAttachmentsByAuthor uid) =
    dbQuery (GetDocuments [AttachmentsOfAuthorDeleteValue uid False] [] [Asc DocumentOrderByMTime] (DocumentPagination 0 maxBound))

data GetTemplatesByAuthor = GetTemplatesByAuthor UserID
instance DBQuery GetTemplatesByAuthor [Document] where
  dbQuery (GetTemplatesByAuthor uid) = 
    dbQuery (GetDocuments [TemplatesOfAuthor uid] [] [Asc DocumentOrderByMTime] (DocumentPagination 0 maxBound))

data GetAvailableTemplates = GetAvailableTemplates UserID [DocumentProcess]
instance DBQuery GetAvailableTemplates [Document] where
  dbQuery (GetAvailableTemplates uid processes) =
    dbQuery (GetDocuments [TemplatesOfAuthor uid, TemplatesSharedInUsersCompany uid]
                            [DocumentFilterByProcess processes]
                            [Asc DocumentOrderByMTime]
                            (DocumentPagination 0 maxBound))

{- |
    All documents where the user is a signatory that are not deleted.  An author is a type
    of signatory, so authored documents are included too.
    This also filters so that documents where a user is a signatory, but that signatory
    has not yet been activated according to the document's sign order, are excluded.
-}
data GetDocumentsBySignatory = GetDocumentsBySignatory [DocumentProcess] UserID
instance DBQuery GetDocumentsBySignatory [Document] where
  dbQuery (GetDocumentsBySignatory processes uid) =
    dbQuery (GetDocuments [DocumentsForSignatory uid] [DocumentFilterByProcess processes] [Asc DocumentOrderByMTime] (DocumentPagination 0 maxBound))

data GetTimeoutedButPendingDocuments = GetTimeoutedButPendingDocuments MinutesTime
instance DBQuery GetTimeoutedButPendingDocuments [Document] where
  dbQuery (GetTimeoutedButPendingDocuments mtime) = do
    selectDocuments $ selectDocumentsSQL
      <++> SQL "WHERE status = ? AND timeout_time IS NOT NULL AND timeout_time < ?" [
        toSql Pending
      , toSql mtime
      ]

data Actor a => MarkDocumentSeen a = MarkDocumentSeen DocumentID SignatoryLinkID MagicHash a
instance Actor a => DBUpdate (MarkDocumentSeen a) (Either String Document) where
  dbUpdate (MarkDocumentSeen did slid mh actor) = do
    -- have to make sure slid and mh match to record log; sorry for inefficiency -EN
    mdoc <- dbQuery $ GetDocumentByDocumentID did
    case mdoc of
      Nothing -> return $ Left $ "document does not exist with id " ++ show did
      Just doc -> case (getSigLinkFor doc (slid, mh)) of
        Nothing -> return $ Left $ "signatory link id and magic hash do not match! documentid: " ++ show did ++ " slid: " ++ show slid ++ " mh: " ++ show mh
        Just _ -> do
          let time = actorTime actor
              ipnumber = fromMaybe noIP $ actorIP actor
              txt = case actorIP actor of
                Just _ ->
                  "Document viewed by " ++ actorWho actor ++ "."
                Nothing ->
                  "Marking document seen for signatory with id " ++ show slid ++ " by " ++ actorWho actor ++ "."

          r <- kRun $ mkSQL UPDATE tableSignatoryLinks [
            sql "seen_time" time
            , sql "seen_ip" ipnumber
            ] <++> SQL "WHERE id = ? AND document_id = ? AND token = ? AND seen_time IS NULL AND sign_time IS NULL AND EXISTS (SELECT 1 FROM documents WHERE id = ? AND type = ? AND status <> ? AND status <> ?)" [
            toSql slid
            
            , toSql did
            , toSql mh
            , toSql did
            , toSql $ Signable undefined
            , toSql Preparation
            , toSql Closed
            ]
                         
          -- it's okay if we don't update the doc because it's been seen or signed already
          -- (see jira #1194)
      
          -- FIXME: (max 1 r) should be there instead of r, but with (max 1 r)
          -- few tests fails. it should be done properly.
    
          _ <- dbUpdate $ InsertEvidenceEvent
               MarkDocumentSeenEvidence
               txt
               (Just did)
               actor
          getOneDocumentAffected "MarkDocumentSeen" r did

data Actor a => AddInvitationEvidence a = AddInvitationEvidence DocumentID SignatoryLinkID a
instance Actor a => DBUpdate (AddInvitationEvidence a) (Either String Document) where
  dbUpdate (AddInvitationEvidence docid slid actor) = do
  -- modifySignable docid $ \document ->
  -- case checkAddEvidence document slid of
  --  [] -> let Just sds = signatorydetails <$> getSigLinkFor document slid
  --        in Right $ document { documentinvitetime = Just (SignInfo time ipnumber) }
  --           `appendHistory` [DocumentHistoryInvitationSent time ipnumber [sds]]
  --  s -> Left $ "Document " ++ show documentid ++ " cannot have evidence attached for signatory " ++ show slid ++ " because " ++ concat s
    mdoc <- dbQuery $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Left "no such document"
      Just doc -> do
        case getSigLinkFor doc slid of
          Just sl -> do
            let eml = getEmail sl
            _ <- dbUpdate $ InsertEvidenceEvent
                 InvitationEvidence
                 ("Invitation sent to " ++ eml ++ " by " ++ actorWho actor ++ ".")
                 (Just docid)
                 actor
            return $ Right doc
          Nothing -> 
            return $ Left $ "SignatoryLinkID " ++ show slid ++ " does not exist in document with id " ++ show docid

data Actor a => MarkInvitationRead a = MarkInvitationRead DocumentID SignatoryLinkID a
instance Actor a => DBUpdate (MarkInvitationRead a) (Either String Document) where
  dbUpdate (MarkInvitationRead did linkid actor) = do
    mdoc <- dbQuery $ GetDocumentByDocumentID did
    case mdoc of
      Nothing -> return $ Left "no such document"
      Just doc -> case getSigLinkFor doc linkid of
        Nothing -> return $ Left "no such siglink id"
        Just sl -> do
          let time = actorTime actor
              eml  = getEmail sl
          r <- kRun $ mkSQL UPDATE tableSignatoryLinks [sql "read_invitation" time]
               <++> SQL "WHERE id = ? AND document_id = ? AND read_invitation IS NULL" [
            toSql linkid
            , toSql did
            ]
          when_ (r == 1) $
            dbUpdate $ InsertEvidenceEvent
            MarkInvitationReadEvidence
            ("Invitation sent to \"" ++ eml ++ "\" was opened, as reported by " ++ actorWho actor ++ ".")
            (Just did)
            actor
          getOneDocumentAffected "MarkInvitationRead" r did

data Actor a => NewDocument a = NewDocument User (Maybe Company) String DocumentType Int a
instance Actor a => DBUpdate (NewDocument a) (Either String Document) where
  dbUpdate (NewDocument user mcompany title documenttype nrOfOtherSignatories actor) = do
  let ctime = actorTime actor  
  if fmap companyid mcompany /= usercompany user
    then return $ Left "company and user don't match"
    else do

      let authorRoles = if ((Just True) == getValueForProcess documenttype processauthorsend)
                        then [SignatoryAuthor]
                        else [SignatoryPartner, SignatoryAuthor]

      magichash <- random

      let authorlink0 = signLinkFromDetails'
                        (signatoryDetailsFromUser user mcompany)
                        authorRoles magichash

      let authorlink = authorlink0 {
                         maybesignatory = Just $ userid user,
                         maybecompany = usercompany user }

      othersignatories <- sequence $ replicate nrOfOtherSignatories $ do
                        mh <- random
                        return $ signLinkFromDetails'
                                SignatoryDetails
                                                {  signatorysignorder = SignOrder 1
                                                 , signatoryfields   = emptySignatoryFields
                                                }
                                [SignatoryPartner] mh

      let doc = blankDocument
                { documenttitle                = title
                , documentsignatorylinks       = authorlink : othersignatories
                , documenttype                 = documenttype
                , documentregion               = getRegion user
                , documentfunctionality        = newDocumentFunctionality documenttype user
                , documentctime                = ctime
                , documentmtime                = ctime
                , documentservice              = userservice user
                , documentauthorattachments    = []
                , documentallowedidtypes       = [EmailIdentification]
                , documentui                   = (documentui blankDocument) { documentmailfooter = customfooter $ usersettings user }
                } `appendHistory` [DocumentHistoryCreated ctime]

      case invariantProblems ctime doc of
        Nothing -> do

           midoc <- insertDocumentAsIs doc
           case midoc of
             Just doc' -> do
               _<- dbUpdate $ InsertEvidenceEvent
                 NewDocumentEvidence
                 ("Document \"" ++ title ++ "\" created by " ++ actorWho actor ++ ".")
                 (Just $ documentid doc')
                 actor
               return $ Right doc'
             Nothing -> do
               Log.debug $ "insertDocumentAsIs could not insert document #" ++ show (documentid doc) ++ " in NewDocument"
               return $ Left $ "insertDocumentAsIs could not insert document #" ++ show (documentid doc) ++ " in NewDocument"
        Just a -> do
           Log.debug $ "insertDocumentAsIs invariants violated: " ++ show a
           return $ Left $ "insertDocumentAsIs invariants violated: " ++ show a

data Actor a => ReallyDeleteDocument a = ReallyDeleteDocument User DocumentID a
instance Actor a => DBUpdate (ReallyDeleteDocument a) (Either String Document) where
  dbUpdate (ReallyDeleteDocument user did actor) = do
    -- I don't like this: we should do this on the DB side, not pass
    -- in a User which could be old. It should be done within a
    -- transaction. -EN
    r <- case (usercompany user, useriscompanyadmin user) of
      (Just cid, True) -> deleteDoc $ SQL "WHERE company_id = ?" [toSql cid]
      _ -> deleteDoc $ SQL "WHERE user_id = ? AND company_id IS NULL" [toSql $ userid user]
    let txt = case (usercompany user, useriscompanyadmin user) of
          (Just _, True) -> "the company with admin email \"" ++ getEmail user ++ "\""
          _ -> "the user with email \"" ++ getEmail user ++ "\""
    when_ (r == 1) $ do
      dbUpdate $ InsertEvidenceEvent
        ReallyDeleteDocumentEvidence
        ("The document was removed from the rubbish bin for " ++ txt ++ " by " ++ actorWho actor ++ ".")
        (Just did)
        actor
    getOneDocumentAffected "ReallyDeleteDocument" r did
    where
      deleteDoc whereClause = kRun $ mconcat [
          mkSQL UPDATE tableSignatoryLinks [sql "really_deleted" True]
        , whereClause
        , SQL " AND document_id = ? AND deleted = TRUE" [toSql did]
        ]

data Actor a => RejectDocument a = RejectDocument DocumentID SignatoryLinkID (Maybe String) a
instance Actor a => DBUpdate (RejectDocument a) (Either String Document) where
  dbUpdate (RejectDocument docid slid customtext actor) = do
    let time = actorTime actor
    mdocument <- dbQuery $ GetDocumentByDocumentID docid
    case mdocument of
      Nothing -> return $ Left $ "Cannot RejectDocument document " ++ show docid ++ " because it does not exist"
      Just document -> case getSigLinkFor document slid of
        Nothing -> return $ Left $ "Cannot RejectDocument document " ++ show docid ++ " because signatory link id does not exist."
        Just sl ->
          case checkRejectDocument document slid of
          [] -> do
            let ipnumber = fromMaybe noIP $ actorIP actor
            r <- kRun $ mkSQL UPDATE tableDocuments [
                sql "status" Rejected
              , sql "mtime" time
              , sql "rejection_time" time
              , sql "rejection_reason" customtext
              , sql "rejection_signatory_link_id" slid
              , sqlLog time $ "Document rejected from " ++ show ipnumber
              ] <++> SQL "WHERE id = ?" [toSql docid]
            let eml = getEmail sl
            when_ (r == 1) $
                dbUpdate $ InsertEvidenceEvent
                RejectDocumentEvidence
                ("Document rejected for signatory with email \"" ++ eml ++ "\" by " ++ actorWho actor ++ ".")
                (Just docid)
                actor
            getOneDocumentAffected "RejectDocument" r docid
          s -> return $ Left $ "Cannot RejectDocument document " ++ show docid ++ " because " ++ concat s

data Actor a => RestartDocument a = RestartDocument Document a
instance Actor a => DBUpdate (RestartDocument a) (Either String Document) where
  dbUpdate (RestartDocument doc actor) = do
    mndoc <- tryToGetRestarted
    case mndoc of
      Right newdoc -> do
        ed <- newFromDocument (const newdoc) (documentid doc)
        case ed of
          Left s -> return $ Left s
          Right d -> do
            ignore $ dbUpdate $ InsertEvidenceEvent
              RestartDocumentEvidence
              ("Document restarted by " ++ actorWho actor ++ ". New document has id " ++ show (documentid d) ++ ".")
              (Just $ documentid doc)
              actor
            copyEvidenceLogToNewDocument (documentid doc) (documentid d)
            ignore $ dbUpdate $ InsertEvidenceEvent
              RestartDocumentEvidence
              ("Document restarted from document with id " ++ (show $ documentid doc) ++ " by " ++ actorWho actor ++ ".")
              (Just $ documentid d)
              actor
            return $ Right d
      other -> return other
   where
    tryToGetRestarted :: DB (Either String Document)
    tryToGetRestarted =
      if (documentstatus doc `notElem` [Canceled, Timedout, Rejected])
      then return $ Left $ "Can't restart document with " ++ (show $ documentstatus doc) ++ " status"
      else do
             let time = actorTime actor
                 ipnumber = fromMaybe noIP $ actorIP actor
             doc' <- clearSignInfofromDoc
             let doc'' = doc' `appendHistory` [DocumentHistoryRestarted time ipnumber]
             return $ Right doc''
    clearSignInfofromDoc :: DB Document
    clearSignInfofromDoc = do
      let signatoriesDetails = map (\x -> (signatorydetails x, signatoryroles x, signatorylinkid x)) $ documentsignatorylinks doc
          Just asl = getAuthorSigLink doc
      newSignLinks <- forM signatoriesDetails $ \(details,roles,linkid) -> do
                           magichash <- random
                           return $ (signLinkFromDetails' details roles magichash) { signatorylinkid = linkid }
      let Just authorsiglink0 = find isAuthor newSignLinks
          authorsiglink = authorsiglink0 {
                            maybesignatory = maybesignatory asl,
                            maybecompany = maybecompany asl
                          }
          othersiglinks = filter (not . isAuthor) newSignLinks
          newsiglinks = authorsiglink : othersiglinks
      return doc {documentstatus = Preparation,
                  documenttimeouttime = Nothing,
                  documentsignatorylinks = newsiglinks
                 }

data Actor a => RestoreArchivedDocument a = RestoreArchivedDocument User DocumentID a
instance Actor a => DBUpdate (RestoreArchivedDocument a) (Either String Document) where
  dbUpdate (RestoreArchivedDocument user did actor) = do
    r <- case (usercompany user, useriscompanyadmin user) of
      (Just cid, True) -> updateRestorableDoc $ SQL "WHERE company_id = ?" [toSql cid]
      _ -> updateRestorableDoc $ SQL "WHERE user_id = ?" [toSql $ userid user]
    let txt = case (usercompany user, useriscompanyadmin user) of
          (Just _, True) -> "the company with admin email \"" ++ getEmail user ++ "\""
          _ -> "the user with email \"" ++ getEmail user ++ "\""
    ignore $ dbUpdate $ InsertEvidenceEvent
      RestoreArchivedDocumentEvidence
      ("Document restored from the rubbish bin for " ++ txt ++ " by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "RestoreArchivedDocument" r did
    where
      updateRestorableDoc whereClause = kRun $ mconcat [
          mkSQL UPDATE tableSignatoryLinks [sql "deleted" False]
        , whereClause
        , SQL " AND document_id = ? AND really_deleted = FALSE" [toSql did]
        ]

{- |
    Links up a signatory link to a user account.  This should happen when
      \1. a document moves from preparation to pending more
      \2. a signer creates an account after signing to save their document
      \3. the email of a signatory is corrected to that of an existing user
-}
data Actor a => SaveDocumentForUser a = SaveDocumentForUser DocumentID User SignatoryLinkID a
instance Actor a => DBUpdate (SaveDocumentForUser a) (Either String Document) where
  dbUpdate (SaveDocumentForUser did user@User{userid, usercompany} slid actor) = do
    r <- kRun $ mkSQL UPDATE tableSignatoryLinks [
        sql "user_id" userid
      , sql "company_id" usercompany
      ] <++> SQL "WHERE document_id = ? AND id = ?" [
        toSql did
      , toSql slid
      ]
    when_ (r == 1) $
      dbUpdate $ InsertEvidenceEvent
      SaveDocumentForUserEvidence
      ("Saving document to user account with email \"" ++ getEmail user ++ "\" by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SaveDocumentForUser" r did

{- |
    Saves a signatory attachment to a document.
    If there's a problem such as the document isn't in a pending or awaiting author state,
    or the document does not exist a Left is returned.
-}
data Actor a => SaveSigAttachment a = SaveSigAttachment DocumentID SignatoryLinkID String FileID a
instance Actor a => DBUpdate (SaveSigAttachment a) (Either String Document) where
  dbUpdate (SaveSigAttachment did slid name fid actor) = do
    r <- kRun $ mkSQL UPDATE tableSignatoryAttachments [sql "file_id" fid]
      <++> SQL "WHERE document_id = ? AND file_id IS NULL AND name = ? AND signatory_link_id = ?" [
        toSql did
      , toSql name
      , toSql slid
      ]
    when_ (r == 1) $
      dbUpdate $ InsertEvidenceEvent
      SaveSigAttachmentEvidence
      ("Saving attachment with name \"" ++ name ++ "\" for signatory by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SaveSigAttachment" r did

data Actor a => SetDocumentTags a = SetDocumentTags DocumentID [DocumentTag] a
instance Actor a => DBUpdate (SetDocumentTags a) (Either String Document) where
  dbUpdate (SetDocumentTags did doctags actor) = do
    -- check if the tags are changed
    ed <- dbQuery $ GetDocumentByDocumentID did
    let changed = case ed of
          Nothing -> True
          Just d -> not $ listsEqualNoOrder doctags $ documenttags d
    _ <- kRun $ SQL "DELETE FROM document_tags WHERE document_id = ?" [toSql did]
    mtags <- mapM (insertDocumentTagAsIs did) doctags
    let tagstr = intercalate "; " $ map (\(DocumentTag k v)-> k ++ "=" ++ v) doctags
    when_ (not (any isNothing mtags) && changed) $
      dbUpdate $ InsertEvidenceEvent
      SetDocumentTagsEvidence
      ("Document tags set to " ++ show tagstr ++ " by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SetDocumentTags" 1 did

data Actor a => SetDocumentInviteTime a = SetDocumentInviteTime DocumentID MinutesTime a
instance Actor a => DBUpdate (SetDocumentInviteTime a) (Either String Document) where
  dbUpdate (SetDocumentInviteTime did invitetime actor) = do
    let ipaddress  = fromMaybe noIP $ actorIP actor
    -- check if it's changed
    ed <- dbQuery $ GetDocumentByDocumentID did
    let changed = case ed of
          Nothing -> True
          Just d -> not $ documentinvitetime d == Just (SignInfo invitetime ipaddress)
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "invite_time" invitetime
      , sql "invite_ip" ipaddress
      ] <++> SQL "WHERE id = ?" [toSql did]
    when_ (r == 1 && changed) $
      dbUpdate $ InsertEvidenceEvent
      SetDocumentInviteTimeEvidence
      ("Document invite time set to " ++ formatMinutesTimeUTC invitetime ++ " UTC by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SetDocumentInviteTime" r did

data Actor a => SetDocumentTimeoutTime a = SetDocumentTimeoutTime DocumentID MinutesTime a
instance Actor a => DBUpdate (SetDocumentTimeoutTime a) (Either String Document) where
  dbUpdate (SetDocumentTimeoutTime did timeouttime actor) = do
    ed <- dbQuery $ GetDocumentByDocumentID did
    let changed = case ed of
          Nothing -> True
          Just d -> not $ documenttimeouttime d == Just (TimeoutTime timeouttime)
    r <- kRun $ mkSQL UPDATE tableDocuments [sql "timeout_time" timeouttime]
      <++> SQL "WHERE id = ? AND deleted = FALSE AND type = ?" [
        toSql did
      , toSql $ Signable undefined
      ]
    when_ (r == 1 && changed) $
      dbUpdate $ InsertEvidenceEvent
      SetDocumentTimeoutTimeEvidence
      ("Document timeout time set to " ++ formatMinutesTimeUTC timeouttime ++ " UTC by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SetDocumentTimeoutTime" r did

data Actor a => SetInviteText a = SetInviteText DocumentID String a
instance Actor a => DBUpdate (SetInviteText a) (Either String Document) where
  dbUpdate (SetInviteText did text actor) = do
    ed <- dbQuery $ GetDocumentByDocumentID did
    let changed = case ed of
          Nothing -> False
          Just d -> not $ documentinvitetext d == text
    let time = actorTime actor
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "invite_text" text
      , sql "mtime" time
      , sqlLog time "Invite text set"
      ] <++> SQL "WHERE id = ?" [toSql did]
    when_ (r == 1 && changed) $
      dbUpdate $ InsertEvidenceEvent
      SetInvitationTextEvidence
      ("Invitation text set to \"" ++ text ++ "\" by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SetInviteText" r did


data Actor a => SetDaysToSign a = SetDaysToSign DocumentID (Maybe Int) a
instance Actor a =>  DBUpdate (SetDaysToSign a) (Either String Document) where
  dbUpdate (SetDaysToSign did mdays actor) = do
    ed <- dbQuery $ GetDocumentByDocumentID did
    let changed = case ed of
          Nothing -> False
          Just d -> not $ documentdaystosign d == mdays
    r <- kRun $ mkSQL UPDATE tableDocuments 
         [ sql "days_to_sign" $ mdays
         , sql "mtime" $ actorTime actor

         ] <++> SQL "WHERE id = ?" [ toSql did ]
    when_ (r == 1 && changed) $
      dbUpdate $ InsertEvidenceEvent
      (SetDaysToSignEvidence <| isJust mdays |> RemoveDaysToSignEvidence)
      ("Days to sign set to " ++ show mdays ++ " by " ++ actorWho actor ++ ".")
      (Just did)
      actor    
    getOneDocumentAffected "SetDaysToSign" r did

data Actor a =>  SetDocumentFunctionality a = SetDocumentFunctionality DocumentID DocumentFunctionality a
instance Actor a => DBUpdate (SetDocumentFunctionality a) (Either String Document) where
  dbUpdate (SetDocumentFunctionality did functionality actor) = do
    ed <- dbQuery $ GetDocumentByDocumentID did
    let changed = case ed of
          Nothing -> False
          Just d -> not $ documentfunctionality d == functionality
    r <- kRun $ mkSQL UPDATE tableDocuments 
         [ sql "functionality" functionality
         , sql "mtime" $ actorTime actor
         ]  <++> SQL "WHERE id = ?" [ toSql did ]
    when_ (r == 1 && changed) $
      dbUpdate $ InsertEvidenceEvent
      SetDocumentAdvancedFunctionalityEvidence
      ("Document functionality set to " ++ show functionality ++ " by " ++ actorWho actor ++ ".")
      (Just did)
      actor     
    getOneDocumentAffected "SetDocumentFunctionality" r did

data Actor a => SetDocumentTitle a = SetDocumentTitle DocumentID String a
instance Actor a => DBUpdate (SetDocumentTitle a) (Either String Document) where
  dbUpdate (SetDocumentTitle did doctitle actor) = do
    ed <- dbQuery $ GetDocumentByDocumentID did
    let changed = case ed of
          Nothing -> False
          Just d -> not $ documenttitle d == doctitle
    let time = actorTime actor
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "title" doctitle
      , sql "mtime" time
      , sqlLog time "Document title changed"
      ] <++> SQL "WHERE id = ?" [toSql did]
    when_ (r == 1 && changed) $
      dbUpdate $ InsertEvidenceEvent
      SetDocumentTitleEvidence
      ("Document title set to \"" ++ doctitle ++ "\" by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SetDocumentTitle" r did

data Actor a => SetDocumentLocale a = SetDocumentLocale DocumentID Locale a
instance Actor a => DBUpdate (SetDocumentLocale a) (Either String Document) where
  dbUpdate (SetDocumentLocale did locale actor) = do
    ed <- dbQuery $ GetDocumentByDocumentID did
    let changed = case ed of
          Nothing -> False
          Just d -> not $ getLocale d == locale
    let time = actorTime actor
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "region" $ getRegion locale
      , sql "mtime" time
      , sqlLog time "Document locale changed"
      ] <++> SQL "WHERE id = ?" [toSql did]
    when_ (r == 1 && changed) $
      dbUpdate $ InsertEvidenceEvent
      SetDocumentLocaleEvidence
      ("Document locale set to " ++ show locale ++ " by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SetDocumentLocale" r did

data Actor a => SetDocumentUI a = SetDocumentUI DocumentID DocumentUI a
instance Actor a => DBUpdate (SetDocumentUI a) (Either String Document) where
  dbUpdate (SetDocumentUI did docui actor) = do
    ed <- dbQuery $ GetDocumentByDocumentID did
    let changed = case ed of
          Nothing -> False
          Just d -> not $ documentui d == docui
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "mail_footer" $ documentmailfooter docui
         , sql "mtime" $ actorTime actor           
      ] <++> SQL "WHERE id = ?" [toSql did]
    let txt = case documentmailfooter docui of
          Nothing -> "Document mail footer removed by " ++ actorWho actor ++ "."
          Just footer -> "Document mail footer set to \"" ++ footer ++ "\" by " ++ actorWho actor ++ "."
    when_ (r == 1 && changed) $
      dbUpdate $ InsertEvidenceEvent
      SetDocumentUIEvidence
      txt
      (Just did)
      actor
    getOneDocumentAffected "SetDocumentUI" r did

data Actor a => SetInvitationDeliveryStatus a = SetInvitationDeliveryStatus DocumentID SignatoryLinkID MailsDeliveryStatus a
instance Actor a => DBUpdate (SetInvitationDeliveryStatus a) (Either String Document) where
  dbUpdate (SetInvitationDeliveryStatus did slid status actor) = do
    ed <- dbQuery $ GetDocumentByDocumentID did
    let changed = case getSigLinkFor ed slid of
          Nothing -> False
          Just sl -> not $ invitationdeliverystatus sl == status
    r <- kRun $ mkSQL UPDATE tableSignatoryLinks [
        sql "invitation_delivery_status" status
      ] <++> SQL "WHERE id = ? AND document_id = ? AND EXISTS (SELECT 1 FROM documents WHERE id = ? AND type = ?)" [
        toSql slid
      , toSql did
      , toSql did
      , toSql $ Signable undefined
      ]
    when_ (r == 1 && changed) $
      dbUpdate $ InsertEvidenceEvent
      SetInvitationDeliveryStatusEvidence
      ("Delivery status for signatory with id " ++ show slid ++ " set to " ++ show status ++ " by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SetInvitationDeliveryStatus" r did

data SetDocumentSharing = SetDocumentSharing [DocumentID] Bool
instance DBUpdate SetDocumentSharing (Either String Bool) where
  dbUpdate (SetDocumentSharing dids flag) = do
    flip mapM_ dids $ \did -> kRun $ mkSQL UPDATE tableDocuments
         [ sql "sharing" $ (if flag then Shared else Private)
         ] <++> SQL
         " WHERE id = ? AND deleted = FALSE " [ toSql did ]
    return (Right True)


data Actor a => SignDocument a = SignDocument DocumentID SignatoryLinkID MagicHash (Maybe SignatureInfo) a
instance Actor a => DBUpdate (SignDocument a) (Either String Document) where
  dbUpdate (SignDocument docid slid mh msiginfo actor) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID docid
    case mdocument of
      Nothing -> return $ Left $ "Cannot SignDocument document " ++ show docid ++ " because it does not exist"
      Just document ->
        case checkSignDocument document slid mh of
          [] -> do
            let ipnumber = fromMaybe noIP $ actorIP actor
                time     = actorTime actor
            r <- kRun $ mkSQL UPDATE tableSignatoryLinks [
                sql "sign_ip" ipnumber
              , sql "sign_time" time
              , sql "signinfo_text" $ signatureinfotext `fmap` msiginfo
              , sql "signinfo_signature" $ signatureinfosignature `fmap` msiginfo
              , sql "signinfo_certificate" $ signatureinfocertificate `fmap` msiginfo
              , sql "signinfo_provider" $ signatureinfoprovider `fmap` msiginfo
              , sql "signinfo_first_name_verified" $ signaturefstnameverified `fmap` msiginfo
              , sql "signinfo_last_name_verified" $ signaturelstnameverified `fmap` msiginfo
              , sql "signinfo_personal_number_verified" $ signaturepersnumverified `fmap` msiginfo
              ] <++> SQL "WHERE id = ? AND document_id = ?" [
                toSql slid
              , toSql docid
              ]
            let using = case msiginfo of
                  Nothing -> ""
                  Just (SignatureInfo { signatureinfotext
                                      , signatureinfoprovider
                                      , signaturefstnameverified
                                      , signaturelstnameverified
                                      , signaturepersnumverified
                                      }) -> let ps = case signatureinfoprovider of
                                                  BankIDProvider -> "BankID"
                                                  TeliaProvider  -> "Telia"
                                                  NordeaProvider -> "Nordea"
                                                pairs = [("first name", signaturefstnameverified)
                                                        ,("last name", signaturelstnameverified)
                                                        ,("personal number", signaturepersnumverified)]
                                                pairstrue = filter (\(_,t)->t) pairs
                                                vs = intercalate "; " $ map (\(s,_) -> s) pairstrue
                                                vstring = case pairstrue of
                                                  [] -> "No fields were verified."
                                                  _ -> "The following fields were verified: " ++ vs
                                            in " using e-legitimation. The signed text was \"" 
                                               ++ signatureinfotext
                                               ++ "\". The provider was " ++ ps ++ ". " 
                                               ++ vstring
            when_ (r == 1) $
              dbUpdate $ InsertEvidenceEvent
              SignDocumentEvidence
              ("Document signed by " ++ actorWho actor ++ using ++ ".")
              (Just docid)
              actor
            getOneDocumentAffected "SignDocument" r docid
          s -> return $ Left $ "Cannot SignDocument document " ++ show docid ++ " because " ++ concat s

data Actor a => ResetSignatoryDetails a = ResetSignatoryDetails DocumentID [(SignatoryDetails, [SignatoryRole])] a
instance Actor a => DBUpdate (ResetSignatoryDetails a) (Either String Document) where
  dbUpdate (ResetSignatoryDetails documentid signatories actor) = 
    dbUpdate (ResetSignatoryDetails2 documentid (map (\(a,b) -> (a,b,[],Nothing)) signatories) actor)


data Actor a => ResetSignatoryDetails2 a = ResetSignatoryDetails2 DocumentID [(SignatoryDetails, [SignatoryRole], [SignatoryAttachment], Maybe CSVUpload)] a
instance Actor a => DBUpdate (ResetSignatoryDetails2 a) (Either String Document) where
  dbUpdate (ResetSignatoryDetails2 documentid signatories actor) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID documentid
    case mdocument of
      Nothing -> return $ Left $ "ResetSignatoryDetails: document #" ++ show documentid ++ " does not exist"
      Just document ->
        case checkResetSignatoryData document signatories of
          [] -> do
            kPrepare "DELETE FROM signatory_links WHERE document_id = ?"
            _ <- kExecute [toSql documentid]

            let mauthorsiglink = getAuthorSigLink document
            forM_ signatories $ \(details, roles, atts, mcsvupload) -> do
                     magichash <- random
                     let link' = (signLinkFromDetails' details roles magichash)
                                 { signatorylinkcsvupload = mcsvupload
                                 , signatoryattachments   = atts }
                         link = if isAuthor link'
                                then link' { maybesignatory = maybe Nothing maybesignatory mauthorsiglink
                                           , maybecompany   = maybe Nothing maybecompany   mauthorsiglink
                                           }
                                else link'
                     r1 <- insertSignatoryLinkAsIs documentid link
                     when (not (isJust r1)) $
                          error "ResetSignatoryDetails signatory_links did not manage to insert a row"

            let (old, _, new) = listDiff (map signatorydetails $ documentsignatorylinks document) (map (\(sd, _, _, _)-> sd) signatories)

            forM_ (emailsOfRemoved old new) $ \eml ->
              dbUpdate $ InsertEvidenceEvent
                ResetSignatoryDetailsEvidence
                ("Signatory with email \"" ++ eml ++ "\" removed by " ++ actorWho actor ++ ".")
                (Just documentid)
                actor

            forM_ (changedStuff old new) $ \(eml, rs, cs) -> do
              forM_ rs $ \removedfield ->
                dbUpdate $ InsertEvidenceEvent
                  ResetSignatoryDetailsEvidence
                  ("Field \"" ++ show (sfType removedfield) ++ "\" for signatory with email \"" ++ eml ++ "\" removed by " ++ actorWho actor ++ ".")
                  (Just documentid)
                  actor
              forM_ cs $ \changedfield ->
                dbUpdate $ InsertEvidenceEvent
                  ResetSignatoryDetailsEvidence
                  ("Field \"" ++ show (sfType changedfield) ++ "\" for signatory with email \"" ++ eml ++ "\" set to \"" ++ sfValue changedfield ++ "\" by " ++ actorWho actor ++ ".")
                  (Just documentid)
                  actor

            forM_ (fieldsOfNew old new) $ \(eml, fs) -> do
              _ <- dbUpdate $ InsertEvidenceEvent
                ResetSignatoryDetailsEvidence
                ("Signatory with email \"" ++ eml ++ "\" added by " ++ actorWho actor ++ ".")
                (Just documentid)
                actor
              forM_ fs $ \changedfield ->
                dbUpdate $ InsertEvidenceEvent
                  ResetSignatoryDetailsEvidence
                  ("Field \"" ++ show (sfType changedfield) ++ "\" for signatory with email \"" ++ eml ++ "\" set to \"" ++ sfValue changedfield ++ "\" by " ++ actorWho actor ++ ".")
                  (Just documentid)
                  actor

            Just newdocument <- dbQuery $ GetDocumentByDocumentID documentid
            let moldcvsupload = msum (map (\(_,_,_,a) -> a) signatories)
            let mnewcsvupload = msum (map (signatorylinkcsvupload) (documentsignatorylinks newdocument))

            when (moldcvsupload /= mnewcsvupload) $ do
                     Log.error $ "ResetSignatoryDetails2 csvupload differs: " ++ show moldcvsupload ++ " vs " ++ show mnewcsvupload
                     error $ "error in ResetSignatoryDetails2"
            return $ Right newdocument

          s -> return $ Left $ "cannot reset signatory details on document " ++ show documentid ++ " because " ++ intercalate ";" s
          where emailsOfRemoved old new = [getEmail x | x <- removedSigs old new, "" /= getEmail x]
                changedStuff    old new = [(getEmail x, removedFields x y, changedFields x y) | (x, y) <- changedSigs old new, not $ null $ getEmail x]
                fieldsOfNew     old new = [(getEmail x, filter (not . null . sfValue) $ signatoryfields x) | x <- newSigs old new, not $ null $ getEmail x]
                removedSigs     old new = [x      | x <- old, getEmail x `notElem` map getEmail new, not $ null $ getEmail x]
                changedSigs     old new = [(x, y) | x <- new, y <- old, getEmail x == getEmail y,    not $ null $ getEmail x]
                newSigs         old new = [x      | x <- new, getEmail x `notElem` map getEmail old, not $ null $ getEmail x]
                removedFields x y = let (r, _, _) = listDiff (signatoryfields x) (signatoryfields y) in filter (not . null . sfValue) r
                changedFields x y = let (_, _, c) = listDiff (signatoryfields x) (signatoryfields y) in filter (not . null . sfValue) c

data SignLinkFromDetailsForTest = SignLinkFromDetailsForTest SignatoryDetails [SignatoryRole]
instance DBUpdate SignLinkFromDetailsForTest SignatoryLink where
  dbUpdate (SignLinkFromDetailsForTest details roles) = do
      magichash <- random

      let link = signLinkFromDetails' details
                        roles magichash

      return link

data Actor a => SignableFromDocument a = SignableFromDocument Document a
instance Actor a => DBUpdate (SignableFromDocument a) Document where
  -- NOTE TO MERGER: I removed this in another branch. If there's a
  -- conflict in a merge, get rid of this whole DBUpdate -- Eric
  dbUpdate (SignableFromDocument document actor ) = do
    d <- insertNewDocument $ templateToDocument document
    ignore $ dbUpdate $ InsertEvidenceEvent
      SignableFromDocumentEvidence
      ("Document created from template by " ++ actorWho actor ++ ".")
      (Just (documentid d))
      actor
    return d

data Actor a => SignableFromDocumentIDWithUpdatedAuthor a = SignableFromDocumentIDWithUpdatedAuthor User (Maybe Company) DocumentID a
instance Actor a => DBUpdate (SignableFromDocumentIDWithUpdatedAuthor a) (Either String Document) where
  dbUpdate (SignableFromDocumentIDWithUpdatedAuthor user mcompany docid actor) =
      if fmap companyid mcompany /= usercompany user
        then return $ Left "company and user don't match"
        else do
          let time = actorTime actor
          r <- (flip newFromDocument) docid $ \doc ->
            (templateToDocument doc) {
              documentsignatorylinks = map replaceAuthorSigLink (documentsignatorylinks doc)
                                       -- FIXME: Need to remove authorfields?
              , documentctime = time
              , documentmtime = time
              , documentui    = DocumentUI { documentmailfooter = customfooter (usersettings user) }
              }
          case r of 
            Right d -> do
              copyEvidenceLogToNewDocument docid (documentid d)
              ignore $ dbUpdate $ InsertEvidenceEvent
                SignableFromDocumentIDWithUpdatedAuthorEvidence
                ("Document created from template with id " ++ show docid ++ " by " ++ actorWho actor ++ ".")
                (Just $ documentid d)
                actor
              return r
            Left _ -> return r
    where replaceAuthorSigLink :: SignatoryLink -> SignatoryLink
          replaceAuthorSigLink sl
            | isAuthor sl = replaceSignatoryUser sl user mcompany
            | otherwise = sl

data StoreDocumentForTesting = StoreDocumentForTesting Document
instance DBUpdate StoreDocumentForTesting DocumentID where
  dbUpdate (StoreDocumentForTesting document) = do
    Just doc <- insertDocumentAsIs document
    return (documentid doc)

{-
   FIXME: this is so wrong on so many different levels
   - should set mtime
   - should not change type or copy this doc into new doc
-}
data Actor a => TemplateFromDocument a = TemplateFromDocument DocumentID a
instance Actor a => DBUpdate (TemplateFromDocument a) (Either String Document) where
  dbUpdate (TemplateFromDocument did actor) = do
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "status" Preparation
      , sql "type" $ Template undefined
      ] <++> SQL "WHERE id = ?" [toSql did]
    when_ (r == 1) $
      dbUpdate $ InsertEvidenceEvent
      TemplateFromDocumentEvidence
      ("Document converted to template by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "TemplateFromDocument" r did

data Actor a => TimeoutDocument a = TimeoutDocument DocumentID a
instance Actor a => DBUpdate (TimeoutDocument a) (Either String Document) where
  dbUpdate (TimeoutDocument did actor) = do
    let time = actorTime actor
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "status" Timedout
      , sql "mtime" time
      , sqlLog time "Document timed out"
      ] <++> SQL "WHERE id = ? AND type = ? AND status = ?" [
        toSql did
      , toSql $ Signable undefined
      , toSql Pending
      ]
    when_ (r == 1) $
      dbUpdate $ InsertEvidenceEvent
      TimeoutDocumentEvidence
      ("Document timed out by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "TimeoutDocument" r did

data Actor a => SetDocumentIdentification a = SetDocumentIdentification DocumentID [IdentificationType] a
instance Actor a => DBUpdate (SetDocumentIdentification a) (Either String Document) where
  dbUpdate (SetDocumentIdentification did identification actor) = do
    ed <- dbQuery $ GetDocumentByDocumentID did
    let changed = case ed of
          Nothing -> False
          Just d -> not $ documentallowedidtypes d == identification
    r <- kRun $ mkSQL UPDATE tableDocuments 
         [ sql "allowed_id_types" $ identification
         ] <++> SQL "WHERE id = ?" [ toSql did ]
    when_ (r == 1 && changed) $
      dbUpdate $ InsertEvidenceEvent
      (SetElegitimationIdentificationEvidence <| ELegitimationIdentification `elem` identification |> SetEmailIdentificationEvidence)
      ("Document identification type set to " ++ show identification ++ " by " ++ actorWho actor ++ ".")
      (Just did)
      actor     
    getOneDocumentAffected "SetDocumentIdentification" r did

data Actor a => UpdateFields a = UpdateFields DocumentID SignatoryLinkID [(String, String)] a
instance Actor a => DBUpdate (UpdateFields a) (Either String Document) where
  dbUpdate (UpdateFields did slid fields actor) = do
  Just document <- dbQuery $ GetDocumentByDocumentID did
  case checkUpdateFields document slid of
    [] -> do
      let updateSigField sf =
                let updateF n = case lookup n fields of
                      Just v  -> sf { sfValue = v }
                      Nothing -> sf
                in case sfType sf of
                  CompanyFT        -> updateF "sigco"
                  PersonalNumberFT -> updateF "sigpersnr"
                  CompanyNumberFT  -> updateF "sigcompnr"
                  SignatureFT      -> updateF "signature"
                  CustomFT label _ -> updateF label
                  _                -> sf

      let Just sl = getSigLinkFor document slid
          eml     = getEmail sl
      r <- kRun $ mkSQL UPDATE tableSignatoryLinks [
          sql "fields" $ map updateSigField $ signatoryfields $ signatorydetails sl
        ] <++> SQL "WHERE EXISTS (SELECT 1 FROM documents WHERE documents.id = signatory_links.document_id AND documents.status = ?) AND document_id = ? AND id = ? " [
          toSql Pending
        , toSql did
        , toSql slid
        ]
      when_ (r == 1) $ forM_ fields $ \(n, v) -> 
        dbUpdate $ InsertEvidenceEvent
        UpdateFieldsEvidence
        ("Information for signatory with email \"" ++ eml ++ "\" for field \"" ++ n ++ "\" was set to \"" ++ v ++ "\" by " ++ actorWho actor ++ ".")
        (Just did)
        actor
      getOneDocumentAffected "UpdateFields" r did
    s -> return $ Left $ "Cannot updateFields on document " ++ show did ++ " because " ++ concat s

data Actor a => UpdateFieldsNoStatusCheck a = UpdateFieldsNoStatusCheck DocumentID SignatoryLinkID (String, String) a
instance Actor a => DBUpdate (UpdateFieldsNoStatusCheck a) (Either String Document) where
  dbUpdate (UpdateFieldsNoStatusCheck did slid (fieldname, fieldvalue) actor) = do
  Just document <- dbQuery $ GetDocumentByDocumentID did
  let updateSigField sf =
        let updateF n = if n == fieldname
                        then sf { sfValue = fieldvalue }
                        else sf
        in case sfType sf of
              FirstNameFT      -> updateF $ "sigfstname"
              LastNameFT       -> updateF $ "sigsndname"
              EmailFT          -> updateF $ "sigemail"
              CompanyFT        -> updateF $ "sigco"
              PersonalNumberFT -> updateF $ "sigpersnr"
              CompanyNumberFT  -> updateF $ "sigcompnr"
              SignatureFT      -> updateF $ "signature"
              CustomFT label _ -> updateF $  label

  let Just sl = getSigLinkFor document slid
  r <- kRun $ mkSQL UPDATE tableSignatoryLinks [
    sql "fields" $ map updateSigField $ signatoryfields $ signatorydetails sl
    ] <++> SQL "WHERE document_id = ? AND id = ? " [
      toSql did
    , toSql slid
    ]
  when_ (r == 1) $
    dbUpdate $ InsertEvidenceEvent
      UpdateFieldsEvidence
      ("Information for signatory with email \"" ++  (getEmail sl) ++ "\" for field \"" ++ fieldname ++ "\" was set to \"" ++ fieldvalue ++ "\" by " ++ actorWho actor ++ ".")
      (Just did)
      actor
  getOneDocumentAffected "UpdateFields" r did

data Actor a => AddDocumentAttachment a = AddDocumentAttachment DocumentID FileID a
instance Actor a => DBUpdate (AddDocumentAttachment a) (Either String Document) where
  dbUpdate (AddDocumentAttachment did fid actor) = do
    r <- kRun $ mkSQL INSERT tableAuthorAttachments [
        sql "document_id" did
      , sql "file_id" fid
      ] <++> SQL "WHERE EXISTS (SELECT 1 FROM documents WHERE id = ? AND status = ?)" [
        toSql did
      , toSql Preparation
      ]            
    when_ (r == 1) $
      dbUpdate $ InsertEvidenceEvent
      AddDocumentAttachmentEvidence
      ("File with ID " ++ show fid ++ " attached to Document by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "AddDocumentAttachment" r did

data Actor a => RemoveDocumentAttachment a = RemoveDocumentAttachment DocumentID FileID a
instance Actor a => DBUpdate (RemoveDocumentAttachment a) (Either String Document) where
  dbUpdate (RemoveDocumentAttachment did fid actor) = do
    kPrepare "DELETE FROM author_attachments WHERE document_id = ? AND file_id = ? AND EXISTS (SELECT 1 FROM documents WHERE id = ? AND status = ?)"
    r <- kExecute [
        toSql did
      , toSql fid
      , toSql did
      , toSql Preparation
      ]
    when_ (r == 1) $
      dbUpdate $ InsertEvidenceEvent
      RemoveDocumentAttachmentEvidence
      ("File with ID " ++ show fid ++ " removed from document by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    
    -- I understand the point of this, but it is a little weird to do the check after - EN
    m <- dbQuery $ GetDocumentByDocumentID did
    case m of
      Just doc -> case documentstatus doc of
                       Preparation -> return $ Right doc
                       _ -> return $ Left "bad document status"
      Nothing -> return $ Left "no such document"

data Actor a => SetSigAttachments a = SetSigAttachments DocumentID SignatoryLinkID [SignatoryAttachment] a
instance Actor a => DBUpdate (SetSigAttachments a) () where
  dbUpdate (SetSigAttachments did slid sigatts _actor) = do
    _ <-doDeleteAll
    forM_ sigatts doInsertOne
    where
     doDeleteAll = kRun $ SQL "DELETE FROM signatory_attachments WHERE document_id = ? AND signatory_link_id = ?" [toSql did, toSql slid]
     doInsertOne SignatoryAttachment{..} = do
        kRun $ mkSQL INSERT tableSignatoryAttachments [
            sql "file_id" signatoryattachmentfile
          , sql "name" signatoryattachmentname
          , sql "description" signatoryattachmentdescription
          , sql "document_id" did
          , sql "signatory_link_id" slid
          ]

data Actor a => UpdateDraft a =  UpdateDraft DocumentID  Document  a
instance Actor a => DBUpdate (UpdateDraft a) (Either String Document) where
  dbUpdate (UpdateDraft did document actor) = do
     _ <- dbUpdate $ SetDocumentTitle did (documenttitle document) actor
     _ <- dbUpdate $ SetDaysToSign  did (documentdaystosign document) actor  
     _ <- dbUpdate $ SetDocumentFunctionality did (documentfunctionality document) actor
     _ <- dbUpdate $ SetDocumentLocale did (getLocale document) actor
     _ <- dbUpdate $ SetDocumentIdentification did (documentallowedidtypes document) actor
     dbUpdate $ SetInviteText did (documentinvitetext document) actor

data SetDocumentModificationData = SetDocumentModificationData DocumentID MinutesTime
instance DBUpdate SetDocumentModificationData (Either String Document) where
  dbUpdate (SetDocumentModificationData did time) = do
    r <- kRun $ mkSQL UPDATE tableDocuments [sql "mtime" time]
      <++> SQL "WHERE id = ?" [toSql did]
    getOneDocumentAffected "SetDocumentModificationData" r did

