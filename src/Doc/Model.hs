{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fcontext-stack=50  #-}
module Doc.Model
  ( module File.File
  , isTemplate -- fromUtils
  , isShared -- fromUtils
  , anyInvitationUndelivered
  , undeliveredSignatoryLinks
  , insertDocumentAsIs
  , toDocumentProcess

  , DocumentFilter(..)
  , DocumentDomain(..)
  , DocumentPagination(..)
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
  , GetDocumentByDocumentID(..)
  , GetDocumentsByService(..)
  , GetDocumentsByCompanyWithFiltering(..)
  , GetDocumentsByAuthorCompanyWithFiltering(..)
  , GetDocumentsByAuthor(..)
  , GetTemplatesByAuthor(..)
  , GetAvailableTemplates(..)
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
import Control.Monad.Trans
import DB
import MagicHash
import Crypto.RNG
import File.File
import File.FileID
import File.Model
import Doc.DocUtils
import User.UserID
import User.Model
import Company.Model
import MinutesTime
import OurPrelude
import Control.Logic
import Doc.DocStateData
import Doc.Invariants
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
import Control.Monad
import qualified Control.Exception as E
import Util.Actor
import Util.MonadUtils
import Templates.Templates
import EvidenceLog.Model
import Util.HasSomeUserInfo
import Templates.Fields (value)

data DocumentPagination =
  DocumentPagination
  { documentOffset :: Int        -- ^ use for SQL OFFSET command
  , documentLimit  :: Int        -- ^ use for SQL LIMIT command
  }

data DocumentFilter
  = DocumentFilterStatuses [DocumentStatus]   -- ^ Any of listed statuses
  | DocumentFilterByStatusClass [StatusClass] -- ^ Any of listed status classes
  | DocumentFilterByTags [DocumentTag]        -- ^ All of listed tags
  | DocumentFilterMinChangeTime MinutesTime   -- ^ Minimal mtime
  | DocumentFilterMaxChangeTime MinutesTime   -- ^ Maximum mtime
  | DocumentFilterByService (Maybe ServiceID) -- ^ Only documents belonging to a service
  | DocumentFilterByRole SignatoryRole        -- ^ Signatory must have role
  | DocumentFilterByProcess [DocumentProcess] -- ^ Any of listed processes
  | DocumentFilterByString String             -- ^ Contains the string in title, list of people involved or anywhere
  | DocumentFilterByIdentification IdentificationType -- ^ Only documents that use selected identification type
  | DocumentFilterByYears [Int]               -- ^ Document time fits in a year
  | DocumentFilterByMonths [Int]              -- ^ Document time has any year but a single month

data DocumentDomain
  = DocumentsOfWholeUniverse                     -- ^ All documents in the system. Only for admin view.
  | DocumentsOfAuthor UserID                     -- ^ Documents by author, not deleted
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
  | DocumentsOfCompany CompanyID Bool Bool       -- ^ All documents of a company, with flag for selecting also drafts and deleted
  | DocumentsOfAuthorCompany CompanyID Bool Bool -- ^ All documents of a company by author, with flag for selecting also drafts and deleted

-- | These are possible order by clauses that make documents sorted by.
data DocumentOrderBy
  = DocumentOrderByTitle       -- ^ Order by title, alphabetically, case insensitive
  | DocumentOrderByMTime       -- ^ Order by modification time
  | DocumentOrderByCTime       -- ^ Order by creation time
  | DocumentOrderByStatusClass -- ^ Order by status class.
  | DocumentOrderByType        -- ^ Order by document type.
  | DocumentOrderByProcess     -- ^ Order by process
  | DocumentOrderByPartners    -- ^ Order by partner names or emails
  | DocumentOrderByAuthor      -- ^ Order by author name or email
  | DocumentOrderByService     -- ^ Order by service


-- | Convert DocumentOrderBy enumeration into proper SQL order by statement
documentOrderByToSQL :: DocumentOrderBy -> SQL
documentOrderByToSQL DocumentOrderByTitle = SQL "documents.title" []
documentOrderByToSQL DocumentOrderByMTime = SQL "documents.mtime" []
documentOrderByToSQL DocumentOrderByCTime = SQL "documents.ctime" []
documentOrderByToSQL DocumentOrderByStatusClass =
  SQL (documentStatusClassExpression) []
documentOrderByToSQL DocumentOrderByType = SQL "documents.type" []
documentOrderByToSQL DocumentOrderByProcess = SQL "documents.process" []
documentOrderByToSQL DocumentOrderByPartners =
  parenthesize (selectSignatoryLinksSmartNames [SignatoryPartner])
documentOrderByToSQL DocumentOrderByAuthor =
  parenthesize (selectSignatoryLinksSmartNames [SignatoryAuthor])
documentOrderByToSQL DocumentOrderByService = SQL "documents.service_id" []

selectSignatoryLinksSmartNames :: [SignatoryRole] -> SQL
selectSignatoryLinksSmartNames roles =
      SQL ("SELECT COALESCE(string_agg(x.value,' '),'no fields') FROM ") [] <++>
      SQL ("(SELECT (") [] <++>
      selectSmartName <++>
      SQL (") AS value FROM signatory_links" ++
           " WHERE signatory_links.document_id = documents.id" ++
           "   AND ((signatory_links.roles & ?) <>0)" ++
           " ORDER BY signatory_links.internal_insert_order) AS x") [toSql roles]
  where
    selectFieldAs xtype name = SQL ("(SELECT signatory_link_fields.value AS value " ++
                                    "FROM signatory_link_fields " ++
                                    "WHERE signatory_link_fields.signatory_link_id = signatory_links.id " ++
                                    "AND type = ? " ++
                                    "LIMIT 1) " ++
                                    "AS " ++ name) [toSql xtype]
    {-
     selectSmartName explanation:
     1. Select fields first name, last name and email into separate tables
     2. Use FULL JOIN so we get NULL when field is not there
     3. Convert possible NULLs into empty strings
     4. Concatenate first name and last name with a space between
     5. Trim that string both ends
     6. See if it is empty, if it is, convert to NULL
     7. If NULL use email address instead
     -}
    selectSmartName = SQL ("SELECT " ++
                           "COALESCE(NULLIF(TRIM(BOTH FROM (COALESCE(first_name.value,'') " ++
                           "                                || ' ' || " ++
                           "                                COALESCE(last_name.value,''))), ''),email.value) " ++
                           "FROM (") [] <++>
                      selectFieldAs FirstNameFT "first_name" <++>
                      SQL " FULL JOIN " [] <++>
                      selectFieldAs LastNameFT "last_name" <++>
                      SQL " ON TRUE " [] <++>
                      SQL " FULL JOIN " [] <++>
                      selectFieldAs EmailFT "email" <++>
                      SQL " ON TRUE " [] <++>
                      SQL ") WHERE signatory_links.document_id = documents.id" []

documentOrderByAscDescToSQL :: AscDesc DocumentOrderBy -> SQL
documentOrderByAscDescToSQL (Asc x) = documentOrderByToSQL x
documentOrderByAscDescToSQL (Desc x) = documentOrderByToSQL x <++> SQL " DESC" []

documentDomainToSQL :: DocumentDomain -> SQL
documentDomainToSQL (DocumentsOfWholeUniverse) =
  SQL "TRUE" []
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
documentDomainToSQL (DocumentsOfCompany cid preparation deleted) =
  SQL "signatory_links.company_id = ? AND (? OR documents.status <> ?) AND signatory_links.deleted = ? AND signatory_links.really_deleted = FALSE"
        [toSql cid,toSql preparation, toSql Preparation, toSql deleted]
documentDomainToSQL (DocumentsOfAuthorCompany cid preparation deleted) =
  SQL "(signatory_links.roles & ?) <> 0 AND signatory_links.company_id = ? AND (? OR documents.status <> ?) AND signatory_links.deleted = ? AND signatory_links.really_deleted = FALSE"
        [toSql [SignatoryAuthor], toSql cid, toSql preparation, toSql Preparation, toSql deleted]


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
documentFilterToSQL (DocumentFilterByStatusClass []) =
  SQL "FALSE" []
documentFilterToSQL (DocumentFilterByStatusClass statuses) =
  SQL (documentStatusClassExpression ++ " IN (" ++ intercalate "," (map (const "?") statuses) ++ ")")
                               (map (toSql . fromEnum) statuses)
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
documentFilterToSQL (DocumentFilterByMonths months) =
  sqlConcatOR $ map (\month -> SQL "EXTRACT (MONTH FROM documents.mtime) = ?" [toSql month]) months
documentFilterToSQL (DocumentFilterByYears years) =
  -- Filtering by year is supposed to show all documents with time in
  -- a year, for example for year 2011 it will show all documents from
  -- 2011-01-01 00:00 to 2012-01-01 00:00 last date should be
  -- excluded, but to use BETWEEN we skip this small issue
  --
  -- Note: using mtime isn't the smartest thing we could do here. We
  -- have some times associated with document and we need to sort out
  -- which of times should be used for filtering.
  sqlConcatOR $ map (\year -> SQL ("documents.mtime BETWEEN '" ++ show year ++ "-01-01' AND '" ++ show (year+1) ++ "-01-01'") []) years
documentFilterToSQL (DocumentFilterByTags []) =
  SQL "TRUE" []
documentFilterToSQL (DocumentFilterByTags tags) =
  sqlConcatAND $ map (\tag -> SQL "EXISTS (SELECT 1 FROM document_tags WHERE name = ? AND value = ? AND document_id = documents.id)"
                              [toSql $ tagname tag, toSql $ tagvalue tag]) tags
documentFilterToSQL (DocumentFilterByString string) =
  result
  where
      result = SQL "documents.title ILIKE ?" [sqlpat string] `sqlOR`
         sqlJoinWithAND (map sqlMatch (words string))
      sqlMatch word = SQL ("EXISTS (SELECT TRUE" ++
                                   "  FROM signatory_link_fields JOIN signatory_links AS sl5" ++
                                                                 "  ON sl5.document_id = signatory_links.document_id" ++
                                                                 " AND sl5.id = signatory_link_fields.signatory_link_id" ++
                                   -- " FROM signatory_link_fields " ++
                                   " WHERE signatory_link_fields.value ILIKE ?)") [sqlpat word]
                                   --" WHERE TRUE)") []

      sqlpat text = toSql $ "%" ++ concatMap escape text ++ "%"
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

getOneDocumentAffected :: MonadDB m => String -> Integer -> DocumentID -> DBEnv m (Either String Document)
getOneDocumentAffected text r did =
  case r of
    0 -> do
      return (Left (text ++ " did not affect any rows"))
    1 -> do
      mnewdoc <- query $ GetDocumentByDocumentID did
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
                         , checkEqualBy "signatoryfields" (sort . signatoryfields . signatorydetails)
                         ]

    inequalities = catMaybes $ map (\f -> f d1 d2)
                   [ checkEqualBy "documenttitle" documenttitle
                   , checkEqualBy "documentfiles" documentfiles
                   , checkEqualBy "documentsealedfiles" documentsealedfiles
                   , checkEqualBy "documentstatus" documentstatus
                   , checkEqualBy "documenttype" documenttype
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


documentsSelectors :: SQL
documentsSelectors = SQL (intercalate ", " [
    "id"
  , "title"
  , "file_id"
  , "sealed_file_id"
  , "status"
  , "error_text"
  , "type"
  , "process"
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
  ]) []

selectDocumentsSQL :: SQL
selectDocumentsSQL = SQL "SELECT " [] <++>
                     documentsSelectors <++>
                     SQL " FROM documents " []

fetchDocuments :: MonadDB m => DBEnv m [Document]
fetchDocuments = foldDB decoder []
  where
    -- Note: this function gets documents in reversed order, but all queries
    -- use reversed order too, so in the end everything is properly ordered.
    decoder acc did title file_id sealed_file_id status error_text simple_type
     process ctime mtime days_to_sign timeout_time invite_time
     invite_ip dlog invite_text allowed_id_types cancelationreason rejection_time
     rejection_signatory_link_id rejection_reason service deleted mail_footer
     region sharing status_class
       = Document {
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
  , "signatory_links.signinfo_ocsp_response"
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
  ++ " ON signatory_attachments.signatory_link_id = signatory_links.id) "
  ++ " JOIN documents "
  ++ " ON signatory_links.document_id = documents.id ") []

fetchSignatoryLinks :: MonadDB m => DBEnv m (M.Map DocumentID [SignatoryLink])
fetchSignatoryLinks = do
  sigs <- foldDB decoder (nulldocid, [], M.empty)
  return $ (\(d, l, m) -> M.insertWith' (++) d l m) sigs
  where
    nulldocid = unsafeDocumentID $ -1
    decoder (docid, links, linksmap) slid document_id user_id company_id
     sign_order token sign_time sign_ip seen_time seen_ip read_invitation
     invitation_delivery_status signinfo_text signinfo_signature signinfo_certificate
     signinfo_provider signinfo_first_name_verified signinfo_last_name_verified
     signinfo_personal_number_verified signinfo_ocsp_response 
     roles csv_title csv_contents csv_signatory_index
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
            , signatoryfields = []
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
                , signatureinfoocspresponse = signinfo_ocsp_response
                }
          , signatoryroles = roles
          , signatorylinkdeleted = deleted
          , signatorylinkreallydeleted = really_deleted
          , signatorylinkcsvupload =
              CSVUpload <$> csv_title <*> csv_contents <*> csv_signatory_index
          , signatoryattachments = sigAtt
          , signatorylinkstatusclass = toEnum (status_class :: Int)
          }

insertSignatoryLinkAsIs :: MonadDB m => DocumentID -> SignatoryLink -> DBEnv m (Maybe SignatoryLink)
insertSignatoryLinkAsIs documentid link = do
  _ <- kRun $ mkSQL INSERT tableSignatoryLinks
           [ sql "document_id" documentid
           , sql "user_id" $ maybesignatory link
           , sql "roles" $ signatoryroles link
           , sql "company_id" $ maybecompany link
           , sql "token" $ signatorymagichash link
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
           , sql "signinfo_ocsp_response" $ signatureinfoocspresponse `fmap` signatorysignatureinfo link
           ] <++> SQL " RETURNING id" []

  slids <- foldDB (\acc slid -> slid : acc) []

  _ <- kRun $ selectSignatoryLinksSQL <++> SQL "WHERE signatory_links.id = ? AND signatory_links.document_id = ? ORDER BY internal_insert_order DESC"
       [$(head) slids, toSql documentid]

  msiglink <- fetchSignatoryLinks
              >>= oneObjectReturnedGuard . concatMap snd . M.toList

  case msiglink of
    Nothing -> return Nothing
    Just siglink -> do
      msigattaches <- mapM (insertSignatoryAttachmentAsIs (signatorylinkid siglink)) (signatoryattachments link)
      mfields <- mapM (insertSignatoryLinkFieldAsIs (signatorylinkid siglink)) ((signatoryfields . signatorydetails) link)
      if any isNothing msigattaches || any isNothing mfields
        then return Nothing
        else do
          let newsiglink = siglink { signatoryattachments = catMaybes msigattaches
                                , signatorydetails = (signatorydetails siglink) { signatoryfields = catMaybes mfields }
                                }
          return (Just newsiglink)

signatoryAttachmentsSelectors :: String
signatoryAttachmentsSelectors = intercalate ", "
  [ "signatory_link_id"
  , "file_id"
  , "name"
  , "description"
  ]

fetchSignatoryAttachments :: MonadDB m => DBEnv m (M.Map SignatoryLinkID [SignatoryAttachment])
fetchSignatoryAttachments = foldDB decoder M.empty
  where
    decoder acc signatory_link_id file_id name description =
      M.insertWith' (++) signatory_link_id [SignatoryAttachment {
          signatoryattachmentfile = file_id
        , signatoryattachmentname = name
        , signatoryattachmentdescription = description
        }] acc


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

fetchDocumentTags :: MonadDB m => DBEnv m (M.Map DocumentID [DocumentTag])
fetchDocumentTags = foldDB decoder M.empty
  where
    decoder acc document_id name v =
      M.insertWith' (++) document_id
         [DocumentTag name v] acc

insertDocumentTagAsIs :: MonadDB m => DocumentID -> DocumentTag -> DBEnv m (Maybe DocumentTag)
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

fetchAuthorAttachments :: MonadDB m => DBEnv m (M.Map DocumentID [AuthorAttachment])
fetchAuthorAttachments = foldDB decoder M.empty
  where
    decoder acc document_id file_id =
      M.insertWith' (++) document_id [AuthorAttachment {
        authorattachmentfile = file_id
      }] acc

insertAuthorAttachmentAsIs :: MonadDB m => DocumentID -> AuthorAttachment -> DBEnv m (Maybe AuthorAttachment)
insertAuthorAttachmentAsIs documentid attach = do
  _ <- kRun $ mkSQL INSERT tableAuthorAttachments [
      sql "file_id" $ authorattachmentfile attach
    , sql "document_id" documentid
    ] <++> SQL ("RETURNING " ++ authorAttachmentsSelectors) []

  fetchAuthorAttachments
    >>= oneObjectReturnedGuard . concatMap snd . M.toList

insertSignatoryAttachmentAsIs :: MonadDB m => SignatoryLinkID -> SignatoryAttachment -> DBEnv m (Maybe SignatoryAttachment)
insertSignatoryAttachmentAsIs slid SignatoryAttachment {..} = do
  _ <- kRun $ mkSQL INSERT tableSignatoryAttachments [
        sql "file_id" signatoryattachmentfile
       , sql "name" signatoryattachmentname
       , sql "description" signatoryattachmentdescription
       , sql "signatory_link_id" slid
       ] <++> SQL ("RETURNING " ++ signatoryAttachmentsSelectors) []

  fetchSignatoryAttachments
    >>= oneObjectReturnedGuard . concatMap snd . M.toList

signatoryLinkFieldsSelectors :: String
signatoryLinkFieldsSelectors = intercalate ", "
  [ "signatory_link_id"
  , "type"
  , "custom_name"
  , "is_author_filled"
  , "value"
  , "placements"
  ]

selectSignatoryLinkFieldsSQL :: SQL
selectSignatoryLinkFieldsSQL = SQL ("SELECT "
  ++ signatoryLinkFieldsSelectors
  ++ " FROM signatory_link_fields ") []

fetchSignatoryLinkFields :: MonadDB m => DBEnv m (M.Map SignatoryLinkID [SignatoryField])
fetchSignatoryLinkFields = foldDB decoder M.empty
  where
    decoder acc slid xtype custom_name is_author_filled v placements =
      M.insertWith' (++) slid
         [SignatoryField
          { sfValue = v
          , sfPlacements = placements
          , sfType = case xtype of
                        CustomFT{} -> CustomFT custom_name is_author_filled
                        CheckboxOptionalFT{} -> CheckboxOptionalFT custom_name 
                        CheckboxObligatoryFT{} -> CheckboxObligatoryFT custom_name 
                        _   -> xtype
          }] acc

insertSignatoryLinkFieldAsIs :: MonadDB m => SignatoryLinkID -> SignatoryField -> DBEnv m (Maybe SignatoryField)
insertSignatoryLinkFieldAsIs slid field = do
  _ <- kRun $ mkSQL INSERT tableSignatoryLinkFields
       [ sql "signatory_link_id" $ slid
       , sql "type" $ sfType field
       , sql "custom_name" $ case sfType field of
                                CustomFT name _ -> name
                                CheckboxOptionalFT name -> name
                                CheckboxObligatoryFT name -> name
                                _ -> ""
       , sql "is_author_filled"  $ case sfType field of
                                CustomFT _ authorfilled -> authorfilled
                                CheckboxOptionalFT _  -> False
                                CheckboxObligatoryFT _  -> False
                                _ -> False
       , sql "value" $ sfValue field
       , sql "placements" $ sfPlacements field
       ] <++> SQL ("RETURNING " ++ signatoryLinkFieldsSelectors) []

  fetchSignatoryLinkFields
    >>= oneObjectReturnedGuard . concatMap snd . M.toList

insertDocumentAsIs :: MonadDB m => Document -> DBEnv m (Maybe Document)
insertDocumentAsIs document = do
    let Document { documenttitle
                 , documentsignatorylinks
                 , documentfiles
                 , documentsealedfiles
                 , documentstatus
                 , documenttype
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
      ] <++> SQL "RETURNING " [] <++> documentsSelectors

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

insertNewDocument :: MonadDB m => Document -> DBEnv m Document
insertNewDocument doc = do
  now <- getMinutesTime
  let docWithTime = doc {documentmtime  = now, documentctime = now}
  newdoc <- insertDocumentAsIs docWithTime
  case newdoc of
    Just d -> return d
    Nothing -> error "insertNewDocument failed for some reason"


-- Create new document based on existing one
newFromDocument :: MonadDB m => (Document -> Document) -> DocumentID -> DBEnv m (Either String Document)
newFromDocument f docid = do
  mdoc <- query $ GetDocumentByDocumentID docid
  case mdoc of
      Just doc -> Right `liftM` insertNewDocument (f doc)
      Nothing -> return $ Left $ "Document " ++ show docid ++ " does not exist"

{- |
    The existance of this function is wrong.  What it means is that storing
    maybesignatory and maybecompany on the signatory links is the wrong way of doing it,
    and there should be something else for hooking accounts to sig links that doesn't
    involve editing all the docs as a user moves between private and company accounts.
-}
data AdminOnlySaveForUser = AdminOnlySaveForUser DocumentID User Actor

instance (MonadDB m, TemplatesMonad m) => DBUpdate m AdminOnlySaveForUser (Either String Document) where
  update (AdminOnlySaveForUser did user _actor) = do
    r <- kRun $ mkSQL UPDATE tableSignatoryLinks [sql "company_id" $ usercompany user]
      <++> SQL "WHERE document_id = ? AND user_id = ? " [
        toSql did
      , toSql $ userid user
      ]
    getOneDocumentAffected "AdminOnlySaveForUser" r did

data ArchiveDocument = ArchiveDocument User DocumentID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m ArchiveDocument (Either String Document) where
  update (ArchiveDocument user did _actor) = do
    r <- case (usercompany user, useriscompanyadmin user) of
      (Just cid, True) -> updateArchivableDoc $ SQL "WHERE (company_id = ? OR user_id = ?)" [toSql cid,toSql $ userid user]
      _ -> updateArchivableDoc $ SQL "WHERE user_id = ?" [toSql $ userid user]
    -- a supervisor could delete both their own and another subaccount's links
    -- on the same document, so this would mean the sig link count affected
    -- is more than 1. see bug 1195.
    let fudgedr = if r==0 then 0 else 1
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

data AttachCSVUpload = AttachCSVUpload DocumentID SignatoryLinkID CSVUpload Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m AttachCSVUpload (Either String Document) where
  update (AttachCSVUpload did slid csvupload actor) = do
    mdocument <- query $ GetDocumentByDocumentID did
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
              update $ InsertEvidenceEvent
              AttachCSVUploadEvidence
              (value "csvtitle" (csvtitle csvupload) >> value "actor" (actorWho actor))
              (Just did)
              actor
            getOneDocumentAffected "AttachCSVUpload" r did
          _ -> return $ Left $ "Document #" ++ show documentid ++ " is in " ++ show (documentstatus document) ++ " state, must be Preparation"

data AttachFile = AttachFile DocumentID FileID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m AttachFile (Either String Document) where
  update (AttachFile did fid a) = do
    let time = actorTime a
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "mtime" time
      , sql "file_id" $ fid
      , sqlLog time $ "Attached main file " ++ show fid
      ] <++> SQL "WHERE id = ? AND status = ?" [toSql did, toSql Preparation]
    mf <- query $ GetFileByFileID fid
    when_ (r == 1) $
      update $ InsertEvidenceEvent
        AttachFileEvidence
        (value "actor" (actorWho a) >> value "filename" (filename <$> mf))
        (Just did)
        a
    getOneDocumentAffected "AttachFile" r did

data AttachSealedFile = AttachSealedFile DocumentID FileID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m AttachSealedFile (Either String Document) where
  update (AttachSealedFile did fid actor) = do
    let time = actorTime actor
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "mtime" time
      , sql "sealed_file_id" fid
      , sqlLog time $ "Attached sealed file " ++ show fid
      ] <++> SQL "WHERE id = ? AND status = ?" [toSql did, toSql Closed]
    when_ (r == 1) $
      update $ InsertEvidenceEvent
      AttachSealedFileEvidence
      (value "actor"(actorWho actor))
      (Just did)
      actor
    getOneDocumentAffected "AttachSealedFile" r did

data CancelDocument = CancelDocument DocumentID CancelationReason Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m CancelDocument (Either String Document) where
  update (CancelDocument did reason actor) = do
    let mtime = actorTime actor
    mdocument <- query $ GetDocumentByDocumentID did
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
                  update $ InsertEvidenceEvent
                  CancelDocumentEvidence
                  (value "actor" (actorWho actor))
                  (Just did)
                  actor

                ELegDataMismatch _ sid fn ln num -> do
                  let Just sl = getSigLinkFor document sid
                      trips = [("First name",      getFirstName      sl, fn)
                              ,("Last name",       getLastName       sl, ln)
                              ,("Personal number", getPersonalNumber sl, num)]
                      uneql = filter (\(_,a,b)->a/=b) trips
                      msg = intercalate "; " $ map (\(f,s,e)->f ++ " from transaction was \"" ++ s ++ "\" but from e-legitimation was \"" ++ e ++ "\"") uneql
                  update $ InsertEvidenceEvent
                    CancelDocumenElegEvidence
                    (value "actor" (actorWho actor) >> value "msg" msg )
                    (Just did)
                    actor

            getOneDocumentAffected "CancelDocument" r did
          s -> return $ Left $ "Cannot CancelDocument document " ++ show did ++ " because " ++ concat s

data ChangeMainfile = ChangeMainfile DocumentID FileID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m ChangeMainfile (Either String Document) where
  update (ChangeMainfile did fid actor) = do
    mdocument <- query $ GetDocumentByDocumentID did
    case mdocument of
      Nothing -> return $ Left $ "Cannot ChangeMainfile document " ++ show did ++ " because it does not exist"
      Just document -> do
        case getFileIDsByStatus document of
          [ofid] -> do
            let fieldname = if (documentstatus document == Closed || allHadSigned document)
                            then "sealed_file_id"
                            else "file_id"
            r <- kRun $ mkSQL UPDATE tableDocuments [sql fieldname $ fid]
                 <++> SQL "WHERE id = ?" [toSql did]
        
            mof <- query $ GetFileByFileID ofid
            mnf <- query $ GetFileByFileID fid
            when_ (r == 1) $
              update $ InsertEvidenceEvent
              ChangeMainfileEvidence
              (value "actor" (actorWho actor) >> value "oldfilename" (filename <$> mof) >> value "newfilename" (filename <$> mnf))
              (Just did)
              actor
            getOneDocumentAffected "ChangeMainfile" r did
          fs -> return $ Left $ "Cannot ChangeMainfile document " ++ show did ++ ": non-one number of main files: " ++ show (length fs)
    where
        allHadSigned doc = all (hasSigned ||^ (not . isSignatory)) $ documentsignatorylinks doc

data ChangeSignatoryEmailWhenUndelivered = ChangeSignatoryEmailWhenUndelivered DocumentID SignatoryLinkID (Maybe User) String Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m ChangeSignatoryEmailWhenUndelivered (Either String Document) where
  update (ChangeSignatoryEmailWhenUndelivered did slid muser email actor) = do
    Just doc <- query $ GetDocumentByDocumentID did
    if (documentstatus doc /= Pending)
     then
         return $ Left $ "Cannot ChangeSignatoryEmailWhenUndelivered for document #" ++ show did
               ++ " that is in " ++ show (documentstatus doc) ++ " state"
     else do

      let Just sl = getSigLinkFor doc slid
          oldemail = getEmail sl
      r1 <- kRun $ mkSQL UPDATE tableSignatoryLinkFields [
             sql "value" email
            ] <++> SQL (" WHERE signatory_link_id = ? AND type = ?") [toSql slid, toSql EmailFT]
      r <- kRun $ mkSQL UPDATE tableSignatoryLinks [
          sql "invitation_delivery_status" Unknown
        , sql "user_id" $ fmap userid muser
        , sql "company_id" $ muser >>= usercompany
        ] <++> SQL "WHERE EXISTS (SELECT 1 FROM documents WHERE documents.id = signatory_links.document_id AND documents.status = ?) AND id = ?" [
          toSql Pending
        , toSql slid
        ]
      when_ (r == 1 && r1 == 1) $
        update $ InsertEvidenceEvent
          ChangeSignatoryEmailWhenUndeliveredEvidence
          (value "oldemail" oldemail >> value "newemail" email >> value "actor" (actorWho actor))
          (Just did)
          actor

      getOneDocumentAffected "ChangeSignatoryEmailWhenUndelivered" r did

data PreparationToPending = PreparationToPending DocumentID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m PreparationToPending (Either String Document) where
  update (PreparationToPending docid actor) = do
    let time = actorTime actor
    mdocument <- query $ GetDocumentByDocumentID docid
    case mdocument of
      Nothing -> return $ Left $ "Cannot PreparationToPending document " ++ show docid ++ " because it does not exist"
      Just document ->
        case checkPreparationToPending document of
          [] -> do
            let mtt = (\days -> (days * 24 *60) `minutesAfter` time) <$> documentdaystosign document
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
              update $ InsertEvidenceEvent
              PreparationToPendingEvidence
              (value "actor" (actorWho actor) >> value "timeouttime" (formatMinutesTimeUTC <$> mtt))
              (Just docid)
              actor
            getOneDocumentAffected "PreparationToPending" r docid
          s -> return $ Left $ "Cannot PreparationToPending document " ++ show docid ++ " because " ++ concat s

data CloseDocument = CloseDocument DocumentID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m CloseDocument (Either String Document) where
  update (CloseDocument docid actor) = do
    let time = actorTime actor
        ipaddress = fromMaybe noIP $ actorIP actor
    mdocument <- query $ GetDocumentByDocumentID docid
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
              update $ InsertEvidenceEvent
              CloseDocumentEvidence
              (value "actor" (actorWho actor))
              (Just docid)
              actor
            getOneDocumentAffected "CloseDocument" r docid
          s -> return $ Left $ "Cannot CloseDocument " ++ show docid ++ " because " ++ concat s

data DeleteSigAttachment = DeleteSigAttachment DocumentID SignatoryLinkID FileID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m DeleteSigAttachment (Either String Document) where
  update (DeleteSigAttachment did slid fid actor) = do
    mdoc <- query $ GetDocumentByDocumentID did
    case mdoc of
      Nothing -> return $ Left $ "Document does not exist. Trying to Delete Sig Attachment. docid: " ++ show did
      Just doc -> case getSigLinkFor doc slid of
        Nothing -> return $ Left $ "SignatoryLinkID not in document: docid: " ++ show did ++ " slid: " ++ show slid
        Just sig -> case find (\sl->signatoryattachmentfile sl == Just fid) $ signatoryattachments sig of
          Nothing -> return $ Left $ "No signatory attachment for that file id: " ++ show fid
          Just sa -> do
            r <- kRun $ mkSQL UPDATE tableSignatoryAttachments [sql "file_id" SqlNull]
                 <++> SQL "WHERE file_id = ? AND signatory_link_id = ?"
                 [ toSql fid
                 , toSql slid
                 ]
            when_ (r == 1) $
              update $ InsertEvidenceEvent
              DeleteSigAttachmentEvidence
              (value "actor" (actorWho actor) >> value "name" (signatoryattachmentname sa) >> value "email" (getEmail sig))
              (Just did)
              actor
            getOneDocumentAffected "DeleteSigAttachment" r did

data DocumentFromSignatoryData = DocumentFromSignatoryData DocumentID String String String String String String [String] Actor
instance (CryptoRNG m, MonadDB m,TemplatesMonad m) => DBUpdate m DocumentFromSignatoryData (Either String Document) where
  update (DocumentFromSignatoryData docid fstname sndname email company personalnumber companynumber fieldvalues actor) = do
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Left $ "In DocumentFromSignatoryData: Document does not exist for id: " ++ show docid
      Just doc -> do
        mhs <- mapM (\_ -> lift random) (documentsignatorylinks doc)
        ed <- newFromDocument (toNewDoc mhs) docid
        when_ (isRight ed) $
          let Right d = ed
          in do
            copyEvidenceLogToNewDocument docid (documentid d)
            update $ InsertEvidenceEvent
                  AuthorUsesCSVEvidence
                  (value "actor" (actorWho actor) >> value "did" (show docid))
                  (Just $ documentid d)
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
     toNewSigLink :: MagicHash -> SignatoryLink -> SignatoryLink
     toNewSigLink mh sl
         | isJust (signatorylinkcsvupload sl) = (pumpData sl) { signatorylinkcsvupload = Nothing, signatorymagichash = mh }
         | otherwise = sl { signatorymagichash = mh }
     pumpData :: SignatoryLink -> SignatoryLink
     pumpData siglink = replaceSignatoryData siglink fstname sndname email company personalnumber companynumber fieldvalues

data ErrorDocument = ErrorDocument DocumentID String Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m ErrorDocument (Either String Document) where
  update (ErrorDocument docid errmsg actor) = do
    mdocument <- query $ GetDocumentByDocumentID docid
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
              update $ InsertEvidenceEvent
              ErrorDocumentEvidence
              (value "errmsg" errmsg >> value "actor" (actorWho actor))
              (Just docid)
              actor
            getOneDocumentAffected "ErrorDocument" r docid
          s -> return $ Left $ "Cannot ErrorDocument document " ++ show docid ++ " because " ++ concat s

selectDocuments :: MonadDB m => SQL -> DBEnv m [Document]
selectDocuments sqlquery = do
    _ <- kRun $ SQL "CREATE TEMP TABLE docs AS " [] <++> sqlquery

    _ <- kRun $ SQL "SELECT * FROM docs" []
    docs <- reverse `liftM` fetchDocuments

    _ <- kRun $ SQL "CREATE TEMP TABLE links AS " [] <++>
         selectSignatoryLinksSQL <++>
         SQL "WHERE EXISTS (SELECT 1 FROM docs WHERE signatory_links.document_id = docs.id) ORDER BY document_id DESC, internal_insert_order DESC" []
    _ <- kRun $ SQL "SELECT * FROM links" []
    sls <- fetchSignatoryLinks

    _ <- kRun $ selectSignatoryLinkFieldsSQL <++> SQL "WHERE EXISTS (SELECT 1 FROM links WHERE links.id = signatory_link_fields.signatory_link_id) ORDER BY signatory_link_fields.id" []
    fields <- fetchSignatoryLinkFields

    _ <- kRun $ selectAuthorAttachmentsSQL <++> SQL "WHERE EXISTS (SELECT 1 FROM docs WHERE author_attachments.document_id = docs.id) ORDER BY document_id DESC" []
    ats <- fetchAuthorAttachments

    _ <- kRun $ selectDocumentTagsSQL <++> SQL "WHERE EXISTS (SELECT 1 FROM docs WHERE document_tags.document_id = docs.id)" []
    tags <- fetchDocumentTags

    kRunRaw "DROP TABLE docs"
    kRunRaw "DROP TABLE links"

    let sls2 = M.map (map $ \sl -> sl { signatorydetails =
                                    (signatorydetails sl) { signatoryfields = reverse $ M.findWithDefault [] (signatorylinkid sl) fields }}) sls

    let fill doc = doc
                   { documentsignatorylinks       = M.findWithDefault [] (documentid doc) sls2
                   , documentauthorattachments    = M.findWithDefault [] (documentid doc) ats
                   , documenttags                 = M.findWithDefault [] (documentid doc) tags
                   }

    return $ map fill docs

data GetDocumentByDocumentID = GetDocumentByDocumentID DocumentID
instance MonadDB m => DBQuery m GetDocumentByDocumentID (Maybe Document) where
  query (GetDocumentByDocumentID did) = do
    selectDocuments (selectDocumentsSQL
      <++> SQL "WHERE id = ? AND deleted = FALSE" [toSql did])
      >>= oneObjectReturnedGuard

data GetDocumentsByService = GetDocumentsByService (Maybe ServiceID)
instance MonadDB m => DBQuery m GetDocumentsByService [Document] where
  query (GetDocumentsByService msid) =
    query (GetDocuments [DocumentsOfService msid] [] [Asc DocumentOrderByMTime] (DocumentPagination 0 maxBound))

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
instance MonadDB m => DBQuery m GetDocuments [Document] where
  query (GetDocuments domains filters orderbys pagination) = do
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
instance MonadDB m => DBQuery m GetDocumentsByCompanyWithFiltering [Document] where
  query (GetDocumentsByCompanyWithFiltering companyid filters) =
    query (GetDocuments [DocumentsOfCompany companyid True False] filters [Asc DocumentOrderByMTime] (DocumentPagination 0 maxBound))

data GetDocumentsByAuthorCompanyWithFiltering = GetDocumentsByAuthorCompanyWithFiltering CompanyID [DocumentFilter]
instance MonadDB m => DBQuery m GetDocumentsByAuthorCompanyWithFiltering [Document] where
  query (GetDocumentsByAuthorCompanyWithFiltering companyid filters) =
    query (GetDocuments [DocumentsOfAuthorCompany companyid True False] filters [Asc DocumentOrderByMTime] (DocumentPagination 0 maxBound))


data GetDeletedDocumentsByUser = GetDeletedDocumentsByUser UserID
instance MonadDB m => DBQuery m GetDeletedDocumentsByUser [Document] where
  query (GetDeletedDocumentsByUser uid) =
    query (GetDocuments [DocumentsForSignatoryDeleteValue uid True] [] [Asc DocumentOrderByMTime] (DocumentPagination 0 maxBound))

{- |
    All documents authored by the user that have never been deleted.
-}
data GetDocumentsByAuthor = GetDocumentsByAuthor UserID
instance MonadDB m => DBQuery m GetDocumentsByAuthor [Document] where
  query (GetDocumentsByAuthor uid) =
    query (GetDocuments [DocumentsOfAuthor uid, TemplatesOfAuthor uid] [] [Asc DocumentOrderByMTime] (DocumentPagination 0 maxBound))

data GetTemplatesByAuthor = GetTemplatesByAuthor UserID
instance MonadDB m => DBQuery m GetTemplatesByAuthor [Document] where
  query (GetTemplatesByAuthor uid) =
    query (GetDocuments [TemplatesOfAuthor uid] [] [Asc DocumentOrderByMTime] (DocumentPagination 0 maxBound))

data GetAvailableTemplates = GetAvailableTemplates UserID [DocumentProcess]
instance MonadDB m => DBQuery m GetAvailableTemplates [Document] where
  query (GetAvailableTemplates uid processes) =
    query (GetDocuments [TemplatesOfAuthor uid, TemplatesSharedInUsersCompany uid]
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
instance MonadDB m => DBQuery m GetDocumentsBySignatory [Document] where
  query (GetDocumentsBySignatory processes uid) =
    query (GetDocuments [DocumentsForSignatory uid] [DocumentFilterByProcess processes] [Asc DocumentOrderByMTime] (DocumentPagination 0 maxBound))

data GetTimeoutedButPendingDocuments = GetTimeoutedButPendingDocuments MinutesTime
instance MonadDB m => DBQuery m GetTimeoutedButPendingDocuments [Document] where
  query (GetTimeoutedButPendingDocuments mtime) = do
    selectDocuments $ selectDocumentsSQL
      <++> SQL "WHERE status = ? AND timeout_time IS NOT NULL AND timeout_time < ?" [
        toSql Pending
      , toSql mtime
      ]

data MarkDocumentSeen = MarkDocumentSeen DocumentID SignatoryLinkID MagicHash Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m MarkDocumentSeen (Either String Document) where
  update (MarkDocumentSeen did slid mh actor) = do
    -- have to make sure slid and mh match to record log; sorry for inefficiency -EN
    mdoc <- query $ GetDocumentByDocumentID did
    case mdoc of
      Nothing -> return $ Left $ "document does not exist with id " ++ show did
      Just doc -> case (getSigLinkFor doc (slid, mh)) of
        Nothing -> return $ Left $ "signatory link id and magic hash do not match! documentid: " ++ show did ++ " slid: " ++ show slid ++ " mh: " ++ show mh
        Just sig -> do
          let time = actorTime actor
              ipnumber = fromMaybe noIP $ actorIP actor

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

          _ <- update $ InsertEvidenceEvent
               MarkDocumentSeenEvidence
               (value "actor" (actorWho actor) >> value "email"  (getEmail sig) >> value "ip" (isJust $ actorIP actor))
               (Just did)
               actor
          getOneDocumentAffected "MarkDocumentSeen" r did

data AddInvitationEvidence = AddInvitationEvidence DocumentID SignatoryLinkID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m AddInvitationEvidence (Either String Document) where
  update (AddInvitationEvidence docid slid actor) = do
  -- modifySignable docid $ \document ->
  -- case checkAddEvidence document slid of
  --  [] -> let Just sds = signatorydetails <$> getSigLinkFor document slid
  --        in Right $ document { documentinvitetime = Just (SignInfo time ipnumber) }
  --           `appendHistory` [DocumentHistoryInvitationSent time ipnumber [sds]]
  --  s -> Left $ "Document " ++ show documentid ++ " cannot have evidence attached for signatory " ++ show slid ++ " because " ++ concat s
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Left "no such document"
      Just doc -> do
        case getSigLinkFor doc slid of
          Just sl -> do
            let eml = getEmail sl
            _ <- update $ InsertEvidenceEvent
                 InvitationEvidence
                 (value "email" eml >> value "actor" (actorWho actor))
                 (Just docid)
                 actor
            return $ Right doc
          Nothing ->
            return $ Left $ "SignatoryLinkID " ++ show slid ++ " does not exist in document with id " ++ show docid

data MarkInvitationRead = MarkInvitationRead DocumentID SignatoryLinkID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m MarkInvitationRead (Either String Document) where
  update (MarkInvitationRead did linkid actor) = do
    mdoc <- query $ GetDocumentByDocumentID did
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
            update $ InsertEvidenceEvent
            MarkInvitationReadEvidence
            (value "email" eml >> value "actor" (actorWho actor))
            (Just did)
            actor
          getOneDocumentAffected "MarkInvitationRead" r did

data NewDocument = NewDocument User (Maybe Company) String DocumentType Int Actor
instance (CryptoRNG m, MonadDB m, TemplatesMonad m) => DBUpdate m NewDocument (Either String Document) where
  update (NewDocument user mcompany title documenttype nrOfOtherSignatories actor) = do
  let ctime = actorTime actor
  if fmap companyid mcompany /= usercompany user
    then return $ Left "company and user don't match"
    else do

      let authorRoles = if ((Just True) == getValueForProcess documenttype processauthorsend)
                        then [SignatoryAuthor]
                        else [SignatoryPartner, SignatoryAuthor]

      magichash <- lift random

      let authorlink0 = signLinkFromDetails'
                        (signatoryDetailsFromUser user mcompany)
                        authorRoles [] magichash

      let authorlink = authorlink0 {
                         maybesignatory = Just $ userid user,
                         maybecompany = usercompany user }

      othersignatories <- sequence $ replicate nrOfOtherSignatories $ do
                        mh <- lift random
                        return $ signLinkFromDetails'
                                SignatoryDetails
                                                {  signatorysignorder = SignOrder 1
                                                 , signatoryfields   = emptySignatoryFields
                                                }
                                [SignatoryPartner] [] mh

      let doc = blankDocument
                { documenttitle                = title
                , documentsignatorylinks       = authorlink : othersignatories
                , documenttype                 = documenttype
                , documentregion               = getRegion user
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
               _<- update $ InsertEvidenceEvent
                 NewDocumentEvidence
                 (value "title" title >> value "actor" (actorWho actor))
                 (Just $ documentid doc')
                 actor
               return $ Right doc'
             Nothing -> do
               Log.debug $ "insertDocumentAsIs could not insert document #" ++ show (documentid doc) ++ " in NewDocument"
               return $ Left $ "insertDocumentAsIs could not insert document #" ++ show (documentid doc) ++ " in NewDocument"
        Just a -> do
           Log.debug $ "insertDocumentAsIs invariants violated: " ++ show a
           return $ Left $ "insertDocumentAsIs invariants violated: " ++ show a

data ReallyDeleteDocument = ReallyDeleteDocument User DocumentID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m ReallyDeleteDocument (Either String Document) where
  update (ReallyDeleteDocument user did _actor) = do
    -- I don't like this: we should do this on the DB side, not pass
    -- in a User which could be old. It should be done within a
    -- transaction. -EN
    r <- case (usercompany user, useriscompanyadmin user) of
      (Just cid, True) -> deleteDoc $ SQL "WHERE (company_id = ? OR user_id = ?)" [toSql cid,toSql $ userid user]
      _ -> deleteDoc $ SQL "WHERE user_id = ? AND (company_id IS NULL OR EXISTS (SELECT 1 FROM documents WHERE id = ? AND status = ?))" [toSql $ userid user,toSql did, toSql Preparation]
    let fudgedr = if r==0 then 0 else 1
    getOneDocumentAffected "ReallyDeleteDocument" fudgedr did
    where
      deleteDoc whereClause = kRun $ mconcat [
          mkSQL UPDATE tableSignatoryLinks [sql "really_deleted" True]
        , whereClause
        , SQL " AND document_id = ? AND deleted = TRUE" [toSql did]

        ]

data RejectDocument = RejectDocument DocumentID SignatoryLinkID (Maybe String) Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m RejectDocument (Either String Document) where
  update (RejectDocument docid slid customtext actor) = do
    let time = actorTime actor
    mdocument <- query $ GetDocumentByDocumentID docid
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
                update $ InsertEvidenceEvent
                RejectDocumentEvidence
                (value "email" eml >> value "actor" (actorWho actor))
                (Just docid)
                actor
            getOneDocumentAffected "RejectDocument" r docid
          s -> return $ Left $ "Cannot RejectDocument document " ++ show docid ++ " because " ++ concat s

data RestartDocument = RestartDocument Document Actor
instance (CryptoRNG m, MonadDB m, TemplatesMonad m) => DBUpdate m RestartDocument (Either String Document) where
  update (RestartDocument doc actor) = do
    mndoc <- tryToGetRestarted
    case mndoc of
      Right newdoc -> do
        ed <- newFromDocument (const newdoc) (documentid doc)
        case ed of
          Left s -> return $ Left s
          Right d -> do
            copyEvidenceLogToNewDocument (documentid doc) (documentid d)
            void $ update $ InsertEvidenceEvent
              RestartDocumentEvidence
              (value "did" (show $ documentid doc) >> value "actor" (actorWho actor))
              (Just $ documentid d)
              actor
            return $ Right d
      other -> return other
   where

    tryToGetRestarted =
      if (documentstatus doc `notElem` [Canceled, Timedout, Rejected])
      then return $ Left $ "Can't restart document with " ++ (show $ documentstatus doc) ++ " status"
      else do
             let time = actorTime actor
                 ipnumber = fromMaybe noIP $ actorIP actor
             doc' <- clearSignInfofromDoc
             let doc'' = doc' `appendHistory` [DocumentHistoryRestarted time ipnumber]
             return $ Right doc''

    clearSignInfofromDoc = do
      let signatoriesDetails = map (\x -> (signatorydetails x, signatoryroles x, signatorylinkid x, signatoryattachments x)) $ documentsignatorylinks doc
          Just asl = getAuthorSigLink doc
      newSignLinks <- forM signatoriesDetails $ \(details,roles,linkid, atts) -> do
                           magichash <- lift random
                           return $ (signLinkFromDetails' details roles atts magichash) { signatorylinkid = linkid }
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

data RestoreArchivedDocument = RestoreArchivedDocument User DocumentID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m RestoreArchivedDocument (Either String Document) where
  update (RestoreArchivedDocument user did actor) = do
    r <- case (usercompany user, useriscompanyadmin user) of
      (Just cid, True) -> updateRestorableDoc $ SQL "WHERE company_id = ?" [toSql cid]
      _ -> updateRestorableDoc $ SQL "WHERE user_id = ?" [toSql $ userid user]
    void $ update $ InsertEvidenceEvent
      RestoreArchivedDocumentEvidence
      (value "email" (getEmail user) >> value "actor" (actorWho actor) >> value "company" (isJust (usercompany user) && useriscompanyadmin user))
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
data SaveDocumentForUser = SaveDocumentForUser DocumentID User SignatoryLinkID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SaveDocumentForUser (Either String Document) where
  update (SaveDocumentForUser did User{userid, usercompany} slid _actor) = do
    r <- kRun $ mkSQL UPDATE tableSignatoryLinks [
        sql "user_id" userid
      , sql "company_id" usercompany
      ] <++> SQL "WHERE document_id = ? AND id = ?" [
        toSql did
      , toSql slid
      ]
    getOneDocumentAffected "SaveDocumentForUser" r did

{- |
    Saves a signatory attachment to a document.
    If there's a problem such as the document isn't in a pending or awaiting author state,
    or the document does not exist a Left is returned.
-}
data SaveSigAttachment = SaveSigAttachment DocumentID SignatoryLinkID String FileID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SaveSigAttachment (Either String Document) where
  update (SaveSigAttachment did slid name fid actor) = do
    r <- kRun $ mkSQL UPDATE tableSignatoryAttachments [sql "file_id" fid]
      <++> SQL "WHERE file_id IS NULL AND name = ? AND signatory_link_id = ?"
      [ toSql name
      , toSql slid
      ]
    when_ (r == 1) $
      update $ InsertEvidenceEvent
      SaveSigAttachmentEvidence
      (value "name" name  >> value "actor" (actorWho actor))
      (Just did)
      actor
    getOneDocumentAffected "SaveSigAttachment" r did

data SetDocumentTags = SetDocumentTags DocumentID [DocumentTag] Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetDocumentTags (Either String Document) where
  update (SetDocumentTags did doctags actor) = do
    -- check if the tags are changed
    ed <- query $ GetDocumentByDocumentID did
    let changed = case ed of
          Nothing -> True
          Just d -> not $ listsEqualNoOrder doctags $ documenttags d
    _ <- kRun $ SQL "DELETE FROM document_tags WHERE document_id = ?" [toSql did]
    mtags <- mapM (insertDocumentTagAsIs did) doctags
    let tagstr = intercalate "; " $ map (\(DocumentTag k v)-> k ++ "=" ++ v) doctags
    when_ (not (any isNothing mtags) && changed) $
      update $ InsertEvidenceEvent
      SetDocumentTagsEvidence
      (value "tags" tagstr >> value "actor" (actorWho actor))
      (Just did)
      actor
    getOneDocumentAffected "SetDocumentTags" 1 did

data SetDocumentInviteTime = SetDocumentInviteTime DocumentID MinutesTime Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetDocumentInviteTime (Either String Document) where
  update (SetDocumentInviteTime did invitetime actor) = do
    let ipaddress  = fromMaybe noIP $ actorIP actor
    -- check if it's changed
    ed <- query $ GetDocumentByDocumentID did
    let changed = case ed of
          Nothing -> True
          Just d -> not $ documentinvitetime d == Just (SignInfo invitetime ipaddress)
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "invite_time" invitetime
      , sql "invite_ip" ipaddress
      ] <++> SQL "WHERE id = ?" [toSql did]
    when_ (r == 1 && changed) $
      update $ InsertEvidenceEvent
      SetDocumentInviteTimeEvidence
      (value "time" (formatMinutesTimeUTC invitetime) >> value "actor" (actorWho actor))
      (Just did)
      actor
    getOneDocumentAffected "SetDocumentInviteTime" r did

data SetDocumentTimeoutTime = SetDocumentTimeoutTime DocumentID MinutesTime Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetDocumentTimeoutTime (Either String Document) where
  update (SetDocumentTimeoutTime did timeouttime actor) = do
    ed <- query $ GetDocumentByDocumentID did
    let changed = case ed of
          Nothing -> True
          Just d -> not $ documenttimeouttime d == Just (TimeoutTime timeouttime)
    r <- kRun $ mkSQL UPDATE tableDocuments [sql "timeout_time" timeouttime]
      <++> SQL "WHERE id = ? AND deleted = FALSE AND type = ?" [
        toSql did
      , toSql $ Signable undefined
      ]
    when_ (r == 1 && changed) $
      update $ InsertEvidenceEvent
      SetDocumentTimeoutTimeEvidence
      (value "time" (formatMinutesTimeUTC timeouttime) >> value "actor" (actorWho actor))
      (Just did)
      actor
    getOneDocumentAffected "SetDocumentTimeoutTime" r did

data SetInviteText = SetInviteText DocumentID String Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetInviteText (Either String Document) where
  update (SetInviteText did text actor) = do
    ed <- query $ GetDocumentByDocumentID did
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
      update $ InsertEvidenceEvent
      SetInvitationTextEvidence
      (value "text" text >> value "actor" (actorWho actor))
      (Just did)
      actor
    getOneDocumentAffected "SetInviteText" r did


data SetDaysToSign = SetDaysToSign DocumentID (Maybe Int) Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetDaysToSign (Either String Document) where
  update (SetDaysToSign did mdays actor) = do
    ed <- query $ GetDocumentByDocumentID did
    let changed = case ed of
          Nothing -> False
          Just d -> not $ documentdaystosign d == mdays
    r <- kRun $ mkSQL UPDATE tableDocuments
         [ sql "days_to_sign" $ mdays
         , sql "mtime" $ actorTime actor

         ] <++> SQL "WHERE id = ?" [ toSql did ]
    when_ (r == 1 && changed) $
      update $ InsertEvidenceEvent
      (SetDaysToSignEvidence <| isJust mdays |> RemoveDaysToSignEvidence)
      (value "mdays" (show <$> mdays) >> value "actor" (actorWho actor))
      (Just did)
      actor
    getOneDocumentAffected "SetDaysToSign" r did

data SetDocumentTitle = SetDocumentTitle DocumentID String Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetDocumentTitle (Either String Document) where
  update (SetDocumentTitle did doctitle actor) = do
    ed <- query $ GetDocumentByDocumentID did
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
      update $ InsertEvidenceEvent
      SetDocumentTitleEvidence
      (value "title" doctitle >> value "actor" (actorWho actor))
      (Just did)
      actor
    getOneDocumentAffected "SetDocumentTitle" r did

data SetDocumentLocale = SetDocumentLocale DocumentID Locale Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetDocumentLocale (Either String Document) where
  update (SetDocumentLocale did locale actor) = do
    ed <- query $ GetDocumentByDocumentID did
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
      update $ InsertEvidenceEvent
      SetDocumentLocaleEvidence
      (value "local" (show locale) >> value "actor" (actorWho actor))
      (Just did)
      actor
    getOneDocumentAffected "SetDocumentLocale" r did

data SetDocumentUI = SetDocumentUI DocumentID DocumentUI Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetDocumentUI (Either String Document) where
  update (SetDocumentUI did docui actor) = do
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "mail_footer" $ documentmailfooter docui
         , sql "mtime" $ actorTime actor
      ] <++> SQL "WHERE id = ?" [toSql did]
    getOneDocumentAffected "SetDocumentUI" r did

data SetInvitationDeliveryStatus = SetInvitationDeliveryStatus DocumentID SignatoryLinkID MailsDeliveryStatus Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetInvitationDeliveryStatus (Either String Document) where
  update (SetInvitationDeliveryStatus did slid status actor) = do
    ed <- query $ GetDocumentByDocumentID did
    let (email, changed) = case getSigLinkFor ed slid of
          Nothing -> ("", False)
          Just sl -> (getEmail sl, not $ invitationdeliverystatus sl == status)
    r <- kRun $ mkSQL UPDATE tableSignatoryLinks [
        sql "invitation_delivery_status" status
      ] <++> SQL "WHERE id = ? AND document_id = ? AND EXISTS (SELECT 1 FROM documents WHERE id = ? AND type = ?)" [
        toSql slid
      , toSql did
      , toSql did
      , toSql $ Signable undefined
      ]
    when_ (r == 1 && changed) $
      update $ InsertEvidenceEvent
      SetInvitationDeliveryStatusEvidence
      (value "email" email >> value "status" (show status) >> value "actor" (actorWho actor))
      (Just did)
      actor
    getOneDocumentAffected "SetInvitationDeliveryStatus" r did

data SetDocumentSharing = SetDocumentSharing [DocumentID] Bool
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetDocumentSharing (Either String Bool) where
  update (SetDocumentSharing dids flag) = do
    flip mapM_ dids $ \did -> kRun $ mkSQL UPDATE tableDocuments
         [ sql "sharing" $ (if flag then Shared else Private)
         ] <++> SQL
         " WHERE id = ? AND deleted = FALSE " [ toSql did ]
    return (Right True)

data SignDocument = SignDocument DocumentID SignatoryLinkID MagicHash (Maybe SignatureInfo) Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SignDocument (Either String Document) where
  update (SignDocument docid slid mh msiginfo actor) = do
    mdocument <- query $ GetDocumentByDocumentID docid
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
              , sql "signinfo_ocsp_response" $ signatureinfoocspresponse `fmap` msiginfo
              ] <++> SQL "WHERE id = ? AND document_id = ?" [
                toSql slid
              , toSql docid
              ]
            let signatureFields = case msiginfo of
                  Nothing -> return ()
                  Just si -> do
                            value "eleg" True
                            value "provider" $ case signatureinfoprovider si of
                                                  BankIDProvider -> "BankID"
                                                  TeliaProvider  -> "Telia"
                                                  NordeaProvider -> "Nordea"
                                                  MobileBankIDProvider -> "Mobile BankID"
                            value "fstnameverified" $ signaturefstnameverified si
                            value "lstnameverified" $ signaturelstnameverified si
                            value "persnumverified" $ signaturepersnumverified si
                            value "fieldsverified" $  signaturefstnameverified si || signaturelstnameverified si || signaturepersnumverified si
                            value "signature" $ signatureinfosignature si
                            value "certificate" $ nothingIfEmpty $ signatureinfocertificate si
                            value "ocsp" $ signatureinfoocspresponse si
                            value "infotext" $ signatureinfotext si
            when_ (r == 1) $
              update $ InsertEvidenceEvent
              SignDocumentEvidence
              (signatureFields >> value "actor" (actorWho actor))
              (Just docid)
              actor
            getOneDocumentAffected "SignDocument" r docid
          s -> return $ Left $ "Cannot SignDocument document " ++ show docid ++ " because " ++ concat s

data ResetSignatoryDetails = ResetSignatoryDetails DocumentID [(SignatoryDetails, [SignatoryRole])] Actor
instance (CryptoRNG m, MonadDB m, TemplatesMonad m) => DBUpdate m ResetSignatoryDetails (Either String Document) where
  update (ResetSignatoryDetails documentid signatories actor) =
    update (ResetSignatoryDetails2 documentid (map (\(a,b) -> (a,b,[],Nothing)) signatories) actor)


data ResetSignatoryDetails2 = ResetSignatoryDetails2 DocumentID [(SignatoryDetails, [SignatoryRole], [SignatoryAttachment], Maybe CSVUpload)] Actor
instance (CryptoRNG m, MonadDB m, TemplatesMonad m) => DBUpdate m ResetSignatoryDetails2 (Either String Document) where
  update (ResetSignatoryDetails2 documentid signatories actor) = do
    Log.debug $ "reset: " ++ show signatories
    mdocument <- query $ GetDocumentByDocumentID documentid
    Log.debug $ "got a doc: " ++ show (isJust mdocument)
    case mdocument of
      Nothing -> return $ Left $ "ResetSignatoryDetails: document #" ++ show documentid ++ " does not exist"
      Just document ->
        case checkResetSignatoryData document signatories of
          [] -> do
            kPrepare "DELETE FROM signatory_links WHERE document_id = ?"
            _ <- kExecute [toSql documentid]

            let mauthorsiglink = getAuthorSigLink document
            forM_ signatories $ \(details, roles, atts, mcsvupload) -> do
                     magichash <- lift random
                     let link' = (signLinkFromDetails' details roles atts magichash)
                                 { signatorylinkcsvupload = mcsvupload }
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
              update $ InsertEvidenceEvent
                ResetSignatoryDetailsEvidence
                (value "removed" True >> value "signatory" True >> value "email" eml >> value "actor"  (actorWho actor))
                (Just documentid)
                actor

            forM_ (changedStuff old new) $ \(eml, rs, cs) -> do
              forM_ rs $ \removedfield ->
                update $ InsertEvidenceEvent
                  ResetSignatoryDetailsEvidence
                  (value "removed" True >> value "field" True >> value "email" eml >> value "fieldtype" (show $ sfType removedfield) >> value "actor"  (actorWho actor))
                  (Just documentid)
                  actor
              forM_ cs $ \changedfield ->
                update $ InsertEvidenceEvent
                  ResetSignatoryDetailsEvidence
                  (value "changed" True >> value "field" True >> value "email" eml >> (value "fieldtype" $ show (sfType changedfield)) >>
                   value "value" (sfValue changedfield) >> value "hasplacements" (not $ null $ sfPlacements changedfield) >> value "placements" (show $ sfPlacements changedfield) >> value "actor"  (actorWho actor))
                  (Just documentid)
                  actor

            forM_ (fieldsOfNew old new) $ \(eml, fs) -> do
              _ <- update $ InsertEvidenceEvent
                ResetSignatoryDetailsEvidence
                (value "added" True >> value "signatory" True >> value "email" eml >> value "actor"  (actorWho actor))
                (Just documentid)
                actor
              forM_ fs $ \changedfield ->
                    update $ InsertEvidenceEvent
                        ResetSignatoryDetailsEvidence
                        (value "added" True >> value "field" True >> value "email" eml >> (value "fieldtype" $ show (sfType changedfield)) >>
                         value "value" (sfValue changedfield) >> value "hasplacements" (not $ null $ sfPlacements changedfield) >> value "placements" (show $ sfPlacements changedfield) >> value "actor" (actorWho actor))
                        (Just documentid)
                        actor

            Just newdocument <- query $ GetDocumentByDocumentID documentid
            let moldcvsupload = msum (map (\(_,_,_,a) -> a) signatories)
            let mnewcsvupload = msum (map (signatorylinkcsvupload) (documentsignatorylinks newdocument))

            when (moldcvsupload /= mnewcsvupload) $ do
                     Log.error $ "ResetSignatoryDetails2 csvupload differs: " ++ show moldcvsupload ++ " vs " ++ show mnewcsvupload
                     error $ "error in ResetSignatoryDetails2"
            return $ Right newdocument

          s -> return $ Left $ "cannot reset signatory details on document " ++ show documentid ++ " because " ++ intercalate ";" s
          where emailsOfRemoved old new = [getEmail x | x <- removedSigs old new, "" /= getEmail x]
                changedStuff    old new = [(getEmail x, removedFields x y, changedFields x y) | (x, y) <- changedSigs old new, not $ null $ getEmail x]
                fieldsOfNew     old new = [(getEmail x, [f| f <- signatoryfields x, not $ null $ sfValue f]) | x <- newSigs old new, not $ null $ getEmail x]
                removedSigs     old new = [x      | x <- old, getEmail x `notElem` map getEmail new, not $ null $ getEmail x]
                changedSigs     old new = [(x, y) | x <- new, y <- old, getEmail x == getEmail y,    not $ null $ getEmail x]
                newSigs         old new = [x      | x <- new, getEmail x `notElem` map getEmail old, not $ null $ getEmail x]
                removedFields x y = let (r, _, _) = listDiff (signatoryfields x) (signatoryfields y) in r
                changedFields x y = let (_, _, c) = listDiff (signatoryfields x) (signatoryfields y) in c

data SignLinkFromDetailsForTest = SignLinkFromDetailsForTest SignatoryDetails [SignatoryRole]
instance (CryptoRNG m, MonadDB m, TemplatesMonad m) => DBUpdate m SignLinkFromDetailsForTest SignatoryLink where
  update (SignLinkFromDetailsForTest details roles) = do
      magichash <- lift random

      let link = signLinkFromDetails' details
                        roles [] magichash

      return link

data SignableFromDocumentIDWithUpdatedAuthor = SignableFromDocumentIDWithUpdatedAuthor User (Maybe Company) DocumentID Actor
instance (MonadDB m, TemplatesMonad m)=> DBUpdate m SignableFromDocumentIDWithUpdatedAuthor (Either String Document) where
  update (SignableFromDocumentIDWithUpdatedAuthor user mcompany docid actor) =
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
              void $ update $ InsertEvidenceEvent
                SignableFromDocumentIDWithUpdatedAuthorEvidence
                (value "did" (show docid) >> value "actor" (actorWho actor))
                (Just $ documentid d)
                actor
              return r
            Left _ -> return r
    where replaceAuthorSigLink :: SignatoryLink -> SignatoryLink
          replaceAuthorSigLink sl
            | isAuthor sl = replaceSignatoryUser sl user mcompany
            | otherwise = sl

data StoreDocumentForTesting = StoreDocumentForTesting Document
instance (MonadDB m, TemplatesMonad m) => DBUpdate m StoreDocumentForTesting DocumentID where
  update (StoreDocumentForTesting document) = do
    Just doc <- insertDocumentAsIs document
    return (documentid doc)

{-
   FIXME: this is so wrong on so many different levels
   - should set mtime
   - should not change type or copy this doc into new doc
-}
data TemplateFromDocument = TemplateFromDocument DocumentID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m TemplateFromDocument (Either String Document) where
  update (TemplateFromDocument did actor) = do
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "status" Preparation
      , sql "type" $ Template undefined
      ] <++> SQL "WHERE id = ?" [toSql did]
    when_ (r == 1) $
      update $ InsertEvidenceEvent
      TemplateFromDocumentEvidence
      (value "actor" $ actorWho actor)
      (Just did)
      actor
    getOneDocumentAffected "TemplateFromDocument" r did

data TimeoutDocument = TimeoutDocument DocumentID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m TimeoutDocument (Either String Document) where
  update (TimeoutDocument did actor) = do
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
      update $ InsertEvidenceEvent
      TimeoutDocumentEvidence
      (value "actor" (actorWho actor))
      (Just did)
      actor
    getOneDocumentAffected "TimeoutDocument" r did

data SetDocumentIdentification = SetDocumentIdentification DocumentID [IdentificationType] Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetDocumentIdentification (Either String Document) where
  update (SetDocumentIdentification did identification actor) = do
    ed <- query $ GetDocumentByDocumentID did
    let changed = case ed of
          Nothing -> False
          Just d -> not $ documentallowedidtypes d == identification
    r <- kRun $ mkSQL UPDATE tableDocuments
         [ sql "allowed_id_types" $ identification
         ] <++> SQL "WHERE id = ?" [ toSql did ]
    when_ (r == 1 && changed) $
      update $ InsertEvidenceEvent
      (SetElegitimationIdentificationEvidence <| ELegitimationIdentification `elem` identification |> SetEmailIdentificationEvidence)
      (value "identification" (show identification) >> value "actor" (actorWho actor))
      (Just did)
      actor
    getOneDocumentAffected "SetDocumentIdentification" r did

data UpdateFields = UpdateFields DocumentID SignatoryLinkID [(FieldType, String)] Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m UpdateFields (Either String Document) where
  update (UpdateFields did slid fields actor) = do
    -- Document has to be in Pending state
    -- signatory could not have signed already
    (eml :: String) <- $(fromJust) `liftM` getOne
           (SQL ("SELECT value FROM signatory_link_fields"
                ++ " WHERE signatory_link_fields.signatory_link_id = ?"
                ++ "   AND signatory_link_fields.type = ?")
                 [toSql slid, toSql EmailFT])

    let updateValue fieldtype fvalue = do
          let custom_name = case fieldtype of
                              CustomFT xname _ -> xname
                              CheckboxObligatoryFT xname -> xname
                              CheckboxOptionalFT xname -> xname
                              _ -> ""
          r <- kRun $ mkSQL UPDATE tableSignatoryLinkFields
                 [ sql "value" fvalue ]
                 <++> SQL (" WHERE EXISTS (SELECT 1 FROM documents, signatory_links"
                           ++             " WHERE documents.id = signatory_links.document_id"
                           ++             "   AND documents.status = ?"
                           ++             "   AND signatory_links.sign_time IS NULL"
                           ++             "   AND signatory_links.id = signatory_link_id)"
                           ++      "  AND signatory_link_id = ?"
                           ++      "  AND custom_name = ?"
                           ++      "  AND type = ?")
                        [ toSql Pending, toSql slid, toSql custom_name, toSql fieldtype]
          when_ (r>0) $ do
            update $ InsertEvidenceEvent
               UpdateFieldsEvidence
               (value "email" eml >> value "fieldtype" (show fieldtype) >> value "value" fvalue >> value "actor" (actorWho actor))
               (Just did)
               actor
          return (r :: Integer)

    updatedRows <- forM fields $ \(ft, v) -> updateValue ft v

    -- We don't want to affect too many rows
    getOneDocumentAffected "UpdateFields" (if (fromInteger (sum updatedRows) <= length fields) then 1 else 0) did


data UpdateFieldsNoStatusCheck = UpdateFieldsNoStatusCheck DocumentID SignatoryLinkID (String, String) Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m UpdateFieldsNoStatusCheck (Either String Document) where
  update (UpdateFieldsNoStatusCheck did slid (fieldname, fieldvalue) actor) = do
    (eml :: String) <- $(fromJust) `liftM` getOne
           (SQL ("SELECT value FROM signatory_link_fields"
                ++ " WHERE signatory_link_fields.signatory_link_id = ?"
                ++ "   AND signatory_link_fields.type = ?")
                 [toSql slid, toSql EmailFT])
    let updateValue xtype fvalue = do
          _ <- kRun $ mkSQL UPDATE tableSignatoryLinkFields
                 [ sql "value" fvalue ]
                 <++> SQL (" WHERE EXISTS (SELECT 1 FROM documents, signatory_links"
                           ++             " WHERE documents.id = signatory_links.document_id"
                           ++             "   AND documents.status = ?"
                           ++             "   AND signatory_links.sign_time IS NULL"
                           ++             "   AND signatory_links.id = signatory_link_id)"
                           ++      "  AND signatory_link_id = ?"
                           ++      "  AND custom_name = ?"
                           ++      "  AND type = ?")
                        [ toSql Pending
                        , toSql slid
                        , toSql (case xtype of
                                   CustomFT custom_name _ -> custom_name
                                   _ -> "")
                        , toSql xtype
                        ]
          return ()

    case fieldname of
      "sigfstname" -> updateValue FirstNameFT fieldvalue
      "sigsndname" -> updateValue LastNameFT fieldvalue
      "sigco"      -> updateValue CompanyFT fieldvalue
      "sigpersnr"  -> updateValue PersonalNumberFT fieldvalue
      "sigcompnr"  -> updateValue CompanyNumberFT fieldvalue
      "sigemail"   -> updateValue EmailFT fieldvalue
      "signature"  -> updateValue SignatureFT fieldvalue
      label        -> updateValue (CustomFT label False) fieldvalue

    _ <- update $ InsertEvidenceEvent
               UpdateFieldsEvidence
               (value "email" eml >> value "fieldtype" fieldname >> value "value" fieldvalue >> value "actor" (actorWho actor))
               (Just did)
               actor
    getOneDocumentAffected "UpdateFieldsNoStatusCheck" (1) did


data AddDocumentAttachment = AddDocumentAttachment DocumentID FileID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m AddDocumentAttachment (Either String Document) where
  update (AddDocumentAttachment did fid actor) = do
    mf <- query $ GetFileByFileID fid
    r <- kRun $ mkSQL INSERT tableAuthorAttachments [
        sql "document_id" did
      , sql "file_id" fid
      ] <++> SQL "WHERE EXISTS (SELECT 1 FROM documents WHERE id = ? AND status = ?)" [
        toSql did
      , toSql Preparation
      ]
    when_ (r == 1) $
      update $ InsertEvidenceEvent
      AddDocumentAttachmentEvidence
      (value "fid" (show fid) >> value "actor" (actorWho actor) >> value "name" (filename <$> mf))
      (Just did)
      actor
    getOneDocumentAffected "AddDocumentAttachment" r did

data RemoveDocumentAttachment = RemoveDocumentAttachment DocumentID FileID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m RemoveDocumentAttachment (Either String Document) where
  update (RemoveDocumentAttachment did fid actor) = do
    mf <- query $ GetFileByFileID fid
    kPrepare "DELETE FROM author_attachments WHERE document_id = ? AND file_id = ? AND EXISTS (SELECT 1 FROM documents WHERE id = ? AND status = ?)"
    r <- kExecute [
        toSql did
      , toSql fid
      , toSql did
      , toSql Preparation
      ]
    when_ (r == 1) $
      update $ InsertEvidenceEvent
      RemoveDocumentAttachmentEvidence
      (value "fid" (show fid) >> value "actor" (actorWho actor) >> value "name" (filename <$> mf))
      (Just did)
      actor

    -- I understand the point of this, but it is a little weird to do the check after - EN
    m <- query $ GetDocumentByDocumentID did
    case m of
      Just doc -> case documentstatus doc of
                       Preparation -> return $ Right doc
                       _ -> return $ Left "bad document status"
      Nothing -> return $ Left "no such document"

data SetSigAttachments = SetSigAttachments DocumentID SignatoryLinkID [SignatoryAttachment] Actor
instance MonadDB m => DBUpdate m SetSigAttachments () where
  update (SetSigAttachments _did slid sigatts _actor) = do
    _ <-doDeleteAll
    forM_ sigatts doInsertOne
    where
     doDeleteAll = kRun $ SQL "DELETE FROM signatory_attachments WHERE signatory_link_id = ?" [toSql slid]
     doInsertOne SignatoryAttachment{..} = do
        kRun $ mkSQL INSERT tableSignatoryAttachments [
            sql "file_id" signatoryattachmentfile
          , sql "name" signatoryattachmentname
          , sql "description" signatoryattachmentdescription
          , sql "signatory_link_id" slid
          ]

data UpdateDraft = UpdateDraft DocumentID  Document Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m UpdateDraft (Either String Document) where
  update (UpdateDraft did document actor) = do
     _ <- update $ SetDocumentTitle did (documenttitle document) actor
     _ <- update $ SetDaysToSign  did (documentdaystosign document) actor
     _ <- update $ SetDocumentLocale did (getLocale document) actor
     _ <- update $ SetDocumentIdentification did (documentallowedidtypes document) actor
     update $ SetInviteText did (documentinvitetext document) actor

data SetDocumentModificationData = SetDocumentModificationData DocumentID MinutesTime
instance MonadDB m => DBUpdate m SetDocumentModificationData (Either String Document) where
  update (SetDocumentModificationData did time) = do
    r <- kRun $ mkSQL UPDATE tableDocuments [sql "mtime" time]
      <++> SQL "WHERE id = ?" [toSql did]
    getOneDocumentAffected "SetDocumentModificationData" r did

