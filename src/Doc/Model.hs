{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fcontext-stack=50  #-}
module Doc.Model
  ( module File.File
  , isTemplate -- fromUtils
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
  , SetDocumentAuthenticationMethod(..)
  , SetDocumentDeliveryMethod(..)
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
  , FixClosedErroredDocument(..)
  ) where

import API.Service.Model
import Control.Monad.Trans
import DB
import MagicHash
import Crypto.RNG
import Doc.Checks
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
import Utils.List
import Utils.Monoid
import Utils.Prelude
import Utils.Read
import Utils.Tuples
import IPAddress
import Data.List hiding (tail, head)
import Data.Monoid
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import Doc.Tables
import Control.Applicative
import Util.SignatoryLinkUtils
import Doc.DocProcess
import Doc.DocStateCommon
import qualified Log
import Control.Monad
import Util.Actor
import Util.MonadUtils
import Templates.Templates
import EvidenceLog.Model
import Util.HasSomeUserInfo
import Templates.Fields (value, objects)

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
  | DocumentFilterByDelivery DeliveryMethod -- ^ Only documents that use selected delivery method
  | DocumentFilterByMonthYearFrom (Int,Int)           -- ^ Document time after or in (month,year)
  | DocumentFilterByMonthYearTo   (Int,Int)           -- ^  Document time before or in (month,year)

data DocumentDomain
  = DocumentsOfWholeUniverse                     -- ^ All documents in the system. Only for admin view.
  | DocumentsOfAuthorDeleteValue UserID Bool     -- ^ Documents by author, with delete flag
  | DocumentsForSignatoryDeleteValue UserID Bool -- ^ Documents by signatory, with delete flag
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
documentFilterToSQL (DocumentFilterByMonthYearFrom (month,year)) =
  SQL ("(documents.mtime > '" ++ show year ++  "-" ++ show month ++ "-1')") []
documentFilterToSQL (DocumentFilterByMonthYearTo (month,year)) =
  SQL ("(documents.mtime < '" ++ show (year + 1 <| month == 12 |> year)++ "-" ++ show ((month `mod` 12) + 1) ++ "-1')") []
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

documentFilterToSQL (DocumentFilterByDelivery del) =
  SQL ("documents.delivery_method = ?") [toSql del]

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
                   , checkEqualBy "documentinvitetext" documentinvitetext
                   , checkEqualBy "documentauthenticationmethod" documentauthenticationmethod
                   , checkEqualBy "documentdeliverymethod" documentdeliverymethod
                   , checkEqualBy "documentcancelationreason" documentcancelationreason
                   , checkEqualBy "documentsharing" documentsharing
                   , checkEqualBy "documentrejectioninfo" documentrejectioninfo
                   , checkEqualBy "documenttags" documenttags
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
  , "invite_text"
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
  , "authentication_method"
  , "delivery_method"
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
     invite_ip invite_text cancelationreason rejection_time
     rejection_signatory_link_id rejection_reason service deleted mail_footer
     region sharing status_class authentication_method delivery_method
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
       , documentinvitetext = invite_text
       , documentauthenticationmethod = authentication_method
       , documentdeliverymethod = delivery_method
       , documentcancelationreason = cancelationreason
       , documentsharing = sharing
       , documentrejectioninfo = case (rejection_time, rejection_signatory_link_id, rejection_reason) of
           (Just t, Just sl, mr) -> Just (t, sl, fromMaybe "" mr)
           _ -> Nothing
       , documenttags = S.empty
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

fetchDocumentTags :: MonadDB m => DBEnv m (M.Map DocumentID (S.Set DocumentTag))
fetchDocumentTags = foldDB decoder M.empty
  where
    decoder acc document_id name v =
      M.insertWith' S.union document_id
         (S.singleton $ DocumentTag name v) acc

insertDocumentTagAsIs :: MonadDB m => DocumentID -> DocumentTag -> DBEnv m (Maybe DocumentTag)
insertDocumentTagAsIs documentid tag = do
  _ <- kRun $ mkSQL INSERT tableDocumentTags
       [ sql "name" $ tagname tag
       , sql "value" $ tagvalue tag
       , sql "document_id" documentid
       ] <++> SQL ("RETURNING " ++ documentTagsSelectors) []

  fetchDocumentTags
    >>= oneObjectReturnedGuard . concatMap (S.toList . snd) . M.toList


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
                 , documentinvitetext
                 , documentauthenticationmethod
                 , documentdeliverymethod
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
      , sql "authentication_method" documentauthenticationmethod
      , sql "delivery_method" documentdeliverymethod
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
        newtags <- F.foldlM (\acc tag -> insertDocumentTagAsIs (documentid doc) tag
          >>= return . maybe acc (flip S.insert acc)) S.empty documenttags
        if any isNothing mlinks || any isNothing mauthorattachments || S.size documenttags /= S.size newtags
         then return Nothing
         else do
          let newdocument = doc { documentsignatorylinks    = catMaybes mlinks
                                , documentauthorattachments = catMaybes mauthorattachments
                                , documenttags              = newtags
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
newFromDocument :: MonadDB m => (Document -> Document) -> DocumentID -> DBEnv m (Maybe Document)
newFromDocument f docid = do
  mdoc <- query $ GetDocumentByDocumentID docid
  case mdoc of
      Just doc -> Just `liftM` insertNewDocument (f doc)
      Nothing -> do
        Log.error $ "Document " ++ show docid ++ " does not exist"
        return Nothing

data CheckIfDocumentExists = CheckIfDocumentExists DocumentID
instance MonadDB m => DBQuery m CheckIfDocumentExists Bool where
  query (CheckIfDocumentExists did) =
    checkIfAnyReturned $ SQL "SELECT 1 FROM documents WHERE id = ?" [toSql did]

{- |
    The existance of this function is wrong.  What it means is that storing
    maybesignatory and maybecompany on the signatory links is the wrong way of doing it,
    and there should be something else for hooking accounts to sig links that doesn't
    involve editing all the docs as a user moves between private and company accounts.
-}
data AdminOnlySaveForUser = AdminOnlySaveForUser DocumentID User Actor

instance (MonadDB m, TemplatesMonad m) => DBUpdate m AdminOnlySaveForUser Bool where
  update (AdminOnlySaveForUser did user _actor) = do
    kRun01 $ mkSQL UPDATE tableSignatoryLinks [sql "company_id" $ usercompany user]
      <++> SQL "WHERE document_id = ? AND user_id = ? " [
        toSql did
      , toSql $ userid user
      ]

data ArchiveDocument = ArchiveDocument User DocumentID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m ArchiveDocument Bool where
  update (ArchiveDocument user did _actor) = do
    case (usercompany user, useriscompanyadmin user) of
      (Just cid, True) -> fmap (\x -> x > 0) $  kRun $ updateArchivableDoc $ SQL "WHERE (company_id = ? OR user_id = ?)" [toSql cid,toSql $ userid user]
      _ -> kRun01 $ updateArchivableDoc $ SQL "WHERE user_id = ?" [toSql $ userid user]
    where
      updateArchivableDoc whereClause = mconcat [
          mkSQL UPDATE tableSignatoryLinks [sql "deleted" True]
        , whereClause
        , SQL " AND document_id = ? AND EXISTS (SELECT 1 FROM documents WHERE id = ? AND status <> ?)" [
            toSql did
          , toSql did
          , toSql Pending
          ]
        ]

data AttachCSVUpload = AttachCSVUpload DocumentID SignatoryLinkID CSVUpload Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m AttachCSVUpload Bool where
  update (AttachCSVUpload did slid csvupload actor) = do
    mstatus <- getOne $ SQL "SELECT status FROM documents WHERE id = ? AND deleted = FALSE" [toSql did]
    case mstatus of
      Nothing -> do
        Log.error $ "Cannot AttachCSVUpload document " ++ show did ++ " because it does not exist"
        return False
      Just Preparation -> do
        success <- kRun01 $ mkSQL UPDATE tableSignatoryLinks [
            sql "csv_title" $ csvtitle csvupload
          , sql "csv_signatory_index" $ csvsignatoryindex csvupload
          , sql "csv_contents" $ csvcontents csvupload
          ] <++> SQL "WHERE document_id = ? AND signatory_links.id = ? AND deleted = FALSE AND ((roles & ?) = 0)" [
            toSql did
          , toSql slid
          , toSql [SignatoryAuthor]
          ]
        when_ success $
          update $ InsertEvidenceEvent
            AttachCSVUploadEvidence
            (value "csvtitle" (csvtitle csvupload) >> value "actor" (actorWho actor))
            (Just did)
            actor
        return success
      -- standard conversion puts error in DocumentError argument, so we
      -- can't try to evaluate it here.
      Just DocumentError{} -> do
        Log.error $ errmsg "DocumentError"
        return False
      Just status -> do
        Log.error $ errmsg $ show status
        return False
    where
      errmsg status = "Document #" ++ show did ++ " is in " ++ status ++ " state, must be Preparation"

data AttachFile = AttachFile DocumentID FileID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m AttachFile Bool where
  update (AttachFile did fid a) = do
    let time = actorTime a
    success <- kRun01 $ mkSQL UPDATE tableDocuments [
        sql "mtime" time
      , sql "file_id" $ fid
      ] <++> SQL "WHERE id = ? AND status = ?" [toSql did, toSql Preparation]
    when_ success $ do
      f <- exactlyOneObjectReturnedGuard =<< query (GetFileByFileID fid)
      update $ InsertEvidenceEvent
        AttachFileEvidence
        (value "actor" (actorWho a) >> value "filename" (filename f))
        (Just did)
        a
    return success

data AttachSealedFile = AttachSealedFile DocumentID FileID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m AttachSealedFile Bool where
  update (AttachSealedFile did fid actor) = do
    let time = actorTime actor
    success <- kRun01 $ mkSQL UPDATE tableDocuments [
        sql "mtime" time
      , sql "sealed_file_id" fid
      ] <++> SQL "WHERE id = ? AND status = ?" [toSql did, toSql Closed]
    when_ success $
      update $ InsertEvidenceEvent
      AttachSealedFileEvidence
      (value "actor"(actorWho actor))
      (Just did)
      actor
    return success

data FixClosedErroredDocument = FixClosedErroredDocument DocumentID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m FixClosedErroredDocument Bool where
  update (FixClosedErroredDocument did _actor) = do
    kRun01 $ mkSQL UPDATE tableDocuments [
        sql "status" Closed
      ] <++> SQL "WHERE id = ? AND status = ?" [toSql did, toSql $ DocumentError undefined]
    
data CancelDocument = CancelDocument DocumentID CancelationReason Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m CancelDocument Bool where
  update (CancelDocument did reason actor) = do
    let mtime = actorTime actor
    doc_exists <- query $ CheckIfDocumentExists did
    if not doc_exists
      then do
        Log.error $ "Cannot CancelDocument document " ++ show did ++ " because it does not exist"
        return False
      else do
        errmsgs <- checkCancelDocument did
        case errmsgs of
          [] -> do
            success <- kRun01 $ mkSQL UPDATE tableDocuments [
                sql "status" Canceled
              , sql "mtime" mtime
              , sql "cancelation_reason" $ reason
              ] <++> SQL "WHERE id = ? AND type = ?" [
                toSql did
              , toSql $ Signable undefined
              ]
            when_ success $
              case reason of
                ManualCancel -> update $ InsertEvidenceEvent
                  CancelDocumentEvidence
                  (value "actor" (actorWho actor))
                  (Just did)
                  actor
                ELegDataMismatch _ sid fn ln num -> do
                  Just sl <- query $ GetSignatoryLinkByID did sid Nothing
                  let trips = [("First name",      getFirstName      sl, fn)
                              ,("Last name",       getLastName       sl, ln)
                              ,("Personal number", getPersonalNumber sl, num)]
                      uneql = filter (\(_,a,b)->a/=b) trips
                      msg = intercalate "; " $ map (\(f,s,e)->f ++ " from transaction was \"" ++ s ++ "\" but from e-legitimation was \"" ++ e ++ "\"") uneql
                  update $ InsertEvidenceEvent
                    CancelDocumenElegEvidence
                    (value "actor" (actorWho actor) >> value "msg" msg )
                    (Just did)
                    actor
            return success
          s -> do
            Log.error $ "Cannot CancelDocument document " ++ show did ++ " because " ++ concat s
            return False

data ChangeMainfile = ChangeMainfile DocumentID FileID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m ChangeMainfile Bool where
  update (ChangeMainfile did fid actor) = do
    mdocument <- query $ GetDocumentByDocumentID did
    case mdocument of
      Nothing -> do
        Log.error $ "Cannot ChangeMainfile document " ++ show did ++ " because it does not exist"
        return False
      Just document -> do
        case getFileIDsByStatus document of
          [ofid] -> do
            let fieldname = if (documentstatus document == Closed || allHadSigned document)
                            then "sealed_file_id"
                            else "file_id"
            success <- kRun01 $ mkSQL UPDATE tableDocuments [sql fieldname $ fid]
                 <++> SQL "WHERE id = ?" [toSql did]
            oldf <- exactlyOneObjectReturnedGuard =<< query (GetFileByFileID ofid)
            newf <- exactlyOneObjectReturnedGuard =<< query (GetFileByFileID fid)
            when_ success $
              update $ InsertEvidenceEvent
                ChangeMainfileEvidence
                (value "actor" (actorWho actor) >> value "oldfilename" (filename oldf) >> value "newfilename" (filename newf))
                (Just did)
                actor
            return success
          fs -> do
            Log.error $ "Cannot ChangeMainfile document " ++ show did ++ ": non-one number of main files: " ++ show (length fs)
            return False
    where
        allHadSigned doc = all (hasSigned ||^ (not . isSignatory)) $ documentsignatorylinks doc

data ChangeSignatoryEmailWhenUndelivered = ChangeSignatoryEmailWhenUndelivered DocumentID SignatoryLinkID (Maybe User) String Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m ChangeSignatoryEmailWhenUndelivered Bool where
  update (ChangeSignatoryEmailWhenUndelivered did slid muser email actor) = do
    Just doc <- query $ GetDocumentByDocumentID did
    if (documentstatus doc /= Pending)
     then do
       Log.error $ "Cannot ChangeSignatoryEmailWhenUndelivered for document #" ++ show did ++ " that is in " ++ show (documentstatus doc) ++ " state"
       return False;
     else do
      let Just sl = getSigLinkFor doc slid
          oldemail = getEmail sl
      success1 <- kRun01 $ mkSQL UPDATE tableSignatoryLinkFields [
             sql "value" email
            ] <++> SQL (" WHERE signatory_link_id = ? AND type = ?") [toSql slid, toSql EmailFT]
      success2 <- kRun01 $ mkSQL UPDATE tableSignatoryLinks [
          sql "invitation_delivery_status" Unknown
        , sql "user_id" $ fmap userid muser
        , sql "company_id" $ muser >>= usercompany
        ] <++> SQL "WHERE EXISTS (SELECT 1 FROM documents WHERE documents.id = signatory_links.document_id AND documents.status = ?) AND id = ?" [
          toSql Pending
        , toSql slid
        ]
      when_ (success1 && success2) $
        update $ InsertEvidenceEvent
          ChangeSignatoryEmailWhenUndeliveredEvidence
          (value "oldemail" oldemail >> value "newemail" email >> value "actor" (actorWho actor))
          (Just did)
          actor
      return $ success1 && success2

data PreparationToPending = PreparationToPending DocumentID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m PreparationToPending Bool where
  update (PreparationToPending docid actor) = do
    let time = actorTime actor
    doc_exists <- query $ CheckIfDocumentExists docid
    if not doc_exists
      then do
        Log.error $ "Cannot PreparationToPending document " ++ show docid ++ " because it does not exist"
        return False
      else do
        errmsgs <- checkPreparationToPending docid
        case errmsgs of
          [] -> do
            daystosign :: Maybe Int <- getOne (SQL "SELECT days_to_sign FROM documents WHERE id = ?" [toSql docid]) >>= exactlyOneObjectReturnedGuard
            let mtt = (\days -> (days * 24 *60) `minutesAfter` time) <$> daystosign
            success <- kRun01 $ mkSQL UPDATE tableDocuments [
                sql "status" Pending
              , sql "mtime" time
              , sql "timeout_time" $ (\days -> (days * 24 * 60) `minutesAfter` time)
                  <$> daystosign
              ] <++> SQL "WHERE id = ? AND type = ?" [
                toSql docid
              , toSql $ Signable undefined
              ]
            when_ success $
              update $ InsertEvidenceEvent
                PreparationToPendingEvidence
                (value "actor" (actorWho actor) >> value "timeouttime" (formatMinutesTimeUTC <$> mtt))
                (Just docid)
                actor
            return success
          s -> do
            Log.error $ "Cannot PreparationToPending document " ++ show docid ++ " because " ++ concat s
            return False

data CloseDocument = CloseDocument DocumentID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m CloseDocument Bool where
  update (CloseDocument docid actor) = do
    let time = actorTime actor
    doc_exists <- query $ CheckIfDocumentExists docid
    if not doc_exists
      then do
        Log.error $ "Cannot Close document " ++ show docid ++ " because it does not exist"
        return False
      else do
        errmsgs <- checkCloseDocument docid
        case errmsgs of
          [] -> do
            success <- kRun01 $ mkSQL UPDATE tableDocuments [
                sql "status" Closed
              , sql "mtime" time
              ] <++> SQL "WHERE id = ? AND type = ?" [
                toSql docid
              , toSql $ Signable undefined
              ]
            when_ success $
              update $ InsertEvidenceEvent
                CloseDocumentEvidence
                (value "actor" (actorWho actor))
                (Just docid)
                actor
            return success
          s -> do
            Log.error $ "Cannot CloseDocument " ++ show docid ++ " because " ++ concat s
            return False

data DeleteSigAttachment = DeleteSigAttachment DocumentID SignatoryLinkID FileID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m DeleteSigAttachment Bool where
  update (DeleteSigAttachment did slid fid actor) = do
    msig <- query $ GetSignatoryLinkByID did slid Nothing
    case msig of
      Nothing -> do
        Log.error $ "SignatoryLink does not exist. Trying to Delete Sig Attachment. docid: " ++ show did
        return False
      Just sig -> case find (\sl->signatoryattachmentfile sl == Just fid) $ signatoryattachments sig of
        Nothing -> do
          Log.error $ "No signatory attachment for that file id: " ++ show fid
          return False
        Just sa -> do
          success <- kRun01 $ mkSQL UPDATE tableSignatoryAttachments [sql "file_id" SqlNull]
               <++> SQL "WHERE file_id = ? AND signatory_link_id = ?"
               [ toSql fid
               , toSql slid
               ]
          when_ success $
            update $ InsertEvidenceEvent
              DeleteSigAttachmentEvidence
              (value "actor" (actorWho actor) >> value "name" (signatoryattachmentname sa) >> value "email" (getEmail sig))
              (Just did)
              actor
          return success

data DocumentFromSignatoryData = DocumentFromSignatoryData DocumentID String String String String String String [String] Actor
instance (CryptoRNG m, MonadDB m,TemplatesMonad m) => DBUpdate m DocumentFromSignatoryData (Maybe Document) where
  update (DocumentFromSignatoryData docid fstname sndname email company personalnumber companynumber fieldvalues actor) = do
    Just sigs <- getOne $ SQL "SELECT COUNT(*) FROM signatory_links WHERE document_id = ?" [toSql docid]
    if sigs == 0
      then do
        Log.error $ "In DocumentFromSignatoryData: 0 signatory links for document_id: " ++ show docid
        return Nothing
      else do
        mhs <- lift $ replicateM sigs random
        md <- newFromDocument (toNewDoc mhs) docid
        when_ (isJust md) $ do
          let d = $fromJust md
          copyEvidenceLogToNewDocument docid (documentid d)
          update $ InsertEvidenceEvent
            AuthorUsesCSVEvidence
            (value "actor" (actorWho actor) >> value "did" (show docid))
            (Just $ documentid d)
            actor
        return md
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
instance (MonadDB m, TemplatesMonad m) => DBUpdate m ErrorDocument Bool where
  update (ErrorDocument docid errmsg actor) = do
    doc_exists <- getOne $ SQL "SELECT TRUE FROM documents WHERE id = ? AND deleted = FALSE" [toSql docid]
    case doc_exists of
      Nothing -> do
        Log.error $ "Cannot ErrorDocument document " ++ show docid ++ " because it does not exist"
        return False
      Just (_::Bool) -> do
        case [] of -- WTF ?
          [] -> do
            success <- kRun01 $ mkSQL UPDATE tableDocuments [
                sql "status" $ DocumentError errmsg
              , sql "error_text" errmsg
              ] <++> SQL "WHERE id = ?" [toSql docid]
            when_ success $
              update $ InsertEvidenceEvent
                ErrorDocumentEvidence
                (value "errmsg" errmsg >> value "actor" (actorWho actor))
                (Just docid)
                actor
            return success
          s -> do
            Log.error $ "Cannot ErrorDocument document " ++ show docid ++ " because " ++ concat s
            return False

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
                   , documenttags                 = M.findWithDefault S.empty (documentid doc) tags
                   }

    return $ map fill docs

data GetDocumentTags = GetDocumentTags DocumentID
instance MonadDB m => DBQuery m GetDocumentTags (S.Set DocumentTag) where
  query (GetDocumentTags did) = do
    _ <- kRun $ selectDocumentTagsSQL <++> SQL "WHERE document_id = ?" [toSql did]
    fetchDocumentTags
      >>= oneObjectReturnedGuard . map snd . M.toList
      >>= return . fromMaybe S.empty

data GetSignatoryLinkByID = GetSignatoryLinkByID DocumentID SignatoryLinkID (Maybe MagicHash)
instance MonadDB m => DBQuery m GetSignatoryLinkByID (Maybe SignatoryLink) where
  query (GetSignatoryLinkByID did sid mmh) = do
    _ <- kRun $ selectSignatoryLinksSQL
      <++> SQL "WHERE documents.id = ? AND signatory_links.id = ?" [toSql did, toSql sid]
      <++> case mmh of
             Nothing -> mempty
             Just mh -> SQL " AND signatory_links.token = ?" [toSql mh]
    mlink <- fetchSignatoryLinks
      >>= oneObjectReturnedGuard . concatMap snd . M.toList
    _ <- kRun $ selectSignatoryLinkFieldsSQL
      <++> SQL "WHERE signatory_link_id = ?" [toSql sid]
    fields <- fetchSignatoryLinkFields
      >>= return . concatMap snd . M.toList
    return $ (\link -> link {
      signatorydetails = (signatorydetails link) {
        signatoryfields = fields
      }
    }) <$> mlink

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
  query (GetDocuments [] _filters _orderbys _pagination)   = return []
  query (GetDocuments domains filters orderbys pagination) = do
    selectDocuments $ mconcat
      [ selectDocumentsSQL
      , SQL "WHERE EXISTS (SELECT 1 FROM signatory_links WHERE documents.id = signatory_links.document_id " []
      , if null domains
          then mempty
          else mconcat [
              SQL "AND (" []
            , sqlConcatOR (map documentDomainToSQL domains)
            , SQL ")" []
            ]
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
    query (GetDocuments [DocumentsOfAuthorDeleteValue uid False, TemplatesOfAuthorDeleteValue uid False] [] [Asc DocumentOrderByMTime] (DocumentPagination 0 maxBound))

data GetTemplatesByAuthor = GetTemplatesByAuthor UserID
instance MonadDB m => DBQuery m GetTemplatesByAuthor [Document] where
  query (GetTemplatesByAuthor uid) =
    query (GetDocuments [TemplatesOfAuthorDeleteValue uid False] [] [Asc DocumentOrderByMTime] (DocumentPagination 0 maxBound))

data GetAvailableTemplates = GetAvailableTemplates UserID [DocumentProcess]
instance MonadDB m => DBQuery m GetAvailableTemplates [Document] where
  query (GetAvailableTemplates uid processes) =
    query (GetDocuments [TemplatesOfAuthorDeleteValue uid False, TemplatesSharedInUsersCompany uid]
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
    query (GetDocuments [DocumentsForSignatoryDeleteValue uid False] [DocumentFilterByProcess processes] [Asc DocumentOrderByMTime] (DocumentPagination 0 maxBound))

data GetTimeoutedButPendingDocuments = GetTimeoutedButPendingDocuments MinutesTime
instance MonadDB m => DBQuery m GetTimeoutedButPendingDocuments [Document] where
  query (GetTimeoutedButPendingDocuments mtime) = do
    selectDocuments $ selectDocumentsSQL
      <++> SQL "WHERE status = ? AND timeout_time IS NOT NULL AND timeout_time < ?" [
        toSql Pending
      , toSql mtime
      ]

data MarkDocumentSeen = MarkDocumentSeen DocumentID SignatoryLinkID MagicHash Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m MarkDocumentSeen Bool where
  update (MarkDocumentSeen did slid mh actor) = do
    -- have to make sure slid and mh match to record log; sorry for inefficiency -EN
    msig <- query $ GetSignatoryLinkByID did slid (Just mh)
    case msig of
      Nothing -> do
        Log.error $ "signatory link with given magichash or document does not exist. slid: " ++ show slid ++ ", did: " ++ show did ++ ", mh: " ++ show mh
        return False
      Just sig -> do
        let time = actorTime actor
            ipnumber = fromMaybe noIP $ actorIP actor
        success <- kRun01 $ mkSQL UPDATE tableSignatoryLinks [
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
        return success

data AddInvitationEvidence = AddInvitationEvidence DocumentID SignatoryLinkID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m AddInvitationEvidence Bool where
  update (AddInvitationEvidence docid slid actor) = do
    msig <- query $ GetSignatoryLinkByID docid slid Nothing
    case msig of
      Nothing -> do
        Log.error $ "No document/signatory link with id = " ++ show docid ++ "/" ++ show slid
        return False
      Just sig -> do
        let eml = getEmail sig
        _ <- update $ InsertEvidenceEvent
          InvitationEvidence
          (value "email" eml >> value "actor" (actorWho actor))
          (Just docid)
          actor
        return True

data MarkInvitationRead = MarkInvitationRead DocumentID SignatoryLinkID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m MarkInvitationRead Bool where
  update (MarkInvitationRead did slid actor) = do
    msig <- query $ GetSignatoryLinkByID did slid Nothing
    case msig of
      Nothing -> do
        Log.error $ "No document/signatory link with id = " ++ show did ++ "/" ++ show slid
        return False
      Just sig -> do
        let time = actorTime actor
            eml  = getEmail sig
        success <- kRun01 $ mkSQL UPDATE tableSignatoryLinks [sql "read_invitation" time]
          <++> SQL "WHERE id = ? AND document_id = ? AND read_invitation IS NULL" [
            toSql slid
          , toSql did
          ]
        _ <- update $ InsertEvidenceEvent
          MarkInvitationReadEvidence
          (value "email" eml >> value "actor" (actorWho actor))
          (Just did)
          actor
        return success

data NewDocument = NewDocument User (Maybe Company) String DocumentType Int Actor
instance (CryptoRNG m, MonadDB m, TemplatesMonad m) => DBUpdate m NewDocument (Maybe Document) where
  update (NewDocument user mcompany title documenttype nrOfOtherSignatories actor) = do
  let ctime = actorTime actor
  if fmap companyid mcompany /= usercompany user
    then do
      Log.error $ "NewDocument: company and user don't match"
      return Nothing
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
                , documentauthenticationmethod = EmailAuthentication
                , documentdeliverymethod       = EmailDelivery
                , documentui                   = (documentui blankDocument) { documentmailfooter = customfooter $ usersettings user }
                } 

      case invariantProblems ctime doc of
        Nothing -> do

           midoc <- insertDocumentAsIs doc
           case midoc of
             Just doc' -> do
               _ <- update $ InsertEvidenceEvent
                 NewDocumentEvidence
                 (value "title" title >> value "actor" (actorWho actor))
                 (Just $ documentid doc')
                 actor
               return $ Just doc'
             Nothing -> do
               Log.debug $ "insertDocumentAsIs could not insert document #" ++ show (documentid doc) ++ " in NewDocument"
               return Nothing
        Just a -> do
           Log.debug $ "insertDocumentAsIs invariants violated: " ++ show a
           return Nothing

data ReallyDeleteDocument = ReallyDeleteDocument User DocumentID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m ReallyDeleteDocument Bool where
  update (ReallyDeleteDocument user did _actor) = do
    -- I don't like this: we should do this on the DB side, not pass
    -- in a User which could be old. It should be done within a
    -- transaction. -EN
    case (usercompany user, useriscompanyadmin user) of
      (Just cid, True) -> fmap (\x -> x > 0) $ kRun $ deleteDoc $ SQL "WHERE (company_id = ? OR user_id = ?)" [toSql cid,toSql $ userid user] -- This can remove more then one link (subaccounts)
      _ -> kRun01 $ deleteDoc $ SQL "WHERE user_id = ? AND (company_id IS NULL OR EXISTS (SELECT 1 FROM documents WHERE id = ? AND status = ?))" [toSql $ userid user,toSql did, toSql Preparation]
    where
      deleteDoc whereClause = mconcat [
          mkSQL UPDATE tableSignatoryLinks [sql "really_deleted" True]
        , whereClause
        , SQL " AND document_id = ? AND deleted = TRUE" [toSql did]

        ]

data RejectDocument = RejectDocument DocumentID SignatoryLinkID (Maybe String) Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m RejectDocument Bool where
  update (RejectDocument docid slid customtext actor) = do
    let time = actorTime actor
    msig <- query $ GetSignatoryLinkByID docid slid Nothing
    case msig of
      Nothing -> do
        Log.error $ "Cannot RejectDocument document " ++ show docid ++ " because there is no docid/siglinkid = " ++ show docid ++ "/" ++ show slid
        return False
      Just sig -> do
        errmsgs <- checkRejectDocument docid slid
        case errmsgs of
          [] -> do
            success <- kRun01 $ mkSQL UPDATE tableDocuments [
                sql "status" Rejected
              , sql "mtime" time
              , sql "rejection_time" time
              , sql "rejection_reason" customtext
              , sql "rejection_signatory_link_id" slid
              ] <++> SQL "WHERE id = ?" [toSql docid]
            when_ success $
                update $ InsertEvidenceEvent
                  RejectDocumentEvidence
                  (value "email" (getEmail sig) >> value "actor" (actorWho actor))
                  (Just docid)
                  actor
            return success
          s -> do
            Log.error $ "Cannot RejectDocument document " ++ show docid ++ " because " ++ concat s
            return False

data RestartDocument = RestartDocument Document Actor
instance (CryptoRNG m, MonadDB m, TemplatesMonad m) => DBUpdate m RestartDocument (Maybe Document) where
  update (RestartDocument doc actor) = do
    mndoc <- tryToGetRestarted
    case mndoc of
      Right newdoc -> do
        md <- newFromDocument (const newdoc) (documentid doc)
        case md of
          Nothing -> return Nothing
          Just d -> do
            copyEvidenceLogToNewDocument (documentid doc) (documentid d)
            _ <- update $ InsertEvidenceEvent
              RestartDocumentEvidence
              (value "did" (show $ documentid doc) >> value "actor" (actorWho actor))
              (Just $ documentid d)
              actor
            return $ Just d
      Left err -> do
        Log.error err
        return Nothing
   where

    tryToGetRestarted =
      if (documentstatus doc `notElem` [Canceled, Timedout, Rejected])
      then return $ Left $ "Can't restart document with " ++ (show $ documentstatus doc) ++ " status"
      else do
             doc' <- clearSignInfofromDoc
             return $ Right doc'

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
instance (MonadDB m, TemplatesMonad m) => DBUpdate m RestoreArchivedDocument Bool where
  update (RestoreArchivedDocument user did actor) = do
    success <- case (usercompany user, useriscompanyadmin user) of
      (Just cid, True) -> updateRestorableDoc $ SQL "WHERE company_id = ?" [toSql cid]
      _ -> updateRestorableDoc $ SQL "WHERE user_id = ?" [toSql $ userid user]
    when_ success $
      update $ InsertEvidenceEvent
        RestoreArchivedDocumentEvidence
        (value "email" (getEmail user) >> value "actor" (actorWho actor) >> value "company" (isJust (usercompany user) && useriscompanyadmin user))
        (Just did)
        actor
    return success
    where
      updateRestorableDoc whereClause = kRun01 $ mconcat [
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
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SaveDocumentForUser Bool where
  update (SaveDocumentForUser did User{userid, usercompany} slid _actor) = do
    kRun01 $ mkSQL UPDATE tableSignatoryLinks [
        sql "user_id" userid
      , sql "company_id" usercompany
      ] <++> SQL "WHERE document_id = ? AND id = ?" [
        toSql did
      , toSql slid
      ]

{- |
    Saves a signatory attachment to a document.
    If there's a problem such as the document isn't in a pending or awaiting author state,
    or the document does not exist a Left is returned.
-}
data SaveSigAttachment = SaveSigAttachment DocumentID SignatoryLinkID String FileID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SaveSigAttachment Bool where
  update (SaveSigAttachment did slid name fid actor) = do
    success <- kRun01 $ mkSQL UPDATE tableSignatoryAttachments [sql "file_id" fid]
      <++> SQL "WHERE file_id IS NULL AND name = ? AND signatory_link_id = ?"
      [ toSql name
      , toSql slid
      ]
    when_ success $
      update $ InsertEvidenceEvent
        SaveSigAttachmentEvidence
        (value "name" name  >> value "actor" (actorWho actor))
        (Just did)
        actor
    return success

data SetDocumentTags = SetDocumentTags DocumentID (S.Set DocumentTag) Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetDocumentTags Bool where
  update (SetDocumentTags did doctags actor) = do
    oldtags <- query $ GetDocumentTags did
    let changed = doctags /= oldtags
    _ <- kRun $ SQL "DELETE FROM document_tags WHERE document_id = ?" [toSql did]
    success <- F.foldlM (\acc tag -> insertDocumentTagAsIs did tag >>= return . maybe False (const acc)) True doctags
    when_ (success && changed) $ do
      tagstr <- intercalate "; " <$> F.foldrM (\(DocumentTag k v) acc -> return $ (k ++ "=" ++ v) : acc) [] doctags
      update $ InsertEvidenceEvent
        SetDocumentTagsEvidence
        (value "tags" tagstr >> value "actor" (actorWho actor))
        (Just did)
        actor
    return success

data SetDocumentInviteTime = SetDocumentInviteTime DocumentID MinutesTime Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetDocumentInviteTime Bool where
  update (SetDocumentInviteTime did invitetime actor) = do
    let ipaddress  = fromMaybe noIP $ actorIP actor
    changed <- checkIfAnyReturned $ SQL "SELECT 1 FROM documents WHERE id = ? AND (invite_time IS DISTINCT FROM ? OR invite_ip IS DISTINCT FROM ?)" [toSql did, toSql invitetime, toSql ipaddress]
    Log.debug $ show changed
    success <- kRun01 $ mkSQL UPDATE tableDocuments [
        sql "invite_time" invitetime
      , sql "invite_ip" ipaddress
      ] <++> SQL "WHERE id = ?" [toSql did]
    when_ (success && changed) $
      update $ InsertEvidenceEvent
        SetDocumentInviteTimeEvidence
        (value "time" (formatMinutesTimeUTC invitetime) >> value "actor" (actorWho actor))
        (Just did)
        actor
    return success

data SetDocumentTimeoutTime = SetDocumentTimeoutTime DocumentID MinutesTime Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetDocumentTimeoutTime Bool where
  update (SetDocumentTimeoutTime did timeouttime actor) = do
    changed <- checkIfAnyReturned $ SQL "SELECT 1 FROM documents WHERE id = ? AND timeout_time IS DISTINCT FROM ?" [toSql did, toSql timeouttime]
    success <- kRun01 $ mkSQL UPDATE tableDocuments [sql "timeout_time" timeouttime]
      <++> SQL "WHERE id = ? AND deleted = FALSE AND type = ?" [
        toSql did
      , toSql $ Signable undefined
      ]
    when_ (success && changed) $
      update $ InsertEvidenceEvent
        SetDocumentTimeoutTimeEvidence
        (value "time" (formatMinutesTimeUTC timeouttime) >> value "actor" (actorWho actor))
        (Just did)
        actor
    return success

data SetInviteText = SetInviteText DocumentID String Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetInviteText Bool where
  update (SetInviteText did text actor) = do
    changed <- checkIfAnyReturned $ SQL "SELECT 1 FROM documents WHERE id = ? AND invite_text IS DISTINCT FROM ?" [toSql did, toSql text]
    let time = actorTime actor
    success <- kRun01 $ mkSQL UPDATE tableDocuments [
        sql "invite_text" text
      , sql "mtime" time
      ] <++> SQL "WHERE id = ?" [toSql did]
    when_ (success && changed) $
      update $ InsertEvidenceEvent
        SetInvitationTextEvidence
        (value "text" text >> value "actor" (actorWho actor))
        (Just did)
        actor
    return success

data SetDaysToSign = SetDaysToSign DocumentID (Maybe Int) Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetDaysToSign Bool where
  update (SetDaysToSign did mdays actor) = do
    changed <- checkIfAnyReturned $ SQL "SELECT 1 FROM documents WHERE id = ? AND days_to_sign IS DISTINCT FROM ?" [toSql did, toSql mdays]
    success <- kRun01 $ mkSQL UPDATE tableDocuments
         [ sql "days_to_sign" $ mdays
         , sql "mtime" $ actorTime actor
         ] <++> SQL "WHERE id = ?" [ toSql did ]
    when_ (success && changed) $
      update $ InsertEvidenceEvent
        (SetDaysToSignEvidence <| isJust mdays |> RemoveDaysToSignEvidence)
        (value "mdays" (show <$> mdays) >> value "actor" (actorWho actor))
        (Just did)
        actor
    return success

data SetDocumentTitle = SetDocumentTitle DocumentID String Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetDocumentTitle Bool where
  update (SetDocumentTitle did doctitle actor) = do
    changed <- checkIfAnyReturned $ SQL "SELECT 1 FROM documents WHERE id = ? AND title IS DISTINCT FROM ?" [toSql did, toSql doctitle]
    let time = actorTime actor
    success <- kRun01 $ mkSQL UPDATE tableDocuments [
        sql "title" doctitle
      , sql "mtime" time
      ] <++> SQL "WHERE id = ?" [toSql did]
    when_ (success && changed) $
      update $ InsertEvidenceEvent
        SetDocumentTitleEvidence
        (value "title" doctitle >> value "actor" (actorWho actor))
        (Just did)
        actor
    return success

data SetDocumentLocale = SetDocumentLocale DocumentID Locale Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetDocumentLocale Bool where
  update (SetDocumentLocale did locale actor) = do
    changed <- checkIfAnyReturned $ SQL "SELECT 1 FROM documents WHERE id = ? AND region IS DISTINCT FROM ?" [toSql did, toSql $ getRegion locale]
    let time = actorTime actor
    success <- kRun01 $ mkSQL UPDATE tableDocuments [
        sql "region" $ getRegion locale
      , sql "mtime" time
      ] <++> SQL "WHERE id = ?" [toSql did]
    when_ (success && changed) $
      update $ InsertEvidenceEvent
        SetDocumentLocaleEvidence
        (value "local" (show locale) >> value "actor" (actorWho actor))
        (Just did)
        actor
    return success

data SetDocumentUI = SetDocumentUI DocumentID DocumentUI Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetDocumentUI Bool where
  update (SetDocumentUI did docui actor) = do
    kRun01 $ mkSQL UPDATE tableDocuments [
        sql "mail_footer" $ documentmailfooter docui
      , sql "mtime" $ actorTime actor
      ] <++> SQL "WHERE id = ?" [toSql did]

data SetInvitationDeliveryStatus = SetInvitationDeliveryStatus DocumentID SignatoryLinkID MailsDeliveryStatus Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetInvitationDeliveryStatus Bool where
  update (SetInvitationDeliveryStatus did slid status actor) = do
    msig <- query $ GetSignatoryLinkByID did slid Nothing
    let (email, changed) = case msig of
          Nothing -> ("", False)
          Just sl -> (getEmail sl, invitationdeliverystatus sl /= status)
    success <- kRun01 $ mkSQL UPDATE tableSignatoryLinks [
        sql "invitation_delivery_status" status
      ] <++> SQL "WHERE id = ? AND document_id = ? AND EXISTS (SELECT 1 FROM documents WHERE id = ? AND type = ?)" [
        toSql slid
      , toSql did
      , toSql did
      , toSql $ Signable undefined
      ]
    when_ (success && changed) $
      update $ InsertEvidenceEvent
        SetInvitationDeliveryStatusEvidence
        (value "email" email >> value "status" (show status) >> value "actor" (actorWho actor))
        (Just did)
        actor
    return success

data SetDocumentSharing = SetDocumentSharing [DocumentID] Bool
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetDocumentSharing Bool where
  update (SetDocumentSharing dids flag) = do
    results <- forM dids $ \did -> kRun01 $ mkSQL UPDATE tableDocuments
         [ sql "sharing" $ (if flag then Shared else Private)
         ] <++> SQL
         " WHERE id = ? AND deleted = FALSE " [ toSql did ]
    return $ and results

data SignDocument = SignDocument DocumentID SignatoryLinkID MagicHash (Maybe SignatureInfo) Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SignDocument Bool where
  update (SignDocument docid slid mh msiginfo actor) = do
    doc_exists <- query $ CheckIfDocumentExists docid
    if not doc_exists
      then do
        Log.error $ "Cannot SignDocument document " ++ show docid ++ " because it does not exist"
        return False
      else do
        errmsgs <- checkSignDocument docid slid mh
        case errmsgs of
          [] -> do
            let ipnumber = fromMaybe noIP $ actorIP actor
                time     = actorTime actor
            success <- kRun01 $ mkSQL UPDATE tableSignatoryLinks [
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
            when_ success $
              update $ InsertEvidenceEvent
                SignDocumentEvidence
                (signatureFields >> value "actor" (actorWho actor))
                (Just docid)
                actor
            return success
          s -> do
            Log.error $ "Cannot SignDocument document " ++ show docid ++ " because " ++ concat s
            return False

data ResetSignatoryDetails = ResetSignatoryDetails DocumentID [(SignatoryDetails, [SignatoryRole])] Actor
instance (CryptoRNG m, MonadDB m, TemplatesMonad m) => DBUpdate m ResetSignatoryDetails Bool where
  update (ResetSignatoryDetails documentid signatories actor) =
    update (ResetSignatoryDetails2 documentid (map (\(a,b) -> (a,b,[],Nothing)) signatories) actor)

splitImage :: String -> Maybe (Int, Int, String)
splitImage s = do
  let ws = takeWhile (/= '|') s
      hs = takeWhile (/= '|') s'
      is = dropWhile (== '|') $ dropWhile (/= '|') s'
      s' = dropWhile (== '|') $ dropWhile (/= '|') s
  w <- maybeRead ws
  h <- maybeRead hs
  return (w, h, is)
data ResetSignatoryDetails2 = ResetSignatoryDetails2 DocumentID [(SignatoryDetails, [SignatoryRole], [SignatoryAttachment], Maybe CSVUpload)] Actor
instance (CryptoRNG m, MonadDB m, TemplatesMonad m) => DBUpdate m ResetSignatoryDetails2 Bool where
  update (ResetSignatoryDetails2 documentid signatories actor) = do
    Log.debug $ "reset: " ++ show signatories
    mdocument <- query $ GetDocumentByDocumentID documentid
    Log.debug $ "got a doc: " ++ show (isJust mdocument)
    case mdocument of
      Nothing -> do
        Log.error $ "ResetSignatoryDetails: document #" ++ show documentid ++ " does not exist"
        return False
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
                RemoveSignatoryEvidence
                (do value "email" eml
                    value "actor" $ actorWho actor)
                (Just documentid)
                actor

            forM_ (changedStuff old new) $ \(eml, rs, cs) -> do
              forM_ rs $ \removedfield ->
                update $ InsertEvidenceEvent
                  RemoveFieldEvidence
                  (do value "email" eml
                      value "fieldtype" $ show $ sfType removedfield
                      value "actor" $ actorWho actor)
                  (Just documentid)
                  actor
              forM_ cs $ \changedfield ->
                update $ InsertEvidenceEvent
                  ChangeFieldEvidence
                  (do value "email" eml 
                      value "fieldtype" $ show (sfType changedfield)
                      case sfType changedfield of
                        SignatureFT -> case splitImage $ sfValue changedfield of
                          Just (_w,_h,"") -> return ()
                          Just (_w,_h,i)  -> value "signature" i
                          Nothing -> return ()
                        CheckboxOptionalFT   _ | sfValue changedfield == "" -> value "unchecked" True
                        CheckboxOptionalFT   _                              -> value "checked" True
                        CheckboxObligatoryFT _ | sfValue changedfield == "" -> value "unchecked" True
                        CheckboxObligatoryFT _                              -> value "checked" True 
                        _                                                   -> value "value" $ sfValue changedfield
                      value "hasplacements" (not $ null $ sfPlacements changedfield)
                      objects "placements" $ for (sfPlacements changedfield) $ \p -> do
                        value "x"    $ show $ placementx p
                        value "y"    $ show $ placementy p
                        value "page" $ show $ placementpage p
                      value "actor"  (actorWho actor))
                  (Just documentid)
                  actor

            forM_ (fieldsOfNew old new) $ \(eml, fs) -> do
              _ <- update $ InsertEvidenceEvent
                AddSignatoryEvidence
                (do value "email" eml
                    value "actor"  (actorWho actor))
                (Just documentid)
                actor
              forM_ fs $ \changedfield ->
                update $ InsertEvidenceEvent
                  AddFieldEvidence
                  (do value "email" eml
                      value "fieldtype" $ show (sfType changedfield)
                      case sfType changedfield of
                        SignatureFT -> case splitImage $ sfValue changedfield of
                          Just (_,_,"") -> return ()
                          Just (_,_,i)  -> value "signature" i
                          Nothing -> return ()
                        CheckboxOptionalFT   _ | sfValue changedfield == "" -> value "unchecked" True
                        CheckboxOptionalFT   _                              -> value "checked" True
                        CheckboxObligatoryFT _ | sfValue changedfield == "" -> value "unchecked" True
                        CheckboxObligatoryFT _                              -> value "checked" True 
                        _                                                   -> value "value" $ sfValue changedfield
                      value "hasplacements" (not $ null $ sfPlacements changedfield)
                      objects "placements" $ for (sfPlacements changedfield) $ \p -> do
                        value "x"    $ show $ placementx p
                        value "y"    $ show $ placementy p
                        value "page" $ show $ placementpage p
                      value "actor" (actorWho actor))
                  (Just documentid)
                  actor

            Just newdocument <- query $ GetDocumentByDocumentID documentid
            let moldcvsupload = msum (map (\(_,_,_,a) -> a) signatories)
            let mnewcsvupload = msum (map (signatorylinkcsvupload) (documentsignatorylinks newdocument))

            when (moldcvsupload /= mnewcsvupload) $ do
                     Log.error $ "ResetSignatoryDetails2 csvupload differs: " ++ show moldcvsupload ++ " vs " ++ show mnewcsvupload
                     error $ "error in ResetSignatoryDetails2"

            return True

          s -> do
            Log.error $ "cannot reset signatory details on document " ++ show documentid ++ " because " ++ intercalate ";" s
            return False
          where emailsOfRemoved old new = [getEmail x | x <- removedSigs old new, "" /= getEmail x]
                changedStuff    old new = [(getEmail x, removedFields x y, changedFields x y) | (x, y) <- changedSigs old new, not $ null $ getEmail x]
                fieldsOfNew     old new = [(getEmail x, [f| f <- signatoryfields x, (not $ null $ sfValue f) || (not $ null $ sfPlacements f)]) | x <- newSigs old new, not $ null $ getEmail x]
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
instance (MonadDB m, TemplatesMonad m)=> DBUpdate m SignableFromDocumentIDWithUpdatedAuthor (Maybe Document) where
  update (SignableFromDocumentIDWithUpdatedAuthor user mcompany docid actor) =
      if fmap companyid mcompany /= usercompany user
        then do
          Log.error $ "company and user don't match"
          return Nothing
        else do
          let time = actorTime actor
          res <- (flip newFromDocument) docid $ \doc ->
            (templateToDocument doc) {
              documentsignatorylinks = map replaceAuthorSigLink (documentsignatorylinks doc)
                                       -- FIXME: Need to remove authorfields?
              , documentctime = time
              , documentmtime = time
              , documentui    = DocumentUI { documentmailfooter = customfooter (usersettings user) }
              }
          case res of
            Nothing -> return Nothing
            Just d -> do
              copyEvidenceLogToNewDocument docid (documentid d)
              void $ update $ InsertEvidenceEvent
                SignableFromDocumentIDWithUpdatedAuthorEvidence
                (value "did" (show docid) >> value "actor" (actorWho actor))
                (Just $ documentid d)
                actor
              return $ Just d
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
instance (MonadDB m, TemplatesMonad m) => DBUpdate m TemplateFromDocument Bool where
  update (TemplateFromDocument did actor) = do
    success <- kRun01 $ mkSQL UPDATE tableDocuments [
        sql "status" Preparation
      , sql "type" $ Template undefined
      ] <++> SQL "WHERE id = ?" [toSql did]
    when_ success $
      update $ InsertEvidenceEvent
        TemplateFromDocumentEvidence
        (value "actor" $ actorWho actor)
        (Just did)
        actor
    return success

data TimeoutDocument = TimeoutDocument DocumentID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m TimeoutDocument Bool where
  update (TimeoutDocument did actor) = do
    let time = actorTime actor
    success <- kRun01 $ mkSQL UPDATE tableDocuments [
        sql "status" Timedout
      , sql "mtime" time
      ] <++> SQL "WHERE id = ? AND type = ? AND status = ?" [
        toSql did
      , toSql $ Signable undefined
      , toSql Pending
      ]
    when_ success $
      update $ InsertEvidenceEvent
        TimeoutDocumentEvidence
        (value "actor" (actorWho actor))
        (Just did)
        actor
    return success

data SetDocumentAuthenticationMethod = SetDocumentAuthenticationMethod DocumentID AuthenticationMethod Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetDocumentAuthenticationMethod Bool where
  update (SetDocumentAuthenticationMethod did auth actor) = do
    changed <- checkIfAnyReturned $ SQL "SELECT 1 FROM documents WHERE id = ? AND authentication_method IS DISTINCT FROM ?" [toSql did, toSql auth]
    success <- kRun01 $ mkSQL UPDATE tableDocuments
         [ sql "authentication_method" auth
         ] <++> SQL "WHERE id = ?" [ toSql did ]
    when_ (success && changed) $ do
      let evidence = case auth of
            EmailAuthentication -> SetEmailAuthenticationMethodEvidence
            ELegAuthentication  -> SetELegAuthenticationMethodEvidence
      update $ InsertEvidenceEvent evidence
        (value "authentication" (show auth) >> value "actor" (actorWho actor))
        (Just did)
        actor
    return success

data SetDocumentDeliveryMethod = SetDocumentDeliveryMethod DocumentID DeliveryMethod Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetDocumentDeliveryMethod Bool where
  update (SetDocumentDeliveryMethod did del actor) = do
    changed <- checkIfAnyReturned $ SQL "SELECT 1 FROM documents WHERE id = ? AND delivery_method IS DISTINCT FROM ?" [toSql did, toSql del]
    success <- kRun01 $ mkSQL UPDATE tableDocuments
         [ sql "delivery_method" del
         ] <++> SQL "WHERE id = ?" [ toSql did ]
    when_ (success && changed) $ do
      let evidence = case del of
            EmailDelivery -> SetEmailDeliveryMethodEvidence
            PadDelivery   -> SetPadDeliveryMethodEvidence
      update $ InsertEvidenceEvent evidence
        (value "delivery" (show del) >> value "actor" (actorWho actor))
        (Just did)
        actor
    return success

data UpdateFields = UpdateFields DocumentID SignatoryLinkID [(FieldType, String)] Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m UpdateFields Bool where
  update (UpdateFields did slid fields actor) = do
    -- Document has to be in Pending state
    -- signatory could not have signed already
    (eml :: String) <- $fromJust `liftM` getOne
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
          when_ (r > 0) $ do
            update $ InsertEvidenceEvent
               UpdateFieldsEvidence
               (do value "email" eml 
                   value "fieldtype" (show fieldtype)
                   case fieldtype of
                        SignatureFT -> case splitImage fvalue of
                          Just (_,_,"") -> return ()
                          Just (_,_,i)  -> do
                            value "signature" i
                          Nothing -> return ()
                        CheckboxOptionalFT   _ | fvalue == "" -> value "unchecked" True
                        CheckboxOptionalFT   _                -> value "checked" True
                        CheckboxObligatoryFT _ | fvalue == "" -> value "unchecked" True
                        CheckboxObligatoryFT _                -> value "checked" True 
                        _                                     -> value "value" $ fvalue
                   
                   value "actor" (actorWho actor))
               (Just did)
               actor
          return (r :: Integer)

    updatedRows <- forM fields $ \(ft, v) -> updateValue ft v
    -- We don't want to affect too many rows
    return $ sum updatedRows <= fromIntegral (length fields)

data UpdateFieldsNoStatusCheck = UpdateFieldsNoStatusCheck DocumentID SignatoryLinkID (String, String) Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m UpdateFieldsNoStatusCheck () where
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
    return ()

data AddDocumentAttachment = AddDocumentAttachment DocumentID FileID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m AddDocumentAttachment Bool where
  update (AddDocumentAttachment did fid actor) = do
    mf <- query (GetFileByFileID fid)
    success <- kRun01 $ mkSQL INSERT tableAuthorAttachments [
        sql "document_id" did
      , sql "file_id" fid
      ] <++> SQL "WHERE EXISTS (SELECT 1 FROM documents WHERE id = ? AND status = ?)" [
        toSql did
      , toSql Preparation
      ]
    when_ success $
      update $ InsertEvidenceEvent
        AddDocumentAttachmentEvidence
        (value "fid" (show fid) >> value "actor" (actorWho actor) >> value "name" (filename <$> mf))
        (Just did)
        actor
    return success

data RemoveDocumentAttachment = RemoveDocumentAttachment DocumentID FileID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m RemoveDocumentAttachment Bool where
  update (RemoveDocumentAttachment did fid actor) = do
    mf <- query (GetFileByFileID fid)
    kPrepare "DELETE FROM author_attachments WHERE document_id = ? AND file_id = ? AND EXISTS (SELECT 1 FROM documents WHERE id = ? AND status = ?)"
    success <- kExecute01 [
        toSql did
      , toSql fid
      , toSql did
      , toSql Preparation
      ]
    when_ success $
      update $ InsertEvidenceEvent
        RemoveDocumentAttachmentEvidence
        (value "fid" (show fid) >> value "actor" (actorWho actor) >> value "name" (filename <$> mf))
        (Just did)
        actor
    return success

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
instance (MonadDB m, TemplatesMonad m) => DBUpdate m UpdateDraft Bool where
  update (UpdateDraft did document actor) = and `liftM` sequence [
      update $ SetDocumentTitle did (documenttitle document) actor
    , update $ SetDaysToSign  did (documentdaystosign document) actor
    , update $ SetDocumentLocale did (getLocale document) actor
    , update $ SetDocumentAuthenticationMethod did (documentauthenticationmethod document) actor
    , update $ SetDocumentDeliveryMethod did (documentdeliverymethod document) actor
    , update $ SetInviteText did (documentinvitetext document) actor
    ]

data SetDocumentModificationData = SetDocumentModificationData DocumentID MinutesTime
instance MonadDB m => DBUpdate m SetDocumentModificationData Bool where
  update (SetDocumentModificationData did time) = do
    kRun01 $ mkSQL UPDATE tableDocuments [sql "mtime" time]
      <++> SQL "WHERE id = ?" [toSql did]
