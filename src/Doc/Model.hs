{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fcontext-stack=50  -fno-warn-orphans #-}
module Doc.Model
  ( isTemplate -- fromUtils
  , DocumentFilter(..)
  , DocumentDomain(..)
  , DocumentOrderBy(..)

  , AddDocumentAttachment(..)
  , AddInvitationEvidence(..)
  , ArchiveDocument(..)
  , AttachFile(..)
  , DetachFile(..)
  , AppendSealedFile(..)
  , AppendExtendedSealedFile(..)
  , CancelDocument(..)
  , LogSignWithELegFailureForDocument(..)
  , ChangeSignatoryEmailWhenUndelivered(..)
  , ChangeSignatoryPhoneWhenUndelivered(..)
  , CloseDocument(..)
  , DeleteSigAttachment(..)
  , FileInDocument(..)
  , RemoveOldDrafts(..)
  , ErrorDocument(..)
  , GetDocuments(..)
  , GetDocuments2(..)
  , GetDocumentByDocumentID(..)
  , GetDocumentsByDocumentIDs(..)
  , GetDocumentBySignatoryLinkID(..)
  , GetDocumentsBySignatoryLinkIDs(..)
  , GetDocumentByDocumentIDSignatoryLinkIDMagicHash(..)
  , GetDocumentsByAuthor(..)
  , GetSignatoryScreenshots(..)
  , GetTemplatesByAuthor(..)
  , GetAvailableTemplates(..)
  , GetTimeoutedButPendingDocumentsChunk(..)
  , MarkDocumentSeen(..)
  , MarkInvitationRead(..)
  , NewDocument(..)
  , PreparationToPending(..)
  , PurgeDocuments(..)
  , RejectDocument(..)
  , RemoveDocumentAttachment(..)
  , ResetSignatoryDetails(..)
  , RestartDocument(..)
  , ProlongDocument(..)
  , RestoreArchivedDocument(..)
  , SaveDocumentForUser(..)
  , SaveSigAttachment(..)
  , SetDaysToSign(..)
  , SetDocumentInviteTime(..)
  , SetDocumentLang(..)
  , SetDocumentSharing(..)
  , SetDocumentUnsavedDraft(..)
  , SetDocumentTags(..)
  , SetDocumentTitle(..)
  , SetEmailInvitationDeliveryStatus(..)
  , SetSMSInvitationDeliveryStatus(..)
  , SetInviteText(..)
  , SignDocument(..)
  , CloneDocumentWithUpdatedAuthor(..)
  , StoreDocumentForTesting(..)
  , TemplateFromDocument(..)
  , DocumentFromTemplate(..)
  , TimeoutDocument(..)
  , PostReminderSend(..)
  , AddSignatoryLinkVisitedEvidence(..)
  , UpdateFieldsForSigning(..)
  , SetSigAttachments(..)
  , UpdateDraft(..)
  , GetDocsSentBetween(..)
  , FixClosedErroredDocument(..)
  , GetDocsSent(..)
  , GetSignatoriesByEmail(..)
  , CheckDocumentObjectVersionIs(..)

   -- only for use in tests
  , updateMTimeAndObjectVersion
  ) where

import Control.Monad.Identity (Identity)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class
import DB
import DB.RowCache (GetRow(..))
import MagicHash
import Crypto.RNG
import Doc.Conditions
import Doc.DocumentMonad (updateDocumentWithID, updateDocument, DocumentMonad, theDocumentID)
import File.FileID
import File.Model
import File.Storage
import qualified Amazon
import qualified Control.Monad.State.Lazy as State
import Doc.SealStatus (SealStatus(..), hasGuardtimeSignature)
import Doc.DocUtils
import User.UserID
import User.Model
import Doc.SignatoryLinkID
import MinutesTime
import Doc.DocumentID
import OurPrelude
import Control.Logic
import Doc.DocStateData
import Data.Maybe hiding (fromJust)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Utils.Default
import Utils.Monad
import Utils.Monoid
import Instances ()
import IPAddress
import Data.List hiding (tail, head)
import qualified Data.Map as M
import qualified Data.Set as S
import Doc.SignatoryScreenshots
import Doc.Screenshot
import qualified Doc.Screenshot as Screenshot
import Doc.Tables
import Control.Applicative
import Control.Arrow (second)
import Util.SignatoryLinkUtils
import Doc.DocStateCommon
import qualified Log
import Control.Monad
import Util.Actor
import Text.StringTemplates.Templates
import EvidenceLog.Model
import Util.HasSomeUserInfo
import Text.StringTemplates.Fields (value)
import qualified Text.StringTemplates.Fields as F
import DB.TimeZoneName (TimeZoneName, mkTimeZoneName, withTimeZone)
import qualified DB.TimeZoneName as TimeZoneName
import DB.SQL2
import Control.Monad.State.Class
import Company.Model

-- | Document filtering options
--
-- Each of these options should narrow down set of documents that are
-- visible. Most have some relation to some features of either the
-- document itself or to a signatory link that makes the document
-- available to see.
--
-- This is important: it is not only about document, it is also about
-- the route we got to the document, i. e. signatory_links!.
data DocumentFilter
  = DocumentFilterStatuses [DocumentStatus]   -- ^ Any of listed statuses
  | DocumentFilterBySealStatus [SealStatus]   -- ^ Any of listed seal statuses
  | DocumentFilterByStatusClass [StatusClass] -- ^ Any of listed status classes
  | DocumentFilterByTags [DocumentTag]        -- ^ All of listed tags (warning: this is ALL tags)
  | DocumentFilterByString String             -- ^ Contains the string in title, list of people involved or anywhere
  | DocumentFilterByDelivery DeliveryMethod   -- ^ Only documents that use selected delivery method
  | DocumentFilterByMonthYearFrom (Int,Int)   -- ^ Document time after or in (month,year)
  | DocumentFilterByMonthYearTo   (Int,Int)   -- ^ Document time before or in (month,year)
  | DocumentFilterByAuthor UserID             -- ^ Only documents created by this user
  | DocumentFilterByAuthorCompany CompanyID   -- ^ Onl documents where author is in given company
  | DocumentFilterByCanSign UserID            -- ^ Only if given person can sign right now given document
  | DocumentFilterByDocumentID DocumentID     -- ^ Document by specific ID
  | DocumentFilterByDocumentIDs [DocumentID]  -- ^ Documents by specific IDs
  | DocumentFilterSignable                    -- ^ Document is signable
  | DocumentFilterTemplate                    -- ^ Document is template
  | DocumentFilterDeleted Bool                -- ^ Only deleted (=True) or non-deleted (=False) documents.
  | DocumentFilterLinkIsAuthor Bool           -- ^ Only documents visible by signatory_links.is_author equal to param
  | DocumentFilterLinkIsPartner Bool          -- ^ Only documents visible by signatory_links.is_partner equal to param
  | DocumentFilterUnsavedDraft Bool           -- ^ Only documents with unsaved draft flag equal to this one
  | DocumentFilterByModificationTimeAfter MinutesTime -- ^ That were modified after given time
  | DocumentFilterByLatestSignTimeBefore MinutesTime  -- ^ With latest sign time before given time
  | DocumentFilterByLatestSignTimeAfter MinutesTime   -- ^ With latest sign time after given time
  | DocumentFilterPurged Bool                         -- ^ Only purged (=True) or non-purged (=False) documents
  deriving Show

-- | Document security domain.
--
-- This data type should be regarder our most precious and important
-- security measure against unarthorized document access. When using
-- `GetDocuments` or derivatives we should always specify on whose
-- behalf are we acting upon a document. If we have user id then using
-- `DocumentsVisibleToUser` ensures that we are touching only
-- documents that we are allowed to touch and nothing else.
--
-- I wonder if we should refactor `DocumentDomain` into specialized
-- functions like GetDocumentsVisibleToUser [DocumentFilter]
-- [DocumentOrderBy] Pagination. For now it is `GetDocuments` and
-- parameters.
data DocumentDomain
  = DocumentsOfWholeUniverse                     -- ^ All documents in the system. Only for admin view.
  | DocumentsVisibleToUser UserID                -- ^ Documents that a user has possible access to

-- | These are possible order by clauses that make documents sorted by.
data DocumentOrderBy
  = DocumentOrderByTitle       -- ^ Order by title, alphabetically, case insensitive
  | DocumentOrderByMTime       -- ^ Order by modification time
  | DocumentOrderByCTime       -- ^ Order by creation time
  | DocumentOrderByStatusClass -- ^ Order by status class.
  | DocumentOrderByType        -- ^ Order by document type.
  | DocumentOrderByPartners    -- ^ Order by partner names or emails
  | DocumentOrderByAuthor      -- ^ Order by author name or email


-- | Convert DocumentOrderBy enumeration into proper SQL order by statement
documentOrderByToSQL :: DocumentOrderBy -> SQL
documentOrderByToSQL DocumentOrderByTitle = SQL "documents.title" []
documentOrderByToSQL DocumentOrderByMTime = SQL "documents.mtime" []
documentOrderByToSQL DocumentOrderByCTime = SQL "documents.ctime" []
documentOrderByToSQL DocumentOrderByStatusClass = documentStatusClassExpression
documentOrderByToSQL DocumentOrderByType = SQL "documents.type" []
documentOrderByToSQL DocumentOrderByPartners =
  parenthesize (selectSignatoryLinksSmartNames False True)
documentOrderByToSQL DocumentOrderByAuthor =
  parenthesize (selectSignatoryLinksSmartNames True False)

selectSignatoryLinksSmartNames :: Bool -> Bool -> SQL
selectSignatoryLinksSmartNames is_author is_partner =
      SQL ("SELECT COALESCE(string_agg(x.value,' '),'no fields') FROM ") [] <>
      SQL ("(SELECT (") [] <>
      selectSmartName <>
      SQL (") AS value FROM signatory_links" <>
           " WHERE signatory_links.document_id = documents.id" <>
           "   AND signatory_links.is_author = ? AND signatory_links.is_partner = ?" <>
           " ORDER BY signatory_links.id) AS x") [toSql is_author, toSql is_partner]
  where
    selectFieldAs xtype name = "(SELECT signatory_link_fields.value AS value" <+>
                                    "FROM signatory_link_fields" <+>
                                    "WHERE signatory_link_fields.signatory_link_id = signatory_links.id" <+>
                                    "AND type =" <?> xtype <+>
                                    "LIMIT 1) " <+>
                                    "AS" <+> name
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
    selectSmartName = SQL ("SELECT " <>
                           "COALESCE(NULLIF(TRIM(BOTH FROM (COALESCE(first_name.value,'') " <>
                           "                                || ' ' || " <>
                           "                                COALESCE(last_name.value,''))), ''),email.value) " <>
                           "FROM (") [] <>
                      selectFieldAs FirstNameFT "first_name" <>
                      SQL " FULL JOIN " [] <>
                      selectFieldAs LastNameFT "last_name" <>
                      SQL " ON TRUE " [] <>
                      SQL " FULL JOIN " [] <>
                      selectFieldAs EmailFT "email" <>
                      SQL " ON TRUE " [] <>
                      SQL ") WHERE signatory_links.document_id = documents.id" []

documentOrderByAscDescToSQL :: AscDesc DocumentOrderBy -> SQL
documentOrderByAscDescToSQL (Asc x) = documentOrderByToSQL x
documentOrderByAscDescToSQL (Desc x) = documentOrderByToSQL x <> SQL " DESC" []


--
-- Document visibility rules:
--
-- A document is either signable or template.
--
-- Any of number points must be true, all of letter subpoints must be
-- true.
--
-- Visibility rules:
--
-- 1. User can see her own authored documents.
-- 2. User can see a document when:
--     a. document is signable
--     b. document is not in Preparation
--     c. was invited to sign (is_partner = TRUE)
--     d. sign order says it is ok to see
-- 3. User can see a document when:
--     a. document is signable
--     b. document is not in Preparation
--     c. was invited to view (is_partner = FALSE)
-- 4. User can see a document when:
--     a. document is template
--     b. author is in the same company
--     c. document has company sharing set
-- 5. User can see a document when:
--     a. user is company admin
--     b. there is another user in the same company that can see the document
--     c. document is not in preparation state
--
-- A really_deleted document is never shown.
--
-- To do anything with document a user has at least see it. Usually
-- more strict rules apply.
documentDomainToSQL :: (MonadState v m, SqlWhere v)
                    => DocumentDomain
                    -> m ()
documentDomainToSQL (DocumentsOfWholeUniverse) = do
  sqlWhere "TRUE"
documentDomainToSQL (DocumentsVisibleToUser uid) =
  sqlWhereAny $ do
    sqlWhereAll $ do           -- 1: see own documents
      sqlWhereEq "same_company_users.id" uid
      sqlWhere "users.id = same_company_users.id"
      sqlWhere "signatory_links.is_author"
    sqlWhereAll $ do           -- 2. see signables as partner
      sqlWhereEq "same_company_users.id" uid
      sqlWhere "users.id = same_company_users.id"
      sqlWhereNotEq "documents.status" Preparation
      sqlWhereEq "documents.type" $ Signable
      sqlWhere "signatory_links.is_partner"
      sqlWhereNotExists $ sqlSelect "signatory_links AS earlier_signatory_links" $ do
                            sqlWhere "signatory_links.document_id = earlier_signatory_links.document_id"
                            sqlWhere "earlier_signatory_links.is_partner"
                            sqlWhere "earlier_signatory_links.sign_time IS NULL"
                            sqlWhere "earlier_signatory_links.sign_order < signatory_links.sign_order"
    sqlWhereAll $ do           -- 3. see signables as viewer
      sqlWhereEq "same_company_users.id" uid
      sqlWhere "users.id = same_company_users.id"
      sqlWhereNotEq "documents.status" Preparation
      sqlWhereEq "documents.type" $ Signable
      sqlWhere "NOT signatory_links.is_partner"
    sqlWhereAll $ do           -- 4. see shared templates
      sqlWhereEq "same_company_users.id" uid
      sqlWhereEq "documents.sharing" Shared
      sqlWhereEq "documents.type" $ Template
      sqlWhere "signatory_links.is_author"
    sqlWhereAll $ do           -- 5: see documents of subordinates
      sqlWhereEq "same_company_users.id" uid
      sqlWhere "same_company_users.is_company_admin"
      sqlWhere "signatory_links.is_author"
      sqlWhereNotEq "documents.status" Preparation

documentFilterToSQL :: (State.MonadState v m, SqlWhere v) => DocumentFilter -> m ()
documentFilterToSQL (DocumentFilterStatuses statuses) = do
  sqlWhereIn "documents.status" statuses
documentFilterToSQL (DocumentFilterBySealStatus statuses) = do
  sqlWhereExists $ sqlSelect "main_files" $ do
    sqlWhere "main_files.document_id = documents.id"
    sqlWhere "main_files.id = (SELECT (max(id)) FROM main_files where document_id = documents.id)"
    sqlWhereIn "main_files.seal_status" statuses
documentFilterToSQL (DocumentFilterByStatusClass statuses) = do
  -- I think here we can use the result that we define on select
  -- check this one out later
  sqlWhereIn documentStatusClassExpression statuses

documentFilterToSQL (DocumentFilterByModificationTimeAfter mtime) = do
  sqlWhere ("(SELECT max(greatest(signatory_links.sign_time"
            <> ", signatory_links.seen_time"
            <> ", signatory_links.read_invitation"
            <> ", documents.invite_time"
            <> ", signatory_links.rejection_time"
            <> ", documents.mtime"
            <> ", documents.ctime"
            <> ")) FROM signatory_links WHERE signatory_links.document_id = documents.id)"
            <+> ">=" <?> mtime)

documentFilterToSQL (DocumentFilterByLatestSignTimeBefore time) = do
  sqlWhere $ documentLatestSignTimeExpression <+> "<" <?> time
documentFilterToSQL (DocumentFilterByLatestSignTimeAfter time) = do
  sqlWhere $ documentLatestSignTimeExpression <+> ">" <?> time
documentFilterToSQL (DocumentFilterPurged f) =
  if f then sqlWhereIsNotNULL "documents.purged_time"
       else sqlWhereIsNULL    "documents.purged_time"
documentFilterToSQL (DocumentFilterByMonthYearFrom (month,year)) = do
  sqlWhere $ raw $ unsafeFromString $ "(documents.mtime > '" ++ show year ++  "-" ++ show month ++ "-1')"
documentFilterToSQL (DocumentFilterByMonthYearTo (month,year)) = do
  sqlWhere $ raw $ unsafeFromString $ "(documents.mtime < '" ++ show (year + 1 <| month == 12 |> year)++ "-" ++ show ((month `mod` 12) + 1) ++ "-1')"
documentFilterToSQL (DocumentFilterByTags []) = do
  sqlWhere "TRUE"
documentFilterToSQL (DocumentFilterByTags tags) = do
  forM_ tags $ \tag -> do
    sqlWhereExists $ sqlSelect "document_tags" $ do
      sqlWhere "documents.id = document_tags.document_id"
      sqlWhereEq "document_tags.name" (tagname tag)
      sqlWhereEq "document_tags.value" (tagvalue tag)
documentFilterToSQL (DocumentFilterByString string) = do
  sqlWhere result
  where
      result = parenthesize $ (SQL "documents.title ILIKE ?" [sqlpat string] `sqlOR`
                               sqlConcatAND (map sqlMatch (words string)))
      sqlMatch word = SQL ("EXISTS (SELECT TRUE" <>
                                   "  FROM signatory_link_fields JOIN signatory_links AS sl5" <>
                                                                 "  ON sl5.document_id = signatory_links.document_id" <>
                                                                 " AND sl5.id = signatory_link_fields.signatory_link_id" <>
                                   -- " FROM signatory_link_fields " <>
                                   " WHERE (signatory_link_fields.type != ?) " <>
                                           " AND (signatory_link_fields.value ILIKE ?))") [toSql (SignatureFT "signature"),sqlpat word]
                                   --" WHERE TRUE)") []

      sqlpat text = toSql $ "%" ++ concatMap escape text ++ "%"
      escape '\\' = "\\\\"
      escape '%' = "\\%"
      escape '_' = "\\_"
      escape c = [c]

documentFilterToSQL (DocumentFilterByDelivery delivery) = do
  sqlWhereEq "documents.delivery_method" delivery

documentFilterToSQL (DocumentFilterLinkIsAuthor flag) = do
  sqlWhereEq "signatory_links.is_author" flag

documentFilterToSQL (DocumentFilterLinkIsPartner flag) = do
  sqlWhereEq "signatory_links.is_partner" flag

documentFilterToSQL (DocumentFilterUnsavedDraft flag) =
  sqlWhereAny $ do
    sqlWhereEq "documents.unsaved_draft" flag
    sqlWhereNotEq "documents.type" Signable
    sqlWhereNotEq "documents.status" Preparation

documentFilterToSQL (DocumentFilterByAuthor userid) = do
  sqlWhere "signatory_links.is_author"
  sqlWhereEq "signatory_links.user_id" userid

documentFilterToSQL (DocumentFilterByAuthorCompany companyid) = do
  sqlWhere "signatory_links.is_author"
  sqlWhere "signatory_links.user_id = users.id"
  sqlWhereEq "users.company_id" companyid


documentFilterToSQL (DocumentFilterByCanSign userid) = do
  sqlWhere "signatory_links.is_partner"
  sqlWhereEq "signatory_links.user_id" userid
  sqlWhereEq "documents.status" Pending
  sqlWhereIsNULL "signatory_links.sign_time"
  sqlWhereEqSql "signatory_links.sign_order" documentSignorderExpression


documentFilterToSQL (DocumentFilterByDocumentID did) = do
  sqlWhereEq "documents.id" did

documentFilterToSQL (DocumentFilterByDocumentIDs dids) = do
  sqlWhereIn "documents.id" dids

documentFilterToSQL (DocumentFilterSignable) = do
  sqlWhereEq "documents.type" Signable

documentFilterToSQL (DocumentFilterTemplate) = do
  sqlWhereEq "documents.type" Template

documentFilterToSQL (DocumentFilterDeleted flag1) = do
  sqlWhere "signatory_links.really_deleted IS NULL"
  if flag1
     then sqlWhere "signatory_links.deleted IS NOT NULL"
     else sqlWhere "signatory_links.deleted IS NULL"

checkEqualBy :: (Eq b, Show b) => String -> (a -> b) -> a -> a -> Maybe (String, String, String)
checkEqualBy name func obj1 obj2
  | func obj1 /= func obj2 = Just (name, show (func obj1), show (func obj2))
  | otherwise              = Nothing

checkEqualByAllowSecondNothing :: (Eq b, Show b) => String -> (a -> Maybe b) -> a -> a -> Maybe (String, String, String)
checkEqualByAllowSecondNothing name func obj1 obj2
  | func obj1 /= func obj2 && (not (isNothing (func obj2))) = Just (name, show (func obj1), show (func obj2))
  | otherwise              = Nothing

assertEqualDocuments :: (Monad m, Log.MonadLog m) => Document -> Document -> m ()
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
                         [
                           checkEqualByAllowSecondNothing "maybesignatory" maybesignatory
                         , checkEqualBy "maybesigninfo" maybesigninfo
                         , checkEqualBy "maybeseeninfo" maybeseeninfo
                         , checkEqualBy "maybereadinvite" maybereadinvite
                         , checkEqualBy "mailinvitationdeliverystatus" mailinvitationdeliverystatus
                         , checkEqualBy "smsinvitationdeliverystatus" smsinvitationdeliverystatus
                         , checkEqualBy "signatorysignatureinfo" signatorysignatureinfo
                         , checkEqualBy "signatorylinkdeleted" signatorylinkdeleted
                         , checkEqualBy "signatorylinkreallydeleted" signatorylinkreallydeleted
                         , checkEqualBy "signatorylinkcsvupload" signatorylinkcsvupload
                         , checkEqualBy "signatoryfields" (sort . signatoryfields)
                         , checkEqualBy "signatoryisauthor" (signatoryisauthor)
                         , checkEqualBy "signatoryispartner" (signatoryispartner)
                         , checkEqualBy "signatorysignorder" (signatorysignorder)
                         , checkEqualBy "signatorylinkrejectiontime" signatorylinkrejectiontime
                         , checkEqualBy "signatorylinkrejectionreason" signatorylinkrejectionreason
                         , checkEqualBy "signatorylinkauthenticationmethod" signatorylinkauthenticationmethod
                         , checkEqualBy "signatorylinkelegdatamismatchmessage" signatorylinkelegdatamismatchmessage
                         , checkEqualBy "signatorylinkelegdatamismatchfirstname" signatorylinkelegdatamismatchfirstname
                         , checkEqualBy "signatorylinkelegdatamismatchlastname" signatorylinkelegdatamismatchlastname
                         , checkEqualBy "signatorylinkelegdatamismatchpersonalnumber" signatorylinkelegdatamismatchpersonalnumber
                         , checkEqualBy "signatorylinkdeliverymethod" signatorylinkdeliverymethod
                         ]

    inequalities = catMaybes $ map (\f -> f d1 d2)
                   [ checkEqualBy "documenttitle" documenttitle
                   , checkEqualBy "documentfiles" documentfile
                   , checkEqualBy "documentsealedfiles" documentsealedfile
                   , checkEqualBy "documentstatus" documentstatus
                   , checkEqualBy "documenttype" documenttype
                   , checkEqualBy "documentctime" documentctime
                   , checkEqualBy "documentmtime" documentmtime
                   , checkEqualBy "documentdaystosign" documentdaystosign
                   , checkEqualBy "documentdaystoremind" documentdaystoremind
                   , checkEqualBy "documenttimeouttime" documenttimeouttime
                   , checkEqualBy "documentinvitetime" documentinvitetime
                   , checkEqualBy "documentinvitetext" documentinvitetext
                   , checkEqualBy "documentsharing" documentsharing
                   , checkEqualBy "documenttags" documenttags
                   , checkEqualBy "documentauthorattachments" (sort . documentauthorattachments)
                   , checkEqualBy "documentlang" documentlang
                   , checkEqualBy "documentapicallbackurl" documentapicallbackurl
                   , checkEqualBy "documentsealstatus" documentsealstatus
                   , checkEqualBy "documentsignatorylinks count" (length . documentsignatorylinks)
                   ] ++
                   concat (zipWith checkSigLink sl1 sl2)

selectTablesForDocumentSelectors :: State.State SqlSelect () -> SqlSelect
selectTablesForDocumentSelectors = sqlSelect2 "documents as documents LEFT JOIN document_automatic_reminders as document_automatic_reminders ON documents.id = document_automatic_reminders.document_id"

documentsSelectors :: [SQL]
documentsSelectors =
  [ "documents.id"
  , "documents.title"
  , "documents.status"
  , "documents.error_text"
  , "documents.type"
  , "documents.ctime"
  , "documents.mtime"
  , "documents.days_to_sign"
  , "documents.days_to_remind"
  , "documents.timeout_time"
  , "document_automatic_reminders.expires"
  , "documents.invite_time"
  , "documents.invite_ip"
  , "documents.invite_text"
  , "documents.lang"
  , "documents.sharing"
  , "documents.api_callback_url"
  , "documents.object_version"
  , documentStatusClassExpression
  ]



fetchDocuments :: MonadDB m => m [Document]
fetchDocuments = kFold decoder []
  where
    -- Note: this function gets documents in reversed order, but all queries
    -- use reversed order too, so in the end everything is properly ordered.
    decoder acc did title status error_text doc_type
      ctime mtime days_to_sign days_to_remind timeout_time auto_remind_time invite_time
     invite_ip invite_text
     lang sharing apicallback objectversion status_class
       = Document {
         documentid = did
       , documenttitle = title
       , documentsignatorylinks = []
       , documentmainfiles = []
       , documentstatus = case (status, error_text) of
           (DocumentError{}, Just text) -> DocumentError text
           (DocumentError{}, Nothing) -> DocumentError "document error"
           _ -> status
       , documenttype = doc_type
       , documentctime = ctime
       , documentmtime = mtime
       , documentdaystosign = days_to_sign
       , documentdaystoremind = days_to_remind
       , documenttimeouttime = timeout_time
       , documentautoremindtime = case status of
                                    Pending -> auto_remind_time
                                    _ -> Nothing
       , documentinvitetime = case invite_time of
           Nothing -> Nothing
           Just t -> Just (SignInfo t $ fromMaybe noIP invite_ip)
       , documentinvitetext = invite_text
       , documentsharing = sharing
       , documenttags = S.empty
       , documentauthorattachments = []
       , documentlang = lang
       , documentstatusclass = status_class
       , documentapicallbackurl = apicallback
       , documentobjectversion = objectversion
       } : acc

documentLatestSignTimeExpression :: SQL
documentLatestSignTimeExpression = "(SELECT max(signatory_links.sign_time) FROM signatory_links WHERE signatory_links.document_id = documents.id)"

documentStatusClassExpression :: SQL
documentStatusClassExpression =
    "(SELECT COALESCE((SELECT min(" <> statusClassCaseExpression <> ")"
                     <> "FROM signatory_links WHERE signatory_links.document_id = documents.id AND signatory_links.is_partner),"
                  <> "(SELECT " <> statusClassCaseExpressionForDocument <> "), "
                  <?> SCDraft <> "))"

documentSignorderExpression :: SQL
documentSignorderExpression =
       SQL "(COALESCE((SELECT min(signatory_links.sign_order) FROM signatory_links WHERE signatory_links.document_id = documents.id AND signatory_links.is_partner AND signatory_links.sign_time IS NULL), 1))" []


statusClassCaseExpression :: SQL
statusClassCaseExpression =
  "(CASE"
   <+> "WHEN documents.status = " <?> (DocumentError "") <+> "THEN" <?> SCError
   <+> "WHEN documents.status = " <?> Preparation        <+> "THEN" <?> SCDraft
   <+> "WHEN documents.status = " <?> Canceled           <+> "THEN" <?> SCCancelled
   <+> "WHEN documents.status = " <?> Timedout           <+> "THEN" <?> SCTimedout
   <+> "WHEN documents.status = " <?> Rejected           <+> "THEN" <?> SCRejected
   <+> "WHEN signatory_links.sign_time IS NOT NULL THEN"         <?> SCSigned
   <+> "WHEN signatory_links.seen_time IS NOT NULL THEN"         <?> SCOpened
   <+> "WHEN signatory_links.read_invitation IS NOT NULL THEN"   <?> SCRead
   <+> "WHEN signatory_links.mail_invitation_delivery_status = " <?> Undelivered <+> "THEN" <?> SCDeliveryProblem
   <+> "WHEN signatory_links.sms_invitation_delivery_status = "  <?> Undelivered <+> "THEN" <?> SCDeliveryProblem
   <+> "WHEN signatory_links.mail_invitation_delivery_status = " <?> Delivered   <+> "THEN" <?> SCDelivered
   <+> "WHEN signatory_links.sms_invitation_delivery_status = "  <?> Delivered   <+> "THEN" <?> SCDelivered
   <+> "ELSE" <?> SCSent
  <+> "END :: INTEGER)"

statusClassCaseExpressionForDocument :: SQL
statusClassCaseExpressionForDocument =
  "(CASE"
   <+> "WHEN documents.status = " <?> (DocumentError "") <+> "THEN" <?> SCError
   <+> "WHEN documents.status = " <?> Preparation        <+> "THEN" <?> SCDraft
   <+> "WHEN documents.status = " <?> Canceled           <+> "THEN" <?> SCCancelled
   <+> "WHEN documents.status = " <?> Timedout           <+> "THEN" <?> SCTimedout
   <+> "WHEN documents.status = " <?> Rejected           <+> "THEN" <?> SCRejected
  <+> "END :: INTEGER)"


selectSignatoryLinksX :: State.State SqlSelect () -> SqlSelect
selectSignatoryLinksX extension = sqlSelect "signatory_links" $ do
  sqlResult "signatory_links.id"
  sqlResult "signatory_links.document_id"
  sqlResult "signatory_links.user_id"
  sqlResult "signatory_links.sign_order"
  sqlResult "signatory_links.token"
  sqlResult "signatory_links.sign_time"
  sqlResult "signatory_links.sign_ip"
  sqlResult "signatory_links.seen_time"
  sqlResult "signatory_links.seen_ip"
  sqlResult "signatory_links.read_invitation"
  sqlResult "signatory_links.mail_invitation_delivery_status"
  sqlResult "signatory_links.sms_invitation_delivery_status"
  sqlResult "signatory_links.signinfo_text"
  sqlResult "signatory_links.signinfo_signature"
  sqlResult "signatory_links.signinfo_certificate"
  sqlResult "signatory_links.signinfo_provider"
  sqlResult "signatory_links.signinfo_first_name_verified"
  sqlResult "signatory_links.signinfo_last_name_verified"
  sqlResult "signatory_links.signinfo_personal_number_verified"
  sqlResult "signatory_links.signinfo_ocsp_response"
  sqlResult "signatory_links.is_author"
  sqlResult "signatory_links.is_partner"
  sqlResult "signatory_links.csv_title"
  sqlResult "signatory_links.csv_contents"
  sqlResult "signatory_links.deleted"
  sqlResult "signatory_links.really_deleted"
  sqlResult "signatory_links.sign_redirect_url"
  sqlResult "signatory_links.reject_redirect_url"
  sqlResult "signatory_links.rejection_time"
  sqlResult "signatory_links.rejection_reason"
  sqlResult "signatory_links.authentication_method"
  sqlResult "signatory_links.eleg_data_mismatch_message"
  sqlResult "signatory_links.eleg_data_mismatch_first_name"
  sqlResult "signatory_links.eleg_data_mismatch_last_name"
  sqlResult "signatory_links.eleg_data_mismatch_personal_number"
  sqlResult "signatory_links.delivery_method"

  sqlResult (statusClassCaseExpression <> SQL " AS status_class" [])
  sqlResult "signatory_attachments.file_id AS sigfileid"
  sqlResult "signatory_attachments.name AS signame"
  sqlResult "signatory_attachments.description AS sigdesc"
  sqlLeftJoinOn "signatory_attachments" "signatory_attachments.signatory_link_id = signatory_links.id"
  sqlJoinOn "documents" "signatory_links.document_id = documents.id"
  extension

selectSignatoryLinksSQL :: SQL
selectSignatoryLinksSQL = toSQLCommand (selectSignatoryLinksX (return ())) <+> ""

fetchSignatoryLinks :: MonadDB m => m (M.Map DocumentID [SignatoryLink])
fetchSignatoryLinks = do
  sigs <- kFold decoder (nulldocid, [], M.empty)
  return $ (\(d, l, m) -> M.insertWith' (++) d l m) sigs
  where
    nulldocid = unsafeDocumentID $ -1
    decoder (docid, links, linksmap) slid document_id user_id
     sign_order token sign_time sign_ip seen_time seen_ip read_invitation
     mail_invitation_delivery_status sms_invitation_delivery_status signinfo_text signinfo_signature signinfo_certificate
     signinfo_provider signinfo_first_name_verified signinfo_last_name_verified
     signinfo_personal_number_verified signinfo_ocsp_response
     is_author is_partner csv_title csv_contents
     deleted really_deleted signredirecturl rejectredirecturl
     rejection_time rejection_reason
     authentication_method
     eleg_data_mismatch_message
     eleg_data_mismatch_first_name
     eleg_data_mismatch_last_name
     eleg_data_mismatch_personal_number
     delivery_method
     status_class
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
          , signatorysignorder = sign_order
          , signatoryfields = []
          , signatoryisauthor = is_author
          , signatoryispartner = is_partner
          , signatorymagichash = token
          , maybesignatory = user_id
          , maybesigninfo = SignInfo <$> sign_time <*> sign_ip
          , maybeseeninfo = SignInfo <$> seen_time <*> seen_ip
          , maybereadinvite = read_invitation
          , mailinvitationdeliverystatus = mail_invitation_delivery_status
          , smsinvitationdeliverystatus = sms_invitation_delivery_status
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
          , signatorylinkdeleted = deleted
          , signatorylinkreallydeleted = really_deleted
          , signatorylinkcsvupload =
              CSVUpload <$> csv_title <*> csv_contents
          , signatoryattachments = sigAtt
          , signatorylinkstatusclass = status_class
          , signatorylinksignredirecturl = signredirecturl
          , signatorylinkrejectredirecturl = rejectredirecturl
          , signatorylinkrejectionreason = rejection_reason
          , signatorylinkrejectiontime = rejection_time
          , signatorylinkauthenticationmethod = authentication_method
          , signatorylinkelegdatamismatchmessage = eleg_data_mismatch_message
          , signatorylinkelegdatamismatchfirstname = eleg_data_mismatch_first_name
          , signatorylinkelegdatamismatchlastname = eleg_data_mismatch_last_name
          , signatorylinkelegdatamismatchpersonalnumber = eleg_data_mismatch_personal_number
          , signatorylinkdeliverymethod = delivery_method
          }

-- For this to work well we assume that signatories are ordered: author first, then all with ids set, then all with id == 0
insertSignatoryLinksAsAre :: MonadDB m => DocumentID -> [SignatoryLink] -> m [SignatoryLink]
insertSignatoryLinksAsAre _documentid [] = return []
insertSignatoryLinksAsAre documentid links = do
  _ <- kRun $ sqlInsert "signatory_links" $ do
           sqlSet "document_id" documentid
           sqlSetListWithDefaults "id" $ map (\sl -> if (unsafeSignatoryLinkID 0 == signatorylinkid sl) then Nothing else (Just $ signatorylinkid sl)) links
           sqlSetList "user_id" $ maybesignatory <$> links
           sqlSetList "is_author" $ signatoryisauthor <$> links
           sqlSetList "is_partner" $ signatoryispartner <$> links
           sqlSetList "token" $ signatorymagichash <$> links
           sqlSetList "sign_order"$ signatorysignorder <$> links
           sqlSetList "sign_time" $ fmap signtime <$> maybesigninfo <$> links
           sqlSetList "sign_ip" $ fmap signipnumber <$> maybesigninfo <$> links
           sqlSetList "seen_time" $ fmap signtime <$> maybeseeninfo <$> links
           sqlSetList "seen_ip" $ fmap signipnumber <$> maybeseeninfo <$> links
           sqlSetList "read_invitation" $ maybereadinvite <$> links
           sqlSetList "mail_invitation_delivery_status" $ mailinvitationdeliverystatus <$> links
           sqlSetList "sms_invitation_delivery_status" $ smsinvitationdeliverystatus <$> links
           sqlSetList "signinfo_text" $ fmap signatureinfotext <$> signatorysignatureinfo <$> links
           sqlSetList "signinfo_signature" $ fmap signatureinfosignature <$> signatorysignatureinfo <$> links
           sqlSetList "signinfo_certificate" $ fmap signatureinfocertificate <$> signatorysignatureinfo <$> links
           sqlSetList "signinfo_provider" $ fmap signatureinfoprovider <$> signatorysignatureinfo <$> links
           sqlSetList "signinfo_first_name_verified" $ fmap signaturefstnameverified <$> signatorysignatureinfo <$> links
           sqlSetList "signinfo_last_name_verified" $ fmap signaturelstnameverified <$> signatorysignatureinfo <$> links
           sqlSetList "signinfo_personal_number_verified" $ fmap signaturepersnumverified <$> signatorysignatureinfo <$> links
           sqlSetList "csv_title" $ fmap csvtitle <$> signatorylinkcsvupload <$> links
           sqlSetList "csv_contents" $ fmap csvcontents <$> signatorylinkcsvupload <$> links
           sqlSetList "deleted" $ signatorylinkdeleted <$> links
           sqlSetList "really_deleted" $ signatorylinkreallydeleted <$> links
           sqlSetList "signinfo_ocsp_response" $ fmap signatureinfoocspresponse <$> signatorysignatureinfo <$> links
           sqlSetList "sign_redirect_url" $ signatorylinksignredirecturl <$> links
           sqlSetList "reject_redirect_url" $ signatorylinkrejectredirecturl <$> links
           sqlSetList "rejection_time" $ signatorylinkrejectiontime <$> links
           sqlSetList "rejection_reason" $ signatorylinkrejectionreason <$> links
           sqlSetList "authentication_method" $ signatorylinkauthenticationmethod <$> links
           sqlSetList "eleg_data_mismatch_message" $ signatorylinkelegdatamismatchmessage <$> links
           sqlSetList "eleg_data_mismatch_first_name" $ signatorylinkelegdatamismatchfirstname <$> links
           sqlSetList "eleg_data_mismatch_last_name" $ signatorylinkelegdatamismatchlastname <$> links
           sqlSetList "eleg_data_mismatch_personal_number" $ signatorylinkelegdatamismatchpersonalnumber <$> links
           sqlSetList "delivery_method" $ signatorylinkdeliverymethod <$> links
           sqlResult "id"

  (slids :: [SignatoryLinkID]) <- kFold (\acc slid -> slid : acc) []

  _ <- kRun $ selectSignatoryLinksX $ do
         sqlWhereIn "signatory_links.id" slids
         sqlWhereEq "signatory_links.document_id" documentid
         sqlOrderBy "signatory_links.id DESC"

  siglinks <- fetchSignatoryLinks

  let newLinksAsList = concatMap snd $ M.toList siglinks

  let replaceAttachments newlink oldlink = (signatorylinkid newlink, signatoryattachments oldlink)
  sigattaches <- insertSignatoryAttachmentsAsAre $ zipWith replaceAttachments newLinksAsList links

  let replaceFields newlink oldlink = (signatorylinkid newlink, (signatoryfields) oldlink)

  fields <- insertSignatoryLinkFieldsAsAre $ zipWith replaceFields newLinksAsList links

  forM newLinksAsList $ \newlink -> do
      let newlinkid = signatorylinkid newlink
      let newlinkfull = newlink { signatoryattachments = M.findWithDefault [] newlinkid sigattaches
                                , signatoryfields = M.findWithDefault [] newlinkid fields
                                }
      return newlinkfull

signatoryAttachmentsSelectors :: String
signatoryAttachmentsSelectors = intercalate ", "
  [ "signatory_link_id"
  , "file_id"
  , "name"
  , "description"
  ]

fetchSignatoryAttachments :: MonadDB m => m (M.Map SignatoryLinkID [SignatoryAttachment])
fetchSignatoryAttachments = kFold decoder M.empty
  where
    decoder acc signatory_link_id file_id name description =
      M.insertWith' (++) signatory_link_id [SignatoryAttachment {
          signatoryattachmentfile = file_id
        , signatoryattachmentname = name
        , signatoryattachmentdescription = description
        }] acc


documentTagsSelectors :: SQL
documentTagsSelectors = sqlConcatComma [
    "document_id"
  , "name"
  , "value"
  ]

selectDocumentTagsSQL :: SQL
selectDocumentTagsSQL = "SELECT" <+> documentTagsSelectors <+> "FROM document_tags "

fetchDocumentTags :: MonadDB m => m (M.Map DocumentID (S.Set DocumentTag))
fetchDocumentTags = kFold decoder M.empty
  where
    decoder acc document_id name v =
      M.insertWith' S.union document_id
         (S.singleton $ DocumentTag name v) acc

insertDocumentTagsAsAre :: MonadDB m => DocumentID -> [DocumentTag] -> m [DocumentTag]
insertDocumentTagsAsAre _documentid [] = return []
insertDocumentTagsAsAre documentid tags = do
  _ <- kRun $ sqlInsert "document_tags" $ do
         sqlSet "document_id" documentid
         sqlSetList "name" $ tagname <$> tags
         sqlSetList "value" $ tagvalue <$> tags
         sqlResult "document_tags.document_id"
         sqlResult "document_tags.name"
         sqlResult "document_tags.value"

  fetchDocumentTags
    >>= return . concatMap (S.toList . snd) . M.toList


authorAttachmentsSelectors :: SQL
authorAttachmentsSelectors = sqlConcatComma [
    "document_id"
  , "file_id"
  ]

selectAuthorAttachmentsSQL :: SQL
selectAuthorAttachmentsSQL = "SELECT" <+> authorAttachmentsSelectors <+> "FROM author_attachments "

fetchAuthorAttachments :: MonadDB m => m (M.Map DocumentID [AuthorAttachment])
fetchAuthorAttachments = kFold decoder M.empty
  where
    decoder acc document_id file_id =
      M.insertWith' (++) document_id [AuthorAttachment {
        authorattachmentfile = file_id
      }] acc

insertAuthorAttachmentsAsAre :: MonadDB m => DocumentID -> [AuthorAttachment] -> m [AuthorAttachment]
insertAuthorAttachmentsAsAre _documentid [] = return []
insertAuthorAttachmentsAsAre documentid attachments = do
  _ <- kRun $ sqlInsert "author_attachments" $ do
        sqlSet "document_id" documentid
        sqlSetList "file_id" $ authorattachmentfile <$> attachments
        sqlResult "document_id"
        sqlResult "file_id"

  fetchAuthorAttachments
    >>= return . concatMap snd . M.toList

mainFilesSelectors :: [SQL]
mainFilesSelectors =
  [ "document_id"
  , "file_id"
  , "document_status"
  , "seal_status"
  ]

selectMainFilesSQL :: SQL
selectMainFilesSQL = "SELECT" <+> sqlConcatComma mainFilesSelectors <+> "FROM main_files "

fetchMainFiles :: MonadDB m => m (M.Map DocumentID [MainFile])
fetchMainFiles = kFold decoder M.empty
  where
    decoder acc document_id file_id document_status seal_status =
      M.insertWith' (++) document_id [MainFile {
        mainfileid = file_id
      , mainfiledocumentstatus = document_status
      , mainfilesealstatus = seal_status
      }] acc

insertMainFilesAsAre :: MonadDB m => DocumentID -> [MainFile] -> m [MainFile]
insertMainFilesAsAre _documentid [] = return []
insertMainFilesAsAre documentid rfiles = do
  let files = reverse rfiles -- rfiles should be inserted with descending id: newer files come first in rfiles
  _ <- kRun $ sqlInsert "main_files" $ do
        sqlSet "document_id" documentid
        sqlSetList "file_id" $ mainfileid <$> files
        sqlSetList "document_status" $ mainfiledocumentstatus <$> files
        sqlSetList "seal_status" $ mainfilesealstatus <$> files
        mapM_ sqlResult mainFilesSelectors
  fetchMainFiles
    >>= return . concatMap snd . M.toList

insertSignatoryAttachmentsAsAre :: MonadDB m
                                => [(SignatoryLinkID,[SignatoryAttachment])]
                                -> m (M.Map SignatoryLinkID [SignatoryAttachment])
insertSignatoryAttachmentsAsAre attachments | all (null . snd) attachments = return M.empty
insertSignatoryAttachmentsAsAre attachments = do
  _ <- kRun $ sqlInsert "signatory_attachments" $ do
          sqlSetList "signatory_link_id" $ concatMap (\(d,l) -> map (const d) l) attachments
          sqlSetList "file_id" $ signatoryattachmentfile <$> concatMap snd attachments
          sqlSetList "name" $ signatoryattachmentname <$> concatMap snd attachments
          sqlSetList "description" $ signatoryattachmentdescription <$> concatMap snd attachments
          sqlResult "signatory_link_id"
          sqlResult "file_id"
          sqlResult "name"
          sqlResult "description"

  fetchSignatoryAttachments

signatoryLinkFieldsSelectors :: [SQL]
signatoryLinkFieldsSelectors =
  [ "signatory_link_id"
  , "type"
  , "custom_name"
  , "is_author_filled"
  , "value"
  , "obligatory"
  , "should_be_filled_by_author"
  , "placements"
  ]

selectSignatoryLinkFieldsSQL :: SQL
selectSignatoryLinkFieldsSQL = "SELECT"
  <+> sqlConcatComma signatoryLinkFieldsSelectors
  <+> "FROM signatory_link_fields "

fetchSignatoryLinkFields :: MonadDB m => m (M.Map SignatoryLinkID [SignatoryField])
fetchSignatoryLinkFields = kFold decoder M.empty
  where
    decoder acc slid xtype custom_name is_author_filled v obligatory should_be_filled_by_sender placements =
      M.insertWith' (++) slid
         [SignatoryField
          { sfValue = v
          , sfPlacements = placements
          , sfType = case xtype of
                        CustomFT{} -> CustomFT custom_name is_author_filled
                        CheckboxFT{} -> CheckboxFT custom_name
                        SignatureFT{} -> SignatureFT (if null custom_name
                                                      then "signature"
                                                      else custom_name)
                        _   -> xtype
          , sfObligatory = obligatory
          , sfShouldBeFilledBySender = should_be_filled_by_sender
          }] acc

insertSignatoryLinkFieldsAsAre :: MonadDB m
                               => [(SignatoryLinkID,[SignatoryField])]
                               -> m (M.Map SignatoryLinkID [SignatoryField])
insertSignatoryLinkFieldsAsAre fields | all (null . snd) fields = return M.empty
insertSignatoryLinkFieldsAsAre fields = do
  let getCustomName field = case sfType field of
                              CustomFT name _ -> name
                              CheckboxFT name -> name
                              SignatureFT name -> name
                              _ -> ""
      isAuthorFilled field = case sfType field of
                               CustomFT _ authorfilled -> authorfilled
                               CheckboxFT _  -> False
                               _ -> False
  _ <- kRun $ sqlInsert "signatory_link_fields" $ do
         sqlSetList "signatory_link_id" $ concatMap (\(d,l) -> map (const d) l) fields
         sqlSetList "type" $ sfType <$> concatMap snd fields
         sqlSetList "custom_name" $ getCustomName <$> concatMap snd fields
         sqlSetList "is_author_filled" $ isAuthorFilled <$> concatMap snd fields
         sqlSetList "value" $ sfValue <$> concatMap snd fields
         sqlSetList "placements" $ sfPlacements <$> concatMap snd fields
         sqlSetList "obligatory" $ sfObligatory <$> concatMap snd fields
         sqlSetList "should_be_filled_by_author" $ sfShouldBeFilledBySender <$> concatMap snd fields
         mapM_ sqlResult signatoryLinkFieldsSelectors

  fetchSignatoryLinkFields


insertSignatoryScreenshots :: (MonadDB m, Applicative m, CryptoRNG m)
                           => [(SignatoryLinkID, SignatoryScreenshots)] -> m Integer
insertSignatoryScreenshots l = do
  let (slids, types, times, ss) = unzip4 $ [ (slid, "first",     t, s) | (slid, Just (t, s)) <- map (second first) l ]
                                        <> [ (slid, "signing",   t, s) | (slid, Just (t, s)) <- map (second signing) l ]
                                        <> [ (slid, "reference", t, s) | (slid,      (t, s)) <- map (second reference) l ]
  (fileids :: [FileID]) <- mapM (\(t,s) -> dbUpdate $ NewFile (t ++ "_screenshot.jpeg") (Screenshot.image s)) (zip types ss)
  if null slids then return 0 else
    kRun $ sqlInsert "signatory_screenshots" $ do
           sqlSetList "signatory_link_id" $ slids
           sqlSetList "type"              $ (types :: [String])
           sqlSetList "time"              $ times
           sqlSetList "file_id"           $ fileids

data GetSignatoryScreenshots = GetSignatoryScreenshots [SignatoryLinkID]
instance (MonadDB m, MonadIO m, Amazon.AmazonMonad m) => DBQuery m GetSignatoryScreenshots [(SignatoryLinkID, SignatoryScreenshots)] where
  query (GetSignatoryScreenshots l) = do
    kRun_ $ sqlSelect "signatory_screenshots" $ do
                sqlWhereIn "signatory_link_id" l
                sqlOrderBy "signatory_link_id"

                sqlResult "signatory_link_id"
                sqlResult "type"
                sqlResult "time"
                sqlResult "file_id"
    let folder1 a slid ty time fid = (slid :: SignatoryLinkID, ty :: String, time :: MinutesTime, fid :: FileID) : a
    screenshotsWithoutBinaryData <- flip kFold [] folder1
    let getBinaries (slid, ty, time, fid) = do
           bin <- getFileIDContents fid
           return (slid, ty, time, Binary bin)
    screenshotsWithBinaryData <- mapM getBinaries screenshotsWithoutBinaryData

    let folder ((slid', s):a) (slid, ty, time, i) | slid' == slid = (slid, mkss ty time i s):a
        folder a (slid, ty, time, i) = (slid, mkss ty time i emptySignatoryScreenshots) : a

        mkss :: String -> MinutesTime -> Binary -> SignatoryScreenshots -> SignatoryScreenshots
        mkss "first"     time i s = s{ first = Just (time, Screenshot i) }
        mkss "signing"   time i s = s{ signing = Just (time, Screenshot i) }
        mkss "reference" time i s = s{ reference = (time, Screenshot i) }
        mkss t           _    _ _ = error $ "GetSignatoryScreenshots: invalid type: " <> show t
    return $ foldl' folder [] screenshotsWithBinaryData


insertDocumentAsIs :: MonadDB m => Document -> m (Maybe Document)
insertDocumentAsIs document@(Document
                   _documentid
                   documenttitle
                   documentsignatorylinks
                   documentmainfiles
                   documentstatus
                   documenttype
                   documentctime
                   documentmtime
                   documentdaystosign
                   documentdaystoremind
                   documenttimeouttime
                   _documentautoremindtime
                   documentinvitetime
                   documentinvitetext
                   documentsharing
                   documenttags
                   documentauthorattachments
                   documentlang
                   _documentstatusclass
                   documentapicallbackurl
                   documentobjectversion
                 ) = do
    _ <- kRun $ sqlInsert "documents" $ do
        sqlSet "title" documenttitle
        sqlSet "status" documentstatus
        sqlSet "error_text" $ case documentstatus of
          DocumentError msg -> toSql msg
          _ -> SqlNull
        sqlSet "type" documenttype
        sqlSet "ctime" documentctime
        sqlSet "mtime" documentmtime
        sqlSet "days_to_sign" documentdaystosign
        sqlSet "days_to_remind" documentdaystoremind
        sqlSet "timeout_time" documenttimeouttime
        sqlSet "invite_time" $ signtime `fmap` documentinvitetime
        sqlSet "invite_ip" (fmap signipnumber documentinvitetime)
        sqlSet "invite_text" documentinvitetext
        sqlSet "lang" documentlang
        sqlSet "sharing" documentsharing
        sqlSet "object_version" documentobjectversion
        sqlSet "api_callback_url" documentapicallbackurl
        sqlResult "documents.id"

    mdid <-  (kFold (\acc did -> did:acc) []) >>= oneObjectReturnedGuard
    case mdid of
      Nothing -> return Nothing
      Just did -> do
        void $ insertSignatoryLinksAsAre did documentsignatorylinks
        void $ insertAuthorAttachmentsAsAre did documentauthorattachments
        void $ S.fromList <$> insertDocumentTagsAsAre did (S.toList documenttags)
        void $ insertMainFilesAsAre did documentmainfiles
        newdocument <- dbQuery $ GetDocumentByDocumentID did
        assertEqualDocuments document newdocument
        return (Just newdocument)

insertNewDocument :: (MonadDB m, MonadIO m) => Document -> m Document
insertNewDocument doc = do
  now <- getMinutesTime
  let docWithTime = doc {documentmtime  = now, documentctime = now}
  newdoc <- insertDocumentAsIs docWithTime
  case newdoc of
    Just d -> return d
    Nothing -> error "insertNewDocument failed for some reason"

-- Create new document based on existing one
newFromDocumentID :: (MonadDB m, MonadIO m) => (Document -> Document) -> DocumentID -> m (Maybe Document)
newFromDocumentID f docid = do
  doc <- query $ GetDocumentByDocumentID docid
  newFromDocument f doc

newFromDocument :: (MonadDB m, MonadIO m) => (Document -> Document) -> Document -> m (Maybe Document)
newFromDocument f doc = do
  Just `liftM` insertNewDocument (f doc)

data ArchiveDocument = ArchiveDocument UserID Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m ArchiveDocument () where
  update (ArchiveDocument uid _actor) = updateDocumentWithID $ \did -> do
    kRunManyOrThrowWhyNot $ sqlUpdate "signatory_links" $ do
        sqlSetCmd "deleted" "now()"

        sqlWhereExists $ sqlSelect "users" $ do
          sqlJoinOn "users AS same_company_users" "(users.company_id = same_company_users.company_id OR users.id = same_company_users.id)"
          sqlWhere "signatory_links.user_id = users.id"

          sqlWhereUserIsDirectlyOrIndirectlyRelatedToDocument uid
          sqlWhereUserIsSelfOrCompanyAdmin

        sqlWhereExists $ sqlSelect "documents" $ do
          sqlJoinOn "users AS same_company_users" "TRUE"

          sqlWhere $ "signatory_links.document_id = " <?> did
          sqlWhere "documents.id = signatory_links.document_id"

          sqlWhereDocumentStatusIsOneOf [Preparation, Closed, Canceled, Timedout, Rejected, DocumentError ""]


-- | Attach a main file to a document associating it with preparation
-- status.  Any old main file in preparation status will be removed.
-- Can only be done on documents in preparation.
data AttachFile = AttachFile FileID Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m AttachFile () where
  update (AttachFile fid a) = updateDocumentWithID $ \did -> do
    kRun_ $ sqlDelete "main_files" $ do
      sqlWhereEq "document_id" did
      sqlWhereEq "document_status" Preparation
    kRun1OrThrowWhyNot $ sqlInsertSelect "main_files" "" $ do
      sqlSet "file_id" fid
      sqlSet "document_id" did
      sqlSet "document_status" Preparation
      sqlSet "seal_status" Missing
      sqlWhereExists $ sqlSelect "documents" $ do
        sqlWhereDocumentIDIs did
        sqlWhereDocumentStatusIs Preparation
      -- FIXME:
      --
      -- We do not need to check if the file really exists because if
      -- it does not then at the end of the transation we will get
      -- foreign key violation.
      --
      -- But there is another thing to check here: if the actor really
      -- has access rights to the file. It might be that we will
      -- connect somebody elses file to the document thus letting
      -- unrecognized person to see contents of somebody elses
      -- document.
      --
      -- Some magic needs to be invented to prevent that from
      -- happening.
    updateMTimeAndObjectVersion (actorTime a)
    return ()

-- | Detach main files in Preparation status.  Document must be in Preparation.
data DetachFile = DetachFile Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m DetachFile () where
  update (DetachFile a) = updateDocumentWithID $ \did -> do
    kRunManyOrThrowWhyNot $ sqlDelete "main_files" $ do
      sqlWhereEq "document_id" did
      sqlWhereEq "document_status" Preparation
      sqlWhereExists $ sqlSelect "documents" $ do
        sqlWhereDocumentIDIs did
        sqlWhereDocumentStatusIs Preparation
    updateMTimeAndObjectVersion (actorTime a)

-- | Append a sealed file to a document, updating modification time.
-- If it has a Guardtime signature, generate an event.
data AppendSealedFile = AppendSealedFile FileID SealStatus Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m AppendSealedFile () where
  update (AppendSealedFile fid status actor) = updateDocumentWithID $ \did -> do
    appendSealedFile did fid status
    when (hasGuardtimeSignature status) $ do
      void $ update $ InsertEvidenceEvent
            AttachGuardtimeSealedFileEvidence
            (return ())
            actor
            did
    updateMTimeAndObjectVersion (actorTime actor)

-- | Append an extended sealed file to a document, as a result of
-- improving an already sealed document.
data AppendExtendedSealedFile = AppendExtendedSealedFile FileID SealStatus Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m AppendExtendedSealedFile () where
  update (AppendExtendedSealedFile fid status actor) = updateDocumentWithID $ \did -> do
    appendSealedFile did fid status
    _ <- update $ InsertEvidenceEvent
      AttachExtendedSealedFileEvidence
      (return ())
      actor
      did
    return ()

appendSealedFile :: (MonadDB m, TemplatesMonad m) => DocumentID -> FileID -> SealStatus -> m ()
appendSealedFile did fid status = do
    kRun1OrThrowWhyNot $ sqlInsertSelect "main_files" "" $ do
      sqlSet "document_id" did
      sqlSet "file_id" fid
      sqlSet "document_status" Closed
      sqlSet "seal_status" status
      sqlWhereExists $ sqlSelect "documents" $ do
        sqlWhereDocumentIDIs did
        sqlWhereDocumentStatusIs Closed

data FixClosedErroredDocument = FixClosedErroredDocument Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m FixClosedErroredDocument () where
  update (FixClosedErroredDocument _actor) = updateDocumentWithID $ \did -> do
    kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
        sqlSet "status" Closed
        sqlWhereEq "id" did
        sqlWhereEq "status" $ DocumentError undefined

data LogSignWithELegFailureForDocument = LogSignWithELegFailureForDocument SignatoryLinkID (Maybe String) (Maybe String) String String String Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m LogSignWithELegFailureForDocument () where
  update (LogSignWithELegFailureForDocument slid mname mnumber firstName lastName personNumber actor) = updateDocumentWithID $ \did -> do
    sl <- query $ GetSignatoryLinkByID did slid Nothing
    let trips = [("Name",    fromMaybe (getFullName sl) mname, firstName ++ " " ++ lastName)
                ,("Personal number", fromMaybe (getPersonalNumber sl) mnumber, personNumber)]
        uneql = filter (\(_,a,b)->a/=b) trips
        msg2 = intercalate "; " $ map (\(f,s,e)->f ++ " from transaction was \"" ++ s ++ "\" but from e-legitimation was \"" ++ e ++ "\"") uneql
    _ <- update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
                    SignWithELegFailureEvidence
                    (value "msg" msg2)
                    (Just sl)
                    Nothing
                    actor
                    did
    updateMTimeAndObjectVersion (actorTime actor)
    return ()

data CancelDocument = CancelDocument Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m CancelDocument () where
  update (CancelDocument actor) = updateDocumentWithID $ \did -> do
    kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
                 sqlSet "status" Canceled
                 sqlWhereDocumentIDIs did
                 sqlWhereDocumentTypeIs Signable
                 sqlWhereDocumentStatusIs Pending
    _ <- update $ InsertEvidenceEvent
                  CancelDocumentEvidence
                  (return ())
                  actor
                  did
    updateMTimeAndObjectVersion (actorTime actor)
    return ()

data ChangeSignatoryEmailWhenUndelivered = ChangeSignatoryEmailWhenUndelivered SignatoryLinkID (Maybe User) String Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m ChangeSignatoryEmailWhenUndelivered () where
  update (ChangeSignatoryEmailWhenUndelivered slid muser email actor) = updateDocumentWithID $ const $ do
      oldemail <- kRunAndFetch1OrThrowWhyNot (\acc (m :: String) -> acc ++ [m]) $ sqlUpdate "signatory_link_fields" $ do
             sqlFrom "signatory_link_fields AS signatory_link_fields_old"
             sqlWhere "signatory_link_fields.id = signatory_link_fields_old.id"
             sqlSet "value" email
             sqlResult "signatory_link_fields_old.value"
             sqlWhereEq "signatory_link_fields.signatory_link_id" slid
             sqlWhereEq "signatory_link_fields.type" EmailFT
      kRun1OrThrowWhyNot $ sqlUpdate "signatory_links" $ do
          sqlSet "mail_invitation_delivery_status" Unknown
          sqlSet "user_id" $ fmap userid muser
          sqlWhereEq "signatory_links.id" slid
          sqlWhereExists $ sqlSelect "documents" $ do
              sqlWhere "documents.id = signatory_links.document_id"
              sqlWhereDocumentStatusIs Pending
      _ <- theDocumentID >>= update . InsertEvidenceEvent
          ChangeSignatoryEmailWhenUndeliveredEvidence
          (value "oldemail" oldemail >> value "newemail" email)
          actor
      updateMTimeAndObjectVersion (actorTime actor)
      return ()


data FileInDocument = FileInDocument DocumentID FileID
instance (MonadDB m) => DBQuery m FileInDocument Bool where
  query (FileInDocument did fid) = do
    let s1 = sqlSelect "main_files" $ do
                   sqlWhereEq "file_id" fid
                   sqlWhereEq "document_id" did
                   sqlResult "TRUE"
    let s2 = sqlSelect "author_attachments" $ do
                   sqlWhereEq "file_id" fid
                   sqlWhereEq "document_id" did
                   sqlResult "TRUE"
    let s3 = sqlSelect "signatory_attachments" $ do
                   sqlJoinOn "signatory_links" "signatory_attachments.signatory_link_id = signatory_links.id"
                   sqlWhereEq "file_id" fid
                   sqlWhereEq "document_id" did
                   sqlResult "TRUE"
    Just result <- getOne ("SELECT EXISTS (" <> toSQLCommand s1 <> ") OR " <>
                                  "EXISTS (" <> toSQLCommand s2 <> ") OR " <>
                                  "EXISTS (" <> toSQLCommand s3 <> ")")
    return result


data ChangeSignatoryPhoneWhenUndelivered = ChangeSignatoryPhoneWhenUndelivered SignatoryLinkID String Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m ChangeSignatoryPhoneWhenUndelivered () where
  update (ChangeSignatoryPhoneWhenUndelivered slid phone actor) = updateDocumentWithID $ const $ do
      oldphone <- kRunAndFetch1OrThrowWhyNot (\acc (m :: String) -> acc ++ [m]) $ sqlUpdate "signatory_link_fields" $ do
             sqlFrom "signatory_link_fields AS signatory_link_fields_old"
             sqlWhere "signatory_link_fields.id = signatory_link_fields_old.id"
             sqlSet "value" phone
             sqlResult "signatory_link_fields_old.value"
             sqlWhereEq "signatory_link_fields.signatory_link_id" slid
             sqlWhereEq "signatory_link_fields.type" MobileFT
      kRun1OrThrowWhyNot $ sqlUpdate "signatory_links" $ do
          sqlSet "sms_invitation_delivery_status" Unknown
          sqlSet "user_id" $ SqlNull
          sqlWhereEq "signatory_links.id" slid
          sqlWhereExists $ sqlSelect "documents" $ do
              sqlWhere "documents.id = signatory_links.document_id"
              sqlWhereDocumentStatusIs Pending
      _ <- theDocumentID >>= update . InsertEvidenceEvent
          ChangeSignatoryPhoneWhenUndeliveredEvidence
          (value "oldphone" oldphone >> value "newphone" phone)
          actor
      updateMTimeAndObjectVersion (actorTime actor)
      return ()

data PreparationToPending = PreparationToPending Actor TimeZoneName
instance (MonadBaseControl IO m, DocumentMonad m, TemplatesMonad m) => DBUpdate m PreparationToPending () where
  update (PreparationToPending actor tzn) = updateDocumentWithID $ \docid -> do
            let time = actorTime actor

            -- If we know actor's time zone:
            --   Set timeout to the beginning of the day: start of actorTime day + days to sign + 1
            --   Example: if actor time is 13:00 October 24, and days to sign is 1, then timeout is October 25 23:59 59
            --   Rationale: actor may have picked October 25 from calendar as last day to sign, which gave days to sign = 1, and so
            --   we should time out when October 25 has passed in actor's time zone.
            -- If we don't know actor's time zone:
            --   Set timeout to actorTime + days to sign + 1
            --   Example: if actor time is 13:00 October 24, and days to sign is 1, then timeout is October 26 12:59:59
            --   Rationale: Signatories will have at least until the end of the intended last day to sign.
            -- We try to match expectation when one day after 24 december is understood as till last minute of 25 december.
            let timestamp = formatTime defaultTimeLocale "%F" (toUTCTime time) ++ " " ++ TimeZoneName.toString tzn
            -- Need to temporarily set session timezone to any one
            -- that recognizes daylight savings so that the day
            -- interval addition advances the time properly across DST changes
            -- (i.e., so that we stay on midnight)
            -- http://www.postgresql.org/docs/9.2/static/functions-datetime.html
            dstTz <- mkTimeZoneName "Europe/Stockholm"
            withTimeZone dstTz $ do
              lang :: Lang <- kRunAndFetch1OrThrowWhyNot (flip (:)) $ sqlUpdate "documents" $ do
                sqlSet "status" Pending
                sqlSetCmd "timeout_time" $ "cast (" <?> timestamp <+> "as timestamp with time zone)"
                            <+> "+ ((interval '1 day') * documents.days_to_sign) + (interval '23 hours 59 minutes 59 seconds')" -- This interval add almoust one they from description above.
                sqlResult "lang"
                sqlWhereDocumentIDIs docid
                sqlWhereDocumentTypeIs Signable
                sqlWhereDocumentStatusIs Preparation

              kRun_ $ sqlUpdate "signatory_links" $ do
                sqlSet "csv_title" (Nothing :: Maybe String)
                sqlSet "csv_contents" (Nothing :: Maybe String)
                sqlWhereEq "document_id" docid

              Just tot <- getOne ("SELECT timeout_time FROM documents WHERE id =" <?> docid) >>= exactlyOneObjectReturnedGuard
              _ <- update $ InsertEvidenceEvent
                PreparationToPendingEvidence
                (  value "timezone" (TimeZoneName.toString tzn)
                >> value "lang" (show lang)
                >> value "timeouttime" (formatMinutesTimeUTC tot))
                actor
                docid
              updateMTimeAndObjectVersion (actorTime actor)
            return ()

data CloseDocument = CloseDocument Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m CloseDocument () where
  update (CloseDocument actor) = updateDocumentWithID $ \docid -> do
    kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
                 sqlSet "status" Closed
                 sqlWhereDocumentIDIs docid
                 sqlWhereDocumentTypeIs Signable
                 sqlWhereDocumentStatusIs Pending
                 sqlWhereAllSignatoriesHaveSigned
    _ <- update $ InsertEvidenceEvent
                CloseDocumentEvidence
                (return ())
                actor
                docid
    updateMTimeAndObjectVersion (actorTime actor)
    return ()


data DeleteSigAttachment = DeleteSigAttachment SignatoryLinkID SignatoryAttachment Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m DeleteSigAttachment () where
  update (DeleteSigAttachment slid sa actor) = updateDocument $ \doc -> do
    let decode acc saname = acc ++ [saname]
    (saname::String) <- kRunAndFetch1OrThrowWhyNot decode $ sqlUpdate "signatory_attachments" $ do
      sqlFrom "signatory_links"
      sqlWhere "signatory_links.id = signatory_attachments.signatory_link_id"
      sqlSet "file_id" SqlNull
      sqlResult "signatory_attachments.name"
      sqlWhereEq "signatory_attachments.name" (signatoryattachmentname sa)
      sqlWhereSignatoryLinkIDIs slid
      sqlWhereSignatoryHasNotSigned
    _ <- update $ InsertEvidenceEvent
                    DeleteSigAttachmentEvidence
                    (do value "name" saname
                        value "author" $ getIdentifier $ $(fromJust) $ getAuthorSigLink doc)
                    actor
                    (documentid doc)
    return ()


data ErrorDocument = ErrorDocument String CurrentEvidenceEventType (F.Fields Identity ()) Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m ErrorDocument () where
  update (ErrorDocument errmsg event textFields actor) = updateDocumentWithID $ \docid -> do
    kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
      sqlSet "status" $ DocumentError errmsg
      sqlSet "error_text" errmsg
      sqlWhereDocumentIDIs docid
    void $ update $ InsertEvidenceEvent event textFields actor docid

selectDocuments :: MonadDB m => SqlSelect -> m [Document]
selectDocuments sqlquery = snd <$> selectDocumentsWithSoftLimit True Nothing sqlquery

selectDocument :: MonadDB m => SqlSelect -> m Document
selectDocument sqlquery = $(head) . snd <$> selectDocumentsWithSoftLimit False Nothing sqlquery

selectDocumentsWithSoftLimit :: MonadDB m => Bool ->  Maybe Int -> SqlSelect -> m (Int,[Document])
selectDocumentsWithSoftLimit allowzeroresults softlimit sqlquery = do

    {-
    kRun_ $ "EXPLAIN (ANALYZE, VERBOSE, COSTS, BUFFERS)" <+> toSQLCommand sqlquery
    let fetchTextLines acc line = line:acc
    textlines <- kFold fetchTextLines []
    mapM_ Log.debug (reverse textlines)
    -}

    allDocumentsCount <- case softlimit of
      Nothing -> do
        _ <- kRun $ SQL "CREATE TEMP TABLE docs AS " [] <> toSQLCommand sqlquery
        Just count <- getOne $ SQL "SELECT count(*) FROM docs" []
        return count

      Just limit -> do
        _ <- kRun $ SQL "CREATE TEMP TABLE docs1 AS " [] <> toSQLCommand sqlquery
        _ <- kRun $ SQL ("CREATE TEMP TABLE docs AS SELECT * FROM docs1 LIMIT " <> unsafeFromString (show limit)) []
        Just count <- getOne $ SQL "SELECT count(*) FROM docs1" []
        kRunRaw "DROP TABLE docs1"
        return count

    when (allDocumentsCount==0 && not allowzeroresults) $ do
      kRunRaw "DROP TABLE docs"
      exception <- kWhyNot1 sqlquery

      kThrow $ exception

    kRun_ $ SQL "SELECT * FROM docs" []
    docs <- reverse `liftM` fetchDocuments


    kRun_ $ SQL "CREATE TEMP TABLE links AS " [] <>
         selectSignatoryLinksSQL <>
         SQL "WHERE EXISTS (SELECT 1 FROM docs WHERE signatory_links.document_id = docs.id) ORDER BY document_id DESC, signatory_links.id DESC" []
    kRun_ $ SQL "SELECT * FROM links" []
    sls <- fetchSignatoryLinks

    kRun_ $ selectSignatoryLinkFieldsSQL <> SQL "WHERE EXISTS (SELECT 1 FROM links WHERE links.id = signatory_link_fields.signatory_link_id) ORDER BY signatory_link_fields.id DESC" []
    fields <- fetchSignatoryLinkFields

    kRun_ $ selectAuthorAttachmentsSQL <> SQL "WHERE EXISTS (SELECT 1 FROM docs WHERE author_attachments.document_id = docs.id) ORDER BY document_id DESC" []
    ats <- fetchAuthorAttachments

    kRun_ $ selectDocumentTagsSQL <> SQL "WHERE EXISTS (SELECT 1 FROM docs WHERE document_tags.document_id = docs.id)" []
    tags <- fetchDocumentTags

    kRun_ $ selectMainFilesSQL <> "WHERE EXISTS (SELECT 1 FROM docs WHERE main_files.document_id = docs.id) ORDER BY document_id DESC, id ASC"
    mainfiles <- fetchMainFiles

    kRunRaw "DROP TABLE docs"
    kRunRaw "DROP TABLE links"

    let extendSignatoryLinkWithFields sl =
           sl{ signatoryfields = M.findWithDefault [] (signatorylinkid sl) fields }


    let fill doc = doc
                   { documentsignatorylinks    = extendSignatoryLinkWithFields <$> M.findWithDefault [] (documentid doc) sls
                   , documentauthorattachments = M.findWithDefault [] (documentid doc) ats
                   , documenttags              = M.findWithDefault S.empty (documentid doc) tags
                   , documentmainfiles         = M.findWithDefault [] (documentid doc) mainfiles
                   }

    return (allDocumentsCount, map fill docs)

data GetDocumentTags = GetDocumentTags DocumentID
instance MonadDB m => DBQuery m GetDocumentTags (S.Set DocumentTag) where
  query (GetDocumentTags did) = do
    _ <- kRun $ selectDocumentTagsSQL <> SQL "WHERE document_id = ?" [toSql did]
    fetchDocumentTags
      >>= oneObjectReturnedGuard . map snd . M.toList
      >>= return . fromMaybe S.empty

data GetSignatoryLinkByID = GetSignatoryLinkByID DocumentID SignatoryLinkID (Maybe MagicHash)
instance (MonadDB m) => DBQuery m GetSignatoryLinkByID SignatoryLink where
  query (GetSignatoryLinkByID did slid mmh) = do
    let queryx = selectSignatoryLinksX $ do
                  sqlWhereDocumentIDIs did
                  sqlWhereSignatoryLinkIDIs slid
                  case mmh of
                    Nothing -> return ()
                    Just mh -> sqlWhereSignatoryLinkMagicHashIs mh
    kRun_ queryx
    mlink <- fetchSignatoryLinks
      >>= oneObjectReturnedGuard . concatMap snd . M.toList
    case mlink of
      Just link -> do
         kRun_ $ selectSignatoryLinkFieldsSQL
               <> SQL "WHERE signatory_link_id = ?" [toSql slid]
         fields <- fetchSignatoryLinkFields
               >>= return . concatMap snd . M.toList
         return $ link { signatoryfields = fields }
      Nothing -> do
         exception <- kWhyNot1 queryx

         kThrow $ exception

data GetDocumentByDocumentID = GetDocumentByDocumentID DocumentID
instance MonadDB m => DBQuery m GetDocumentByDocumentID Document where
  query (GetDocumentByDocumentID did) = do
    selectDocument $ selectTablesForDocumentSelectors $ do
      mapM_ sqlResult documentsSelectors
      sqlWhereDocumentIDIs did

data GetDocumentsByDocumentIDs = GetDocumentsByDocumentIDs [DocumentID]
instance MonadDB m => DBQuery m GetDocumentsByDocumentIDs [Document] where
  query (GetDocumentsByDocumentIDs dids) = do
    selectDocuments $ selectTablesForDocumentSelectors $ do
      mapM_ sqlResult documentsSelectors
      sqlWhereIn "documents.id" dids

data GetDocumentBySignatoryLinkID = GetDocumentBySignatoryLinkID SignatoryLinkID
instance MonadDB m => DBQuery m GetDocumentBySignatoryLinkID (Maybe Document) where
  query (GetDocumentBySignatoryLinkID slid) =
     (Just <$> (selectDocument $ selectTablesForDocumentSelectors $ do
       mapM_ sqlResult documentsSelectors
       sqlWhereExists $ sqlSelect "signatory_links" $ do
         sqlWhereEq "signatory_links.id" slid
         sqlWhere "signatory_links.document_id = documents.id"))

data GetDocumentsBySignatoryLinkIDs = GetDocumentsBySignatoryLinkIDs [SignatoryLinkID]
instance MonadDB m => DBQuery m GetDocumentsBySignatoryLinkIDs [Document] where
  query (GetDocumentsBySignatoryLinkIDs slids) =
     selectDocuments $ selectTablesForDocumentSelectors $ do
       mapM_ sqlResult documentsSelectors
       sqlWhereExists $ sqlSelect "signatory_links" $ do
         sqlWhereIn "signatory_links.id" slids
         sqlWhere "signatory_links.document_id = documents.id"

data GetDocumentByDocumentIDSignatoryLinkIDMagicHash = GetDocumentByDocumentIDSignatoryLinkIDMagicHash DocumentID SignatoryLinkID MagicHash
instance MonadDB m => DBQuery m GetDocumentByDocumentIDSignatoryLinkIDMagicHash Document where
  query (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) = do
    selectDocument $ selectTablesForDocumentSelectors $ do
      mapM_ sqlResult documentsSelectors
      sqlWhereDocumentIDIs did
      sqlWhereExists $ sqlSelect "signatory_links" $ do
         sqlWhere "signatory_links.document_id = documents.id"
         -- Thought for later: Here we might actually check if
         -- visibility rules allow a person to see this document, for
         -- example if sign order allows to see the document. For now
         -- we are sloppy and let a person see the document.
         sqlWhereSignatoryLinkIDIs slid
         sqlWhereSignatoryLinkMagicHashIs mh

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
data GetDocuments = GetDocuments [DocumentDomain] [DocumentFilter] [AscDesc DocumentOrderBy] (Int,Int)
instance MonadDB m => DBQuery m GetDocuments [Document] where
  query (GetDocuments domains filters orderbys (offset,limit)) =
    snd <$> query (GetDocuments2 True domains filters orderbys (offset,limit,Nothing))

data GetDocuments2 = GetDocuments2 Bool [DocumentDomain] [DocumentFilter] [AscDesc DocumentOrderBy] (Int,Int,Maybe Int)
instance MonadDB m => DBQuery m GetDocuments2 (Int,[Document]) where
  query (GetDocuments2 allowZeroResults domains filters orderbys (offset,limit,softlimit)) = do
    selectDocumentsWithSoftLimit allowZeroResults softlimit $ selectTablesForDocumentSelectors $ do
      mapM_ sqlResult documentsSelectors
      mapM_ (sqlOrderBy . documentOrderByAscDescToSQL) orderbys
      sqlOffset $ fromIntegral offset
      sqlLimit $ fromIntegral limit
      sqlWhereExists $ sqlSelect "signatory_links" $ do
        sqlWhere "documents.id = signatory_links.document_id"
        sqlLeftJoinOn "users" "signatory_links.user_id = users.id"
        sqlLeftJoinOn "companies" "users.company_id = companies.id"
        sqlLeftJoinOn "users AS same_company_users" "users.company_id = same_company_users.company_id OR users.id = same_company_users.id"
        sqlWhereAny (mapM_ documentDomainToSQL domains)
        mapM_ documentFilterToSQL filters


{- |
    All documents authored by the user that have never been deleted.
-}
data GetDocumentsByAuthor = GetDocumentsByAuthor UserID
instance MonadDB m => DBQuery m GetDocumentsByAuthor [Document] where
  query (GetDocumentsByAuthor uid) =
    query (GetDocuments [DocumentsVisibleToUser uid] [DocumentFilterByAuthor uid, DocumentFilterDeleted False] [Asc DocumentOrderByMTime] (0,maxBound))

data GetTemplatesByAuthor = GetTemplatesByAuthor UserID
instance MonadDB m => DBQuery m GetTemplatesByAuthor [Document] where
  query (GetTemplatesByAuthor uid) =
    query (GetDocuments [DocumentsVisibleToUser uid] [DocumentFilterByAuthor uid, DocumentFilterDeleted False, DocumentFilterTemplate] [Asc DocumentOrderByMTime] (0,maxBound))

data GetAvailableTemplates = GetAvailableTemplates UserID
instance MonadDB m => DBQuery m GetAvailableTemplates [Document] where
  query (GetAvailableTemplates uid) =
    query (GetDocuments [DocumentsVisibleToUser uid]
                            [DocumentFilterTemplate, DocumentFilterDeleted False]
                            [Asc DocumentOrderByMTime]
                            (0,maxBound))

data GetTimeoutedButPendingDocumentsChunk = GetTimeoutedButPendingDocumentsChunk MinutesTime Int
instance MonadDB m => DBQuery m GetTimeoutedButPendingDocumentsChunk [Document] where
  query (GetTimeoutedButPendingDocumentsChunk mtime size) = do
    selectDocuments $ selectTablesForDocumentSelectors $ do
      mapM_ sqlResult documentsSelectors
      sqlWhereEq "documents.status" Pending
      sqlWhere $ "timeout_time IS NOT NULL AND timeout_time < " <?> mtime
      sqlLimit (fromIntegral size)

data MarkDocumentSeen = MarkDocumentSeen SignatoryLinkID MagicHash Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m MarkDocumentSeen () where
  update (MarkDocumentSeen slid mh actor) = updateDocumentWithID $ \did -> do
        let time = actorTime actor
            ipnumber = fromMaybe noIP $ actorIP actor
        kRun1OrThrowWhyNotAllowIgnore $ sqlUpdate "signatory_links" $ do
            sqlSet "seen_time" time
            sqlSet "seen_ip" ipnumber

            sqlWhereExists $ sqlSelect "documents" $ do
              sqlWhere "documents.id = signatory_links.document_id"
              sqlWhereDocumentIDIs did
              sqlWhereSignatoryLinkIDIs slid
              sqlWhereSignatoryLinkMagicHashIs mh
              sqlWhereDocumentTypeIs (Signable)
              sqlIgnore $ sqlWhere "signatory_links.seen_time IS NULL"
              sqlIgnore $ sqlWhere "signatory_links.sign_time IS NULL"
              sqlWhereDocumentStatusIsOneOf [Pending, Timedout, Canceled, DocumentError undefined, Rejected]

data AddInvitationEvidence = AddInvitationEvidence SignatoryLinkID (Maybe String) Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m AddInvitationEvidence Bool where
  update (AddInvitationEvidence slid mmsg actor) = updateDocumentWithID $ \docid -> do
        sig <- query $ GetSignatoryLinkByID docid slid Nothing
        _ <- update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
          InvitationEvidence
          (return ())
          (Just sig)
          mmsg
          actor
          docid
        return True

data MarkInvitationRead = MarkInvitationRead SignatoryLinkID Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m MarkInvitationRead Bool where
  update (MarkInvitationRead slid actor) = updateDocumentWithID $ \did -> do
        sig <- query $ GetSignatoryLinkByID did slid Nothing
        let time = actorTime actor
        success <- kRun01 $ sqlUpdate "signatory_links" $ do
                      sqlSet "read_invitation" time
                      sqlWhereEq "id" slid
                      sqlWhereEq "document_id" did
                      sqlWhere "read_invitation IS NULL"
        _ <- update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
            MarkInvitationReadEvidence
            (return ())
            (Just sig)
            Nothing
            actor
            did
        return success

data NewDocument = NewDocument User String DocumentType Int Actor
instance (CryptoRNG m, MonadDB m, TemplatesMonad m) => DBUpdate m NewDocument (Maybe Document) where
  update (NewDocument user title documenttype nrOfOtherSignatories actor) = do
    let ctime = actorTime actor
    magichash <- random
    authorFields <- signatoryFieldsFromUser user
    let authorlink0 = signLinkFromDetails' authorFields True True (SignOrder 1) [] magichash

    let authorlink = authorlink0 {
                           maybesignatory = Just $ userid user }

    othersignatories <- sequence $ replicate nrOfOtherSignatories $ do
                          mh <- random
                          return $ signLinkFromDetails' emptySignatoryFields False True (SignOrder 2) [] mh

    let doc = defaultValue
                  { documenttitle                = title
                  , documentsignatorylinks       = authorlink : othersignatories
                  , documenttype                 = documenttype
                  , documentlang                 = getLang user
                  , documentctime                = ctime
                  , documentmtime                = ctime
                  , documentauthorattachments    = []
                  }

    midoc <- insertDocumentAsIs doc
    case midoc of
        Just _ -> return midoc
        Nothing -> do
          Log.debug $ "insertDocumentAsIs could not insert document #" ++ show (documentid doc) ++ " in NewDocument"
          return Nothing


data RejectDocument = RejectDocument SignatoryLinkID (Maybe String) Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m RejectDocument () where
  update (RejectDocument slid customtext actor) = updateDocumentWithID $ \docid -> do
    let time = actorTime actor
    kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
                                     sqlSet "status" Rejected
                                     sqlFrom "signatory_links"
                                     sqlWhere "signatory_links.document_id = documents.id"

                                     sqlWhereDocumentIDIs docid
                                     sqlWhereSignatoryLinkIDIs slid
                                     sqlWhereDocumentTypeIs Signable
                                     sqlWhereDocumentStatusIs Pending

    kRun1OrThrowWhyNot $ sqlUpdate "signatory_links" $ do
                                     sqlSet "rejection_time" time
                                     sqlSet "rejection_reason" customtext
                                     sqlFrom "documents"
                                     sqlWhere "signatory_links.document_id = documents.id"
                                     sqlWhereSignatoryIsPartner
                                     sqlWhereSignatoryHasNotSigned
                                     sqlWhereDocumentIDIs docid
                                     sqlWhereSignatoryLinkIDIs slid
                                     sqlWhereSignatoryHasNotSigned

    _ <- update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
                  RejectDocumentEvidence
                  (return ())
                  Nothing
                  customtext
                  actor
                  docid
    updateMTimeAndObjectVersion (actorTime actor)
    return ()

data RestartDocument = RestartDocument Document Actor
instance (CryptoRNG m, MonadDB m, TemplatesMonad m) => DBUpdate m RestartDocument (Maybe Document) where
  update (RestartDocument doc actor) = do
    mndoc <- tryToGetRestarted
    case mndoc of
      Right newdoc -> do
        md <- newFromDocument (const newdoc) doc
        case md of
          Nothing -> return Nothing
          Just d -> do
            copyEvidenceLogToNewDocument (documentid doc) (documentid d)
            _ <- update $ InsertEvidenceEvent
              RestartDocumentEvidence
              (return ())
              actor
              (documentid d)
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
      newSignLinks <- forM (documentsignatorylinks doc) $ \sl -> do
                           magichash <- random
                           return $ defaultValue {
                                signatorylinkid            = (unsafeSignatoryLinkID 0)
                              , signatorymagichash = magichash
                              , signatoryfields            = signatoryfields sl
                              , signatoryisauthor          = signatoryisauthor sl
                              , signatoryispartner         = signatoryispartner sl
                              , signatorysignorder         = signatorysignorder sl
                              , signatorylinkcsvupload       = signatorylinkcsvupload sl
                              , signatoryattachments         = signatoryattachments sl
                              , signatorylinksignredirecturl = signatorylinksignredirecturl sl
                              , signatorylinkrejectredirecturl = signatorylinkrejectredirecturl sl
                              , signatorylinkauthenticationmethod = signatorylinkauthenticationmethod sl
                              , signatorylinkdeliverymethod       = signatorylinkdeliverymethod sl
                              , maybesignatory = if (isAuthor sl) then maybesignatory sl else Nothing
                          }
      return doc {documentstatus = Preparation,
                  documenttimeouttime = Nothing,
                  documentsignatorylinks = newSignLinks
                 }

data RestoreArchivedDocument = RestoreArchivedDocument User Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m RestoreArchivedDocument () where
  update (RestoreArchivedDocument user _actor) = updateDocumentWithID $ \did -> do
    kRunManyOrThrowWhyNot $ sqlUpdate "signatory_links" $ do

      sqlSet "deleted" SqlNull

      sqlWhereExists $ sqlSelect "users" $ do
          sqlJoinOn "users AS same_company_users" "(users.company_id = same_company_users.company_id OR users.id = same_company_users.id)"
          sqlWhere "signatory_links.user_id = users.id"

          sqlWhereUserIsDirectlyOrIndirectlyRelatedToDocument (userid user)
          sqlWhereUserIsSelfOrCompanyAdmin

      sqlWhereExists $ sqlSelect "documents" $ do
          sqlJoinOn "users AS same_company_users" "TRUE"

          sqlWhere "documents.purged_time IS NULL"

          sqlWhere $ "signatory_links.document_id = " <?> did
          sqlWhere "documents.id = signatory_links.document_id"

{- |
    Links up a signatory link to a user account.  This should happen when
      \1. a document moves from preparation to pending more
      \2. a signer creates an account after signing to save their document
      \3. the email of a signatory is corrected to that of an existing user
-}
data SaveDocumentForUser = SaveDocumentForUser User SignatoryLinkID
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SaveDocumentForUser Bool where
  update (SaveDocumentForUser User{userid} slid) = updateDocumentWithID $ \did -> do
    kRun01 $ sqlUpdate "signatory_links" $ do
        sqlSet "user_id" userid
        sqlWhereEq "document_id" did
        sqlWhereEq "id" slid

{- |
    Saves a signatory attachment to a document.
    If there's a problem such as the document isn't in a pending or awaiting author state,
    or the document does not exist a Left is returned.
-}
data SaveSigAttachment = SaveSigAttachment SignatoryLinkID SignatoryAttachment FileID Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SaveSigAttachment () where
  update (SaveSigAttachment slid sigattach fid actor) = updateDocument $ \doc -> do
    let name = signatoryattachmentname sigattach
    kRun1OrThrowWhyNot $ sqlUpdate "signatory_attachments" $ do
       sqlFrom "signatory_links"
       sqlWhere "signatory_links.id = signatory_attachments.signatory_link_id"
       sqlSet "file_id"  fid
       sqlWhere "file_id IS NULL"
       sqlWhereEq "name" name
       sqlWhereSignatoryLinkIDIs slid

    _ <- update $ InsertEvidenceEvent
        SaveSigAttachmentEvidence
        (do value "name" name
            value "description" $ signatoryattachmentdescription sigattach
            value "author" $ getIdentifier $ $(fromJust) $ getAuthorSigLink doc)
        actor
        (documentid doc)
    return ()

data SetDocumentTags = SetDocumentTags (S.Set DocumentTag) Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetDocumentTags Bool where
  update (SetDocumentTags doctags _actor) = updateDocumentWithID $ \did -> do
    oldtags <- query $ GetDocumentTags did
    let changed = doctags /= oldtags
    if changed
      then do
        _ <- kRun $ SQL "DELETE FROM document_tags WHERE document_id = ?" [toSql did]
        newtags <- insertDocumentTagsAsAre did (S.toList doctags)
        return $ length newtags == S.size doctags
      else
        return True


data SetDocumentInviteTime = SetDocumentInviteTime MinutesTime Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetDocumentInviteTime () where
  update (SetDocumentInviteTime invitetime actor) = updateDocumentWithID $ \did -> do
    let ipaddress  = fromMaybe noIP $ actorIP actor
    kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
       sqlSet "invite_time" invitetime
       sqlSet "invite_ip" ipaddress
       sqlWhereDocumentIDIs did

data SetInviteText = SetInviteText String Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetInviteText Bool where
  update (SetInviteText text _actor) = updateWithoutEvidence "invite_text" text

data SetDaysToSign = SetDaysToSign Int Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetDaysToSign Bool where
  update (SetDaysToSign days _actor) = updateWithoutEvidence "days_to_sign" days

data SetDaysToRemind = SetDaysToRemind (Maybe Int) Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetDaysToRemind Bool where
  update (SetDaysToRemind days _actor) = updateWithoutEvidence "days_to_remind" days

data SetDocumentTitle = SetDocumentTitle String Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetDocumentTitle Bool where
  update (SetDocumentTitle doctitle _actor) = updateWithoutEvidence "title" doctitle

data SetDocumentLang = SetDocumentLang Lang Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetDocumentLang Bool where
  update (SetDocumentLang lang _actor) = updateWithoutEvidence "lang" lang

data SetEmailInvitationDeliveryStatus = SetEmailInvitationDeliveryStatus SignatoryLinkID DeliveryStatus Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetEmailInvitationDeliveryStatus Bool where
  update (SetEmailInvitationDeliveryStatus slid status actor) = updateDocumentWithID $ \did -> do
    sig <- query $ GetSignatoryLinkByID did slid Nothing
    kRun1OrThrowWhyNot $  sqlUpdate "signatory_links" $ do
        sqlFrom "documents"
        sqlJoin "signatory_links AS signatory_links_old"
        sqlWhere "signatory_links.id = signatory_links_old.id"
        sqlSet "mail_invitation_delivery_status" status
        sqlWhereSignatoryLinkIDIs slid
        sqlWhereDocumentIDIs did
        sqlWhereDocumentTypeIs Signable
    nsig <- query $ GetSignatoryLinkByID did slid Nothing
    let changed = mailinvitationdeliverystatus sig /= mailinvitationdeliverystatus nsig

    when_ (changed && status == Delivered) $
      update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
        InvitationDeliveredByEmail
        (return ())
        (Just nsig)
        Nothing
        actor
        did
    when_ (changed && status == Undelivered) $
      update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
        InvitationUndeliveredByEmail
        (return ())
        (Just nsig)
        Nothing
        actor
        did
    return True

data SetSMSInvitationDeliveryStatus = SetSMSInvitationDeliveryStatus SignatoryLinkID DeliveryStatus Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetSMSInvitationDeliveryStatus Bool where
  update (SetSMSInvitationDeliveryStatus slid status actor) = updateDocumentWithID $ \did -> do
    sig <- query $ GetSignatoryLinkByID did slid Nothing
    kRun_ $  sqlUpdate "signatory_links" $ do
        sqlFrom "documents"
        sqlJoin "signatory_links AS signatory_links_old"
        sqlWhere "signatory_links.id = signatory_links_old.id"
        sqlSet "sms_invitation_delivery_status" status
        sqlWhereSignatoryLinkIDIs slid
        sqlWhereDocumentIDIs did
        sqlWhereDocumentTypeIs Signable
    nsig <- query $ GetSignatoryLinkByID did slid Nothing
    let changed = smsinvitationdeliverystatus sig /= smsinvitationdeliverystatus nsig
    when_ (changed && status == Delivered) $
      update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
        InvitationDeliveredBySMS
        (return ())
        (Just nsig)
        Nothing
        actor
        did
    when_ (changed && status == Undelivered) $
      update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
        InvitationUndeliveredBySMS
        (return ())
        (Just nsig)
        Nothing
        actor
        did
    return True


data SetDocumentSharing = SetDocumentSharing [DocumentID] Bool
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetDocumentSharing Bool where
  update (SetDocumentSharing dids flag) = do
    results <- kRun $ sqlUpdate "documents" $ do
          sqlSet "sharing" $ (if flag then Shared else Private)
          sqlWhereIn "id" dids
    return $ results == (fromIntegral $ length dids)

data SetDocumentUnsavedDraft = SetDocumentUnsavedDraft Bool
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetDocumentUnsavedDraft () where
  update (SetDocumentUnsavedDraft flag) = updateDocumentWithID $ \did -> do
    kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
      sqlSet "unsaved_draft" flag
      sqlWhereDocumentIDIs did

data SignDocument = SignDocument SignatoryLinkID MagicHash (Maybe SignatureInfo) SignatoryScreenshots Actor
instance (DocumentMonad m, TemplatesMonad m, Applicative m, CryptoRNG m) => DBUpdate m SignDocument () where
  update (SignDocument slid mh msiginfo screenshots actor) = updateDocumentWithID $ \docid -> do
            let ipnumber = fromMaybe noIP $ actorIP actor
                time     = actorTime actor
            kRun1OrThrowWhyNot $ sqlUpdate "signatory_links" $ do
                 sqlFrom "documents"
                 sqlSet "sign_ip"                            ipnumber
                 sqlSet "sign_time"                          time
                 sqlSet "signinfo_text"                      $ signatureinfotext `fmap` msiginfo
                 sqlSet "signinfo_signature"                 $ signatureinfosignature `fmap` msiginfo
                 sqlSet "signinfo_certificate"               $ signatureinfocertificate `fmap` msiginfo
                 sqlSet "signinfo_provider"                  $ signatureinfoprovider `fmap` msiginfo
                 sqlSet "signinfo_first_name_verified"       $ signaturefstnameverified `fmap` msiginfo
                 sqlSet "signinfo_last_name_verified"        $ signaturelstnameverified `fmap` msiginfo
                 sqlSet "signinfo_personal_number_verified"  $ signaturepersnumverified `fmap` msiginfo
                 sqlSet "signinfo_ocsp_response"             $ signatureinfoocspresponse `fmap` msiginfo
                 sqlWhere "documents.id = signatory_links.document_id"
                 sqlWhereDocumentIDIs docid
                 sqlWhereSignatoryLinkIDIs slid
                 sqlWhereDocumentTypeIs Signable
                 sqlWhereDocumentStatusIs Pending
                 sqlWhereSignatoryIsPartner
                 sqlWhereSignatoryHasNotSigned
                 sqlWhereSignatoryAuthenticationMethodIs (if isJust msiginfo
                                                             then ELegAuthentication
                                                             else StandardAuthentication)
                 sqlWhereSignatoryLinkMagicHashIs mh
            let signatureFields = case msiginfo of
                    Nothing -> return ()
                    Just si -> do
                              value "eleg" True
                              value "provider" $ case signatureinfoprovider si of
                                                    BankIDProvider -> "BankID" :: String
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
            sl <- query $ GetSignatoryLinkByID docid slid Nothing
            _ <- update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
                SignDocumentEvidence
                signatureFields
                (Just sl)
                Nothing
                actor
                docid
            _ <- insertSignatoryScreenshots [(slid, screenshots)]
            updateMTimeAndObjectVersion (actorTime actor)
            return ()


-- For this to work well we assume that signatories are ordered: author first, then all with ids set, then all with id == 0
data ResetSignatoryDetails = ResetSignatoryDetails [SignatoryLink] Actor
instance (CryptoRNG m, DocumentMonad m, TemplatesMonad m) => DBUpdate m ResetSignatoryDetails Bool where
  update (ResetSignatoryDetails signatories _actor) = updateDocumentWithID $ \documentid -> do
    document <- query $ GetDocumentByDocumentID documentid
    case checkResetSignatoryData document signatories of
          [] -> do
            kRun_ $ "DELETE FROM signatory_links WHERE document_id = " <?> documentid
            siglinks <- forM signatories $ \sl -> do
                     magichash <- random
                     return $ sl {  signatorymagichash = magichash,
                                    maybesignatory = if (isAuthor sl) then (maybesignatory sl) else Nothing
                                 }
            _r1 <- insertSignatoryLinksAsAre documentid siglinks
            return True

          s -> do
            Log.error $ "cannot reset signatory details on document " ++ show documentid ++ " because " ++ intercalate ";" s
            return False

data CloneDocumentWithUpdatedAuthor = CloneDocumentWithUpdatedAuthor User Document Actor
instance (MonadDB m, TemplatesMonad m, MonadIO m,CryptoRNG m) => DBUpdate m CloneDocumentWithUpdatedAuthor (Maybe DocumentID) where
  update (CloneDocumentWithUpdatedAuthor user document actor) = do
          company <- query $ GetCompanyByUserID (userid user)
          siglinks <- forM (documentsignatorylinks document) $ \sl -> do
                magichash <- random
                let sl' = if (isAuthor sl) then (replaceSignatoryUser sl user company) else sl
                return sl' {signatorylinkid = unsafeSignatoryLinkID 0, signatorymagichash = magichash}
          res <- (flip newFromDocumentID) (documentid document) $ \doc ->
            doc {
                documentstatus = Preparation
              , documentsharing = Private
              , documentsignatorylinks = siglinks
                                       -- FIXME: Need to remove authorfields?
              , documentctime = actorTime actor
              , documentmtime = actorTime actor
              }
          case res of
            Nothing -> return Nothing
            Just d -> do
              copyEvidenceLogToNewDocument (documentid document) $ documentid d
              return $ Just $ documentid d

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
data TemplateFromDocument = TemplateFromDocument Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m TemplateFromDocument () where
  update (TemplateFromDocument _actor) = updateDocumentWithID $ \did -> do
    kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
       sqlSet "status" Preparation
       sqlSet "type" Template
       sqlWhereDocumentIDIs did
       sqlWhereEq "status" Preparation


data DocumentFromTemplate = DocumentFromTemplate Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m DocumentFromTemplate () where
  update (DocumentFromTemplate _actor) = updateDocumentWithID $ \did -> do
    kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
       sqlSet "status" Preparation
       sqlSet "type" Signable
       sqlWhereDocumentIDIs did
       sqlWhereEq "status" Preparation

data TimeoutDocument = TimeoutDocument Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m TimeoutDocument () where
  update (TimeoutDocument actor) = updateDocumentWithID $ \did -> do
    kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
       sqlSet "status" Timedout
       sqlWhereDocumentIDIs did
       sqlWhereDocumentTypeIs Signable
       sqlWhereDocumentStatusIs Pending
    _ <- update $ InsertEvidenceEvent
        TimeoutDocumentEvidence
        (return ())
        actor
        did
    updateMTimeAndObjectVersion (actorTime actor)
    return ()

data ProlongDocument = ProlongDocument Int (Maybe TimeZoneName) Actor
instance (DocumentMonad m, MonadBaseControl IO m, TemplatesMonad m) => DBUpdate m ProlongDocument () where
  update (ProlongDocument days mtzn actor) = updateDocumentWithID $ \did -> do
    -- Whole TimeZome behaviour is a clone of what is happending with making document ready for signing.
    let time = actorTime actor
    let timestamp = case mtzn of
                  Just tzn -> formatTime defaultTimeLocale "%F" (toUTCTime time) ++ " " ++ TimeZoneName.toString tzn
                  Nothing  -> formatTime defaultTimeLocale "%F %T %Z" (toUTCTime time)
    dstTz <- mkTimeZoneName "Europe/Stockholm"
    withTimeZone dstTz $ kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
       sqlSet "status" Pending
       sqlSet "mtime" time
       sqlSetCmd "timeout_time" $ "cast (" <?> timestamp <+> "as timestamp with time zone)"
                            <+> "+ (interval '1 day') * " <?> (show days) <+> " + (interval '23 hours 59 minutes 59 seconds')"
       sqlWhereDocumentIDIs did
       sqlWhereDocumentTypeIs Signable
       sqlWhereDocumentStatusIs Timedout
    _ <- update $ InsertEvidenceEvent
        ProlongDocumentEvidence
        (return ())
        actor
        did
    return ()


data SetDocumentAPICallbackURL = SetDocumentAPICallbackURL (Maybe String)
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetDocumentAPICallbackURL Bool where
  update (SetDocumentAPICallbackURL mac) = updateDocumentWithID $ \did -> do
    kRun01 $ sqlUpdate "documents" $ do
               sqlSet "api_callback_url" mac
               sqlWhereEq "id" did


data AddSignatoryLinkVisitedEvidence = AddSignatoryLinkVisitedEvidence Actor DocumentID
instance (MonadDB m, TemplatesMonad m) => DBUpdate m AddSignatoryLinkVisitedEvidence () where
   update (AddSignatoryLinkVisitedEvidence actor did) = do
        _ <-update $ InsertEvidenceEvent
          SignatoryLinkVisited
          (return ())
          actor
          did
        return ()

data PostReminderSend = PostReminderSend SignatoryLink (Maybe String) Bool Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m PostReminderSend () where
   update (PostReminderSend sl mmsg automatic actor) = updateDocument $ \doc -> do
     let docid = documentid doc
     kRun1OrThrowWhyNot $ sqlUpdate "signatory_links" $ do
       sqlFrom "documents"
       sqlSet "read_invitation" SqlNull
       sqlSet "mail_invitation_delivery_status" Unknown
       sqlSet "sms_invitation_delivery_status" Unknown
       sqlWhere "documents.id = signatory_links.document_id"

       sqlWhereDocumentIDIs docid
       sqlWhereSignatoryLinkIDIs (signatorylinkid sl)
       sqlWhereSignatoryHasNotSigned
       sqlWhereDocumentStatusIs Pending

     _ <- update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
          (if automatic then AutomaticReminderSent else ReminderSend)
          (F.value "author" $ getIdentifier $ $(fromJust) $ getAuthorSigLink doc)
          (Just sl)
          mmsg
          actor
          docid
     updateMTimeAndObjectVersion (actorTime actor)
     return ()

data UpdateFieldsForSigning = UpdateFieldsForSigning SignatoryLinkID [(FieldType, String)] Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m UpdateFieldsForSigning () where
  update (UpdateFieldsForSigning slid fields _actor) = updateDocumentWithID $ const $ do
    -- Document has to be in Pending state
    -- signatory could not have signed already
    let updateValue (fieldtype, fvalue) = do
          let custom_name = case fieldtype of
                              CustomFT xname _ -> xname
                              CheckboxFT xname -> xname
                              SignatureFT xname -> xname
                              _ -> ""
          kRun $ sqlUpdate "signatory_link_fields" $ do
                   sqlSet "value" fvalue
                   sqlWhereEq "signatory_link_id" slid
                   sqlWhereEq "custom_name" custom_name
                   sqlWhereEq "type" fieldtype
                   sqlWhereAny $ do
                       sqlWhereAll $ do
                         sqlWhereEq "value" (""::String)
                         sqlWhereIn "type" [CustomFT undefined undefined, FirstNameFT,LastNameFT,EmailFT,CompanyFT,PersonalNumberFT,PersonalNumberFT,CompanyNumberFT, MobileFT]
                       sqlWhereIn "type" [CheckboxFT undefined,SignatureFT undefined]
                   sqlWhereExists $ sqlSelect "documents" $ do
                     sqlWhere "signatory_links.id = signatory_link_id"
                     sqlLeftJoinOn "signatory_links" "documents.id = signatory_links.document_id"
                     sqlWhereEq "documents.status" Pending
                     sqlWhere "signatory_links.sign_time IS NULL"

    forM_ fields updateValue

data AddDocumentAttachment = AddDocumentAttachment FileID Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m AddDocumentAttachment Bool where
  update (AddDocumentAttachment fid _actor) = updateDocumentWithID $ \did -> do
    kRun01 $ sqlInsertSelect "author_attachments" "" $ do
        sqlSet "document_id" did
        sqlSet "file_id" fid
        sqlWhereExists $ sqlSelect "documents" $ do
          sqlWhereEq "id" did
          sqlWhereEq "status" Preparation

data RemoveDocumentAttachment = RemoveDocumentAttachment FileID Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m RemoveDocumentAttachment Bool where
  update (RemoveDocumentAttachment fid _actor) = updateDocumentWithID $ \did -> do
    kRun01 $ "DELETE FROM author_attachments WHERE document_id =" <?> did <+> "AND file_id =" <?> fid <+> "AND EXISTS (SELECT 1 FROM documents WHERE id = author_attachments.document_id AND status = " <?> Preparation <+> ")"

-- Remove unsaved drafts (older than 1 week) from db.
-- Uses chunking to not overload db when there's a lot of old drafts
data RemoveOldDrafts = RemoveOldDrafts Int
instance MonadDB m => DBUpdate m RemoveOldDrafts Integer where
    update (RemoveOldDrafts limit) = kRun $
      "DELETE FROM documents" <+>
      "WHERE id = any (array(SELECT id" <+>
                            "FROM documents" <+>
                            "WHERE unsaved_draft IS TRUE" <+>
                              "AND type = " <?> Signable <+>
                              "AND status = " <?> Preparation <+>
                              "AND mtime < (now() - '7 days'::interval)" <+>
                            "LIMIT " <?> limit <+>
                           ")" <+>
                     ")"

data SetSigAttachments = SetSigAttachments SignatoryLinkID [SignatoryAttachment] Actor
instance (DocumentMonad m) => DBUpdate m SetSigAttachments () where
  update (SetSigAttachments slid sigatts _actor) = updateDocumentWithID $ const $ do
    _ <-doDeleteAll
    forM_ sigatts doInsertOne
    where
     doDeleteAll = kRun $ SQL "DELETE FROM signatory_attachments WHERE signatory_link_id = ?" [toSql slid]
     doInsertOne SignatoryAttachment{..} = do
        kRun $ sqlInsert "signatory_attachments" $ do
            sqlSet "file_id" signatoryattachmentfile
            sqlSet "name" signatoryattachmentname
            sqlSet "description" signatoryattachmentdescription
            sqlSet "signatory_link_id" slid

data UpdateDraft = UpdateDraft Document Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m UpdateDraft Bool where
  update (UpdateDraft document actor) = updateDocument $ const $ and `liftM` sequence [
      update $ SetDocumentTitle (documenttitle document) actor
    , update $ SetDaysToSign (documentdaystosign document) actor
    , update $ SetDaysToRemind (documentdaystoremind document) actor
    , update $ SetDocumentLang (getLang document) actor
    , update $ SetInviteText (documentinvitetext document) actor
    , update $ SetDocumentTags (documenttags document) actor
    , update $ SetDocumentAPICallbackURL (documentapicallbackurl document)
    , updateMTimeAndObjectVersion (actorTime actor) >> return True
    ]

data GetDocsSentBetween = GetDocsSentBetween UserID MinutesTime MinutesTime
instance MonadDB m => DBQuery m GetDocsSentBetween Int where
  query (GetDocsSentBetween uid start end) = do
    kRun_ $ SQL ("SELECT count(documents.id) " <>
               "FROM documents " <>
               "JOIN signatory_links ON documents.id = signatory_links.document_id " <>
               "WHERE signatory_links.user_id = ? " <>
               "AND is_author " <>
               "AND documents.invite_time >= ? " <>
               "AND documents.invite_time <  ? " <>
               "AND documents.type = ? "   <>
               "AND documents.status <> ? ")
            [toSql uid, toSql start, toSql end, toSql $ Signable, toSql Preparation]
    kFold (+) 0

data GetDocsSent = GetDocsSent UserID
instance MonadDB m => DBQuery m GetDocsSent Int where
  query (GetDocsSent uid) = do
    kRun_ $ SQL ("SELECT count(documents.id) " <>
               "FROM documents " <>
               "JOIN signatory_links ON documents.id = signatory_links.document_id " <>
               "WHERE signatory_links.user_id = ? " <>
               "AND is_author " <>
               "AND documents.type = ? "   <>
               "AND documents.status <> ? ")
            [toSql uid, toSql $ Signable, toSql Preparation]
    kFold (+) 0

-- | Get the signatories that belong to this email that were viewed or signed
--   since time
data GetSignatoriesByEmail = GetSignatoriesByEmail Email MinutesTime
instance MonadDB m => DBQuery m GetSignatoriesByEmail [(DocumentID, SignatoryLinkID)] where
  query (GetSignatoriesByEmail email time) = do
    kRun_ $ "SELECT DISTINCT signatory_links.document_id, signatory_links.id " <+>
            "FROM signatory_links " <+>
            "JOIN signatory_link_fields ON (signatory_link_fields.signatory_link_id = signatory_links.id " <+>
            "                           AND signatory_link_fields.type = " <?> EmailFT <+>
            "                           AND signatory_link_fields.value = " <?> email <+>
            "                              )" <+>
            "WHERE sign_time > " <?> time <+>
            "   OR seen_time > " <?> time
    kFold f []
    where f acc did slid = (did, slid) : acc


data CheckDocumentObjectVersionIs = CheckDocumentObjectVersionIs DocumentID Int
instance MonadDB m => DBQuery m CheckDocumentObjectVersionIs () where
  query (CheckDocumentObjectVersionIs did ov) = do
    _ <- kRunAndFetch1OrThrowWhyNot (\acc (v::Bool) -> v : acc) $ sqlSelect "documents" $ do
       sqlResult "TRUE"
       sqlWhereDocumentObjectVersionIs did ov
    return ()

data PurgeDocuments = PurgeDocuments Int Int
instance MonadDB m => DBUpdate m PurgeDocuments Int where
  update (PurgeDocuments savedDocumentLingerDays unsavedDocumentLingerDays) = do

    kRun_ $ ("CREATE TEMP TABLE documents_to_purge(id, title) AS" :: SQL)
        <+> "SELECT documents.id, documents.title"
        <+> "  FROM documents"
        -- document wasn't purged yet
        <+> " WHERE documents.purged_time IS NULL"
        -- has not been deleted at least in a single account
        <+> "   AND NOT EXISTS(SELECT TRUE"
        <+> "                    FROM signatory_links"
        <+> "                   WHERE signatory_links.document_id = documents.id"
        <+> "                     AND signatory_links.user_id IS NOT NULL"
                                  -- not deleted or linger time hasn't elapsed
        <+> "                     AND (signatory_links.deleted IS NULL OR"
        <+> "                          signatory_links.deleted + (" <?> (show savedDocumentLingerDays ++ "days") <+> " :: INTERVAL) > now()))"

        -- is not saved but time to save the document went by
        <+> "   AND NOT EXISTS(SELECT TRUE"
        <+> "                    FROM signatory_links"
        <+> "                   WHERE signatory_links.document_id = documents.id"
        <+> "                     AND signatory_links.user_id IS NULL"
                                  -- linger time hasn't elapsed yet
        <+> "                     AND documents.mtime + (" <?> (show unsavedDocumentLingerDays ++ "days") <+> " :: INTERVAL) > now())"

    -- set purged time on documents
    rows <- kRun $ ("" :: SQL)
        <+> "UPDATE documents"
        <+> "   SET purged_time = now()"
        <+> " WHERE documents.id IN (SELECT id"
        <+> "                          FROM documents_to_purge)"

    -- blank out sensitive data
    kRun_ $ ("UPDATE signatory_links" :: SQL)
        <+> "   SET sign_ip = 0"
        <+> "     , seen_ip = 0"
        <+> "     , eleg_data_mismatch_first_name = ''"
        <+> "     , eleg_data_mismatch_last_name = ''"
        <+> "     , eleg_data_mismatch_personal_number = ''"
        <+> " WHERE signatory_links.document_id IN (SELECT id FROM documents_to_purge)"

    -- blank out sensitive data in fields
    kRun_ $ ("UPDATE signatory_link_fields" :: SQL)
        <+> "   SET value = ''"
        <+> " WHERE signatory_link_fields.signatory_link_id IN"
        <+> "       (SELECT id"
        <+> "          FROM signatory_links"
        <+> "         WHERE signatory_links.document_id IN (SELECT id FROM documents_to_purge))"

    -- remove whole evidence log as it is sensitive data
    kRun_ $ ("DELETE" :: SQL)
        <+> "  FROM evidence_log"
        <+> " WHERE document_id IN (SELECT id FROM documents_to_purge)"

    kRun_ $ ("DROP TABLE documents_to_purge" :: SQL)
    return (fromIntegral rows)


-- Update utilities
updateWithoutEvidence :: (DocumentMonad m, Convertible a SqlValue) => SQL -> a -> m Bool
updateWithoutEvidence col newValue = updateDocumentWithID $ \did -> do
  kRun01 $ "UPDATE" <+> raw (tblName tableDocuments) <+> "SET" <+> (col <+> "=" <?> newValue <+> "WHERE id =" <?> did)

updateMTimeAndObjectVersion :: DocumentMonad m  => MinutesTime -> m ()
updateMTimeAndObjectVersion mtime = updateDocumentWithID $ \did -> do
  kRun_ $ sqlUpdate "documents" $ do
       sqlSetInc "object_version"
       sqlSet "mtime" mtime
       sqlWhereEq "id" did

instance MonadDB m => GetRow Document m where
  getRow did = dbQuery $ GetDocumentByDocumentID did
