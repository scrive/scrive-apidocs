module Doc.Types.Document (
    DocumentType(..)
  , DocumentSharing(..)
  , StatusClass(..)
  , Document(..)
  , defaultDocument
  , documentsSelectors
  , documentStatusClassExpression
  , documentfile
  , documentsealedfile
  , documentsealstatus
  , getSealingMethodForDocument
  , getForceHidePersonalNumbers
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Data.Aeson.Types hiding ((<?>))
import Data.Int
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Model.CompositeType
import Database.PostgreSQL.PQTypes.SQL.Builder
import qualified Data.Set as S

import DB (dbQuery)
import DB.RowCache (HasID(..), ID)
import DB.TimeZoneName
import Doc.DocumentID
import Doc.SealStatus (SealStatus)
import Doc.Tables
import Doc.Types.AuthorAttachment
import Doc.Types.DocumentStatus
import Doc.Types.DocumentTag
import Doc.Types.MainFile
import Doc.Types.SignatoryLink
import Folder.Types
import IPAddress
import Log.Identifier
import MagicHash
import MinutesTime
import SealingMethod
import User.Lang
import UserGroup.Model (UserGroupGetWithParents(..))
import UserGroup.Types
import Util.MonadUtils

data DocumentType = Signable | Template
  deriving (Eq, Ord, Show, Read)

instance PQFormat DocumentType where
  pqFormat = pqFormat @Int16

instance FromSQL DocumentType where
  type PQBase DocumentType = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return Signable
      2 -> return Template
      _ -> throwM RangeError { reRange = [(1, 2)], reValue = n }

instance ToSQL DocumentType where
  type PQDest DocumentType = PQDest Int16
  toSQL Signable = toSQL (1 :: Int16)
  toSQL Template = toSQL (2 :: Int16)

---------------------------------

data DocumentSharing
  = Private
  -- | The document is shared with subaccounts, and those with same parent accounts
  | Shared
    deriving (Eq, Ord, Show)

instance PQFormat DocumentSharing where
  pqFormat = pqFormat @Int16

instance FromSQL DocumentSharing where
  type PQBase DocumentSharing = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return Private
      2 -> return Shared
      _ -> throwM RangeError { reRange = [(1, 2)], reValue = n }

instance ToSQL DocumentSharing where
  type PQDest DocumentSharing = PQDest Int16
  toSQL Private = toSQL (1 :: Int16)
  toSQL Shared  = toSQL (2 :: Int16)

---------------------------------

data StatusClass
  = SCDraft
  | SCCancelled
  | SCRejected
  | SCTimedout
  | SCError
  | SCDeliveryProblem -- ^ Order is important for SQLs
  | SCSent
  | SCDelivered
  | SCRead
  | SCOpened
  | SCSigned
  | SCProlonged
  | SCSealed -- ^ Has a digital seal
  | SCExtended -- ^ Has an extended digital seal
  | SCInitiated
  | SCOpenedAuthToView
  | SCAuthenticatedToView
  | SCApproved
    deriving (Eq, Ord, Enum, Bounded)

instance PQFormat StatusClass where
  pqFormat = pqFormat @Int16

instance FromSQL StatusClass where
  type PQBase StatusClass = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1  -> return SCDraft
      2  -> return SCCancelled
      3  -> return SCRejected
      4  -> return SCTimedout
      5  -> return SCError
      6  -> return SCDeliveryProblem
      7  -> return SCSent
      8  -> return SCDelivered
      9  -> return SCRead
      10 -> return SCOpened
      11 -> return SCSigned
      12 -> return SCProlonged
      13 -> return SCSealed
      14 -> return SCExtended
      15 -> return SCInitiated
      16 -> return SCOpenedAuthToView
      17 -> return SCAuthenticatedToView
      18 -> return SCApproved
      _  -> throwM RangeError { reRange = [(1, 18)], reValue = n }

instance ToSQL StatusClass where
  type PQDest StatusClass = PQDest Int16
  toSQL SCDraft               = toSQL (1 :: Int16)
  toSQL SCCancelled           = toSQL (2 :: Int16)
  toSQL SCRejected            = toSQL (3 :: Int16)
  toSQL SCTimedout            = toSQL (4 :: Int16)
  toSQL SCError               = toSQL (5 :: Int16)
  toSQL SCDeliveryProblem     = toSQL (6 :: Int16)
  toSQL SCSent                = toSQL (7 :: Int16)
  toSQL SCDelivered           = toSQL (8 :: Int16)
  toSQL SCRead                = toSQL (9 :: Int16)
  toSQL SCOpened              = toSQL (10 :: Int16)
  toSQL SCSigned              = toSQL (11 :: Int16)
  toSQL SCProlonged           = toSQL (12 :: Int16)
  toSQL SCSealed              = toSQL (13 :: Int16)
  toSQL SCExtended            = toSQL (14 :: Int16)
  toSQL SCInitiated           = toSQL (15 :: Int16)
  toSQL SCOpenedAuthToView    = toSQL (16 :: Int16)
  toSQL SCAuthenticatedToView = toSQL (17 :: Int16)
  toSQL SCApproved            = toSQL (18 :: Int16)

instance Show StatusClass where
  show SCInitiated           = "initiated"
  show SCDraft               = "draft"
  show SCCancelled           = "cancelled"
  show SCRejected            = "rejected"
  show SCTimedout            = "timeouted"
  show SCError               = "problem"
  show SCDeliveryProblem     = "deliveryproblem"
  show SCSent                = "sent"
  show SCDelivered           = "delivered"
  show SCRead                = "read"
  show SCOpened              = "opened"
  show SCSigned              = "signed"
  show SCProlonged           = "prolonged"
  show SCSealed              = "sealed"
  show SCExtended            = "extended"
  show SCOpenedAuthToView    = "authenticationview"
  show SCAuthenticatedToView = "authenticated"
  show SCApproved            = "approved"

instance Read StatusClass where
  readsPrec _ str =
    [ (v, drop (length (show v)) str)
    | v <- [minBound .. maxBound]
    , show v `isPrefixOf` str
    ]

---------------------------------

data Document = Document
  { documentid                     :: !DocumentID
  , documenttitle                  :: !Text
  , documentsignatorylinks         :: ![SignatoryLink]
  -- | Order: most recently added files first (FIXME: encode this in the type)
  , documentmainfiles              :: ![MainFile]
  , documentstatus                 :: !DocumentStatus
  , documenttype                   :: !DocumentType
  , documentctime                  :: !UTCTime
  , documentmtime                  :: !UTCTime
  , documentdaystosign             :: !Int32
  , documentdaystoremind           :: !(Maybe Int32)
  , documenttimeouttime            :: !(Maybe UTCTime)
  , documentautoremindtime         :: !(Maybe UTCTime)
  , documentinvitetime             :: !(Maybe SignInfo)
  , documentinvitetext             :: !Text -- TODO Should be changed to Maybe
  , documentconfirmtext            :: !Text -- TODO Should be changed to Maybe
  , documentsmsinvitetext          :: !(Maybe Text)
  , documentsmsconfirmtext         :: !(Maybe Text)
  , documentshowheader             :: !Bool
  , documentshowpdfdownload        :: !Bool
  , documentshowrejectoption       :: !Bool
  , documentallowrejectreason      :: !Bool
  , documentshowfooter             :: !Bool
  , documentisreceipt              :: !Bool
  , documentsharing                :: !DocumentSharing
  , documenttags                   :: !(S.Set DocumentTag)
  , documentauthorattachments      :: ![AuthorAttachment]
  , documentlang                   :: !Lang
  , documentstatusclass            :: !StatusClass
  , documentapiv1callbackurl       :: !(Maybe Text)
  , documentapiv2callbackurl       :: !(Maybe Text)
  , documentunsaveddraft           :: !Bool
  , documentobjectversion          :: !Int64
  , documentmagichash              :: !MagicHash
  , documentauthorugid             :: !(Maybe UserGroupID)
  , documenttimezonename           :: !TimeZoneName
  , documentshareablelinkhash      :: !(Maybe MagicHash)
  , documenttemplateid             :: !(Maybe DocumentID)
  , documentfromshareablelink      :: !Bool
  , documentshowarrow              :: !Bool
  -- | Folder, where the document belongs. The folder facilitates access to the
  -- document.
  , documentfolderid               :: !FolderID
  -- | When set, use the EID settings of the specified user group (rather than
  -- that of the author) for EID transactions related to the document. This
  -- mainly affects the 'display name', i.e. the name presented to the user as
  -- the party they are entering into a transaction with. Implements CORE-1633.
  , documentusergroupforeid        :: !(Maybe UserGroupID)
  , documentsealingmethod          :: !SealingMethod
  } deriving (Show)

type instance ID Document = DocumentID

instance Loggable Document where
  logValue Document {..} = object
    [identifier documentid, "title" .= documenttitle, "status" .= show documentstatus]
  logDefaultLabel _ = "document"

defaultDocument :: Document
defaultDocument = Document { documentid                = unsafeDocumentID 0
                           , documenttitle             = ""
                           , documentsignatorylinks    = []
                           , documentmainfiles         = []
                           , documentstatus            = Preparation
                           , documenttype              = Signable
                           , documentctime             = unixEpoch
                           , documentmtime             = unixEpoch
                           , documentdaystosign        = 90
                           , documentdaystoremind      = Nothing
                           , documenttimeouttime       = Nothing
                           , documentautoremindtime    = Nothing
                           , documentshowheader        = True
                           , documentshowpdfdownload   = True
                           , documentshowrejectoption  = True
                           , documentallowrejectreason = True
                           , documentshowfooter        = True
                           , documentisreceipt         = False
                           , documentinvitetext        = ""
                           , documentconfirmtext       = ""
                           , documentsmsinvitetext     = Nothing
                           , documentsmsconfirmtext    = Nothing
                           , documentinvitetime        = Nothing
                           , documentsharing           = Private
                           , documenttags              = S.empty
                           , documentauthorattachments = []
                           , documentlang              = defaultLang
                           , documentstatusclass       = SCDraft
                           , documentapiv1callbackurl  = Nothing
                           , documentapiv2callbackurl  = Nothing
                           , documentunsaveddraft      = False
                           , documentobjectversion     = 0
                           , documentmagichash         = unsafeMagicHash 0
                           , documentauthorugid        = Nothing
                           , documenttimezonename      = defaultTimeZoneName
                           , documentshareablelinkhash = Nothing
                           , documenttemplateid        = Nothing
                           , documentfromshareablelink = False
                           , documentshowarrow         = True
                           , documentfolderid          = unsafeFolderID 0
                           , documentusergroupforeid   = Nothing
                           , documentsealingmethod     = Guardtime
                           }

instance HasID Document where
  getID = documentid

instance HasLang Document where
  getLang = documentlang

---------------------------------

documentsSelectors :: [SQL]
documentsSelectors =
  [ "documents.id"
  , "documents.title"
  , "ARRAY(SELECT ("
    <>  mintercalate ", " signatoryLinksSelectors
    <>  ")::"
    <>  raw (ctName ctSignatoryLink)
    <+> "FROM signatory_links WHERE documents.id = signatory_links.document_id ORDER BY signatory_links.id)"
  , "ARRAY(SELECT ("
    <>  mintercalate ", " mainFilesSelectors
    <>  ")::"
    <>  raw (ctName ctMainFile)
    <+> "FROM main_files, files WHERE documents.id = main_files.document_id AND main_files.file_id = files.id ORDER BY main_files.id DESC)"
  , "documents.status"
  , "documents.type"
  , "documents.ctime"
  , "documents.mtime"
  , "documents.days_to_sign"
  , "documents.days_to_remind"
  , "documents.timeout_time"
  , "(SELECT dar.expires FROM document_automatic_reminders dar WHERE dar.document_id = documents.id)"
  , "documents.invite_time"
  , "documents.invite_ip"
  , "documents.invite_text"
  , "documents.confirm_text"
  , "documents.sms_invite_text"
  , "documents.sms_confirm_text"
  , "documents.show_header"
  , "documents.show_pdf_download"
  , "documents.show_reject_option"
  , "documents.allow_reject_reason"
  , "documents.show_footer"
  , "documents.is_receipt"
  , "documents.lang"
  , "documents.sharing"
  , "ARRAY(SELECT ("
    <>  mintercalate ", " documentTagsSelectors
    <>  ")::"
    <>  raw (ctName ctDocumentTag)
    <+> "FROM document_tags WHERE documents.id = document_tags.document_id ORDER BY document_tags.name)"
  -- needs ROW since composite type has only one field for now
  , "ARRAY(SELECT ROW("
    <>  mintercalate ", " authorAttachmentsSelectors
    <>  ")::"
    <>  raw (ctName ctAuthorAttachment)
    <+> "FROM author_attachments WHERE documents.id = author_attachments.document_id ORDER BY author_attachments.file_id)"
  , "documents.api_v1_callback_url"
  , "documents.api_v2_callback_url"
  , "documents.unsaved_draft"
  , "documents.object_version"
  , "documents.token"
  , "documents.time_zone_name"
  , "(SELECT u.user_group_id FROM users u WHERE u.id = documents.author_user_id)"
  , documentStatusClassExpression
  , "documents.shareable_link_hash"
  , "documents.template_id"
  , "documents.from_shareable_link"
  , "documents.show_arrow"
  , "documents.folder_id"
  , "documents.user_group_to_impersonate_for_eid"
  , "documents.sealing_method"
  ]

documentStatusClassExpression :: SQL
documentStatusClassExpression = mconcat
  [ "(SELECT COALESCE((SELECT min(" <> statusClassCaseExpression <> ")"
  , "FROM signatory_links "
  , "WHERE signatory_links.document_id = documents.id "
  , "AND" <+> parenthesize
    (   "signatory_links.signatory_role ="
    <?> SignatoryRoleSigningParty
    <+> "OR"
    <+> "signatory_links.signatory_role ="
    <?> SignatoryRoleApprover
    )
  , "),"
  , "(SELECT " <> statusClassCaseExpressionForDocument <> ")," <?> SCDraft
  , "))::SMALLINT"
  ]
  where
    -- FIXME: Add to DB.SQL functionality that encodes CASE expression.
    statusClassCaseExpression = smconcat
      [ "(CASE"
      , "WHEN documents.status =" <?> DocumentError
      , "THEN" <?> SCError
      , "WHEN documents.status =" <?> Preparation
      , "THEN" <?> SCDraft
      , "WHEN signatory_links.sign_time IS NOT NULL"
      , "THEN" <?> SCSigned
      , "WHEN documents.status =" <?> Canceled
      , "THEN" <?> SCCancelled
      , "WHEN documents.status =" <?> Timedout
      , "THEN" <?> SCTimedout
      , "WHEN documents.status =" <?> Rejected
      , "THEN" <?> SCRejected
      , "WHEN signatory_links.seen_time IS NOT NULL"
      , "THEN" <?> SCOpened
      , "WHEN signatory_links.read_invitation IS NOT NULL"
      , "THEN" <?> SCRead
      , "WHEN signatory_links.mail_invitation_delivery_status =" <?> Undelivered
      , "THEN" <?> SCDeliveryProblem
      , "WHEN signatory_links.sms_invitation_delivery_status =" <?> Undelivered
      , "THEN" <?> SCDeliveryProblem
      , "WHEN signatory_links.mail_invitation_delivery_status =" <?> Delivered
      , "THEN" <?> SCDelivered
      , "WHEN signatory_links.sms_invitation_delivery_status =" <?> Delivered
      , "THEN" <?> SCDelivered
      , "ELSE" <?> SCSent
      , "END :: SMALLINT)"
      ]
    statusClassCaseExpressionForDocument = smconcat
      [ "(CASE"
      , "WHEN documents.status =" <?> DocumentError <+> "THEN" <?> SCError
      , "WHEN documents.status =" <?> Preparation <+> "THEN" <?> SCDraft
      , "WHEN documents.status =" <?> Canceled <+> "THEN" <?> SCCancelled
      , "WHEN documents.status =" <?> Timedout <+> "THEN" <?> SCTimedout
      , "WHEN documents.status =" <?> Rejected <+> "THEN" <?> SCRejected
      , "END :: INTEGER)"
      ]

type instance CompositeRow Document
  = ( DocumentID
    , Text
    , CompositeArray1 SignatoryLink
    , CompositeArray1 MainFile
    , DocumentStatus
    , DocumentType
    , UTCTime
    , UTCTime
    , Int32
    , Maybe Int32
    , Maybe UTCTime
    , Maybe UTCTime
    , Maybe UTCTime
    , Maybe IPAddress
    , Text
    , Text
    , Maybe Text
    , Maybe Text
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Lang
    , DocumentSharing
    , CompositeArray1 DocumentTag
    , CompositeArray1 AuthorAttachment
    , Maybe Text
    , Maybe Text
    , Bool
    , Int64
    , MagicHash
    , TimeZoneName
    , Maybe UserGroupID
    , StatusClass
    , Maybe MagicHash
    , Maybe DocumentID
    , Bool
    , Bool
    , FolderID
    , Maybe UserGroupID
    , SealingMethod
    )

instance PQFormat Document where
  pqFormat = compositeTypePqFormat ctDocument

instance CompositeFromSQL Document where
  toComposite (did, title, CompositeArray1 signatory_links, CompositeArray1 main_files, status, doc_type, ctime, mtime, days_to_sign, days_to_remind, timeout_time, auto_remind_time, invite_time, invite_ip, invite_text, confirm_text, sms_invite_text, sms_confirm_text, show_header, show_pdf_download, show_reject_option, allow_reject_reason, show_footer, is_receipt, lang, sharing, CompositeArray1 tags, CompositeArray1 author_attachments, apiv1callback, apiv2callback, unsaved_draft, objectversion, token, time_zone_name, author_ugid, status_class, shareable_link_hash, template_id, from_shareable_link, show_arrow, fid, user_group_to_impersonate_for_eid, sealing_method)
    = Document
      { documentid                = did
      , documenttitle             = title
      , documentsignatorylinks    = signatory_links
      , documentmainfiles         = main_files
      , documentstatus            = status
      , documenttype              = doc_type
      , documentctime             = ctime
      , documentmtime             = mtime
      , documentdaystosign        = days_to_sign
      , documentdaystoremind      = days_to_remind
      , documenttimeouttime       = timeout_time
      , documentautoremindtime    = case status of
                                      Pending -> auto_remind_time
                                      _       -> Nothing
      , documentinvitetime        = case invite_time of
                                      Nothing -> Nothing
                                      Just t -> Just (SignInfo t $ fromMaybe noIP invite_ip)
      , documentinvitetext        = invite_text
      , documentconfirmtext       = confirm_text
      , documentsmsinvitetext     = sms_invite_text
      , documentsmsconfirmtext    = sms_confirm_text
      , documentshowheader        = show_header
      , documentshowpdfdownload   = show_pdf_download
      , documentshowrejectoption  = show_reject_option
      , documentallowrejectreason = allow_reject_reason
      , documentshowfooter        = show_footer
      , documentisreceipt         = is_receipt
      , documentsharing           = sharing
      , documenttags              = S.fromList tags
      , documentauthorattachments = author_attachments
      , documentlang              = lang
      , documentstatusclass       = status_class
      , documentapiv1callbackurl  = apiv1callback
      , documentapiv2callbackurl  = apiv2callback
      , documentunsaveddraft      = unsaved_draft
      , documentobjectversion     = objectversion
      , documentmagichash         = token
      , documentauthorugid        = author_ugid
      , documenttimezonename      = time_zone_name
      , documentshareablelinkhash = shareable_link_hash
      , documenttemplateid        = template_id
      , documentfromshareablelink = from_shareable_link
      , documentshowarrow         = show_arrow
      , documentfolderid          = fid
      , documentusergroupforeid   = user_group_to_impersonate_for_eid
      , documentsealingmethod     = sealing_method
      }

---------------------------------

documentfile :: Document -> Maybe MainFile
documentfile = find ((Preparation ==) . mainfiledocumentstatus) . documentmainfiles

-- | Here we assume that the most recently sealed file is closest to the head of the list.
documentsealedfile :: Document -> Maybe MainFile
documentsealedfile = find ((Preparation /=) . mainfiledocumentstatus) . documentmainfiles

documentsealstatus :: Document -> Maybe SealStatus
documentsealstatus = fmap mainfilesealstatus . documentsealedfile

getSealingMethodForDocument
  :: (MonadBase IO m, MonadDB m, MonadThrow m) => Document -> m SealingMethod
getSealingMethodForDocument doc = do
  ugid <- guardJust $ documentauthorugid doc
  ugwp <- guardJustM . dbQuery $ UserGroupGetWithParents ugid
  return $ ugwpSettings ugwp ^. #sealingMethod

getForceHidePersonalNumbers
  :: (MonadBase IO m, MonadDB m, MonadThrow m) => Document -> m Bool
getForceHidePersonalNumbers doc = do
  ugid <- guardJust $ documentauthorugid doc
  ugwp <- guardJustM . dbQuery $ UserGroupGetWithParents ugid
  return $ ugwpSettings ugwp ^. #forceHidePN
