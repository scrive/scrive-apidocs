{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-orphans -fcontext-stack=50 -fno-warn-unused-do-bind #-}
{-# LANGUAGE CPP #-}

module Doc.Model
  ( module File.File
  , isTemplate -- fromUtils
  , isShared -- fromUtils
  , isDeletableDocument -- fromUtils
  , anyInvitationUndelivered
  , undeliveredSignatoryLinks

  , PutDocumentUnchecked(..)
  , AuthorAttachment(..)
  , DocumentID(..)
  , SignatoryAttachment(..)
  , SignatoryLinkID(..)
  , SignOrder(..)
  , CancelationReason(..)
  , DocumentFunctionality(..)
  , DocumentLogEntry(..)
  , DocumentProcess(..)
  , DocumentStatus(..)
  , DocumentSharing(..)
  , DocumentType(..)
  , FieldType(..)
  , IdentificationType(..)
  , SignatoryRole(..)
  , SignatureProvider(..)
  , CSVUpload(..)
  , DocumentTag(..)
  , DocumentUI(..)
  , FieldPlacement(..)
  , SignatoryDetails(..)
  , SignatoryField(..)
  , SignatureInfo(..)
  , SignInfo(..)
  , SignatoryLink(..)
  , Document(..)
  , DocStats(..)

  , ArchiveDocuments(..)
  , ArchiveDocumentForAll(..)
  , ArchiveDocumentForAuthor(..)
  , RestoreArchivedDocuments(..)
  , ReallyDeleteDocuments(..)
  , DeleteDocumentRecordIfRequired(..)
  , AttachFile(..)
  , AttachSealedFile(..)
  , ChangeMainfile(..)
  , RejectDocument(..)
  , GetDocumentByDocumentID(..)
  , GetDocumentStats(..)
  , GetDocumentStatsByUser(..)
  , GetDocuments(..)
  , GetDocumentsByAuthor(..)
  , GetDocumentsBySignatory(..)
  , GetDocumentsByCompany(..)
  , GetDocumentsSharedInCompany(..)
  , GetDocumentsByUser(..)
  , GetDeletedDocumentsByCompany(..)
  , GetDeletedDocumentsByUser(..)
  , GetNumberOfDocumentsOfUser(..)
  , GetTimeoutedButPendingDocuments(..)
  , MarkDocumentSeen(..)
  , MarkInvitationRead(..)
  , SetInvitationDeliveryStatus(..)
  , NewDocument(..)
  , SaveDocumentForUser(..)
  , SetDocumentTimeoutTime(..)
  , SetDocumentTags(..)
  , SetDocumentUI(..)
  , GetDocumentsByCompanyAndTags(..)
  , ShareDocument(..)
  , SetDocumentTitle(..)
  , SignDocument(..)
  , TimeoutDocument(..)
  , UpdateDocumentSimple(..)
  , AttachCSVUpload(..)
  , AddDocumentAttachment(..)
  , RemoveDocumentAttachment(..)
  , CloseDocument(..)
  , CancelDocument(..)
  , RestartDocument(..)
  , ChangeSignatoryEmailWhenUndelivered(..)
  , GetUniqueSignatoryLinkID(..)
  , GetMagicHash(..)
  , ErrorDocument(..)
  , TemplateFromDocument(..)
  , SignableFromDocument(..)
  , SignableFromDocumentIDWithUpdatedAuthor(..)
  , DocumentFromSignatoryData(..)
  , UpdateSigAttachments(..)
  , SaveSigAttachment(..)
  , PreparationToPending(..)
  , AddInvitationEvidence(..)
  , UpdateFields(..)
  , PendingToAwaitingAuthor(..)
  , SetSignatoryCompany(..)
  , RemoveSignatoryCompany(..)
  , SetSignatoryUser(..)
  , RemoveSignatoryUser(..)
  , SetInviteText(..)
  , SetDaysToSign(..)
  , RemoveDaysToSign(..)
  , SetDocumentFunctionality(..)
  , SetCSVSigIndex(..)
  , SetEmailIdentification(..)
  , SetElegitimationIdentification(..)
  , ResetSignatoryDetails(..)
  , SetDocumentLocale(..)
  --, MigrateDocumentSigAccounts(..)
  , MigrateDocumentSigLinkCompanies(..)
  -- , FixBug510ForDocument(..)
  , StoreDocumentForTesting(..)
  , SignLinkFromDetailsForTest(..)
  , DeleteSigAttachment(..)
  , GetSignatoryLinkIDs(..)
  , AdminOnlySaveForUser(..)
  ) where

import API.Service.Model
import DB.Classes
--import DB.Derive
import DB.Types
import DB.Utils
import File.File
import File.FileID
--import User.Region
import Doc.DocUtils
import User.UserID
import User.Model
import Company.Model
import MinutesTime
import Doc.DocStateData
import Data.Data
import Database.HDBC
import Data.Word
import qualified Data.ByteString.Char8 as BS
import qualified Mails.MailsUtil as Mail
import Data.Maybe
import Misc
import Data.Convertible
import Data.List
import Doc.Tables
import Control.Applicative
--import Doc.DocStateUtils
import Doc.DocProcess
import Doc.DocStateCommon
import System.Random
--import Happstack.Server
--import Happstack.State
--import Happstack.Util.Common
--import Numeric
--import Data.Int
import Control.Monad.IO.Class
import Control.Monad
import qualified Control.Exception as E

data SqlField = SqlField String String SqlValue

instance Convertible SqlValue SqlValue where
    safeConvert = return . id

sqlField :: (Convertible v SqlValue) => String -> v -> SqlField
sqlField name value = SqlField name "" (toSql value)

sqlFieldType :: (Convertible v SqlValue) => String -> String -> v -> SqlField
sqlFieldType name xtype value = SqlField name xtype (toSql value)

-- here we can add encoding and better error reporting in case conversion fails
mkInsertStatement :: String -> [SqlField] -> String
mkInsertStatement tableName fields =
   "INSERT INTO " ++ tableName ++
   " (" ++ concat (intersperse "," (map name fields)) ++ ")" ++
   " VALUES (" ++ concat (intersperse "," (map xtype fields)) ++ ")"
   where
     name (SqlField x _ _) = x
     xtype (SqlField _ x _) = insertType x
     insertType "" = "?"
     insertType "timestamp" = "to_timestamp(?)"
     insertType "base64" = "decode(?, 'base64')"
     insertType ytype = error $ "mkInsertStatement: invalid insert type " ++ ytype
                           

runInsertStatement :: String -> [SqlField] -> DB Integer
runInsertStatement tableName fields = do
  wrapDB $ \conn -> run conn statement (map value fields)
  where
    value (SqlField _ _ v) = v
    statement = mkInsertStatement tableName fields

-- here we can add encoding and better error reporting in case conversion fails
mkUpdateStatement :: String -> [SqlField] -> String
mkUpdateStatement tableName fields =
   "UPDATE " ++ tableName ++
   " SET " ++ concat (intersperse "," (map one fields)) ++ " "
   where
     name (SqlField x _ _) = x
     xtype (SqlField _ x _) = insertType x
     insertType "" = "?"
     insertType "timestamp" = "to_timestamp(?)"
     insertType "base64" = "decode(?, 'base64')"
     insertType ytype = error $ "mkUpdateStatement: invalid insert type " ++ ytype
     one field = name field ++ "=" ++ xtype field
                           

runUpdateStatement :: String -> [SqlField] -> String -> [SqlValue] -> DB Integer
runUpdateStatement tableName fields whereClause whereValues = do
  wrapDB $ \conn -> run conn statement (map value fields ++ whereValues)
  where
    value (SqlField _ _ v) = v
    statement = mkUpdateStatement tableName fields ++ whereClause


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
      liftIO $ E.throwIO TooManyObjects { DB.Classes.originalQuery = ""
                                        , tmoExpected = 1
                                        , tmoGiven = fromIntegral r
                                        }


toDocumentSimpleType :: DocumentType -> Int
toDocumentSimpleType (Signable _) = 1
toDocumentSimpleType (Template _) = 2
toDocumentSimpleType (Attachment) = 3
toDocumentSimpleType (AttachmentTemplate) = 4

toDocumentProcess :: DocumentType -> Maybe DocumentProcess
toDocumentProcess (Signable p) = Just p
toDocumentProcess (Template p) = Just p
toDocumentProcess (Attachment) = Nothing
toDocumentProcess (AttachmentTemplate) = Nothing

unimplemented :: String -> a
unimplemented msg = error ("Unimplemented in Doc/Model: " ++ msg)


decodeRowAsDocument :: DocumentID
                    -> BS.ByteString
                    -> Maybe FileID
                    -> Maybe FileID
                    -> DocumentStatus
                    -> Int
                    -> Maybe DocumentProcess
                    -> DocumentFunctionality
                    -> MinutesTime
                    -> MinutesTime
                    -> Maybe Int
                    -> Maybe TimeoutTime
                    -> Maybe MinutesTime
                    -> Maybe Word32
                    -> [DocumentLogEntry]
                    -> BS.ByteString
                    -> [IdentificationType]
                    -> Maybe BS.ByteString
                    -> Maybe [[BS.ByteString]]
                    -> Maybe Int
                    -> Maybe CancelationReason
                    -> DocumentSharing
                    -> Maybe MinutesTime
                    -> Maybe BS.ByteString
                    -> Maybe SignatoryLinkID
                    -> [DocumentTag]
                    -> Maybe ServiceID
                    -> Bool
                    -> Region
                    -> Either DBException Document
decodeRowAsDocument did
                    title
                    file_id
                    sealed_file_id
                    status
                    simple_type
                    process
                    functionality
                    ctime
                    mtime
                    days_to_sign
                    timeout_time
                    invite_time
                    invite_ip
                    dlog
                    invite_text
                    allowed_id_types
                    csv_title
                    csv_contents
                    csv_signatory_index
                    cancelationreason
                    sharing
                    rejection_time
                    rejection_signatory_link_id
                    rejection_reason
                    tags
                    service
                    deleted
                    --authorattachments
                    --signatoryattachments
                    region = (Right $ Document { documentid = did
                                               , documenttitle = title
                                               , documentsignatorylinks = []
                                               , documentfiles = maybeToList file_id
                                               , documentsealedfiles = maybeToList sealed_file_id
                                               , documentstatus = status
                                               , documenttype = case (simple_type, process) of
                                                                  (1, Just p) -> Signable p
                                                                  (2, Just p) -> Template p
                                                                  (3, _) -> Attachment
                                                                  (4, _) -> AttachmentTemplate
                                                                  (_,_) -> error "Illegal simpletype"
                                               , documentfunctionality = functionality
                                               , documentctime = ctime
                                               , documentmtime = mtime
                                               , documentdaystosign = days_to_sign
                                               , documenttimeouttime = timeout_time
                                               , documentinvitetime = case invite_time of
                                                                        Nothing -> Nothing
                                                                        Just t -> Just (SignInfo t (maybe 0 id invite_ip))
                                               , documentlog = dlog
                                               , documentinvitetext = invite_text
                                               , documentallowedidtypes = allowed_id_types
                                               , documentcsvupload = case (csv_title, csv_contents, csv_signatory_index) of
                                                                       (Just t, Just c, Just si) -> Just (CSVUpload t c si)
                                                                       _ -> Nothing
                                               , documentcancelationreason = cancelationreason
                                               , documentsharing = sharing
                                               , documentrejectioninfo = case (rejection_time, rejection_reason, rejection_signatory_link_id) of
                                                                           (Just t, Just r, Just sl) -> Just (t, r, sl)
                                                                           _ -> Nothing
                                               , documenttags = tags
                                               , documentservice = service
                                               , documentdeleted = deleted
                                               , documentauthorattachments = []
                                               , documentsignatoryattachments = []
                                               , documentui = emptyDocumentUI
                                               , documentregion = region
                                               }) :: Either DBException Document

selectDocumentsSQL :: String
selectDocumentsSQL = "SELECT id" ++
                     ",title" ++
                     ",file_id" ++
                     ",sealed_file_id" ++
                     ",status" ++
                     ",type" ++
                     ",process" ++
                     ",functionality" ++
                     ",EXTRACT(EPOCH FROM ctime)" ++
                     ",EXTRACT(EPOCH FROM mtime)" ++
                     ",days_to_sign" ++
                     ",EXTRACT(EPOCH FROM timeout_time)" ++
                     ",EXTRACT(EPOCH FROM invite_time)" ++
                     ",invite_ip" ++
                     ",log" ++
                     ",invite_text" ++
                     ",allowed_id_types" ++
                     ",csv_title" ++
                     ",csv_contents" ++
                     ",csv_signatory_index" ++
                     ",cancelation_reason" ++
                     ",sharing" ++
                     ",EXTRACT(EPOCH FROM rejection_time)" ++
                     ",rejection_signatory_link_id" ++
                     ",rejection_reason" ++
                     ",tags" ++
                     ",service_id" ++
                     ",deleted" ++
                     --authorattachments
                     --signatoryattachments
                     --",ui" ++
                     ",region " ++
                     "FROM documents "

fetchDocuments :: Statement -> IO [Document]
fetchDocuments st = do
  fetchValues st decodeRowAsDocument


selectSignatoryLinksSQL :: String
selectSignatoryLinksSQL = "SELECT id" ++
                          ", document_id" ++
                          ", user_id" ++
                          ", company_id" ++
                          ", fields" ++
                          ", sign_order" ++
                          ", token" ++
                          ", sign_time" ++
                          ", sign_ip" ++
                          ", seen_time" ++
                          ", seen_ip" ++
                          ", read_invitation" ++
                          ", invitation_delivery_status" ++
                          ", signinfo_text" ++
                          ", signinfo_signature" ++
                          ", signinfo_certificate" ++
                          ", signinfo_provider" ++
                          ", signinfo_first_name_verified" ++
                          ", signinfo_last_name_verified" ++
                          ", signinfo_personal_number_verified" ++
                          ", roles" ++
                          ", deleted" ++
                          ", really_deleted" ++
                          " FROM signatory_links "


decodeRowAsSignatoryLink :: SignatoryLinkID
                         -> DocumentID
                         -> Maybe UserID
                         -> Maybe CompanyID
                         -> [SignatoryField]
                         -> SignOrder
                         -> MagicHash
                         -> Maybe MinutesTime
                         -> Maybe Word32
                         -> Maybe MinutesTime
                         -> Maybe Word32
                         -> Maybe MinutesTime
                         -> Mail.MailsDeliveryStatus
                         -> Maybe String
                         -> Maybe String
                         -> Maybe String
                         -> Maybe SignatureProvider
                         -> Maybe Bool
                         -> Maybe Bool
                         -> Maybe Bool
                         -> [SignatoryRole]
                         -> Bool
                         -> Bool
                         -> Either DBException SignatoryLink
decodeRowAsSignatoryLink slid
                         _document_id
                         user_id
                         company_id
                         fields
                         sign_order
                         token
                         sign_time
                         sign_ip
                         seen_time
                         seen_ip
                         read_invitation
                         invitation_delivery_status
                         signinfo_text
                         signinfo_signature
                         signinfo_certificate
                         signinfo_provider
                         signinfo_first_name_verified
                         signinfo_last_name_verified
                         signinfo_personal_number_verified
                         roles
                         deleted
                         really_deleted =
    (return $ SignatoryLink
    { signatorylinkid = slid
    , signatorydetails = SignatoryDetails 
                         { signatorysignorder = sign_order
                         , signatoryfields = fields
                         }
    , signatorymagichash = token       
    , maybesignatory     = user_id        
    , maybesupervisor    = Nothing        
    , maybecompany       = company_id        
    , maybesigninfo      = case (sign_time, sign_ip) of
                             (Just st, Just sip) -> Just (SignInfo st sip)
                             _ -> Nothing
    , maybeseeninfo      = case (seen_time, seen_ip) of
                             (Just st, Just sip) -> Just (SignInfo st sip)
                             _ -> Nothing        
    , maybereadinvite    = read_invitation        
    , invitationdeliverystatus = invitation_delivery_status  
    , signatorysignatureinfo = do -- Maybe Monad
        signinfo_text' <- signinfo_text
        signinfo_signature' <- signinfo_signature
        signinfo_certificate' <- signinfo_certificate
        signinfo_provider' <- signinfo_provider
        signinfo_first_name_verified' <- signinfo_first_name_verified
        signinfo_last_name_verified' <- signinfo_last_name_verified
        signinfo_personal_number_verified' <- signinfo_personal_number_verified
        return $ SignatureInfo { signatureinfotext        = signinfo_text'
                               , signatureinfosignature   = signinfo_signature'
                               , signatureinfocertificate = signinfo_certificate'
                               , signatureinfoprovider    = signinfo_provider'
                               , signaturefstnameverified = signinfo_first_name_verified'
                               , signaturelstnameverified = signinfo_last_name_verified'
                               , signaturepersnumverified = signinfo_personal_number_verified'
                               }
    
    , signatoryroles     = roles        
    , signatorylinkdeleted  = deleted     
    , signatorylinkreallydeleted = really_deleted 
    }) :: Either DBException SignatoryLink

fetchSignatoryLinks :: Statement -> IO [SignatoryLink]
fetchSignatoryLinks st = do
  fetchValues st decodeRowAsSignatoryLink

                              
data PutDocumentUnchecked = PutDocumentUnchecked Document
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate PutDocumentUnchecked Document where
  dbUpdate (PutDocumentUnchecked document) = do
    let Document { documentid
                 , documenttitle
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
                 , documentcsvupload
                 , documentcancelationreason
                 , documentsharing
                 , documentrejectioninfo
                 , documenttags
                 , documentservice
                 , documentdeleted
                 , documentauthorattachments
                 , documentsignatoryattachments
                 , documentui
                 , documentregion
                 } = document
        simpletype = toDocumentSimpleType documenttype
        process = toDocumentProcess documenttype

    rowsInserted <- runInsertStatement "documents"
                                     [ sqlField "id" documentid
                                     , sqlField "title" documenttitle
                                     , sqlField "tags" documenttags
                                     , sqlField "file_id" (listToMaybe documentfiles)
                                     , sqlField "sealed_file_id" (listToMaybe documentsealedfiles)
                                     , sqlField "status" documentstatus
                                     , sqlField "error_text" $ case documentstatus of
                                                                 DocumentError msg -> toSql msg
                                                                 _ -> SqlNull
                                     , sqlField "type" simpletype
                                     , sqlField "process" process

                                     , sqlField "functionality" documentfunctionality
                                     , sqlFieldType "ctime" "timestamp" $ toSeconds documentctime
                                     , sqlFieldType "mtime" "timestamp" $ toSeconds documentmtime
                                     , sqlField "days_to_sign" documentdaystosign
                                     , sqlFieldType "timeout_time" "timestamp" $ fmap (toSeconds . unTimeoutTime) documenttimeouttime
                                     , sqlFieldType "invite_time" "timestamp" $ fmap (toSeconds . signtime) documentinvitetime
                                     , sqlField "invite_ip" (fmap signipnumber documentinvitetime)
                                     , sqlField "invite_text" documentinvitetext
                                     , sqlField "log" documentlog
                                     , sqlField "allowed_id_types" documentallowedidtypes
                                     , sqlField "csv_title" $ csvtitle `fmap` documentcsvupload
                                     , sqlField "csv_contents" $ csvcontents `fmap` documentcsvupload
                                     , sqlField "csv_signatory_index" $ csvsignatoryindex `fmap` documentcsvupload
                                     , sqlField "cancelation_reason" documentcancelationreason
                                     , sqlField "sharing" documentsharing
                                     , sqlFieldType "rejection_time" "timestamp" $ fmap toSeconds $ fst3 `fmap` documentrejectioninfo
                                     , sqlField "rejection_signatory_link_id" $ snd3 `fmap` documentrejectioninfo
                                     , sqlField "rejection_reason" $ thd3 `fmap` documentrejectioninfo
                                     , sqlField "service_id" documentservice
                                     , sqlField "deleted" documentdeleted
                                     -- , toSql documentauthorattachments      -- many to many
                                     -- , toSql documentsignatoryattachments   -- many to many
                                     -- , toSql documentui  -- should go into separate table?
                                     , sqlField "region" documentregion
                                     ]
    when (rowsInserted /= 1) $
         error "PutDocumentUnchecked did not manage to insert a row"
    Just newdocument <- dbQuery $ GetDocumentByDocumentID documentid
    when (document /= newdocument) $
         error "GetDocumentByDocumentID . PutDocumentUnchecked is not identity"
    return newdocument

data AdminOnlySaveForUser = AdminOnlySaveForUser DocumentID User
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AdminOnlySaveForUser (Either String Document) where
  dbUpdate (AdminOnlySaveForUser docid user) = wrapDB $ \conn -> do
    unimplemented "AdminOnlySaveForUser"

data ArchiveDocumentForAll = ArchiveDocumentForAll DocumentID
                             deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ArchiveDocumentForAll (Either String Document) where
  dbUpdate (ArchiveDocumentForAll docid) = wrapDB $ \conn -> do
    unimplemented "ArchiveDocumentForAll"

data ArchiveDocumentForAuthor = ArchiveDocumentForAuthor DocumentID
                             deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ArchiveDocumentForAuthor (Either String Document) where
  dbUpdate (ArchiveDocumentForAuthor docid) = wrapDB $ \conn -> do
    unimplemented "ArchiveDocumentForAuthor"

data ArchiveDocuments = ArchiveDocuments User [DocumentID] deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ArchiveDocuments (Either String [Document]) where
  dbUpdate (ArchiveDocuments user docids) = wrapDB $ \conn -> do
    unimplemented "ArchiveDocuments"

data AttachCSVUpload = AttachCSVUpload DocumentID CSVUpload
                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AttachCSVUpload (Either String Document) where
  dbUpdate (AttachCSVUpload docid csvupload) = wrapDB $ \conn -> do
    unimplemented "AttachCSVUpload"

data AttachFile = AttachFile DocumentID FileID MinutesTime
                  deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AttachFile (Either String Document) where
  dbUpdate (AttachFile docid fid time) = wrapDB $ \conn -> do
    unimplemented "AttachFile"

data AttachSealedFile = AttachSealedFile DocumentID FileID
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AttachSealedFile (Either String Document) where
  dbUpdate (AttachSealedFile docid fid) = wrapDB $ \conn -> do
    unimplemented "AttachSealedFile"

data AuthorSendDocument = AuthorSendDocument DocumentID MinutesTime Word32 (Maybe SignatureInfo)
                          deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AuthorSendDocument (Either String Document) where
  dbUpdate (AuthorSendDocument docid mtime ipaddress msiginfo) = wrapDB $ \conn -> do
    unimplemented "AuthorSendDocument"

data AuthorSignDocument = AuthorSignDocument DocumentID MinutesTime Word32 (Maybe SignatureInfo)
                          deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AuthorSignDocument (Either String Document) where
  dbUpdate (AuthorSignDocument docid mtime ipaddress msiginfo) = wrapDB $ \conn -> do
    unimplemented "AuthorSignDocument"

data CancelDocument = CancelDocument DocumentID CancelationReason MinutesTime Word32
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate CancelDocument (Either String Document) where
  dbUpdate (CancelDocument docid reason mtime ipaddress) = wrapDB $ \conn -> do
    unimplemented "CancelDocument"

data ChangeMainfile = ChangeMainfile DocumentID FileID
instance DBUpdate ChangeMainfile (Either String Document) where
  dbUpdate (ChangeMainfile did fid) = wrapDB $ \conn -> do
    unimplemented "ChangeMainfile"

data ChangeSignatoryEmailWhenUndelivered = ChangeSignatoryEmailWhenUndelivered DocumentID SignatoryLinkID (Maybe User) BS.ByteString
                                           deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ChangeSignatoryEmailWhenUndelivered (Either String Document) where
  dbUpdate (ChangeSignatoryEmailWhenUndelivered docid siglinkid muser email) = wrapDB $ \conn -> do
    unimplemented "ChangeSignatoryEmailWhenUndelivered"


data PreparationToPending = PreparationToPending DocumentID MinutesTime
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate PreparationToPending (Either String Document) where
  dbUpdate (PreparationToPending docid time) = wrapDB $ \conn -> do
    unimplemented "PreparationToPending"

data CloseDocument = CloseDocument DocumentID MinutesTime Word32
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate CloseDocument (Either String Document) where
  dbUpdate (CloseDocument docid time ipaddress) = wrapDB $ \conn -> do
    unimplemented "CloseDocument"

data DeleteDocumentRecordIfRequired = DeleteDocumentRecordIfRequired DocumentID [User]
                                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate DeleteDocumentRecordIfRequired (Either String Document) where
  dbUpdate (DeleteDocumentRecordIfRequired docid users) = wrapDB $ \conn -> do
    unimplemented "DeleteDocumentRecordIfRequired"

data DeleteSigAttachment = DeleteSigAttachment DocumentID BS.ByteString FileID
                           deriving (Eq, Ord, Show, Typeable)
instance DBUpdate DeleteSigAttachment (Either String Document) where
  dbUpdate (DeleteSigAttachment docid email fid) = wrapDB $ \conn -> do
    unimplemented "DeleteSigAttachment"

data DocumentFromSignatoryData = DocumentFromSignatoryData DocumentID Int BS.ByteString BS.ByteString BS.ByteString BS.ByteString BS.ByteString BS.ByteString [BS.ByteString]
                                 deriving (Eq, Ord, Show, Typeable)
instance DBUpdate DocumentFromSignatoryData (Either String Document) where
  dbUpdate (DocumentFromSignatoryData docid sigindex fstname sndname email company personalnumber companynumber fieldvalues) = wrapDB $ \conn -> do
    unimplemented "DocumentFromSignatoryData"

data ErrorDocument = ErrorDocument DocumentID String
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ErrorDocument (Either String Document) where
  dbUpdate (ErrorDocument docid errmsg) = wrapDB $ \conn -> do
    unimplemented "ErrorDocument"

data FileModTime = FileModTime FileID deriving (Eq, Ord, Show, Typeable)
instance DBQuery FileModTime MinutesTime where
  dbQuery (FileModTime fid) = wrapDB $ \conn -> do
    unimplemented "FileModTime"

data GetDeletedDocumentsByCompany = GetDeletedDocumentsByCompany User
                                    deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDeletedDocumentsByCompany [Document] where
  dbQuery (GetDeletedDocumentsByCompany uid) = wrapDB $ \conn -> do
    unimplemented "GetDeletedDocumentsByCompany"

data GetDeletedDocumentsByUser = GetDeletedDocumentsByUser User
                                 deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDeletedDocumentsByUser [Document] where
  dbQuery (GetDeletedDocumentsByUser uid) = wrapDB $ \conn -> do
    unimplemented "GetDeletedDocumentsByUser"

data GetDocumentByDocumentID = GetDocumentByDocumentID DocumentID
                               deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentByDocumentID (Maybe Document) where
  dbQuery (GetDocumentByDocumentID did) = wrapDB $ \conn -> do
    st <- prepare conn $ selectDocumentsSQL ++ " WHERE id = ? AND deleted = FALSE"
    _ <- execute st [toSql did]
    docs <- fetchDocuments st
    mdoc <- oneObjectReturnedGuard docs
    case mdoc of
      Nothing -> return Nothing
      Just doc -> do
                stx <- prepare conn $ selectSignatoryLinksSQL ++ " WHERE document_id = ? AND deleted = FALSE"
                _ <- execute stx [toSql did]
                sls <- fetchSignatoryLinks stx
                return (Just (doc { documentsignatorylinks = sls }))



data GetDocumentStats = GetDocumentStats
                        deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentStats DocStats where
  dbQuery (GetDocumentStats) = wrapDB $ \conn -> do
    unimplemented "GetDocumentStats"

data GetDocumentStatsByUser = GetDocumentStatsByUser User MinutesTime
                              deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentStatsByUser DocStats where
  dbQuery (GetDocumentStatsByUser user time) = wrapDB $ \conn -> do
    unimplemented "GetDocumentStatsByUser"

data GetDocuments = GetDocuments (Maybe ServiceID)
                    deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocuments [Document] where
  dbQuery (GetDocuments mserviceid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectDocumentsSQL
    _ <- execute st []
    docs <- fetchDocuments st

    let fetchSignatoryLinksOfDocument doc = do
             stx <- prepare conn $ selectSignatoryLinksSQL ++ " WHERE document_id = ?"
             _ <- execute stx [toSql (documentid doc)]
             sls <- fetchSignatoryLinks stx
             return (doc { documentsignatorylinks = sls })
    mapM fetchSignatoryLinksOfDocument docs

data GetDocumentsByAuthor = GetDocumentsByAuthor UserID
                            deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsByAuthor [Document] where
  dbQuery (GetDocumentsByAuthor userid) = wrapDB $ \conn -> do
    unimplemented "GetDocumentsByAuthor"

data GetDocumentsByCompany = GetDocumentsByCompany User
                             deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsByCompany [Document] where
  dbQuery (GetDocumentsByCompany userid) = wrapDB $ \conn -> do
    unimplemented "GetDocumentsByCompany"

data GetDocumentsByCompanyAndTags = GetDocumentsByCompanyAndTags (Maybe ServiceID) CompanyID [DocumentTag]
                                    deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsByCompanyAndTags [Document] where
  dbQuery (GetDocumentsByCompanyAndTags mservice companyid doctags) = wrapDB $ \conn -> do
    unimplemented "GetDocumentsByCompanyAndTags"

data GetDocumentsBySignatory = GetDocumentsBySignatory User
                               deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsBySignatory [Document] where
  dbQuery (GetDocumentsBySignatory user) = wrapDB $ \conn -> do
    unimplemented "GetDocumentsBySignatory"

data GetDocumentsByUser = GetDocumentsByUser User
                          deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsByUser [Document] where
  dbQuery (GetDocumentsByUser user) = wrapDB $ \conn -> do
    unimplemented "GetDocumentsByUser"

data GetDocumentsSharedInCompany = GetDocumentsSharedInCompany User
                                   deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsSharedInCompany [Document] where
  dbQuery (GetDocumentsSharedInCompany user) = wrapDB $ \conn -> do
    unimplemented "GetDocumentsSharedInCompany"

data GetMagicHash = GetMagicHash
                    deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetMagicHash MagicHash where
  dbQuery (GetMagicHash) = wrapDB $ \conn -> do
    unimplemented "GetMagicHash"

data GetNumberOfDocumentsOfUser = GetNumberOfDocumentsOfUser User
                                  deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetNumberOfDocumentsOfUser Int where
  dbQuery (GetNumberOfDocumentsOfUser user) = wrapDB $ \conn -> do
    unimplemented "GetNumberOfDocumentsOfUser"

data GetSignatoryLinkIDs = GetSignatoryLinkIDs
                           deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetSignatoryLinkIDs [SignatoryLinkID] where
  dbQuery (GetSignatoryLinkIDs) = wrapDB $ \conn -> do
    unimplemented "GetSignatoryLinkIDs"

data GetTimeoutedButPendingDocuments = GetTimeoutedButPendingDocuments MinutesTime
                                       deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetTimeoutedButPendingDocuments [Document] where
  dbQuery (GetTimeoutedButPendingDocuments mtime) = wrapDB $ \conn -> do
    unimplemented "GetTimeoutedButPendingDocuments"

data GetUniqueSignatoryLinkID = GetUniqueSignatoryLinkID
                                deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetUniqueSignatoryLinkID SignatoryLinkID where
  dbQuery (GetUniqueSignatoryLinkID) = wrapDB $ \conn -> do
    unimplemented "GetUniqueSignatoryLinkID"

data MarkDocumentSeen = MarkDocumentSeen DocumentID SignatoryLinkID MagicHash MinutesTime Word32
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate MarkDocumentSeen (Either String Document) where
  dbUpdate (MarkDocumentSeen documentid signatorylinkid1 mh time ipnumber) = wrapDB $ \conn -> do
    unimplemented "MarkDocumentSeen"

data AddInvitationEvidence = AddInvitationEvidence DocumentID SignatoryLinkID MinutesTime Word32
                          deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AddInvitationEvidence (Either String Document) where
  dbUpdate (AddInvitationEvidence docid slid time ipnumber) = wrapDB $ \conn -> do
    unimplemented "AddInvitationEvidence"

data MarkInvitationRead = MarkInvitationRead DocumentID SignatoryLinkID MinutesTime
                          deriving (Eq, Ord, Show, Typeable)
instance DBUpdate MarkInvitationRead (Either String Document) where
  dbUpdate (MarkInvitationRead documentid linkid time) = wrapDB $ \conn -> do
    unimplemented "MarkInvitationRead"

data MigrateDocumentSigLinkCompanies = MigrateDocumentSigLinkCompanies DocumentID [User]
                                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate MigrateDocumentSigLinkCompanies (Either String Document) where
  dbUpdate (MigrateDocumentSigLinkCompanies docid sigusers) = wrapDB $ \conn -> do
    unimplemented "MigrateDocumentSigLinkCompanies"

data NewDocument = NewDocument User (Maybe Company) BS.ByteString DocumentType MinutesTime
                 deriving (Eq, Ord, Show, Typeable)
instance DBUpdate NewDocument (Either String Document) where
  dbUpdate (NewDocument user mcompany title documenttype ctime) = do
  if fmap companyid mcompany /= usercompany user
    then return $ Left "company and user don't match"
    else do
      wrapDB $ \conn -> runRaw conn "LOCK TABLE signatory_links IN ACCESS EXCLUSIVE MODE"
      wrapDB $ \conn -> runRaw conn "LOCK TABLE documents IN ACCESS EXCLUSIVE MODE"
      did <- DocumentID <$> getUniqueID tableDocuments

      let authorRoles = if ((Just True) == getValueForProcess documenttype processauthorsend)
                        then [SignatoryAuthor]
                        else [SignatoryPartner, SignatoryAuthor]
      linkid <- SignatoryLinkID <$> getUniqueID tableSignatoryLinks

      magichash <- liftIO $ MagicHash <$> randomRIO (0,maxBound)
    
      let authorlink0 = signLinkFromDetails'
                        (signatoryDetailsFromUser user mcompany)
                        authorRoles linkid magichash

      let authorlink = authorlink0 {
                         maybesignatory = Just $ userid user, 
                         maybecompany = usercompany user }

      let doc = blankDocument {
                  documenttitle                = title
                , documentsignatorylinks       = [authorlink]
                , documenttype                 = documenttype
                , documentregion               = getRegion user
                , documentfunctionality        = newDocumentFunctionality documenttype user
                , documentctime                = ctime
                , documentmtime                = ctime
                , documentservice              = userservice user
                , documentauthorattachments    = []
                , documentsignatoryattachments = []
                } `appendHistory` [DocumentHistoryCreated ctime]


      -- here we emulate old behaviour and use current time instead of the one specified above
      -- FIXME: should use time given from above (I think0
      now <- liftIO $ getMinutesTime
      runInsertStatement "documents" 
            [ sqlField "id" did
            , sqlField "status" Preparation
            , sqlField "functionality" $ newDocumentFunctionality documenttype user
            , sqlField "title" title
            , sqlField "log" "[]"
            , sqlField "invite_text" ""
            , sqlField "allowed_id_types" (1::Int)
            , sqlField "type" (1::Int)
            , sqlField "process" (1::Int)
            , sqlField "sharing" (1::Int)
            , sqlField "tags" "[]"
            , sqlField "region" $ getRegion user
            , sqlField "deleted" False
            , sqlFieldType "mtime" "timestamp" $ now
            , sqlFieldType "ctime" "timestamp" $ now
            , sqlField "service_id" $ userservice user
            ]
      slid <- SignatoryLinkID <$> getUniqueID tableSignatoryLinks
      _ <- runInsertStatement "signatory_links"
                      [ sqlField "id" slid
                      , sqlField "document_id" did
                      , sqlField "user_id" (userid user)
                      , sqlField "roles" [SignatoryAuthor]
                      , sqlField "company_id" (companyid `fmap` mcompany)
                      , sqlField "token" magichash
                      , sqlField "fields" $ signatoryfields $ signatorydetails authorlink
                      ]
      v <- dbQuery $ GetDocumentByDocumentID did
      return $ maybe (Left "no such document") Right v

data ReallyDeleteDocuments = ReallyDeleteDocuments User [(DocumentID, [User])]
                             deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ReallyDeleteDocuments (Either String [Document]) where
  dbUpdate (ReallyDeleteDocuments deletinguser docidsAndUsers) = wrapDB $ \conn -> do
    unimplemented "ReallyDeleteDocuments"

data RejectDocument = RejectDocument DocumentID SignatoryLinkID MinutesTime Word32 (Maybe BS.ByteString)
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RejectDocument (Either String Document) where
  dbUpdate (RejectDocument documentid signatorylinkid1 time ipnumber customtext) = wrapDB $ \conn -> do
    unimplemented "RejectDocument"

data RestartDocument = RestartDocument Document User MinutesTime Word32
                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RestartDocument (Either String Document) where
  dbUpdate (RestartDocument doc user time ipnumber) = wrapDB $ \conn -> do
    unimplemented "RestartDocument"

data RestoreArchivedDocuments = RestoreArchivedDocuments User [DocumentID]
                                deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RestoreArchivedDocuments (Either String [Document]) where
  dbUpdate (RestoreArchivedDocuments user docids) = wrapDB $ \conn -> do
    unimplemented "RestoreArchivedDocuments"

data SaveDocumentForUser = SaveDocumentForUser DocumentID User SignatoryLinkID
                           deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SaveDocumentForUser (Either String Document) where
  dbUpdate (SaveDocumentForUser documentid User{userid, usercompany} signatorylinkid1) = wrapDB $ \conn -> do
    unimplemented "SaveDocumentForUser"

data SaveSigAttachment = SaveSigAttachment DocumentID BS.ByteString BS.ByteString FileID
                         deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SaveSigAttachment (Either String Document) where
  dbUpdate (SaveSigAttachment docid name email fid) = wrapDB $ \conn -> do
    unimplemented "SaveSigAttachment"


data SetDocumentTags = SetDocumentTags DocumentID [DocumentTag]
                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentTags (Either String Document) where
  dbUpdate (SetDocumentTags docid doctags) = wrapDB $ \conn -> do
    unimplemented "SetDocumentTags"


data SetDocumentTimeoutTime = SetDocumentTimeoutTime DocumentID MinutesTime
                              deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentTimeoutTime (Either String Document) where
  dbUpdate (SetDocumentTimeoutTime documentid timeouttime) = wrapDB $ \conn -> do
    unimplemented "SetDocumentTimeoutTime"

data SetSignatoryCompany = SetSignatoryCompany DocumentID SignatoryLinkID CompanyID
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetSignatoryCompany (Either String Document) where
  dbUpdate (SetSignatoryCompany documentid slid cid) = wrapDB $ \conn -> do
    unimplemented "SetSignatoryCompany"

data RemoveSignatoryCompany = RemoveSignatoryCompany DocumentID SignatoryLinkID
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RemoveSignatoryCompany (Either String Document) where
  dbUpdate (RemoveSignatoryCompany documentid slid) = wrapDB $ \conn -> do
    unimplemented "RemoveSignatoryCompany"

data SetSignatoryUser = SetSignatoryUser DocumentID SignatoryLinkID UserID
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetSignatoryUser (Either String Document) where
  dbUpdate (SetSignatoryUser documentid slid uid) = wrapDB $ \conn -> do
    unimplemented "SetSignatoryUser"

data RemoveSignatoryUser = RemoveSignatoryUser DocumentID SignatoryLinkID
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RemoveSignatoryUser (Either String Document) where
  dbUpdate (RemoveSignatoryUser documentid slid) = wrapDB $ \conn -> do
    unimplemented "RemoveSignatoryUser"

data SetInviteText = SetInviteText DocumentID BS.ByteString MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetInviteText (Either String Document) where
  dbUpdate (SetInviteText docid text time) = wrapDB $ \conn -> do
    unimplemented "SetInviteText"

data SetDaysToSign = SetDaysToSign DocumentID Int MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDaysToSign (Either String Document) where
  dbUpdate (SetDaysToSign docid days time) = wrapDB $ \conn -> do
    unimplemented "SetDaysToSign"

data RemoveDaysToSign = RemoveDaysToSign DocumentID MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RemoveDaysToSign (Either String Document) where
  dbUpdate (RemoveDaysToSign docid time) = wrapDB $ \conn -> do
    unimplemented "RemoveDaysToSign"

data SetDocumentFunctionality = SetDocumentFunctionality DocumentID DocumentFunctionality MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentFunctionality (Either String Document) where
  dbUpdate (SetDocumentFunctionality did docfunc time) = wrapDB $ \conn -> do
    unimplemented "SetDocumentFunctionality"


data SetDocumentTitle = SetDocumentTitle DocumentID BS.ByteString MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentTitle (Either String Document) where
  dbUpdate (SetDocumentTitle docid doctitle mtime) = wrapDB $ \conn -> do
    unimplemented "SetDocumentTitle"

data SetDocumentLocale = SetDocumentLocale DocumentID Locale MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentLocale (Either String Document) where
  dbUpdate (SetDocumentLocale did locale time) = do
    r <- runUpdateStatement "documents" 
         [ sqlField "region" $ getRegion locale
         , sqlFieldType "mtime" "timestamp" $ time
         ]
         "WHERE id = ?" [ toSql did ]
    getOneDocumentAffected "SetDocumentLocale" r did

data SetDocumentTrustWeaverReference = SetDocumentTrustWeaverReference DocumentID String
                                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentTrustWeaverReference (Either String Document) where
  dbUpdate (SetDocumentTrustWeaverReference documentid reference) = wrapDB $ \conn -> do
    unimplemented "SetDocumentTrustWeaverReference"

data SetDocumentUI = SetDocumentUI DocumentID DocumentUI
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentUI (Either String Document) where
  dbUpdate (SetDocumentUI did documentui) = do
    r <- runUpdateStatement "documents" 
         [ sqlField "mail_footer" $ documentmailfooter documentui
         -- , sqlFieldType "mtime" "timestamp" $ time
         ]
         "WHERE id = ?" [ toSql did ]
    getOneDocumentAffected "SetDocumentUI" r did


data SetInvitationDeliveryStatus = SetInvitationDeliveryStatus DocumentID SignatoryLinkID Mail.MailsDeliveryStatus
                                   deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetInvitationDeliveryStatus (Either String Document) where
  dbUpdate (SetInvitationDeliveryStatus docid siglnkid status) = wrapDB $ \conn -> do
    unimplemented "SetInvitationDeliveryStatus"


data ShareDocument = ShareDocument DocumentID
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ShareDocument (Either String Document) where
  dbUpdate (ShareDocument docid) = wrapDB $ \conn -> do
    unimplemented "ShareDocument"


data SignDocument = SignDocument DocumentID SignatoryLinkID MagicHash MinutesTime Word32 (Maybe SignatureInfo)
                    deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SignDocument (Either String Document) where
  dbUpdate (SignDocument documentid signatorylinkid1 mh time ipnumber msiginfo) = wrapDB $ \conn -> do
    unimplemented "SignDocument"


data ResetSignatoryDetails = ResetSignatoryDetails DocumentID [(SignatoryDetails, [SignatoryRole])] MinutesTime
                                  deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ResetSignatoryDetails (Either String Document) where
  dbUpdate (ResetSignatoryDetails documentid signatories time) = wrapDB $ \conn -> do
    unimplemented "ResetSignatoryDetails"


data SignLinkFromDetailsForTest = SignLinkFromDetailsForTest SignatoryDetails [SignatoryRole]
                                  deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SignLinkFromDetailsForTest SignatoryLink where
  dbUpdate (SignLinkFromDetailsForTest details roles) = do
      wrapDB $ \conn -> runRaw conn "LOCK TABLE signatory_links IN ACCESS EXCLUSIVE MODE"
      linkid <- SignatoryLinkID <$> getUniqueID tableSignatoryLinks

      magichash <- liftIO $ MagicHash <$> randomRIO (0,maxBound)
    
      let link = signLinkFromDetails' details
                        roles linkid magichash

      return link

data SignableFromDocument = SignableFromDocument Document
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SignableFromDocument (Either String Document) where
  dbUpdate (SignableFromDocument document) = wrapDB $ \conn -> do
    unimplemented "SignableFromDocument"


data SignableFromDocumentIDWithUpdatedAuthor = SignableFromDocumentIDWithUpdatedAuthor User (Maybe Company) DocumentID MinutesTime
                                               deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SignableFromDocumentIDWithUpdatedAuthor (Either String Document) where
  dbUpdate (SignableFromDocumentIDWithUpdatedAuthor user mcompany docid time) = wrapDB $ \conn -> do
    unimplemented "SignableFromDocumentIDWithUpdatedAuthor"


data StoreDocumentForTesting = StoreDocumentForTesting Document
                               deriving (Eq, Ord, Show, Typeable)
instance DBUpdate StoreDocumentForTesting DocumentID where
  dbUpdate (StoreDocumentForTesting document) = do
    -- FIXME: this requires more thinking...
    wrapDB $ \conn -> runRaw conn "LOCK TABLE documents IN ACCESS EXCLUSIVE MODE"
    did <- DocumentID <$> getUniqueID tableDocuments
    doc <- dbUpdate (PutDocumentUnchecked (document { documentid = did }))
    return (documentid doc)


data TemplateFromDocument = TemplateFromDocument DocumentID
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate TemplateFromDocument (Either String Document) where
  dbUpdate (TemplateFromDocument documentid) = wrapDB $ \conn -> do
    unimplemented "TemplateFromDocument"

data TimeoutDocument = TimeoutDocument DocumentID MinutesTime
                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate TimeoutDocument (Either String Document) where
  dbUpdate (TimeoutDocument did time) = do
    r <- runUpdateStatement "documents" 
         [ sqlField "status" $ Timedout
         , sqlFieldType "mtime" "timestamp" $ time
         ]
         "WHERE id = ? AND type = ? AND status = ? " [ toSql did
                                                     , toSql (toDocumentSimpleType (Signable undefined))
                                                     , toSql Pending 
                                                     ]
    getOneDocumentAffected "TimeoutDocument" r did

data UpdateDocument = UpdateDocument MinutesTime DocumentID BS.ByteString [(SignatoryDetails,[SignatoryRole])] (Maybe Int)
                      BS.ByteString (SignatoryDetails, [SignatoryRole], UserID, Maybe CompanyID) [IdentificationType]
                      (Maybe Int) DocumentFunctionality
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate UpdateDocument (Either String Document) where
  dbUpdate (UpdateDocument time documentid docname signatories daystosign invitetext (authordetails, authorroles, authorid, mcompanyid) idtypes mcsvsigindex docfunctionality) = wrapDB $ \conn -> do
    unimplemented "UpdateDocument"


data SetCSVSigIndex = SetCSVSigIndex DocumentID Int MinutesTime
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetCSVSigIndex (Either String Document) where
  dbUpdate (SetCSVSigIndex did csvsigindex time) = wrapDB $ \conn -> do
    unimplemented "SetCSVSigIndex"

data SetEmailIdentification = SetEmailIdentification DocumentID MinutesTime
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetEmailIdentification (Either String Document) where
  dbUpdate (SetEmailIdentification did time) = wrapDB $ \conn -> do
    unimplemented "SetEmailIdentification"

data SetElegitimationIdentification = SetElegitimationIdentification DocumentID MinutesTime
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetElegitimationIdentification (Either String Document) where
  dbUpdate (SetElegitimationIdentification did time) = wrapDB $ \conn -> do
    unimplemented "SetElegitimationIdentification"


data UpdateFields  = UpdateFields DocumentID SignatoryLinkID [(BS.ByteString, BS.ByteString)]
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate UpdateFields (Either String Document) where
  dbUpdate (UpdateFields docid slid fields) = wrapDB $ \conn -> do
    unimplemented "UpdateFields"

data PendingToAwaitingAuthor = PendingToAwaitingAuthor DocumentID MinutesTime
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate PendingToAwaitingAuthor (Either String Document) where
  dbUpdate (PendingToAwaitingAuthor docid time) = wrapDB $ \conn -> do
    unimplemented "PendingToAwaitingAuthor"


data UpdateDocumentAttachments = UpdateDocumentAttachments DocumentID [DocumentID] [FileID]
                                 deriving (Eq, Ord, Show, Typeable)
instance DBUpdate UpdateDocumentAttachments (Either String Document) where
  dbUpdate (UpdateDocumentAttachments docid idstoadd idstoremove) = wrapDB $ \conn -> do
    unimplemented "UpdateDocumentAttachments"

data AddDocumentAttachment = AddDocumentAttachment DocumentID FileID
                                 deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AddDocumentAttachment (Either String Document) where
  dbUpdate (AddDocumentAttachment docid fid) = wrapDB $ \conn -> do
    unimplemented "AddDocumentAttachment"

data RemoveDocumentAttachment = RemoveDocumentAttachment DocumentID FileID
                                 deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RemoveDocumentAttachment (Either String Document) where
  dbUpdate (RemoveDocumentAttachment docid fid) = wrapDB $ \conn -> do
    unimplemented "RemoveDocumentAttachment"


data UpdateDocumentSimple = UpdateDocumentSimple DocumentID (SignatoryDetails, User) [SignatoryDetails]
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate UpdateDocumentSimple (Either String Document) where
  dbUpdate (UpdateDocumentSimple did (authordetails,author) signatories) = wrapDB $ \conn -> do
    unimplemented "UpdateDocumentSimple"


data UpdateSigAttachments = UpdateSigAttachments DocumentID [SignatoryAttachment] MinutesTime
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate UpdateSigAttachments (Either String Document) where
  dbUpdate (UpdateSigAttachments docid sigatts time) = wrapDB $ \conn -> do
    unimplemented "UpdateSigAttachments"
