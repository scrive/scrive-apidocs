{-# OPTIONS_GHC -fcontext-stack=50  #-}
module Doc.Model
  ( module File.File
  , isTemplate -- fromUtils
  , isDeletableDocument -- fromUtils
  , anyInvitationUndelivered
  , undeliveredSignatoryLinks
  , insertDocumentAsIs
  , toDocumentProcess

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
  , GetDocumentByDocumentID(..)
  , GetDocumentStats(..)
  , GetDocumentStatsByUser(..)
  , GetDocuments(..)
  , GetDocumentsByAuthor(..)
  , GetDocumentsByCompanyAndTags(..)
  , GetDocumentsBySignatory(..)
  , GetTimeoutedButPendingDocuments(..)
  , MarkDocumentSeen(..)
  , MarkInvitationRead(..)
  , NewDocument(..)
  , PendingToAwaitingAuthor(..)
  , PreparationToPending(..)
  , ReallyDeleteDocument(..)
  , RejectDocument(..)
  , RemoveDaysToSign(..)
  , RemoveDocumentAttachment(..)
  , ResetSignatoryDetails(..)
  , ResetSignatoryDetails2(..)
  , RestartDocument(..)
  , RestoreArchivedDocument(..)
  , SaveDocumentForUser(..)
  , SaveSigAttachment(..)
  , SetDaysToSign(..)
  , SetDocumentAdvancedFunctionality(..)
  , SetDocumentInviteTime(..)
  , SetDocumentLocale(..)
  , SetDocumentTags(..)
  , SetDocumentTimeoutTime(..)
  , SetDocumentTitle(..)
  , SetDocumentUI(..)
  , SetElegitimationIdentification(..)
  , SetEmailIdentification(..)
  , SetInvitationDeliveryStatus(..)
  , SetInviteText(..)
  , SignDocument(..)
  , SignLinkFromDetailsForTest(..)
  , SignableFromDocumentIDWithUpdatedAuthor(..)
  , StoreDocumentForTesting(..)
  , TemplateFromDocument(..)
  , TimeoutDocument(..)
  , UpdateFields(..)
  , UpdateSigAttachments(..)
  , SetDocumentModificationData(..)
  ) where

import API.Service.Model
import DB.Classes
import DB.Fetcher
import DB.Fetcher2
import DB.Types
import DB.Utils
import File.File
import File.FileID
import Doc.DocUtils
import User.UserID
import User.Model
import Company.Model
import MinutesTime
import Doc.DocStateData
import Doc.Invariants
import Data.Data
import Database.HDBC
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import Data.Maybe
import Misc
import Data.List
import Data.Monoid
import qualified Data.Map as Map
import Doc.Tables
import Control.Applicative
import Util.SignatoryLinkUtils
import Doc.DocProcess
import Doc.DocStateCommon
import qualified Log
import System.Random (randomIO)
import Control.Monad.IO.Class
import Control.Monad
import qualified Control.Exception as E
import File.Model

sqlLog :: MinutesTime -> String -> (String, String, SqlValue)
sqlLog time text = sql' "log" "log || ?" logmsg
  where logmsg = unlines [show $ DocumentLogEntry time $ BS.fromString text]

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
                                        , queryParams = []
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
    sl1 = sort $ documentsignatorylinks d1
    sl2 = sort $ documentsignatorylinks d2
    checkSigLink s1 s2 = map (\f -> f s1 s2)
                         [ checkEqualBy "signatorylinkid" signatorylinkid
                         , checkEqualBy "signatorydetails" signatorydetails
                         , checkEqualBy "signatorymagichash" signatorymagichash
                         , checkEqualByAllowSecondNothing "maybesignatory" maybesignatory
                         , checkEqualByAllowSecondNothing "maybesupervisor" maybesupervisor
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
                   [ checkEqualBy "documentid" documentid
                   , checkEqualBy "documenttitle" documenttitle
                   --, checkEqualBy "documentfiles" documentfiles -- Mariusz: I skipped this test, since for migration I put drop files that are no avaible in database (broken)
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
                   , checkEqualBy "documentrejectioninfo" documentrejectioninfo
                   , checkEqualBy "documenttags" documenttags
                   , checkEqualBy "documentservice" documentservice
                   , checkEqualBy "documentdeleted" documentdeleted
                   , checkEqualBy "documentauthorattachments" documentauthorattachments
                   , checkEqualBy "documentsignatoryattachments" documentsignatoryattachments
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
  , "tags"
  , "service_id"
  , "deleted"
  , "mail_footer"
  , "region"
  ]

selectDocumentsSQL :: String
selectDocumentsSQL = "SELECT "
  ++ documentsSelectors
  ++ " FROM documents "

fetchDocuments :: DB [Document]
fetchDocuments = foldDB decoder []
  where
    decoder acc did title file_id sealed_file_id status error_text simple_type
     process functionality ctime mtime days_to_sign timeout_time invite_time
     invite_ip dlog invite_text allowed_id_types cancelationreason rejection_time
     rejection_signatory_link_id rejection_reason tags service deleted mail_footer
     region = Document {
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
           Just t -> Just (SignInfo t (fromMaybe unknownIPAddress invite_ip))
       , documentlog = dlog
       , documentinvitetext = invite_text
       , documentallowedidtypes = allowed_id_types
       , documentcancelationreason = cancelationreason
       , documentsharing = Private
       , documentrejectioninfo = case (rejection_time, rejection_signatory_link_id, rejection_reason) of
           (Just t, Just sl, mr) -> Just (t, sl, fromMaybe BS.empty mr)
           _ -> Nothing
       , documenttags = tags
       , documentservice = service
       , documentdeleted = deleted
       , documentauthorattachments = []
       , documentsignatoryattachments = []
       , documentui = DocumentUI mail_footer
       , documentregion = region
       } : acc

signatoryLinksSelectors :: String
signatoryLinksSelectors = intercalate ", " [
    "id"
  , "document_id"
  , "user_id"
  , "company_id"
  , "fields"
  , "sign_order"
  , "token"
  , "sign_time"
  , "sign_ip"
  , "seen_time"
  , "seen_ip"
  , "read_invitation"
  , "invitation_delivery_status"
  , "signinfo_text"
  , "signinfo_signature"
  , "signinfo_certificate"
  , "signinfo_provider"
  , "signinfo_first_name_verified"
  , "signinfo_last_name_verified"
  , "signinfo_personal_number_verified"
  , "roles"
  , "csv_title"
  , "csv_contents"
  , "csv_signatory_index"
  , "deleted"
  , "really_deleted"
  ]

selectSignatoryLinksSQL :: String
selectSignatoryLinksSQL = "SELECT "
  ++ signatoryLinksSelectors
  ++ " FROM signatory_links "

fetchSignatoryLinks :: DB [(DocumentID, SignatoryLink)]
fetchSignatoryLinks = foldDB decoder []
  where
    decoder acc slid document_id user_id company_id fields sign_order token
     sign_time sign_ip seen_time seen_ip read_invitation invitation_delivery_status
     signinfo_text signinfo_signature signinfo_certificate signinfo_provider
     signinfo_first_name_verified signinfo_last_name_verified
     signinfo_personal_number_verified roles csv_title csv_contents
     csv_signatory_index deleted really_deleted = (document_id, SignatoryLink {
         signatorylinkid = slid
       , signatorydetails = SignatoryDetails {
           signatorysignorder = sign_order
         , signatoryfields = fields
       }
       , signatorymagichash = token
       , maybesignatory = user_id
       , maybesupervisor = Nothing
       , maybecompany = company_id
       , maybesigninfo = case (sign_time, sign_ip) of
           (Just st, Just sip) -> Just (SignInfo st sip)
           _ -> Nothing
       , maybeseeninfo = case (seen_time, seen_ip) of
           (Just st, Just sip) -> Just (SignInfo st sip)
           _ -> Nothing
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
         case (csv_title, csv_contents, csv_signatory_index) of
           (Just t, Just c, Just si) -> Just (CSVUpload t c si)
           _ -> Nothing
       }) : acc

insertSignatoryLinkAsIs :: DocumentID -> SignatoryLink -> DB (Maybe SignatoryLink)
insertSignatoryLinkAsIs documentid link = do
  ruserid <- case maybesignatory link of
    Nothing -> return Nothing
    Just userid1 -> do
      muser <- dbQuery $ GetUserByID userid1
      case muser of
        Nothing ->
          do
            Just doc <- dbQuery $ GetDocumentByDocumentID documentid
            Log.server $ "User " ++ show (maybesignatory link) ++ " of document #" ++
               show documentid ++ " '" ++ BS.toString (documenttitle doc) ++ "' does not exist, setting to NULL"
            return Nothing
        Just _ -> return (Just userid1)

  _ <- kRun $ mkSQL INSERT tableSignatoryLinks [
      sql "id" $ signatorylinkid link
    , sql "document_id" documentid
    , sql "user_id" $ ruserid
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
    ] <++> SQL ("RETURNING " ++ signatoryLinksSelectors) []

  fetchSignatoryLinks
    >>= oneObjectReturnedGuard
    >>= return . fmap snd

authorAttachmentsSelectors :: String
authorAttachmentsSelectors = intercalate ", " [
    "document_id"
  , "file_id"
  ]

selectAuthorAttachmentsSQL :: String
selectAuthorAttachmentsSQL = "SELECT "
  ++ authorAttachmentsSelectors
  ++ " FROM author_attachments "

fetchAuthorAttachments :: DB [(DocumentID, AuthorAttachment)]
fetchAuthorAttachments = foldDB decoder []
  where
    decoder acc document_id file_id = (document_id, AuthorAttachment {
      authorattachmentfile = file_id
    }) : acc

insertAuthorAttachmentAsIs :: DocumentID -> AuthorAttachment -> DB (Maybe AuthorAttachment)
insertAuthorAttachmentAsIs documentid attach = do
  _ <- kRun $ mkSQL INSERT tableAuthorAttachments [
      sql "file_id" $ authorattachmentfile attach
    , sql "document_id" documentid
    ] <++> SQL ("RETURNING " ++ authorAttachmentsSelectors) []

  fetchAuthorAttachments
    >>= oneObjectReturnedGuard
    >>= return . fmap snd

signatoryAttachmentsSelectors :: String
signatoryAttachmentsSelectors = intercalate ", " [
    "document_id"
  , "file_id"
  , "email"
  , "name"
  , "description"
  ]

selectSignatoryAttachmentsSQL :: String
selectSignatoryAttachmentsSQL = "SELECT "
  ++ signatoryAttachmentsSelectors
  ++ " FROM signatory_attachments "

fetchSignatoryAttachments :: DB [(DocumentID, SignatoryAttachment)]
fetchSignatoryAttachments = foldDB decoder []
  where
    decoder acc document_id file_id email name description =
      (document_id, SignatoryAttachment {
          signatoryattachmentfile = file_id
        , signatoryattachmentemail = email
        , signatoryattachmentname = name
        , signatoryattachmentdescription = description
        }) : acc

insertSignatoryAttachmentAsIs :: DocumentID -> SignatoryAttachment -> DB (Maybe SignatoryAttachment)
insertSignatoryAttachmentAsIs documentid attach = do
  _ <- kRun $ mkSQL INSERT tableSignatoryAttachments [
      sql "file_id" $ signatoryattachmentfile attach
    , sql "document_id" $ documentid
    , sql "email" $ signatoryattachmentemail attach
    , sql "name" $ signatoryattachmentname attach
    , sql "description" $ signatoryattachmentdescription attach
    ] <++> SQL ("RETURNING " ++ signatoryAttachmentsSelectors) []
  fetchSignatoryAttachments
    >>= oneObjectReturnedGuard
    >>= return . fmap snd

insertDocumentAsIs :: Document -> DB (Maybe Document)
insertDocumentAsIs document = do
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
                 , documentcancelationreason
                 , documentrejectioninfo
                 , documenttags
                 , documentservice
                 , documentdeleted
                 , documentauthorattachments
                 , documentsignatoryattachments
                 , documentui
                 , documentregion
                 } = document
        process = toDocumentProcess documenttype
    files <-  sequence $ map (dbQuery . GetFileByFileID)  documentfiles
    let fileLost = (length $ concatMap maybeToList files) <  length documentfiles
    when (fileLost) $
        Log.error $ "!!!!MIGRATION WARN: Document  " ++ (show documentid) ++ " has files ("++ show documentfiles ++ "), but they are not in database. FileID will be dropped."
            ++ "Document was created "++ show documentctime
    _ <- kRun $ mkSQL INSERT tableDocuments [
        sql "id" documentid
      , sql "title" documenttitle
      , sql "tags" documenttags
      , sql "file_id" $ Nothing<| fileLost |> (listToMaybe documentfiles)
      , sql "sealed_file_id" (listToMaybe documentsealedfiles)
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
      , sql "sharing" Private -- this is unused, but does not have default and needs to be specifed here
      ] <++> SQL ("RETURNING " ++ documentsSelectors) []

    mdoc <- fetchDocuments >>= oneObjectReturnedGuard
    case mdoc of
      Nothing -> return Nothing
      Just doc -> do
        mlinks <- mapM (insertSignatoryLinkAsIs documentid) documentsignatorylinks
        mauthorattachments <- mapM (insertAuthorAttachmentAsIs documentid) documentauthorattachments
        msignatoryattachments <- mapM (insertSignatoryAttachmentAsIs documentid) documentsignatoryattachments
        if any isNothing mlinks || any isNothing mauthorattachments || any isNothing msignatoryattachments
         then return Nothing
         else do
          let newdocument = doc { documentsignatorylinks = catMaybes mlinks
                                , documentauthorattachments = catMaybes mauthorattachments
                                , documentsignatoryattachments = catMaybes msignatoryattachments
                                }
          assertEqualDocuments document newdocument
          return (Just newdocument)

insertNewDocument :: Document -> DB Document
insertNewDocument doc = do
  wrapDB $ \conn -> runRaw conn "LOCK TABLE documents IN ACCESS EXCLUSIVE MODE"
  docid <- DocumentID <$> getUniqueID tableDocuments
  now <- liftIO $ getMinutesTime
  let docWithId = doc {documentid = docid, documentmtime  = now, documentctime = now}
  newdoc <- insertDocumentAsIs docWithId
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
data AdminOnlySaveForUser = AdminOnlySaveForUser DocumentID User
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AdminOnlySaveForUser (Either String Document) where
  dbUpdate (AdminOnlySaveForUser did user) = do
    r <- kRun $ mkSQL UPDATE tableSignatoryLinks [sql "company_id" $ usercompany user]
      <++> SQL "WHERE document_id = ? AND user_id = ? " [
        toSql did
      , toSql $ userid user
      ]
    getOneDocumentAffected "AdminOnlySaveForUser" r did

data ArchiveDocument = ArchiveDocument User DocumentID
                         deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ArchiveDocument (Either String Document) where
  dbUpdate (ArchiveDocument user did) = do
    r <- case (usercompany user, useriscompanyadmin user) of
      (Just cid, True) -> updateArchivableDoc $ SQL "WHERE company_id = ?" [toSql cid]
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
        , SQL " AND document_id = ? AND EXISTS (SELECT 1 FROM documents WHERE id = ? AND status <> ? AND status <> ?)" [
            toSql did
          , toSql did
          , toSql Pending
          , toSql AwaitingAuthor
          ]
        ]

data AttachCSVUpload = AttachCSVUpload DocumentID SignatoryLinkID CSVUpload
                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AttachCSVUpload (Either String Document) where
  dbUpdate (AttachCSVUpload did slid csvupload) = do
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
            getOneDocumentAffected "AttachCSVUpload" r did
          _ -> return $ Left $ "Document #" ++ show documentid ++ " is in " ++ show (documentstatus document) ++ " state, must be Preparation"

data AttachFile = AttachFile DocumentID FileID MinutesTime
                  deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AttachFile (Either String Document) where
  dbUpdate (AttachFile did fid time) = do
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "mtime" time
      , sql "file_id" $ fid
      , sqlLog time $ "Attached main file " ++ show fid
      ] <++> SQL "WHERE id = ? AND status = ?" [toSql did, toSql Preparation]
    getOneDocumentAffected "AttachFile" r did

data AttachSealedFile = AttachSealedFile DocumentID FileID MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AttachSealedFile (Either String Document) where
  dbUpdate (AttachSealedFile did fid time) = do
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "mtime" time
      , sql "sealed_file_id" fid
      , sqlLog time $ "Attached sealed file " ++ show fid
      ] <++> SQL "WHERE id = ? AND status = ?" [toSql did, toSql Closed]
    getOneDocumentAffected "AttachSealedFile" r did

data CancelDocument = CancelDocument DocumentID CancelationReason MinutesTime IPAddress
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate CancelDocument (Either String Document) where
  dbUpdate (CancelDocument did reason mtime ipaddress) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID did
    case mdocument of
      Nothing -> return $ Left $ "Cannot CancelDocument document " ++ show did ++ " because it does not exist"
      Just document ->
        case checkCancelDocument document of
          [] -> do
            r <- kRun $ mkSQL UPDATE tableDocuments [
                sql "status" Canceled
              , sql "mtime" mtime
              , sql "cancelation_reason" $ reason
              , sqlLog mtime $ "Document canceled from " ++ formatIP ipaddress
              ] <++> SQL "WHERE id = ? AND type = ?" [
                toSql did
              , toSql $ Signable undefined
              ]
            getOneDocumentAffected "CancelDocument" r did
          s -> return $ Left $ "Cannot CancelDocument document " ++ show did ++ " because " ++ concat s

data ChangeMainfile = ChangeMainfile DocumentID FileID
instance DBUpdate ChangeMainfile (Either String Document) where
  dbUpdate (ChangeMainfile did fid) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID did
    case mdocument of
      Nothing -> return $ Left $ "Cannot ChangeMainfile document " ++ show did ++ " because it does not exist"
      Just document -> do
        let fieldname = if (documentstatus document == Closed || allHadSigned document)
                        then "sealed_file_id"
                        else "file_id"
        r <- kRun $ mkSQL UPDATE tableDocuments [sql fieldname $ fid]
          <++> SQL "WHERE id = ?" [toSql did]
        getOneDocumentAffected "ChangeMainfile" r did
    where
        allHadSigned doc = all (hasSigned ||^ (not . isSignatory)) $ documentsignatorylinks doc

data ChangeSignatoryEmailWhenUndelivered = ChangeSignatoryEmailWhenUndelivered DocumentID SignatoryLinkID (Maybe User) BS.ByteString
                                           deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ChangeSignatoryEmailWhenUndelivered (Either String Document) where
  dbUpdate (ChangeSignatoryEmailWhenUndelivered did slid muser email) = do
    Just doc <- dbQuery $ GetDocumentByDocumentID did
    let setEmail signatoryfields =
         map (\sf -> case sfType sf of
                               EmailFT -> sf { sfValue = email }
                               _       -> sf) signatoryfields

    let signlinks = documentsignatorylinks doc
        Just sl = find ((== slid) . signatorylinkid) signlinks

    r <- kRun $ mkSQL UPDATE tableSignatoryLinks [
        sql "invitation_delivery_status" Unknown
      , sql "fields" $ setEmail $ signatoryfields $ signatorydetails sl
      , sql "user_id" $ fmap userid muser
      , sql "company_id" $ muser >>= usercompany
      ] <++> SQL "WHERE EXISTS (SELECT 1 FROM documents WHERE documents.id = signatory_links.document_id AND (documents.status = ? OR documents.status = ?)) AND  AND id = ?" [
        toSql Pending
      , toSql AwaitingAuthor
      , toSql did
      , toSql slid
      ]

    getOneDocumentAffected "ChangeSignatoryEmailWhenUndelivered" r did

data PreparationToPending = PreparationToPending DocumentID MinutesTime
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate PreparationToPending (Either String Document) where
  dbUpdate (PreparationToPending docid time) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID docid
    case mdocument of
      Nothing -> return $ Left $ "Cannot PreparationToPending document " ++ show docid ++ " because it does not exist"
      Just document ->
        case checkPreparationToPending document of
          [] -> do
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
            getOneDocumentAffected "PreparationToPending" r docid
          s -> return $ Left $ "Cannot PreparationToPending document " ++ show docid ++ " because " ++ concat s

data CloseDocument = CloseDocument DocumentID MinutesTime IPAddress
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate CloseDocument (Either String Document) where
  dbUpdate (CloseDocument docid time ipaddress) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID docid
    case mdocument of
      Nothing -> return $ Left $ "Cannot Close document " ++ show docid ++ " because it does not exist"
      Just document ->
        case checkCloseDocument document of
          [] -> do
            r <- kRun $ mkSQL UPDATE tableDocuments [
                sql "status" Closed
              , sql "mtime" time
              , sqlLog time $ "Document closed from " ++ formatIP ipaddress
              ] <++> SQL "WHERE id = ? AND type = ?" [
                toSql docid
              , toSql $ Signable undefined
              ]
            getOneDocumentAffected "CloseDocument" r docid
          s -> return $ Left $ "Cannot CloseDocument " ++ show docid ++ " because " ++ concat s

data DeleteSigAttachment = DeleteSigAttachment DocumentID BS.ByteString FileID
                           deriving (Eq, Ord, Show, Typeable)
instance DBUpdate DeleteSigAttachment (Either String Document) where
  dbUpdate (DeleteSigAttachment did email fid) = do
    r <- kRun $ mkSQL UPDATE tableSignatoryAttachments [sql "file_id" SqlNull]
      <++> SQL "WHERE document_id = ? AND email = ? AND file_id = ?" [
        toSql did
      , toSql email
      , toSql fid
      ]
    getOneDocumentAffected "DeleteSigAttachment" r did

data DocumentFromSignatoryData = DocumentFromSignatoryData DocumentID BS.ByteString BS.ByteString BS.ByteString BS.ByteString BS.ByteString BS.ByteString [BS.ByteString]
                                 deriving (Eq, Ord, Show, Typeable)
instance DBUpdate DocumentFromSignatoryData (Either String Document) where
  dbUpdate (DocumentFromSignatoryData docid fstname sndname email company personalnumber companynumber fieldvalues) = do
        newFromDocument toNewDoc docid
   where
    toNewDoc :: Document -> Document
    toNewDoc d = d { documentsignatorylinks = map toNewSigLink (documentsignatorylinks d)
                    , documenttype = newDocType $ documenttype d
                    , documentsignatoryattachments = map replaceCSV (documentsignatoryattachments d)
                    }
    replaceCSV :: SignatoryAttachment -> SignatoryAttachment
    replaceCSV sa =
      if signatoryattachmentemail sa == BS.fromString "csv"
      then sa { signatoryattachmentemail = email }
      else sa
    newDocType :: DocumentType -> DocumentType
    newDocType (Signable p) = Signable p
    newDocType (Template p) = Signable p
    newDocType dt = dt
    toNewSigLink :: SignatoryLink -> SignatoryLink
    toNewSigLink sl
      | isJust (signatorylinkcsvupload sl) = pumpData sl { signatorylinkcsvupload = Nothing }
      | otherwise = sl
    pumpData :: SignatoryLink -> SignatoryLink
    pumpData siglink = replaceSignatoryData siglink fstname sndname email company personalnumber companynumber fieldvalues

data ErrorDocument = ErrorDocument DocumentID String
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ErrorDocument (Either String Document) where
  dbUpdate (ErrorDocument docid errmsg) = do
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
            getOneDocumentAffected "ErrorDocument" r docid
          s -> return $ Left $ "Cannot ErrorDocument document " ++ show docid ++ " because " ++ concat s

data GetDeletedDocumentsByUser = GetDeletedDocumentsByUser User
                                 deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDeletedDocumentsByUser [Document] where
  dbQuery (GetDeletedDocumentsByUser user) = do
    docs <- selectDocumentsBySignatory (userid user) True
    return docs

selectDocuments :: String -> [SqlValue] -> DB [Document]
selectDocuments select values = do
    kPrepare $ "CREATE TEMP TABLE docs AS " ++ select
    _ <- kExecute values

    kPrepare "SELECT * FROM docs"
    _ <- kExecute []

    docs <- fetchDocuments

    kPrepare $ selectSignatoryLinksSQL ++ "WHERE EXISTS (SELECT 1 FROM docs WHERE signatory_links.document_id = docs.id) ORDER BY document_id DESC, internal_insert_order DESC"
    _ <- kExecute []
    sls <- fetchSignatoryLinks

    kPrepare $ selectAuthorAttachmentsSQL ++ "WHERE EXISTS (SELECT 1 FROM docs WHERE author_attachments.document_id = docs.id) ORDER BY document_id DESC"
    _ <- kExecute []
    ats <- fetchAuthorAttachments

    kPrepare $ selectSignatoryAttachmentsSQL ++ "WHERE EXISTS (SELECT 1 FROM docs WHERE signatory_attachments.document_id = docs.id) ORDER BY document_id DESC"
    _ <- kExecute []
    sas <- fetchSignatoryAttachments

    kPrepare $ "DROP TABLE docs"
    _ <- kExecute []


    let makeListOfSecond :: (a,b) -> (a,[b])
        makeListOfSecond (a,b) = (a,[b])
        makeMap::(Eq a) => [(a,b)] -> Map.Map a [b]
        makeMap x = Map.fromAscListWith (flip (++)) $ map makeListOfSecond x
        sls_map = makeMap sls
        ats_map = makeMap ats
        sas_map = makeMap sas

        findEmpty :: Document -> Map.Map DocumentID [a] -> [a]
        findEmpty doc mapx = maybe [] id (Map.lookup (documentid doc) mapx)

        fillIn doc = doc { documentsignatorylinks       = findEmpty doc sls_map
                         , documentauthorattachments    = findEmpty doc ats_map
                         , documentsignatoryattachments = findEmpty doc sas_map
                         }

    return $ map fillIn docs


data GetDocumentByDocumentID = GetDocumentByDocumentID DocumentID
                               deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentByDocumentID (Maybe Document) where
  dbQuery (GetDocumentByDocumentID did) = do
    docs <- selectDocuments (selectDocumentsSQL ++ " WHERE id = ? AND deleted = FALSE") [toSql did]
    case docs of
      [doc] -> return (Just doc)
      _ -> return Nothing

data GetDocumentStats = GetDocumentStats
                        deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentStats DocStats where
  dbQuery (GetDocumentStats) = do
  undeleteddocs <- selectDocuments (selectDocumentsSQL ++ " WHERE deleted=FALSE") []
  let signatureCountForDoc :: Document -> Int
      signatureCountForDoc doc = length $ filter (isJust . maybesigninfo) (documentsignatorylinks doc)
  return $ DocStats
      { doccount = (length undeleteddocs)
      , signaturecount = sum $ map signatureCountForDoc undeleteddocs
      , signaturecount1m = 0
      , signaturecount2m = 0
      , signaturecount3m = 0
      , signaturecount6m = 0
      , signaturecount12m = 0
      }


data GetDocumentStatsByUser = GetDocumentStatsByUser User MinutesTime
                              deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentStatsByUser DocStats where
  dbQuery (GetDocumentStatsByUser user time) = do
  docs    <- dbQuery $ GetDocumentsByAuthor (userid user)
  sigdocs <- dbQuery $ GetDocumentsBySignatory user
  let signaturecount'    = length $ allsigns
      signaturecount1m'  = length $ filter (isSignedNotLaterThanMonthsAgo 1)  $ allsigns
      signaturecount2m'  = length $ filter (isSignedNotLaterThanMonthsAgo 2)  $ allsigns
      signaturecount3m'  = length $ filter (isSignedNotLaterThanMonthsAgo 3)  $ allsigns
      signaturecount6m'  = length $ filter (isSignedNotLaterThanMonthsAgo 6)  $ allsigns
      signaturecount12m' = length $ filter (isSignedNotLaterThanMonthsAgo 12) $ allsigns
      isSignedNotLaterThanMonthsAgo m d = monthsBefore m time < documentmtime d
      allsigns = filter (\d -> hasSigned $ getSigLinkFor d (userid user)) sigdocs
  return DocStats { doccount          = length docs
                  , signaturecount    = signaturecount'
                  , signaturecount1m  = signaturecount1m'
                  , signaturecount2m  = signaturecount2m'
                  , signaturecount3m  = signaturecount3m'
                  , signaturecount6m  = signaturecount6m'
                  , signaturecount12m = signaturecount12m'
                  }

data GetDocuments = GetDocuments (Maybe ServiceID)
                    deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocuments [Document] where
  dbQuery (GetDocuments mserviceid) = do
    selectDocuments (selectDocumentsSQL ++ " WHERE (?::TEXT IS NULL AND service_id IS NULL) OR (service_id = ?)")  [toSql mserviceid,toSql mserviceid]

selectDocumentsBySignatoryLink :: String -> [SqlValue] -> DB [Document]
selectDocumentsBySignatoryLink condition values = do
    selectDocuments (selectDocumentsSQL ++ " WHERE EXISTS (SELECT 1 FROM signatory_links WHERE documents.id = document_id AND " ++ condition ++ ") ORDER BY mtime") values

{- |
    All documents authored by the user that have never been deleted.
-}
data GetDocumentsByAuthor = GetDocumentsByAuthor UserID
                            deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsByAuthor [Document] where
  dbQuery (GetDocumentsByAuthor uid) = do
    selectDocumentsBySignatoryLink ("signatory_links.deleted = FALSE AND signatory_links.user_id = ? AND ((signatory_links.roles & ?)<>0) ORDER BY mtime") [toSql uid, toSql [SignatoryAuthor]]


{- |
    Fetches documents by company and tags, this won't return documents that have been deleted (so ones
    that would appear in the recycle bin//trash can.)  It also makes sure to respect the sign order in
    cases where the company is linked via a signatory that hasn't yet been activated.
-}
data GetDocumentsByCompanyAndTags = GetDocumentsByCompanyAndTags (Maybe ServiceID) CompanyID [DocumentTag]
                                    deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsByCompanyAndTags [Document] where
  dbQuery (GetDocumentsByCompanyAndTags mservice companyid doctags) = do
        docs <- selectDocumentsBySignatoryLink ("signatory_links.deleted = FALSE AND " ++
                                                "signatory_links.company_id = ? AND " ++
                                                activatedSQL ++ " AND " ++
                                                "(signatory_links.roles = ? OR signatory_links.roles = ?) AND " ++
                                                "((?::TEXT IS NULL AND service_id IS NULL) OR (service_id = ?)) ")
                [ toSql companyid,
                  toSql [SignatoryAuthor],
                  toSql [SignatoryAuthor, SignatoryPartner],
                  toSql mservice,
                  toSql mservice
                ]
        return (filter hasTags docs)
    where hasTags doc = all (`elem` (documenttags doc)) doctags

activatedSQL :: String
activatedSQL = "(NOT EXISTS (" ++ subselect ++ ")) "
  where
    subselect = "SELECT 1 FROM signatory_links AS sl2 " ++
                "WHERE signatory_links.document_id = sl2.document_id " ++
                "  AND ((sl2.roles & 1) <> 0) " ++
                "  AND sl2.sign_time IS NULL " ++
                "  AND sl2.sign_order < signatory_links.sign_order "

selectDocumentsBySignatory :: UserID -> Bool -> DB [Document]
selectDocumentsBySignatory userid deleted = do
    docs <- selectDocumentsBySignatoryLink
            ("    signatory_links.deleted = " ++ show deleted ++ " " ++
             "AND signatory_links.really_deleted = FALSE " ++
             (if deleted
              then ""
              else "AND " ++ activatedSQL) ++
             "AND (   signatory_links.user_id = ? " ++
             "     OR EXISTS (SELECT 1 FROM users " ++
             "                WHERE users.id = ? " ++
             "                  AND signatory_links.company_id = users.company_id " ++
             "                  AND users.is_company_admin = TRUE))")
             [ toSql userid
             , toSql userid
             ]
    return docs

{- |
    All documents where the user is a signatory that are not deleted.  An author is a type
    of signatory, so authored documents are included too.
    This also filters so that documents where a user is a signatory, but that signatory
    has not yet been activated according to the document's sign order, are excluded.
-}
data GetDocumentsBySignatory = GetDocumentsBySignatory User
                               deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsBySignatory [Document] where
  dbQuery (GetDocumentsBySignatory user) = do
    docs <- selectDocumentsBySignatory (userid user) False
    return docs

data GetTimeoutedButPendingDocuments = GetTimeoutedButPendingDocuments MinutesTime
                                       deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetTimeoutedButPendingDocuments [Document] where
  dbQuery (GetTimeoutedButPendingDocuments mtime) = do
        selectDocuments (selectDocumentsSQL ++ " WHERE status = ? AND timeout_time IS NOT NULL AND timeout_time < ?")
                      [ toSql Pending
                      , toSql mtime
                      ]

data MarkDocumentSeen = MarkDocumentSeen DocumentID SignatoryLinkID MagicHash MinutesTime IPAddress
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate MarkDocumentSeen (Either String Document) where
  dbUpdate (MarkDocumentSeen did slid mh time ipnumber) = do
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
    getOneDocumentAffected "MarkDocumentSeen" (max 1 r) did

data AddInvitationEvidence = AddInvitationEvidence DocumentID SignatoryLinkID MinutesTime IPAddress
                          deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AddInvitationEvidence (Either String Document) where
  dbUpdate (AddInvitationEvidence docid _slid _time _ipnumber) = do
  -- modifySignable docid $ \document ->
  -- case checkAddEvidence document slid of
  --  [] -> let Just sds = signatorydetails <$> getSigLinkFor document slid
  --        in Right $ document { documentinvitetime = Just (SignInfo time ipnumber) }
  --           `appendHistory` [DocumentHistoryInvitationSent time ipnumber [sds]]
  --  s -> Left $ "Document " ++ show documentid ++ " cannot have evidence attached for signatory " ++ show slid ++ " because " ++ concat s
    mdoc <- dbQuery $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Left "no such document"
      Just doc -> return $ Right doc

data MarkInvitationRead = MarkInvitationRead DocumentID SignatoryLinkID MinutesTime
                          deriving (Eq, Ord, Show, Typeable)
instance DBUpdate MarkInvitationRead (Either String Document) where
  dbUpdate (MarkInvitationRead did linkid time) = do
    r <- kRun $ mkSQL UPDATE tableSignatoryLinks [sql "read_invitation" time]
      <++> SQL "WHERE id = ? AND document_id = ? AND read_invitation IS NULL" [
        toSql linkid
      , toSql did
      ]
    getOneDocumentAffected "MarkInvitationRead" r did

data NewDocument = NewDocument User (Maybe Company) BS.ByteString DocumentType MinutesTime
                 deriving (Eq, Ord, Show, Typeable)
instance DBUpdate NewDocument (Either String Document) where
  dbUpdate (NewDocument user mcompany title documenttype ctime) = do
  if fmap companyid mcompany /= usercompany user
    then return $ Left "company and user don't match"
    else do
      kRunRaw "LOCK TABLE documents IN ACCESS EXCLUSIVE MODE"
      kRunRaw "LOCK TABLE signatory_links IN ACCESS EXCLUSIVE MODE"
      did <- DocumentID <$> getUniqueID tableDocuments

      let authorRoles = if ((Just True) == getValueForProcess documenttype processauthorsend)
                        then [SignatoryAuthor]
                        else [SignatoryPartner, SignatoryAuthor]
      linkid <- SignatoryLinkID <$> getUniqueID tableSignatoryLinks

      magichash <- liftIO randomIO

      let authorlink0 = signLinkFromDetails'
                        (signatoryDetailsFromUser user mcompany)
                        authorRoles linkid magichash

      let authorlink = authorlink0 {
                         maybesignatory = Just $ userid user,
                         maybecompany = usercompany user }

      let doc = blankDocument
                { documentid                   = did
                , documenttitle                = title
                , documentsignatorylinks       = [authorlink]
                , documenttype                 = documenttype
                , documentregion               = getRegion user
                , documentfunctionality        = newDocumentFunctionality documenttype user
                , documentctime                = ctime
                , documentmtime                = ctime
                , documentservice              = userservice user
                , documentauthorattachments    = []
                , documentsignatoryattachments = []
                , documentallowedidtypes       = [EmailIdentification]
                , documentui                   = (documentui blankDocument) {documentmailfooter = BS.fromString <$> (customfooter $ usersettings user)}
                } `appendHistory` [DocumentHistoryCreated ctime]

      case invariantProblems ctime doc of
        Nothing -> do

           midoc <- insertDocumentAsIs doc
           case midoc of
             Just doc' -> return $ Right doc'
             Nothing -> do
                        Log.debug $ "insertDocumentAsIs could not insert document #" ++ show (documentid doc) ++ " in NewDocument"
                        return $ Left $ "insertDocumentAsIs could not insert document #" ++ show (documentid doc) ++ " in NewDocument"
        Just a -> do
           Log.debug $ "insertDocumentAsIs invariants violated: " ++ show a
           return $ Left $ "insertDocumentAsIs invariants violated: " ++ show a



data ReallyDeleteDocument = ReallyDeleteDocument User DocumentID
                             deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ReallyDeleteDocument (Either String Document) where
  dbUpdate (ReallyDeleteDocument user did) = do
    r <- case (usercompany user, useriscompanyadmin user) of
      (Just cid, True) -> deleteDoc $ SQL "WHERE company_id = ?" [toSql cid]
      _ -> deleteDoc $ SQL "WHERE user_id = ? AND company_id IS NULL" [toSql $ userid user]
    getOneDocumentAffected "ReallyDeleteDocument" r did
    where
      deleteDoc whereClause = kRun $ mconcat [
          mkSQL UPDATE tableSignatoryLinks [sql "really_deleted" True]
        , whereClause
        , SQL " AND document_id = ? AND deleted = TRUE" [toSql did]
        ]

data RejectDocument = RejectDocument DocumentID SignatoryLinkID MinutesTime IPAddress (Maybe BS.ByteString)
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RejectDocument (Either String Document) where
  dbUpdate (RejectDocument docid slid time ipnumber customtext) =do
    mdocument <- dbQuery $ GetDocumentByDocumentID docid
    case mdocument of
      Nothing -> return $ Left $ "Cannot RejectDocument document " ++ show docid ++ " because it does not exist"
      Just document ->
        case checkRejectDocument document slid of
          [] -> do
            r <- kRun $ mkSQL UPDATE tableDocuments [
                sql "status" Rejected
              , sql "mtime" time
              , sql "rejection_time" time
              , sql "rejection_reason" customtext
              , sql "rejection_signatory_link_id" slid
              , sqlLog time $ "Document rejected from " ++ formatIP ipnumber
              ] <++> SQL "WHERE id = ?" [toSql docid]
            getOneDocumentAffected "RejectDocument" r docid
          s -> return $ Left $ "Cannot RejectDocument document " ++ show docid ++ " because " ++ concat s

data RestartDocument = RestartDocument Document User MinutesTime IPAddress
                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RestartDocument (Either String Document) where
  dbUpdate (RestartDocument doc user time ipnumber) = do
    mndoc <- tryToGetRestarted
    case mndoc of
      Right newdoc -> newFromDocument (const newdoc) (documentid doc)
      other -> return other
   where
    tryToGetRestarted :: DB (Either String Document)
    tryToGetRestarted =
      if (documentstatus doc `notElem` [Canceled, Timedout, Rejected])
      then return $ Left $ "Can't restart document with " ++ (show $ documentstatus doc) ++ " status"
      else if (not $ isAuthor (doc, user))
           then return $ Left $ "Can't restart document if you are not it's author"
           else do
             doc' <- clearSignInfofromDoc
             let doc'' = doc' `appendHistory` [DocumentHistoryRestarted time ipnumber]
             return $ Right doc''
    clearSignInfofromDoc :: DB Document
    clearSignInfofromDoc = do
      let signatoriesDetails = map (\x -> (signatorydetails x, signatoryroles x, signatorylinkid x)) $ documentsignatorylinks doc
          Just asl = getAuthorSigLink doc
      newSignLinks <- flip mapM signatoriesDetails $ do \(a,b,c) -> do
                                                             magichash <- liftIO randomIO

                                                             return $ signLinkFromDetails' a b c magichash
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

data RestoreArchivedDocument = RestoreArchivedDocument User DocumentID
                                deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RestoreArchivedDocument (Either String Document) where
  dbUpdate (RestoreArchivedDocument user did) = do
    r <- case (usercompany user, useriscompanyadmin user) of
      (Just cid, True) -> updateRestorableDoc $ SQL "WHERE company_id = ?" [toSql cid]
      _ -> updateRestorableDoc $ SQL "WHERE user_id = ?" [toSql $ userid user]
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
data SaveDocumentForUser = SaveDocumentForUser DocumentID User SignatoryLinkID
                           deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SaveDocumentForUser (Either String Document) where
  dbUpdate (SaveDocumentForUser did User{userid, usercompany} slid) = do
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
data SaveSigAttachment = SaveSigAttachment DocumentID BS.ByteString BS.ByteString FileID
                         deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SaveSigAttachment (Either String Document) where
  dbUpdate (SaveSigAttachment did name email fid) = do
    r <- kRun $ mkSQL UPDATE tableSignatoryAttachments [sql "file_id" fid]
      <++> SQL "WHERE document_id = ? AND email = ? AND file_id IS NULL AND name = ? " [
        toSql did
      , toSql email
      , toSql name
      ]
    getOneDocumentAffected "SaveSigAttachment" r did

data SetDocumentTags = SetDocumentTags DocumentID [DocumentTag]
                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentTags (Either String Document) where
  dbUpdate (SetDocumentTags did doctags) = do
    r <- kRun $ mkSQL UPDATE tableDocuments [sql "tags" doctags]
      <++> SQL "WHERE id = ?" [toSql did]
    getOneDocumentAffected "SetDocumentTags" r did

data SetDocumentInviteTime = SetDocumentInviteTime DocumentID MinutesTime IPAddress
                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentInviteTime (Either String Document) where
  dbUpdate (SetDocumentInviteTime did invitetime ipaddress) = do
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "invite_time" invitetime
      , sql "invite_ip" ipaddress
      ] <++> SQL "WHERE id = ?" [toSql did]
    getOneDocumentAffected "SetDocumentInviteTime" r did

data SetDocumentTimeoutTime = SetDocumentTimeoutTime DocumentID MinutesTime
                              deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentTimeoutTime (Either String Document) where
  dbUpdate (SetDocumentTimeoutTime did timeouttime) = do
    r <- kRun $ mkSQL UPDATE tableDocuments [sql "timeout_time" timeouttime]
      <++> SQL "WHERE id = ? AND deleted = FALSE AND type = ?" [
        toSql did
      , toSql $ Signable undefined
      ]
    getOneDocumentAffected "SetDocumentTimeoutTime" r did

data SetSignatoryCompany = SetSignatoryCompany DocumentID SignatoryLinkID CompanyID
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetSignatoryCompany (Either String Document) where
  dbUpdate (SetSignatoryCompany did slid cid) = do
    r <- kRun $ mkSQL UPDATE tableSignatoryLinks [sql "company_id" cid]
      <++> SQL "WHERE id = ? AND document_id = ?" [
        toSql slid
      , toSql did
      ]
    getOneDocumentAffected "SetSignatoryCompany" r did

data RemoveSignatoryCompany = RemoveSignatoryCompany DocumentID SignatoryLinkID
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RemoveSignatoryCompany (Either String Document) where
  dbUpdate (RemoveSignatoryCompany did slid) = do
    r <- kRun $ mkSQL UPDATE tableSignatoryLinks [sql "company_id" SqlNull]
      <++> SQL "WHERE id = ? AND document_id = ?" [
        toSql slid
      , toSql did
      ]
    getOneDocumentAffected "RemoveSignatoryCompany" r did

data SetSignatoryUser = SetSignatoryUser DocumentID SignatoryLinkID UserID
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetSignatoryUser (Either String Document) where
  dbUpdate (SetSignatoryUser did slid uid) = do
    r <- kRun $ mkSQL UPDATE tableSignatoryLinks [sql "user_id" uid]
      <++> SQL "WHERE id = ? AND document_id = ?" [
        toSql slid
      , toSql did
      ]
    getOneDocumentAffected "SetSignatoryUser" r did

data RemoveSignatoryUser = RemoveSignatoryUser DocumentID SignatoryLinkID
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RemoveSignatoryUser (Either String Document) where
  dbUpdate (RemoveSignatoryUser did slid) = do
    r <- kRun $ mkSQL UPDATE tableSignatoryLinks [sql "user_id" SqlNull]
      <++> SQL "WHERE id = ? AND document_id = ?" [
        toSql slid
      , toSql did
      ]
    getOneDocumentAffected "RemoveSignatoryUser" r did

data SetInviteText = SetInviteText DocumentID BS.ByteString MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetInviteText (Either String Document) where
  dbUpdate (SetInviteText did text time) = do
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "invite_text" text
      , sql "mtime" time
      , sqlLog time "Invite text set"
      ] <++> SQL "WHERE id = ?" [toSql did]
    getOneDocumentAffected "SetInviteText" r did

data SetDaysToSign = SetDaysToSign DocumentID Int MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDaysToSign (Either String Document) where
  dbUpdate (SetDaysToSign did days time) = do
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "days_to_sign" days
      , sql "mtime" time
      , sqlLog time $ "Days to sign set to " ++ show days
      ] <++> SQL "WHERE id = ?" [toSql did]
    getOneDocumentAffected "SetDaysToSign" r did

data RemoveDaysToSign = RemoveDaysToSign DocumentID MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RemoveDaysToSign (Either String Document) where
  dbUpdate (RemoveDaysToSign did time) = do
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "days_to_sign" SqlNull
      , sql "mtime" time
      , sqlLog time "Removed days to sign"
      ] <++> SQL "WHERE id = ?" [toSql did]
    getOneDocumentAffected "RemoveDaysToSign" r did

data SetDocumentAdvancedFunctionality = SetDocumentAdvancedFunctionality DocumentID MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentAdvancedFunctionality (Either String Document) where
  dbUpdate (SetDocumentAdvancedFunctionality did time) = do
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "functionality" AdvancedFunctionality
      , sql "mtime" time
      , sqlLog time "Document changed to advanced functionality"
      ] <++> SQL "WHERE id = ? AND functionality <> ?" [
        toSql did
      , toSql AdvancedFunctionality
      ]
    getOneDocumentAffected "SetDocumentAdvancedFunctionality" r did

data SetDocumentTitle = SetDocumentTitle DocumentID BS.ByteString MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentTitle (Either String Document) where
  dbUpdate (SetDocumentTitle did doctitle time) = do
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "title" doctitle
      , sql "mtime" time
      , sqlLog time "Document title changed"
      ] <++> SQL "WHERE id = ?" [toSql did]
    getOneDocumentAffected "SetDocumentTitle" r did

data SetDocumentLocale = SetDocumentLocale DocumentID Locale MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentLocale (Either String Document) where
  dbUpdate (SetDocumentLocale did locale time) = do
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "region" $ getRegion locale
      , sql "mtime" time
      , sqlLog time "Document locale changed"
      ] <++> SQL "WHERE id = ?" [toSql did]
    getOneDocumentAffected "SetDocumentLocale" r did

data SetDocumentUI = SetDocumentUI DocumentID DocumentUI
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentUI (Either String Document) where
  dbUpdate (SetDocumentUI did documentui) = do
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "mail_footer" $ documentmailfooter documentui
      ] <++> SQL "WHERE id = ?" [toSql did]
    getOneDocumentAffected "SetDocumentUI" r did

data SetInvitationDeliveryStatus = SetInvitationDeliveryStatus DocumentID SignatoryLinkID MailsDeliveryStatus
                                   deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetInvitationDeliveryStatus (Either String Document) where
  dbUpdate (SetInvitationDeliveryStatus did slid status) = do
    r <- kRun $ mkSQL UPDATE tableSignatoryLinks [
        sql "invitation_delivery_status" status
      ] <++> SQL "WHERE id = ? AND document_id = ? AND EXISTS (SELECT 1 FROM documents WHERE id = ? AND type = ?)" [
        toSql slid
      , toSql did
      , toSql did
      , toSql $ Signable undefined
      ]
    getOneDocumentAffected "SetInvitationDeliveryStatus" r did

data SignDocument = SignDocument DocumentID SignatoryLinkID MagicHash MinutesTime IPAddress (Maybe SignatureInfo)
                    deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SignDocument (Either String Document) where
  dbUpdate (SignDocument docid slid mh time ipnumber msiginfo) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID docid
    case mdocument of
      Nothing -> return $ Left $ "Cannot SignDocument document " ++ show docid ++ " because it does not exist"
      Just document ->
        case checkSignDocument document slid mh of
          [] -> do
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
            getOneDocumentAffected "SignDocument" r docid
          s -> return $ Left $ "Cannot SignDocument document " ++ show docid ++ " because " ++ concat s

data ResetSignatoryDetails = ResetSignatoryDetails DocumentID [(SignatoryDetails, [SignatoryRole])] MinutesTime
                                  deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ResetSignatoryDetails (Either String Document) where
  dbUpdate (ResetSignatoryDetails documentid signatories time) =
    dbUpdate (ResetSignatoryDetails2 documentid (map (\(a,b) -> (a,b,Nothing)) signatories) time)


data ResetSignatoryDetails2 = ResetSignatoryDetails2 DocumentID [(SignatoryDetails, [SignatoryRole], Maybe CSVUpload)] MinutesTime
                                  deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ResetSignatoryDetails2 (Either String Document) where
  dbUpdate (ResetSignatoryDetails2 documentid signatories _time) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID documentid
    case mdocument of
      Nothing -> return $ Left $ "ResetSignatoryDetails: document #" ++ show documentid ++ " does not exist"
      Just document ->
        case checkResetSignatoryData document signatories of
          [] -> do

            kPrepare "LOCK TABLE signatory_links IN ACCESS EXCLUSIVE MODE"
            _ <- kExecute []
            kPrepare "DELETE FROM signatory_links WHERE document_id = ?"
            _ <- kExecute [toSql documentid]

            let mauthorsiglink = getAuthorSigLink document
            forM_ signatories $ \(details, roles, mcsvupload) -> do
                     linkid <- SignatoryLinkID <$> getUniqueID tableSignatoryLinks

                     magichash <- liftIO randomIO

                     let link' = (signLinkFromDetails' details roles linkid magichash)
                                 { signatorylinkcsvupload = mcsvupload }
                         link = if isAuthor link'
                                then link' { maybesignatory = maybe Nothing maybesignatory mauthorsiglink
                                           , maybecompany   = maybe Nothing maybecompany   mauthorsiglink
                                           }
                                else link'
                     r1 <- insertSignatoryLinkAsIs documentid link
                     when (not (isJust r1)) $
                          error "ResetSignatoryDetails signatory_links did not manage to insert a row"

            Just newdocument <- dbQuery $ GetDocumentByDocumentID documentid
            let moldcvsupload = msum (map (\(_,_,a) -> a) signatories)
            let mnewcsvupload = msum (map (signatorylinkcsvupload) (documentsignatorylinks newdocument))

            when (moldcvsupload /= mnewcsvupload) $ do
                     Log.error $ "ResetSignatoryDetails2 csvupload differs: " ++ show moldcvsupload ++ " vs " ++ show mnewcsvupload
                     error $ "error in ResetSignatoryDetails2"
            return $ Right newdocument

          s -> return $ Left $ "cannot reset signatory details on document " ++ show documentid ++ " because " ++ intercalate ";" s


data SignLinkFromDetailsForTest = SignLinkFromDetailsForTest SignatoryDetails [SignatoryRole]
                                  deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SignLinkFromDetailsForTest SignatoryLink where
  dbUpdate (SignLinkFromDetailsForTest details roles) = do
      kRunRaw "LOCK TABLE signatory_links IN ACCESS EXCLUSIVE MODE"
      linkid <- SignatoryLinkID <$> getUniqueID tableSignatoryLinks

      magichash <- liftIO randomIO

      let link = signLinkFromDetails' details
                        roles linkid magichash

      return link

data SignableFromDocumentIDWithUpdatedAuthor = SignableFromDocumentIDWithUpdatedAuthor User (Maybe Company) DocumentID MinutesTime
                                               deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SignableFromDocumentIDWithUpdatedAuthor (Either String Document) where
  dbUpdate (SignableFromDocumentIDWithUpdatedAuthor user mcompany docid time) =
      if fmap companyid mcompany /= usercompany user
        then return $ Left "company and user don't match"
        else do
          (flip newFromDocument) docid $ \doc ->
            (templateToDocument doc) {
                                 documentsignatorylinks = map replaceAuthorSigLink (documentsignatorylinks doc)
                                -- FIXME: Need to remove authorfields?
                                , documentctime = time
                                }
    where replaceAuthorSigLink :: SignatoryLink -> SignatoryLink
          replaceAuthorSigLink sl
            | isAuthor sl = replaceSignatoryUser sl user mcompany
            | otherwise = sl

data StoreDocumentForTesting = StoreDocumentForTesting Document
                               deriving (Eq, Ord, Show, Typeable)
instance DBUpdate StoreDocumentForTesting DocumentID where
  dbUpdate (StoreDocumentForTesting document) = do
    -- FIXME: this requires more thinking...
    kRunRaw "LOCK TABLE documents IN ACCESS EXCLUSIVE MODE"
    did <- DocumentID <$> getUniqueID tableDocuments
    Just doc <- insertDocumentAsIs (document { documentid = did })
    return (documentid doc)

{-
   FIXME: this is so wrong on so many different levels
   - should set mtime
   - should not change type or copy this doc into new doc
-}
data TemplateFromDocument = TemplateFromDocument DocumentID
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate TemplateFromDocument (Either String Document) where
  dbUpdate (TemplateFromDocument did) = do
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "status" Preparation
      , sql "type" $ Template undefined
      ] <++> SQL "WHERE id = ?" [toSql did]
    getOneDocumentAffected "TemplateFromDocument" r did

data TimeoutDocument = TimeoutDocument DocumentID MinutesTime
                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate TimeoutDocument (Either String Document) where
  dbUpdate (TimeoutDocument did time) = do
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "status" Timedout
      , sql "mtime" time
      , sqlLog time "Document timed out"
      ] <++> SQL "WHERE id = ? AND type = ? AND status = ?" [
        toSql did
      , toSql $ Signable undefined
      , toSql Pending
      ]
    getOneDocumentAffected "TimeoutDocument" r did

data SetEmailIdentification = SetEmailIdentification DocumentID MinutesTime
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetEmailIdentification (Either String Document) where
  dbUpdate (SetEmailIdentification did time) = do
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "allowed_id_types" $ [EmailIdentification]
      , sql "mtime" time
      , sqlLog time "Email identification set"
      ] <++> SQL "WHERE id = ?" [toSql did]
    getOneDocumentAffected "SetEmailIdentification" r did

data SetElegitimationIdentification = SetElegitimationIdentification DocumentID MinutesTime
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetElegitimationIdentification (Either String Document) where
  dbUpdate (SetElegitimationIdentification did time) = do
    r <- kRun $ mkSQL UPDATE tableDocuments [
        sql "allowed_id_types" $ [ELegitimationIdentification]
      , sql "mtime" time
      , sqlLog time "E-leg identification set"
      ] <++> SQL "WHERE id = ?" [toSql did]
    getOneDocumentAffected "SetElegitimationIdentification" r did

data UpdateFields  = UpdateFields DocumentID SignatoryLinkID [(BS.ByteString, BS.ByteString)]
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate UpdateFields (Either String Document) where
  dbUpdate (UpdateFields did slid fields) = do
  Just document <- dbQuery $ GetDocumentByDocumentID did
  case checkUpdateFields document slid of
    [] -> do

      let updateSigField sf =
                let updateF n = case lookup n fields of
                      Just v  -> sf { sfValue = v }
                      Nothing -> sf
                in case sfType sf of
                  CompanyFT        -> updateF $ BS.fromString "sigco"
                  PersonalNumberFT -> updateF $ BS.fromString "sigpersnr"
                  CompanyNumberFT  -> updateF $ BS.fromString "sigcompnr"
                  CustomFT label _ -> updateF label
                  _                -> sf

      let signlinks = documentsignatorylinks document
          Just sl = find ((== slid) . signatorylinkid) signlinks

      r <- kRun $ mkSQL UPDATE tableSignatoryLinks [
          sql "fields" $ map updateSigField $ signatoryfields $ signatorydetails sl
        ] <++> SQL "WHERE EXISTS (SELECT 1 FROM documents WHERE documents.id = signatory_links.document_id AND (documents.status = ? OR documents.status = ?)) AND document_id = ? AND id = ? " [
          toSql Pending
        , toSql AwaitingAuthor
        , toSql did
        , toSql slid
        ]
      getOneDocumentAffected "UpdateFields" r did
    s -> return $ Left $ "Cannot updateFields on document " ++ show did ++ " because " ++ concat s

data PendingToAwaitingAuthor = PendingToAwaitingAuthor DocumentID MinutesTime
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate PendingToAwaitingAuthor (Either String Document) where
  dbUpdate (PendingToAwaitingAuthor docid time) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID docid
    case mdocument of
      Nothing -> return $ Left $ "Cannot PendingToAwaitingAuthor document " ++ show docid ++ " because it does not exist"
      Just document ->
        case checkPendingToAwaitingAuthor document of
          [] -> do
            r <- kRun $ mkSQL UPDATE tableDocuments [
                sql "status" AwaitingAuthor
              , sql "mtime" time
              , sqlLog time "Changed to AwaitingAuthor status"
              ] <++> SQL "WHERE id = ? AND type = ?" [
                toSql docid
              , toSql $ Signable undefined
              ]
            getOneDocumentAffected "PendingToAwaitingAuthor" r docid
          s -> return $ Left $ "Cannot PendingToAwaitingAuthor document " ++ show docid ++ " because " ++ concat s

data AddDocumentAttachment = AddDocumentAttachment DocumentID FileID
                                 deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AddDocumentAttachment (Either String Document) where
  dbUpdate (AddDocumentAttachment did fid) = do
    r <- kRun $ mkSQL INSERT tableAuthorAttachments [
        sql "document_id" did
      , sql "file_id" fid
      ] <++> SQL "WHERE EXISTS (SELECT 1 FROM documents WHERE id = ? AND status = ?)" [
        toSql did
      , toSql Preparation
      ]
    getOneDocumentAffected "AddDocumentAttachment" r did

data RemoveDocumentAttachment = RemoveDocumentAttachment DocumentID FileID
                                 deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RemoveDocumentAttachment (Either String Document) where
  dbUpdate (RemoveDocumentAttachment did fid) = do
    kPrepare "DELETE FROM author_attachments WHERE document_id = ? AND file_id = ? AND EXISTS (SELECT 1 FROM documents WHERE id = ? AND status = ?)"
    _ <- kExecute [
        toSql did
      , toSql fid
      , toSql did
      , toSql Preparation
      ]
    m <- dbQuery $ GetDocumentByDocumentID did
    case m of
      Just doc -> case documentstatus doc of
                       Preparation -> return $ Right doc
                       _ -> return $ Left "bad document status"
      Nothing -> return $ Left "no such document"


data UpdateSigAttachments = UpdateSigAttachments DocumentID [SignatoryAttachment] MinutesTime
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate UpdateSigAttachments (Either String Document) where
  dbUpdate (UpdateSigAttachments did sigatts _time) = do
    _ <- kRun $ SQL "DELETE FROM signatory_attachments WHERE document_id = ?" [toSql did]
    forM_ sigatts doInsert
    getOneDocumentAffected "UpdateSigAttachments" 1 did
    where
      doInsert SignatoryAttachment{..} = do
        r <- kRun $ mkSQL INSERT tableSignatoryAttachments [
            sql "file_id" signatoryattachmentfile
          , sql "email" signatoryattachmentemail
          , sql "name" signatoryattachmentname
          , sql "description" signatoryattachmentdescription
          , sql "document_id" did
          ]
        return r

-- For users lists in adminonly
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
  -- Doc and signature for stats:
  ++ ", d.mtime"
  ++ ", d.id as docid"
  ++ ", sl.sign_time"
  ++ ", sl.roles"
  ++ "  FROM users u"
  ++ "  LEFT JOIN companies c ON u.company_id = c.id"
  ++ "  LEFT JOIN signatory_links sl "
  ++ "    ON sl.user_id = u.id"
  ++ "    AND sl.deleted = FALSE"
  ++ "    AND (sl.roles & 255) <> 0"
  ++ "  LEFT JOIN documents d "
  ++ "    ON d.id = sl.document_id"
  ++ "  WHERE u.deleted = FALSE"
  ++ "  ORDER BY u.first_name || ' ' || u.last_name ASC, u.email ASC, userid ASC, docid ASC"

fetchUsersAndStats :: Statement
                   -> IO [(User, Maybe Company, ( Maybe MinutesTime
                                                , Maybe DocumentID
                                                , Maybe MinutesTime
                                                , Maybe [SignatoryRole]))]
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
            -> Maybe MinutesTime        -- d.mtime
            -> Maybe DocumentID         -- d.id as docid
            -> Maybe MinutesTime        -- sl.sign_time
            -> Maybe [SignatoryRole]    -- sl.roles
            -> Either DBException (User, Maybe Company, ( Maybe MinutesTime
                                                        , Maybe DocumentID
                                                        , Maybe MinutesTime
                                                        , Maybe [SignatoryRole]))
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
            mtime
            docid
            sign_time
            roles = return (
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
        , case docid of
            (Just _) -> (mtime,docid,sign_time,roles)
            _        -> (Nothing, Nothing, Nothing, Nothing)
        )

sumUserStats :: [(User, Maybe Company, ( Maybe MinutesTime
                                       , Maybe DocumentID
                                       , Maybe MinutesTime
                                       , Maybe [SignatoryRole]))]
             -> MinutesTime
             -> [(User, Maybe Company, DocStats)]
sumUserStats [] _ = []
sumUserStats list time = map transform' $ groupBy sameUserID' $ sortWith (\(a,_,_)->a) list
  where
    sameUserID' = (\(user,_,_) (user2,_,_) -> userid user == userid user2)
    transform' []                  = error "sumUserStats: empty list grouped"
    transform' l@((user,mcom,_):_) = snd $ foldl' (\(lastDocId,(u,m,stats)) (_,_,st) ->
                                                      let (i,s) = addToStats' stats st lastDocId
                                                      in (i,(u,m,s)))
                                                  (DocumentID 0,(user,mcom,DocStats 0 0 0 0 0 0 0)) l
    addToStats' :: DocStats
                -> (
                     Maybe MinutesTime
                   , Maybe DocumentID
                   , Maybe MinutesTime
                   , Maybe [SignatoryRole]
                   )
                -> DocumentID
                -> (DocumentID,DocStats)
    addToStats' (DocStats cnt scnt s1cnt s2cnt s3cnt s6cnt s12cnt)
                (Just mtime, Just docid, sign_time, Just role) lastid =
        ( if isJust sign_time then docid else lastid
        , DocStats {
              doccount           = cnt + (if SignatoryAuthor `elem` role then 1 else 0)
            , signaturecount     = if isDiffrentSignedDoc then scnt + 1 else scnt
            , signaturecount1m   = if isDiffrentSignedDoc && isSignedNotLaterThanMonthsAgo 1  mtime then s1cnt  + 1 else s1cnt
            , signaturecount2m   = if isDiffrentSignedDoc && isSignedNotLaterThanMonthsAgo 2  mtime then s2cnt  + 1 else s2cnt
            , signaturecount3m   = if isDiffrentSignedDoc && isSignedNotLaterThanMonthsAgo 3  mtime then s3cnt  + 1 else s3cnt
            , signaturecount6m   = if isDiffrentSignedDoc && isSignedNotLaterThanMonthsAgo 6  mtime then s6cnt  + 1 else s6cnt
            , signaturecount12m  = if isDiffrentSignedDoc && isSignedNotLaterThanMonthsAgo 12 mtime then s12cnt + 1 else s12cnt
         })
      where
        isDiffrentSignedDoc = isJust sign_time && docid /= lastid
    addToStats' docstats _ lastid = (lastid, docstats)
    isSignedNotLaterThanMonthsAgo m t = monthsBefore m time < t

data GetUsersAndStats = GetUsersAndStats MinutesTime
instance DBQuery GetUsersAndStats [(User, Maybe Company, DocStats)] where
  dbQuery (GetUsersAndStats time) = wrapDB $ \conn -> do
    st  <- prepare conn $ selectUsersAndStatsSQL
    _   <- executeRaw st
    uas <- fetchUsersAndStats st
    return $ sumUserStats uas time

data SetDocumentModificationData = SetDocumentModificationData DocumentID MinutesTime
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentModificationData (Either String Document) where
  dbUpdate (SetDocumentModificationData did time) = do
    r <- kRun $ mkSQL UPDATE tableDocuments [sql "mtime" time]
      <++> SQL "WHERE id = ?" [toSql did]
    getOneDocumentAffected "SetDocumentModificationData" r did

