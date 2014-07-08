{-# LANGUAGE NoImplicitPrelude #-}
module Doc.Model.Update
  ( AddDocumentAttachment(..)
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
  , RemoveOldDrafts(..)
  , ErrorDocument(..)
  , MarkDocumentSeen(..)
  , MarkInvitationRead(..)
  , NewDocument(..)
  , PreparationToPending(..)
  , PurgeDocuments(..)
  , unsavedDocumentLingerDays
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
  , SetFieldPlacements(..)
  , SetSMSInvitationDeliveryStatus(..)
  , SetInviteText(..)
  , SetConfirmText(..)
  , SetShowHeader(..)
  , SetShowPDFDownload(..)
  , SetShowRejectOption(..)
  , SetShowFooter(..)
  , SignDocument(..)
  , CloneDocumentWithUpdatedAuthor(..)
  , StoreDocumentForTesting(..)
  , TemplateFromDocument(..)
  , DocumentFromTemplate(..)
  , TimeoutDocument(..)
  , PostReminderSend(..)
  , UpdateFieldsForSigning(..)
  , SetSigAttachments(..)
  , UpdateDraft(..)
  , GetDocsSentBetween(..)
  , FixClosedErroredDocument(..)
  , GetDocsSent(..)
  , GetSignatoriesByEmail(..)

   -- only for use in tests
  , updateMTimeAndObjectVersion
  ) where

import Control.Monad.Identity (Identity)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class
import Data.Decimal (realFracToDecimal)
import Data.Int
import Data.Monoid
import Data.Monoid.Space
import DB
import Doc.Model.Expressions
import Doc.Model.Query (GetSignatoryLinkByID(..), GetDocumentByDocumentID(..), GetDocumentTags(..), GetDocsSentBetween(..), GetDocsSent(..), GetSignatoriesByEmail(..))
import Doc.SignatoryFieldID
import MagicHash
import Crypto.RNG
import Doc.Conditions
import Doc.DocumentMonad (updateDocumentWithID, updateDocument, DocumentMonad, withDocument, theDocument, theDocumentID)
import File.FileID
import File.Model
import Doc.SealStatus (SealStatus(..), hasGuardtimeSignature)
import Doc.DocUtils
import User.UserID
import User.Model
import Doc.SignatoryLinkID
import MinutesTime
import Doc.DocumentID
import OurPrelude
import Doc.DocStateData
import Data.Maybe hiding (fromJust)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Utils.Default
import Utils.Monad
import Utils.Monoid
import Utils.Prelude (for)
import Instances ()
import IPAddress
import Data.List hiding (tail, head)
import qualified Data.Map as M
import qualified Data.Set as S
import Doc.SignatoryScreenshots
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
import qualified Text.StringTemplates.Fields as F
import DB.TimeZoneName (TimeZoneName, withTimeZone, defaultTimeZoneName)
import qualified DB.TimeZoneName as TimeZoneName
import Company.Model

-- For this to work well we assume that signatories are ordered: author first, then all with ids set, then all with id == 0
insertSignatoryLinksAsAre :: MonadDB m => DocumentID -> [SignatoryLink] -> m [SignatoryLink]
insertSignatoryLinksAsAre _documentid [] = return []
insertSignatoryLinksAsAre documentid links = do
  runQuery_ . sqlInsert "signatory_links" $ do
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
           sqlSetList "confirmation_delivery_method" $ signatorylinkconfirmationdeliverymethod <$> links

           sqlResult "id"

  slids :: [SignatoryLinkID] <- fetchMany unSingle

  runQuery_ . selectSignatoryLinksX $ do
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

insertDocumentTagsAsAre :: MonadDB m => DocumentID -> [DocumentTag] -> m [DocumentTag]
insertDocumentTagsAsAre _documentid [] = return []
insertDocumentTagsAsAre documentid tags = do
  runQuery_ . sqlInsert "document_tags" $ do
         sqlSet "document_id" documentid
         sqlSetList "name" $ tagname <$> tags
         sqlSetList "value" $ tagvalue <$> tags
         sqlResult "document_tags.document_id"
         sqlResult "document_tags.name"
         sqlResult "document_tags.value"

  fetchDocumentTags
    >>= return . concatMap (S.toList . snd) . M.toList


insertAuthorAttachmentsAsAre :: MonadDB m => DocumentID -> [AuthorAttachment] -> m [AuthorAttachment]
insertAuthorAttachmentsAsAre _documentid [] = return []
insertAuthorAttachmentsAsAre documentid attachments = do
  runQuery_ . sqlInsert "author_attachments" $ do
        sqlSet "document_id" documentid
        sqlSetList "file_id" $ authorattachmentfile <$> attachments
        sqlResult "document_id"
        sqlResult "file_id"

  fetchAuthorAttachments
    >>= return . concatMap snd . M.toList

insertMainFilesAsAre :: MonadDB m => DocumentID -> [MainFile] -> m [MainFile]
insertMainFilesAsAre _documentid [] = return []

insertMainFilesAsAre documentid rfiles = do
  let files = reverse rfiles -- rfiles should be inserted with descending id: newer files come first in rfiles
  runQuery_ . sqlInsert "main_files" $ do
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
  runQuery_ . sqlInsert "signatory_attachments" $ do
          sqlSetList "signatory_link_id" $ concatMap (\(d,l) -> map (const d) l) attachments
          sqlSetList "file_id" $ signatoryattachmentfile <$> concatMap snd attachments
          sqlSetList "name" $ signatoryattachmentname <$> concatMap snd attachments
          sqlSetList "description" $ signatoryattachmentdescription <$> concatMap snd attachments
          sqlResult "signatory_link_id"
          sqlResult "file_id"
          sqlResult "name"
          sqlResult "description"

  fetchSignatoryAttachments

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
  runQuery_ . sqlInsert "signatory_link_fields" $ do
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
                           => [(SignatoryLinkID, SignatoryScreenshots)] -> m Int
insertSignatoryScreenshots l = do
  let (slids, types, times, ss) = unzip4 $ f "first" first
                                        <> f "signing" signing
                                        <> f "reference" getReferenceScreenshot
      f col part = [ (slid, col, Screenshot.time s, Screenshot.image s) | (slid, Just s) <- map (second part) l ]
  (fileids :: [FileID]) <- mapM (\(t,s) -> dbUpdate $ NewFile (t ++ "_screenshot.jpeg") s) (zip types ss)
  if null slids then return 0 else
    runQuery . sqlInsert "signatory_screenshots" $ do
           sqlSetList "signatory_link_id" $ slids
           sqlSetList "type"              $ (types :: [String])
           sqlSetList "time"              $ times
           sqlSetList "file_id"           $ fileids

insertDocumentAsIs :: (Log.MonadLog m, MonadDB m) => Document -> m (Maybe Document)
insertDocumentAsIs document@(Document{..}) = do
    runQuery_ . sqlInsert "documents" $ do
        sqlSet "title" documenttitle
        sqlSet "status" documentstatus
        sqlSet "error_text" $ case documentstatus of
          DocumentError msg -> Just msg
          _ -> Nothing
        sqlSet "type" documenttype
        sqlSet "ctime" documentctime
        sqlSet "mtime" documentmtime
        sqlSet "days_to_sign" documentdaystosign
        sqlSet "days_to_remind" documentdaystoremind
        sqlSet "timeout_time" documenttimeouttime
        sqlSet "invite_time" $ signtime `fmap` documentinvitetime
        sqlSet "invite_ip" (fmap signipnumber documentinvitetime)
        sqlSet "invite_text" documentinvitetext
        sqlSet "confirm_text" documentconfirmtext
        sqlSet "show_header" documentshowheader
        sqlSet "show_pdf_download" documentshowpdfdownload
        sqlSet "show_reject_option" documentshowrejectoption
        sqlSet "show_footer" documentshowfooter
        sqlSet "lang" documentlang
        sqlSet "sharing" documentsharing
        sqlSet "object_version" documentobjectversion
        sqlSet "api_callback_url" documentapicallbackurl
        sqlSet "token" documentmagichash
        sqlSet "time_zone_name" documenttimezonename
        sqlResult "documents.id"
    mdid <- fetchMaybe unSingle
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

insertNewDocument :: (MonadDB m, Log.MonadLog m, MonadIO m,CryptoRNG m) => Document -> m Document
insertNewDocument doc = do
  now <- getMinutesTime
  magichash <- random
  let docWithTime = doc {documentmtime  = now, documentctime = now, documentmagichash = magichash}
  newdoc <- insertDocumentAsIs docWithTime
  case newdoc of
    Just d -> return d
    Nothing -> error "insertNewDocument failed for some reason"

-- Create new document based on existing one
newFromDocumentID :: (MonadDB m, Log.MonadLog m, MonadIO m,CryptoRNG m) => (Document -> Document) -> DocumentID -> m (Maybe Document)
newFromDocumentID f docid = do
  doc <- query $ GetDocumentByDocumentID docid
  newFromDocument f doc

newFromDocument :: (MonadDB m, Log.MonadLog m, MonadIO m,CryptoRNG m) => (Document -> Document) -> Document -> m (Maybe Document)
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
    runQuery_ . sqlDelete "main_files" $ do
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
    runQuery_ $ sqlDelete "main_files" $ do
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
  update (AppendSealedFile fid status actor) = do
    updateDocumentWithID $ \did -> do
      appendSealedFile did fid status
      updateMTimeAndObjectVersion (actorTime actor)
    when (hasGuardtimeSignature status) $ do
      void $ update $ InsertEvidenceEvent
            AttachGuardtimeSealedFileEvidence
            (return ())
            actor

-- | Append an extended sealed file to a document, as a result of
-- improving an already sealed document.
data AppendExtendedSealedFile = AppendExtendedSealedFile FileID SealStatus Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m AppendExtendedSealedFile () where
  update (AppendExtendedSealedFile fid status actor) = do
    updateDocumentWithID $ \did -> do
      appendSealedFile did fid status
    void $ update $ InsertEvidenceEvent
      AttachExtendedSealedFileEvidence
      (return ())
      actor

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
  update (LogSignWithELegFailureForDocument slid mname mnumber firstName lastName personNumber actor) = do
    updateDocumentWithID $ const $ do
      updateMTimeAndObjectVersion (actorTime actor) -- Why this update?
    sl <- theDocumentID >>= \did -> query $ GetSignatoryLinkByID did slid Nothing
    let trips = [("Name",    fromMaybe (getFullName sl) mname, firstName ++ " " ++ lastName)
                ,("Personal number", fromMaybe (getPersonalNumber sl) mnumber, personNumber)]
        uneql = filter (\(_,a,b)->a/=b) trips
        msg2 = intercalate "; " $ map (\(f,s,e)->f ++ " from transaction was \"" ++ s ++ "\" but from e-legitimation was \"" ++ e ++ "\"") uneql
    void $ update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
                    SignWithELegFailureEvidence
                    (F.value "msg" msg2)
                    (Just sl)
                    Nothing
                    actor

data CancelDocument = CancelDocument Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m CancelDocument () where
  update (CancelDocument actor) = do
    updateDocumentWithID $ \did -> do
      kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
                 sqlSet "status" Canceled
                 sqlWhereDocumentIDIs did
                 sqlWhereDocumentTypeIs Signable
                 sqlWhereDocumentStatusIs Pending
      updateMTimeAndObjectVersion (actorTime actor)
    void $ update $ InsertEvidenceEvent
                  CancelDocumentEvidence
                  (return ())
                  actor

data ChangeSignatoryEmailWhenUndelivered = ChangeSignatoryEmailWhenUndelivered SignatoryLinkID (Maybe User) String Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m ChangeSignatoryEmailWhenUndelivered () where
  update (ChangeSignatoryEmailWhenUndelivered slid muser email actor) = do
    oldemail <- updateDocumentWithID $ const $ do
      oldemail :: String <- kRunAndFetch1OrThrowWhyNot unSingle $ sqlUpdate "signatory_link_fields" $ do
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
      updateMTimeAndObjectVersion (actorTime actor)
      return oldemail
    void $ update $ InsertEvidenceEvent
          ChangeSignatoryEmailWhenUndeliveredEvidence
          (F.value "oldemail" oldemail >> F.value "newemail" email)
          actor

data ChangeSignatoryPhoneWhenUndelivered = ChangeSignatoryPhoneWhenUndelivered SignatoryLinkID String Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m ChangeSignatoryPhoneWhenUndelivered () where
  update (ChangeSignatoryPhoneWhenUndelivered slid phone actor) = do
    oldphone <- updateDocumentWithID $ const $ do
      oldphone :: String <- kRunAndFetch1OrThrowWhyNot unSingle $ sqlUpdate "signatory_link_fields" $ do
             sqlFrom "signatory_link_fields AS signatory_link_fields_old"
             sqlWhere "signatory_link_fields.id = signatory_link_fields_old.id"
             sqlSet "value" phone
             sqlResult "signatory_link_fields_old.value"
             sqlWhereEq "signatory_link_fields.signatory_link_id" slid
             sqlWhereEq "signatory_link_fields.type" MobileFT
      kRun1OrThrowWhyNot $ sqlUpdate "signatory_links" $ do
          sqlSet "sms_invitation_delivery_status" Unknown
          sqlSet "user_id" (Nothing :: Maybe UserID)
          sqlWhereEq "signatory_links.id" slid
          sqlWhereExists $ sqlSelect "documents" $ do
              sqlWhere "documents.id = signatory_links.document_id"
              sqlWhereDocumentStatusIs Pending
      updateMTimeAndObjectVersion (actorTime actor)
      return oldphone
    void $ update $ InsertEvidenceEvent
          ChangeSignatoryPhoneWhenUndeliveredEvidence
          (F.value "oldphone" oldphone >> F.value "newphone" phone)
          actor

data PreparationToPending = PreparationToPending Actor TimeZoneName
instance (MonadBaseControl IO m, DocumentMonad m, TemplatesMonad m) => DBUpdate m PreparationToPending () where
  update (PreparationToPending actor tzn) = do
    (lang, tot) <- updateDocumentWithID $ \docid -> do
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
            withTimeZone defaultTimeZoneName $ do
              lang :: Lang <- kRunAndFetch1OrThrowWhyNot unSingle $ sqlUpdate "documents" $ do
                sqlSet "status" Pending
                sqlSetCmd "timeout_time" $ "cast (" <?> timestamp <+> "as timestamp with time zone)"
                            <+> "+ ((interval '1 day') * documents.days_to_sign) + (interval '23 hours 59 minutes 59 seconds')" -- This interval add almoust one they from description above.
                sqlResult "lang"
                sqlWhereDocumentIDIs docid
                sqlWhereDocumentTypeIs Signable
                sqlWhereDocumentStatusIs Preparation

              runQuery_ . sqlUpdate "signatory_links" $ do
                sqlSet "csv_title" (Nothing :: Maybe String)
                sqlSet "csv_contents" (Nothing :: Maybe String)
                sqlWhereEq "document_id" docid

              runQuery_ $ "SELECT timeout_time FROM documents WHERE id =" <?> docid
              tot <- fetchOne unSingle
              updateMTimeAndObjectVersion (actorTime actor)
              return (lang, tot)
    void $ update $ InsertEvidenceEvent
                PreparationToPendingEvidence
                (  F.value "timezone" (TimeZoneName.toString tzn)
                >> F.value "lang" (show lang)
                >> F.value "timeouttime" (formatMinutesTimeUTC tot))
                actor

data CloseDocument = CloseDocument Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m CloseDocument () where
  update (CloseDocument actor) = do
    updateDocumentWithID $ \docid -> do
      kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
                   sqlSet "status" Closed
                   sqlWhereDocumentIDIs docid
                   sqlWhereDocumentTypeIs Signable
                   sqlWhereDocumentStatusIs Pending
                   sqlWhereAllSignatoriesHaveSigned
      updateMTimeAndObjectVersion (actorTime actor)
    void $ update $ InsertEvidenceEvent
                CloseDocumentEvidence
                (return ())
                actor

data DeleteSigAttachment = DeleteSigAttachment SignatoryLinkID SignatoryAttachment Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m DeleteSigAttachment () where
  update (DeleteSigAttachment slid sa actor) = do
    (saname::String) <- updateDocumentWithID $ const $ do
      kRunAndFetch1OrThrowWhyNot unSingle $ sqlUpdate "signatory_attachments" $ do
        sqlFrom "signatory_links"
        sqlWhere "signatory_links.id = signatory_attachments.signatory_link_id"
        sqlSet "file_id" (Nothing :: Maybe FileID)
        sqlResult "signatory_attachments.name"
        sqlWhereEq "signatory_attachments.name" (signatoryattachmentname sa)
        sqlWhereSignatoryLinkIDIs slid
        sqlWhereSignatoryHasNotSigned

    void $ theDocument >>= \doc -> update $ InsertEvidenceEvent
                    DeleteSigAttachmentEvidence
                    (do F.value "name" saname
                        F.value "author" $ getIdentifier $ $(fromJust) $ getAuthorSigLink doc)
                    actor


data ErrorDocument = ErrorDocument String CurrentEvidenceEventType (F.Fields Identity ()) Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m ErrorDocument () where
  update (ErrorDocument errmsg event textFields actor) = do
    updateDocumentWithID $ \docid -> do
      kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
        sqlSet "status" $ DocumentError errmsg
        sqlSet "error_text" errmsg
        sqlWhereDocumentIDIs docid
    void $ update $ InsertEvidenceEvent event textFields actor

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

data MarkInvitationRead = MarkInvitationRead SignatoryLinkID Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m MarkInvitationRead Bool where
  update (MarkInvitationRead slid actor) = do
    success <- updateDocumentWithID $ \did -> do
        let time = actorTime actor
        runQuery01 . sqlUpdate "signatory_links" $ do
                      sqlSet "read_invitation" time
                      sqlWhereEq "id" slid
                      sqlWhereEq "document_id" did
                      sqlWhere "read_invitation IS NULL"
    sig <- theDocumentID >>= \did -> query $ GetSignatoryLinkByID did slid Nothing
    void $ update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
        MarkInvitationReadEvidence
        (return ())
        (Just sig)
        Nothing
        actor
    return success

data NewDocument = NewDocument User String DocumentType TimeZoneName Int Actor
instance (CryptoRNG m, MonadDB m, Log.MonadLog m, TemplatesMonad m) => DBUpdate m NewDocument (Maybe Document) where
  update (NewDocument user title documenttype timezone nrOfOtherSignatories actor) = do
    let ctime = actorTime actor
    magichash <- random
    authorFields <- signatoryFieldsFromUser user
    let authorlink0 = signLinkFromDetails' authorFields True True (SignOrder 1) [] magichash

    let authorlink = authorlink0 {
                           maybesignatory = Just $ userid user }

    othersignatories <- sequence $ replicate nrOfOtherSignatories $ do
                          mh <- random
                          return $ signLinkFromDetails' emptySignatoryFields False True (SignOrder 2) [] mh
    token <- random
    let doc = defaultValue
                  { documenttitle                = title
                  , documentsignatorylinks       = authorlink : othersignatories
                  , documenttype                 = documenttype
                  , documentlang                 = getLang user
                  , documentctime                = ctime
                  , documentmtime                = ctime
                  , documentauthorattachments    = []
                  , documentmagichash            = token
                  , documenttimezonename         = timezone
                  }

    midoc <- insertDocumentAsIs doc
    case midoc of
        Just _ -> return midoc
        Nothing -> do
          Log.mixlog_ $ "insertDocumentAsIs could not insert document #" ++ show (documentid doc) ++ " in NewDocument"
          return Nothing


data RejectDocument = RejectDocument SignatoryLinkID (Maybe String) Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m RejectDocument () where
  update (RejectDocument slid customtext actor) = do
    updateDocumentWithID $ \docid -> do
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
      updateMTimeAndObjectVersion (actorTime actor)

    void $ update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
                  RejectDocumentEvidence
                  (return ())
                  Nothing
                  customtext
                  actor

data RestartDocument = RestartDocument Document Actor
instance (CryptoRNG m, MonadDB m, Log.MonadLog m, TemplatesMonad m) => DBUpdate m RestartDocument (Maybe Document) where
  update (RestartDocument doc actor) = do
    mndoc <- tryToGetRestarted
    case mndoc of
      Right newdoc -> do
        md <- newFromDocument (const newdoc) doc
        case md of
          Nothing -> return Nothing
          Just d -> do
            copyEvidenceLogToNewDocument (documentid doc) (documentid d)
            void $ withDocument d $ update $ InsertEvidenceEvent
              RestartDocumentEvidence
              (return ())
              actor

            return $ Just d
      Left err -> do
        Log.attention_ err
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
                  documentsignatorylinks = newSignLinks,
                  documentapicallbackurl = Nothing
                 }

data RestoreArchivedDocument = RestoreArchivedDocument User Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m RestoreArchivedDocument () where
  update (RestoreArchivedDocument user _actor) = updateDocumentWithID $ \did -> do
    kRunManyOrThrowWhyNot $ sqlUpdate "signatory_links" $ do

      sqlSet "deleted" (Nothing :: Maybe MinutesTime)

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
    runQuery01 . sqlUpdate "signatory_links" $ do
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
  update (SaveSigAttachment slid sigattach fid actor) = do
    let name = signatoryattachmentname sigattach
    updateDocumentWithID $ const $ do
      kRun1OrThrowWhyNot $ sqlUpdate "signatory_attachments" $ do
         sqlFrom "signatory_links"
         sqlWhere "signatory_links.id = signatory_attachments.signatory_link_id"
         sqlSet "file_id"  fid
         sqlWhere "file_id IS NULL"
         sqlWhereEq "name" name
         sqlWhereSignatoryLinkIDIs slid

    void $ theDocument >>= \doc -> update $ InsertEvidenceEvent
        SaveSigAttachmentEvidence
        (do F.value "name" name
            F.value "description" $ signatoryattachmentdescription sigattach
            F.value "author" $ getIdentifier $ $(fromJust) $ getAuthorSigLink doc)
        actor


data SetFieldPlacements = SetFieldPlacements SignatoryFieldID [FieldPlacement]
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetFieldPlacements () where
  update (SetFieldPlacements fieldid placements) =
    updateDocumentWithID $ const $
    runQuery_ . sqlUpdate "signatory_link_fields" $ do
      sqlSet "placements" $ placements
      sqlWhereEq "id" fieldid

data SetDocumentTags = SetDocumentTags (S.Set DocumentTag) Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetDocumentTags Bool where
  update (SetDocumentTags doctags _actor) = updateDocumentWithID $ \did -> do
    oldtags <- query $ GetDocumentTags did
    let changed = doctags /= oldtags
    if changed
      then do
        runQuery_ $ "DELETE FROM document_tags WHERE document_id =" <?> did
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

data SetConfirmText = SetConfirmText String Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetConfirmText Bool where
  update (SetConfirmText text _actor) = updateWithoutEvidence "confirm_text" text



data SetShowHeader = SetShowHeader Bool Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetShowHeader Bool where
  update (SetShowHeader bool _actor) = updateWithoutEvidence "show_header" bool

data SetShowPDFDownload = SetShowPDFDownload Bool Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetShowPDFDownload Bool where
  update (SetShowPDFDownload bool _actor) = updateWithoutEvidence "show_pdf_download" bool

data SetShowRejectOption = SetShowRejectOption Bool Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetShowRejectOption Bool where
  update (SetShowRejectOption bool _actor) = updateWithoutEvidence "show_reject_option" bool

data SetShowFooter = SetShowFooter Bool Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetShowFooter Bool where
  update (SetShowFooter bool _actor) = updateWithoutEvidence "show_footer" bool

data SetDaysToSign = SetDaysToSign Int32 Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetDaysToSign Bool where
  update (SetDaysToSign days _actor) = updateWithoutEvidence "days_to_sign" days

data SetDaysToRemind = SetDaysToRemind (Maybe Int32) Actor
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
  update (SetEmailInvitationDeliveryStatus slid status actor) = do
    sig <- theDocumentID >>= \did -> query $ GetSignatoryLinkByID did slid Nothing
    updateDocumentWithID $ \did -> do
      kRun1OrThrowWhyNot $  sqlUpdate "signatory_links" $ do
          sqlFrom "documents"
          sqlJoin "signatory_links AS signatory_links_old"
          sqlWhere "signatory_links.id = signatory_links_old.id"
          sqlSet "mail_invitation_delivery_status" status
          sqlWhereSignatoryLinkIDIs slid
          sqlWhereDocumentIDIs did
          sqlWhereDocumentTypeIs Signable
    nsig <- theDocumentID >>= \did -> query $ GetSignatoryLinkByID did slid Nothing
    let changed = mailinvitationdeliverystatus sig /= mailinvitationdeliverystatus nsig

    when_ (changed && status == Delivered) $
      update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
        InvitationDeliveredByEmail
        (return ())
        (Just nsig)
        Nothing
        actor
    when_ (changed && status == Undelivered) $
      update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
        InvitationUndeliveredByEmail
        (return ())
        (Just nsig)
        Nothing
        actor
    return True

data SetSMSInvitationDeliveryStatus = SetSMSInvitationDeliveryStatus SignatoryLinkID DeliveryStatus Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetSMSInvitationDeliveryStatus Bool where
  update (SetSMSInvitationDeliveryStatus slid status actor) = do
    sig <- theDocumentID >>= \did -> query $ GetSignatoryLinkByID did slid Nothing
    updateDocumentWithID $ \did -> do
      runQuery_ . sqlUpdate "signatory_links" $ do
          sqlFrom "documents"
          sqlJoin "signatory_links AS signatory_links_old"
          sqlWhere "signatory_links.id = signatory_links_old.id"
          sqlSet "sms_invitation_delivery_status" status
          sqlWhereSignatoryLinkIDIs slid
          sqlWhereDocumentIDIs did
          sqlWhereDocumentTypeIs Signable
    nsig <- theDocumentID >>= \did -> query $ GetSignatoryLinkByID did slid Nothing
    let changed = smsinvitationdeliverystatus sig /= smsinvitationdeliverystatus nsig
    when_ (changed && status == Delivered) $
      update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
        InvitationDeliveredBySMS
        (return ())
        (Just nsig)
        Nothing
        actor
    when_ (changed && status == Undelivered) $
      update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
        InvitationUndeliveredBySMS
        (return ())
        (Just nsig)
        Nothing
        actor
    return True


data SetDocumentSharing = SetDocumentSharing [DocumentID] Bool
instance (MonadDB m, TemplatesMonad m) => DBUpdate m SetDocumentSharing Bool where
  update (SetDocumentSharing dids flag) = do
    results <- runQuery . sqlUpdate "documents" $ do
          sqlSet "sharing" $ (if flag then Shared else Private)
          sqlWhereIn "id" dids
    return $ results == (fromIntegral $ length dids)

data SetDocumentUnsavedDraft = SetDocumentUnsavedDraft Bool
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetDocumentUnsavedDraft () where
  update (SetDocumentUnsavedDraft flag) = updateDocumentWithID $ \did -> do
    kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
      sqlSet "unsaved_draft" flag
      sqlWhereDocumentIDIs did

data SignDocument = SignDocument SignatoryLinkID MagicHash (Maybe SignatureInfo) (Maybe String) SignatoryScreenshots Actor
instance (DocumentMonad m, TemplatesMonad m, Applicative m, CryptoRNG m) => DBUpdate m SignDocument () where
  update (SignDocument slid mh msiginfo mpin screenshots actor) = do
    updateDocumentWithID $ \docid -> do
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
           case (msiginfo,mpin) of
                (Just _,_)        -> sqlWhereSignatoryAuthenticationMethodIs ELegAuthentication
                (_, Just _)       -> sqlWhereSignatoryAuthenticationMethodIs SMSPinAuthentication -- We should check pin here, but for now we do it in controler
                (Nothing,Nothing) -> sqlWhereSignatoryAuthenticationMethodIs StandardAuthentication
           sqlWhereSignatoryLinkMagicHashIs mh
      updateMTimeAndObjectVersion (actorTime actor)
    sl <- theDocumentID >>= \docid -> query $ GetSignatoryLinkByID docid slid Nothing
    let signatureFields = case (msiginfo,mpin) of
            (Just si,_) -> do
                      F.value "eleg" True
                      F.value "provider" $ case signatureinfoprovider si of
                                            BankIDProvider -> "BankID" :: String
                                            TeliaProvider  -> "Telia"
                                            NordeaProvider -> "Nordea"
                                            MobileBankIDProvider -> "Mobile BankID"
                      F.value "fstnameverified" $ signaturefstnameverified si
                      F.value "lstnameverified" $ signaturelstnameverified si
                      F.value "persnumverified" $ signaturepersnumverified si
                      F.value "fieldsverified" $  signaturefstnameverified si || signaturelstnameverified si || signaturepersnumverified si
                      F.value "signature" $ signatureinfosignature si
                      F.value "certificate" $ nothingIfEmpty $ signatureinfocertificate si
                      F.value "ocsp" $ signatureinfoocspresponse si
                      F.value "infotext" $ signatureinfotext si
            (_,Just _) -> do
              F.value "sms_pin" True
              F.value "phone" $ getMobile sl
            _ -> return ()
    void $ update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
        SignDocumentEvidence
        signatureFields
        (Just sl)
        Nothing
        actor
    void $ insertSignatoryScreenshots [(slid, screenshots)]

-- For this to work well we assume that signatories are ordered: author first, then all with ids set, then all with id == 0
data ResetSignatoryDetails = ResetSignatoryDetails [SignatoryLink] Actor
instance (CryptoRNG m, Log.MonadLog m, DocumentMonad m, TemplatesMonad m) => DBUpdate m ResetSignatoryDetails Bool where
  update (ResetSignatoryDetails signatories _actor) = updateDocumentWithID $ \documentid -> do
    document <- query $ GetDocumentByDocumentID documentid
    case checkResetSignatoryData document signatories of
          [] -> do
            runQuery_ $ "DELETE FROM signatory_links WHERE document_id = " <?> documentid
            siglinks <- forM signatories $ \sl -> do
                     magichash <- random
                     return $ sl {  signatorymagichash = magichash,
                                    maybesignatory = if (isAuthor sl) then (maybesignatory sl) else Nothing
                                 }
            _r1 <- insertSignatoryLinksAsAre documentid siglinks
            return True

          s -> do
            Log.attention_ $ "cannot reset signatory details on document " ++ show documentid ++ " because " ++ intercalate ";" s
            return False

data CloneDocumentWithUpdatedAuthor = CloneDocumentWithUpdatedAuthor User Document Actor
instance (MonadDB m, Log.MonadLog m, TemplatesMonad m, MonadIO m,CryptoRNG m) => DBUpdate m CloneDocumentWithUpdatedAuthor (Maybe DocumentID) where
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
instance (MonadDB m, Log.MonadLog m, TemplatesMonad m) => DBUpdate m StoreDocumentForTesting DocumentID where
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
  update (TimeoutDocument actor) = do
    updateDocumentWithID $ \did -> do
      kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
         sqlSet "status" Timedout
         sqlWhereDocumentIDIs did
         sqlWhereDocumentTypeIs Signable
         sqlWhereDocumentStatusIs Pending
      updateMTimeAndObjectVersion (actorTime actor)
    void $ update $ InsertEvidenceEvent
        TimeoutDocumentEvidence
        (return ())
        actor

data ProlongDocument = ProlongDocument Int32 TimeZoneName Actor
instance (DocumentMonad m, MonadBaseControl IO m, TemplatesMonad m) => DBUpdate m ProlongDocument () where
  update (ProlongDocument days tzn actor) = do
    updateDocumentWithID $ \did -> do
      -- Whole TimeZome behaviour is a clone of what is happending with making document ready for signing.
      let time = actorTime actor
      let timestamp = formatTime defaultTimeLocale "%F" (toUTCTime time) ++ " " ++ TimeZoneName.toString tzn
      withTimeZone defaultTimeZoneName $ kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
         sqlSet "status" Pending
         sqlSet "mtime" time
         sqlSetCmd "timeout_time" $ "cast (" <?> timestamp <+> "as timestamp with time zone)"
                              <+> "+ (interval '1 day') * " <?> days <+> " + (interval '23 hours 59 minutes 59 seconds')"
         sqlWhereDocumentIDIs did
         sqlWhereDocumentTypeIs Signable
         sqlWhereDocumentStatusIs Timedout
    void $ update $ InsertEvidenceEvent
        ProlongDocumentEvidence
        (return ())
        actor

data SetDocumentAPICallbackURL = SetDocumentAPICallbackURL (Maybe String)
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetDocumentAPICallbackURL Bool where
  update (SetDocumentAPICallbackURL mac) = updateDocumentWithID $ \did -> do
    runQuery01 . sqlUpdate "documents" $ do
      sqlSet "api_callback_url" mac
      sqlWhereEq "id" did

data SetDocumentTimeZoneName = SetDocumentTimeZoneName TimeZoneName
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetDocumentTimeZoneName Bool where
  update (SetDocumentTimeZoneName timezone) = updateDocumentWithID $ \did -> do
    runQuery01 . sqlUpdate "documents" $ do
      sqlSet "time_zone_name" timezone
      sqlWhereEq "id" did



data PostReminderSend = PostReminderSend SignatoryLink (Maybe String) Bool Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m PostReminderSend () where
   update (PostReminderSend sl mmsg automatic actor) = do
     updateDocument $ \doc -> do
       let docid = documentid doc
       kRun1OrThrowWhyNot $ sqlUpdate "signatory_links" $ do
         sqlFrom "documents"
         sqlSet "read_invitation" (Nothing :: Maybe MinutesTime)
         sqlSet "mail_invitation_delivery_status" Unknown
         sqlSet "sms_invitation_delivery_status" Unknown
         sqlWhere "documents.id = signatory_links.document_id"

         sqlWhereDocumentIDIs docid
         sqlWhereSignatoryLinkIDIs (signatorylinkid sl)
         sqlWhereSignatoryHasNotSigned
         sqlWhereDocumentStatusIs Pending
       updateMTimeAndObjectVersion (actorTime actor)

     void $ theDocument >>= \doc -> update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
          (if automatic then AutomaticReminderSent else ReminderSend)
          (F.value "author" $ getIdentifier $ $(fromJust) $ getAuthorSigLink doc)
          (Just sl)
          mmsg
          actor

data UpdateFieldsForSigning = UpdateFieldsForSigning SignatoryLink [(FieldType, String)] Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m UpdateFieldsForSigning () where
  update (UpdateFieldsForSigning sl fields actor) = updateDocumentWithID $ const $ do
    -- Document has to be in Pending state
    -- signatory could not have signed already
    let slid = signatorylinkid sl
    let updateValue (SignatureFT _, "") = return ()
        updateValue (fieldtype, fvalue) = do
          let custom_name = case fieldtype of
                              CustomFT xname _ -> xname
                              CheckboxFT xname -> xname
                              SignatureFT xname -> xname
                              _ -> ""
          updated <- runQuery . sqlUpdate "signatory_link_fields" $ do
                   sqlSet "value" fvalue
                   sqlWhereEq "signatory_link_id" slid
                   sqlWhereEq "custom_name" custom_name
                   sqlWhereEq "type" fieldtype
                   sqlWhereAny $ do
                       sqlWhereAll $ do
                         sqlWhereEq "value" (""::String) -- Note: if we allow values to be overwritten, the evidence events need to be adjusted to reflect the old value.
                         sqlWhereIn "type" [CustomFT "" False, FirstNameFT,LastNameFT,EmailFT,CompanyFT,PersonalNumberFT,PersonalNumberFT,CompanyNumberFT, MobileFT]
                       sqlWhereIn "type" [CheckboxFT "", SignatureFT ""]
                   sqlWhereExists $ sqlSelect "documents" $ do
                     sqlWhere "signatory_links.id = signatory_link_id"
                     sqlLeftJoinOn "signatory_links" "documents.id = signatory_links.document_id"
                     sqlWhereEq "documents.status" Pending
                     sqlWhere "signatory_links.sign_time IS NULL"

          let mfield = getFieldOfType fieldtype (signatoryfields sl)
              changed = case mfield of
                          Just f | sfValue f == fvalue -> False
                          _                            -> True
          when (updated/=0 && changed) $ do
            let (event, efields) =
                  case fieldtype of
                    SignatureFT _ -> (UpdateFieldSignatureEvidence, return ())
                    CheckboxFT _  -> (UpdateFieldCheckboxEvidence, when (not (null fvalue)) $ F.value "checked" True)
                    _             -> (UpdateFieldTextEvidence, return ())
            void $ update $ InsertEvidenceEvent event
               (do F.value "value" fvalue
                   F.value "fieldname" $ case fieldtype of
                     FirstNameFT      -> "First name"
                     LastNameFT       -> "Last name"
                     CompanyFT        -> "Company"
                     PersonalNumberFT -> "ID number"
                     CompanyNumberFT  -> "Company number"
                     EmailFT          -> "Email"
                     CustomFT s _     -> s
                     MobileFT         -> "Mobile number"
                     _                -> ""
                   efields
                   case mfield of
                     Just f | not (null ps) -> do
                       F.objects "placements" $ for ps $ \p -> do
                         F.value "page" $ placementpage p
                         F.value "x" $ show $ realFracToDecimal 3 $ placementxrel p
                         F.value "y" $ show $ realFracToDecimal 3 $ placementyrel p
                       where ps = sfPlacements f
                     _ -> return ())
               actor

    forM_ fields updateValue

data AddDocumentAttachment = AddDocumentAttachment FileID Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m AddDocumentAttachment Bool where
  update (AddDocumentAttachment fid _actor) = updateDocumentWithID $ \did -> do
    runQuery01 . sqlInsertSelect "author_attachments" "" $ do
        sqlSet "document_id" did
        sqlSet "file_id" fid
        sqlWhereExists $ sqlSelect "documents" $ do
          sqlWhereEq "id" did
          sqlWhereEq "status" Preparation

data RemoveDocumentAttachment = RemoveDocumentAttachment FileID Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m RemoveDocumentAttachment Bool where
  update (RemoveDocumentAttachment fid _actor) = updateDocumentWithID $ \did -> do
    runQuery01 $ "DELETE FROM author_attachments WHERE document_id =" <?> did <+> "AND file_id =" <?> fid <+> "AND EXISTS (SELECT 1 FROM documents WHERE id = author_attachments.document_id AND status = " <?> Preparation <+> ")"

-- Remove unsaved drafts (older than 1 week) from db.
-- Uses chunking to not overload db when there's a lot of old drafts
data RemoveOldDrafts = RemoveOldDrafts Int32
instance MonadDB m => DBUpdate m RemoveOldDrafts Int where
    update (RemoveOldDrafts limit) = runQuery $
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
     doDeleteAll = runQuery $ "DELETE FROM signatory_attachments WHERE signatory_link_id =" <?> slid
     doInsertOne SignatoryAttachment{..} = do
        runQuery . sqlInsert "signatory_attachments" $ do
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
    , update $ SetConfirmText (documentconfirmtext document) actor
    , update $ SetShowHeader (documentshowheader document) actor
    , update $ SetShowPDFDownload (documentshowpdfdownload document) actor
    , update $ SetShowRejectOption (documentshowrejectoption document) actor
    , update $ SetShowFooter (documentshowfooter document) actor
    , update $ SetDocumentTags (documenttags document) actor
    , update $ SetDocumentAPICallbackURL (documentapicallbackurl document)
    , update $ SetDocumentTimeZoneName (documenttimezonename document)
    , updateMTimeAndObjectVersion (actorTime actor) >> return True
    ]

unsavedDocumentLingerDays :: Int
unsavedDocumentLingerDays = 30

data PurgeDocuments = PurgeDocuments Int Int
instance MonadDB m => DBUpdate m PurgeDocuments Int where
  update (PurgeDocuments savedDocumentLingerDays unsavedDocumentLingerDays') = do

    runQuery_ $ "UPDATE signatory_links"
            <+> "  SET really_deleted = now()"
            <+> "WHERE signatory_links.user_id IS NOT NULL" -- document belongs to somebody
            <+> "  AND signatory_links.deleted IS NOT NULL" -- somebody deleted that document long time ago
            <+> "  AND signatory_links.deleted + (" <?> (show savedDocumentLingerDays ++ "days") <> "::INTERVAL) <= now()"
            <+> "  AND signatory_links.really_deleted IS NULL" -- we did not notice this until now

    runQuery_ $ "CREATE TEMP TABLE documents_to_purge(id, title) AS"
        <+> "SELECT documents.id, documents.title"
        <+> "  FROM documents"
        -- document wasn't purged yet
        <+> " WHERE documents.purged_time IS NULL"
        -- has not been deleted at least in a single account
        <+> "   AND NOT EXISTS(SELECT TRUE"
        <+> "                    FROM signatory_links"
        <+> "                   WHERE signatory_links.document_id = documents.id"
        <+> "                     AND signatory_links.user_id IS NOT NULL"
                                  -- not really_deleted yet
        <+> "                     AND signatory_links.really_deleted IS NULL)"

        -- is not saved but time to save the document went by
        <+> "   AND NOT EXISTS(SELECT TRUE"
        <+> "                    FROM signatory_links"
        <+> "                   WHERE signatory_links.document_id = documents.id"
        <+> "                     AND signatory_links.user_id IS NULL"
                                  -- linger time hasn't elapsed yet
        <+> "                     AND documents.mtime + (" <?> (show unsavedDocumentLingerDays' ++ "days") <+> " :: INTERVAL) > now())"

    -- set purged time on documents
    rows <- runSQL $ "UPDATE documents"
        <+> "   SET purged_time = now()"
        <+> " WHERE documents.id IN (SELECT id"
        <+> "                          FROM documents_to_purge)"

    -- blank out sensitive data
    runSQL_ $ "UPDATE signatory_links"
        <+> "   SET sign_ip = 0"
        <+> "     , seen_ip = 0"
        <+> "     , eleg_data_mismatch_first_name = ''"
        <+> "     , eleg_data_mismatch_last_name = ''"
        <+> "     , eleg_data_mismatch_personal_number = ''"
        <+> " WHERE signatory_links.document_id IN (SELECT id FROM documents_to_purge)"

    -- blank out sensitive data in fields
    runSQL_ $ "UPDATE signatory_link_fields"
        <+> "   SET value = ''"
        <+> " WHERE signatory_link_fields.signatory_link_id IN"
        <+> "       (SELECT id"
        <+> "          FROM signatory_links"
        <+> "         WHERE signatory_links.document_id IN (SELECT id FROM documents_to_purge))"

    -- remove whole evidence log as it is sensitive data
    runSQL_ $ "DELETE"
        <+> "  FROM evidence_log"
        <+> " WHERE document_id IN (SELECT id FROM documents_to_purge)"

    runSQL_ $ "DROP TABLE documents_to_purge"
    return rows


-- Update utilities
updateWithoutEvidence :: (DocumentMonad m, Show a, ToSQL a) => SQL -> a -> m Bool
updateWithoutEvidence col newValue = updateDocumentWithID $ \did -> do
  runQuery01 $ "UPDATE" <+> raw (tblName tableDocuments) <+> "SET" <+> (col <+> "=" <?> newValue <+> "WHERE id =" <?> did)

updateMTimeAndObjectVersion :: DocumentMonad m  => MinutesTime -> m ()
updateMTimeAndObjectVersion mtime = updateDocumentWithID $ \did -> do
  runQuery_ . sqlUpdate "documents" $ do
       sqlSetInc "object_version"
       sqlSet "mtime" mtime
       sqlWhereEq "id" did

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
                                Log.mixlog_ message
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
                         , checkEqualBy "signatoryfields" (signatoryfields)
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
                   , checkEqualBy "documentconfirmtext" documentconfirmtext
                   , checkEqualBy "documentsharing" documentsharing
                   , checkEqualBy "documenttags" documenttags
                   , checkEqualBy "documentauthorattachments" (sort . documentauthorattachments)
                   , checkEqualBy "documentlang" documentlang
                   , checkEqualBy "documentapicallbackurl" documentapicallbackurl
                   , checkEqualBy "documentsealstatus" documentsealstatus
                   , checkEqualBy "documentsignatorylinks count" (length . documentsignatorylinks)
                   ] ++
                   concat (zipWith checkSigLink sl1 sl2)
