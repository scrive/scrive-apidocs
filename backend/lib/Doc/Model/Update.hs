{-# LANGUAGE ScopedTypeVariables #-}
module Doc.Model.Update
  ( AddDocumentAttachment(..)
  , ArchiveDocument(..)
  , AttachFile(..)
  , DetachFile(..)
  , AppendSealedFile(..)
  , AppendExtendedSealedFile(..)
  , CancelDocument(..)  , ChangeSignatoryEmail(..)
  , ChangeSignatoryPhone(..)
  , ChangeAuthenticationToViewMethod(..)
  , ChangeAuthenticationToSignMethod(..)
  , CloseDocument(..)
  , DeleteSigAttachment(..)
  , RemoveOldDrafts(..)
  , ErrorDocument(..)
  , MarkDocumentSeen(..)
  , MarkInvitationRead(..)
  , NewDocument(..)
  , PreparationToPending(..)
  , PurgeDocuments(..)
  , archiveIdleDocuments
  , RejectDocument(..)
  , ForwardSigning(..)
  , RemoveDocumentAttachments(..)
  , ResetSignatoryDetails(..)
  , RestartDocument(..)
  , ProlongPendingDocument(..)
  , ProlongTimeoutedDocument(..)
  , RestoreArchivedDocument(..)
  , ReallyDeleteDocument(..)
  , SaveDocumentForUser(..)
  , SaveSigAttachment(..)
  , SetHighlightingForPageAndSignatory(..)
  , ClearHighlightingForPageAndSignatory(..)
  , SetDaysToSign(..)
  , SetDocumentInviteTime(..)
  , SetDocumentLang(..)
  , SetDocumentFolderID(..)
  , SetDocumentSharing(..)
  , SetDocumentUnsavedDraft(..)
  , SetDocumentTags(..)
  , SetDocumentTitle(..)
  , SetEmailInvitationDeliveryStatus(..)
  , SetFieldPlacements(..)
  , SetSMSInvitationDeliveryStatus(..)
  , SetEmailConfirmationDeliveryStatus(..)
  , SetInviteText(..)
  , SetConfirmText(..)
  , SetShowHeader(..)
  , SetShowPDFDownload(..)
  , SetShowRejectOption(..)
  , SetAllowRejectReason(..)
  , SetIsReceipt(..)
  , SetShowFooter(..)
  , SetShowArrow(..)
  , AddAcceptedAuthorAttachmentsEvents(..)
  , ApproveDocument(..)
  , SignDocument(..)
  , CloneDocumentWithUpdatedAuthor(..)
  , StoreDocumentForTesting(..)
  , TemplateFromDocument(..)
  , DocumentFromTemplate(..)
  , TimeoutDocument(..)
  , PostReminderSend(..)
  , UpdateFieldsForSigning(..)
  , UpdateConsentResponsesForSigning(..)
  , UpdatePhoneAfterIdentificationToView(..)
  , UpdateAuthorUserID(..)
  , SetSigAttachments(..)
  , UpdateDraft(..)
  , ClearSignatoryEmail(..)
  , ClearSignatoryMobile(..)
  , FixClosedErroredDocument(..)
  , ConnectSignatoriesToUser(..)
  , AddNotUploadedSignatoryAttachmentsEvents(..)
  , UpdateShareableLinkHash(..)
  , updateMTimeAndObjectVersion
  , NewSignatoryAccessToken(..)
  , NewSignatoryAccessTokenWithHash(..)
  , SetDocumentApiCallbackResult(..)
  , AddCustomEvidenceEvent(..)
  , PurgeTimeoutedSignatoryAccessTokens(..)
  , ExtendSignatoryAccessTokensForAccessBeforeClosing(..)
  ) where

import Control.Arrow (second)
import Control.Monad.Base
import Control.Monad.Catch
import Crypto.RNG
import Data.Decimal (realFracToDecimal)
import Data.Int
import Data.Time
import Log
import Text.StringTemplates.Templates
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

import API.APIVersion
import Control.Monad.Trans.Instances ()
import DataRetentionPolicy
import DB
import DB.TimeZoneName (TimeZoneName, defaultTimeZoneName, withTimeZone)
import Doc.API.V2.JSON.SignatoryConsentQuestion
import Doc.Conditions
import Doc.DocStateCommon
import Doc.DocStateData
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.DocUtils
import Doc.Model.Query
import Doc.SealStatus (SealStatus(..), hasGuardtimeSignature)
import Doc.SignatoryFieldID
import Doc.SignatoryLinkID
import Doc.SignatoryScreenshots
import Doc.Tables
import Doc.Types.SignatoryAccessToken
import EID.Signature.Model
import EvidenceLog.Model
import File.FileID
import File.Storage
import Folder.Types
import IPAddress
import Log.Identifier
import MagicHash
import MinutesTime
import User.Email
import User.Model
import UserGroup.Model
import UserGroup.Types
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.Image
import Utils.Monad
import qualified DB.TimeZoneName as TimeZoneName
import qualified Doc.Screenshot as Screenshot

-- insertAsAdditionalSignatoryLink
insertAsAdditionalSignatoryLink :: (MonadDB m, MonadLog m, MonadThrow m) => DocumentID -> SignatoryLink -> m SignatoryLinkID
insertAsAdditionalSignatoryLink did sl = do
  slid <- head <$> insertSignatoryLinksOnly did [(sl {signatorylinkid = unsafeSignatoryLinkID 0 , signatoryisauthor = False})]
  insertSignatoryAttachments [(slid, att) | att <- signatoryattachments sl]
  insertSignatoryLinkFields [(slid, f) | f <- signatoryfields sl]
  insertSignatoryConsentQuestions [(slid, cq) | cq <- signatorylinkconsentquestions sl]
  return slid

insertSignatoryLinks :: (MonadDB m, MonadLog m, MonadThrow m) => DocumentID -> [SignatoryLink] -> m ()
insertSignatoryLinks _ [] = return ()
insertSignatoryLinks did links = do
  ids <- insertSignatoryLinksOnly did links
  -- Update IDs.
  let linksWithID = zipWith (\sl slid -> sl { signatorylinkid = slid }) links ids

  -- Update the document to reference the author and bail if there
  -- isn't exactly one.
  case filter signatoryisauthor linksWithID of
    [author] -> do
      runQuery_ . sqlUpdate "documents" $ do
        sqlSet "author_id" $ signatorylinkid author
        -- we denormalise due to load
        sqlSet "author_user_id" $ maybesignatory author
        sqlWhereEq "id" did
    authors -> do
      logAttention "Document doesn't have exactly one author" $ object [
          identifier did
        , identifier $ map signatorylinkid authors
        ]
      unexpectedError "Invalid document"

  insertSignatoryAttachments
    [(signatorylinkid sl, att) | sl <- linksWithID, att <- signatoryattachments sl]

  insertSignatoryLinkFields
    [(signatorylinkid sl, fld) | sl <- linksWithID, fld <- signatoryfields sl]

  insertSignatoryConsentQuestions
    [(signatorylinkid sl, cq) | sl <- linksWithID, cq <- signatorylinkconsentquestions sl]


insertSignatoryLinksOnly :: MonadDB m =>  DocumentID -> [SignatoryLink] -> m [SignatoryLinkID]
insertSignatoryLinksOnly _ [] = return []
insertSignatoryLinksOnly did links = do
  runQuery_ . sqlInsert "signatory_links" $ do
    sqlSet "document_id" did
    sqlSetListWithDefaults "id" $ map (\sl -> if (unsafeSignatoryLinkID 0 == signatorylinkid sl) then Nothing else (Just $ signatorylinkid sl)) links
    sqlSetList "user_id" $ maybesignatory <$> links
    sqlSetList "signatory_role" $ signatoryrole <$> links
    sqlSetList "sign_order"$ signatorysignorder <$> links
    sqlSetList "sign_time" $ fmap signtime <$> maybesigninfo <$> links
    sqlSetList "sign_ip" $ fmap signipnumber <$> maybesigninfo <$> links
    sqlSetList "seen_time" $ fmap signtime <$> maybeseeninfo <$> links
    sqlSetList "seen_ip" $ fmap signipnumber <$> maybeseeninfo <$> links
    sqlSetList "read_invitation" $ maybereadinvite <$> links
    sqlSetList "mail_invitation_delivery_status" $ mailinvitationdeliverystatus <$> links
    sqlSetList "sms_invitation_delivery_status" $ smsinvitationdeliverystatus <$> links
    sqlSetList "csv_contents" $ fmap csvcontents <$> signatorylinkcsvupload <$> links
    sqlSetList "deleted" $ signatorylinkdeleted <$> links
    sqlSetList "really_deleted" $ signatorylinkreallydeleted <$> links
    sqlSetList "sign_redirect_url" $ signatorylinksignredirecturl <$> links
    sqlSetList "reject_redirect_url" $ signatorylinkrejectredirecturl <$> links
    sqlSetList "rejection_time" $ signatorylinkrejectiontime <$> links
    sqlSetList "rejection_reason" $ signatorylinkrejectionreason <$> links
    sqlSetList "authentication_to_view_method" $ signatorylinkauthenticationtoviewmethod <$> links
    sqlSetList "authentication_to_view_archived_method" $ signatorylinkauthenticationtoviewarchivedmethod <$> links
    sqlSetList "authentication_to_sign_method" $ signatorylinkauthenticationtosignmethod <$> links
    sqlSetList "delivery_method" $ signatorylinkdeliverymethod <$> links
    sqlSetList "confirmation_delivery_method" $ signatorylinkconfirmationdeliverymethod <$> links
    sqlSetList "notification_delivery_method" $ signatorylinknotificationdeliverymethod <$> links
    sqlSetList "allows_highlighting" $ signatorylinkallowshighlighting <$> links
    sqlSetList "hide_pn_elog" $ signatorylinkhidepn <$> links
    sqlSetList "can_be_forwarded" $ signatorylinkcanbeforwarded <$> links
    sqlSetList "consent_title" $ signatorylinkconsenttitle <$> links
    sqlResult "id"

  fetchMany runIdentity

insertSignatoryAttachments :: MonadDB m => [(SignatoryLinkID, SignatoryAttachment)] -> m ()
insertSignatoryAttachments [] = return ()
insertSignatoryAttachments atts = runQuery_ . sqlInsert "signatory_attachments" $ do
  sqlSetList "signatory_link_id" $ map fst atts
  sqlSetList "file_id" $ map (signatoryattachmentfile . snd) atts
  sqlSetList "name" $ map (signatoryattachmentname . snd) atts
  sqlSetList "description" $ map (signatoryattachmentdescription . snd) atts
  sqlSetList "required" $ map (signatoryattachmentrequired . snd) atts

insertSignatoryLinkFields :: MonadDB m => [(SignatoryLinkID, SignatoryField)] -> m ()
insertSignatoryLinkFields [] = return ()
insertSignatoryLinkFields xs = do
  runQuery_ . sqlInsert "signatory_link_fields" $ do
    sqlSetList "signatory_link_id" ids
    sqlSetList "type" $ map fieldType fields
    sqlSetList "name_order" $ map name_order fields
    sqlSetList "custom_name" $ map custom_name fields
    sqlSetList "is_author_filled" $ map author_filled fields
    sqlSetList "value_text" $ map fieldTextValue fields
    sqlSetList "value_file_id" $ map fieldFileValue fields
    sqlSetList "value_bool" $ map fieldBoolValue fields
    sqlSetList "obligatory" $ map fieldIsObligatory fields
    sqlSetList "should_be_filled_by_author" $ map fieldShouldBeFilledBySender fields
    sqlSetList "radio_button_group_values" $ map (fmap Array1 . fieldRadioGroupValues) fields
    sqlSetList "editable_by_signatory" $ map fieldEditableBySignatory fields
    sqlSetList "custom_validation_pattern" $ map (fmap tcvPattern . fieldTextCustomValidation) fields
    sqlSetList "custom_validation_positive_example" $ map (fmap tcvPositiveExample . fieldTextCustomValidation) fields
    sqlSetList "custom_validation_tooltip" $ map (fmap tcvTooltip . fieldTextCustomValidation) fields
    sqlResult "id"
  insertFieldPlacements
    . concat
    . zipWith (\sf sfid -> [(sfid, pl) | pl <- fieldPlacements sf]) fields
    =<< fetchMany runIdentity
  where
    ids = map fst xs
    fields = map snd xs

    name_order field = case field of
      SignatoryNameField (NameField{snfNameOrder})-> Just snfNameOrder
      _ -> Nothing
    custom_name field = case field of
      SignatoryTextField (TextField{stfName})-> stfName
      SignatorySignatureField (SignatureField{ssfName})-> ssfName
      SignatoryCheckboxField (CheckboxField{schfName})-> schfName
      SignatoryRadioGroupField (RadioGroupField{srgfName}) -> srgfName
      _ -> ""
    author_filled field = case field of
      SignatoryTextField (TextField{stfFilledByAuthor})-> stfFilledByAuthor
      _ -> False

insertFieldPlacements :: MonadDB m => [(SignatoryFieldID, FieldPlacement)] -> m ()
insertFieldPlacements [] = return ()
insertFieldPlacements xs = do
  runQuery_ . sqlInsert "field_placements" $ do
    sqlSetList "signatory_field_id" ids
    sqlSetList "xrel" $ map placementxrel placements
    sqlSetList "yrel" $ map placementyrel placements
    sqlSetList "wrel" $ map placementwrel placements
    sqlSetList "hrel" $ map placementhrel placements
    sqlSetList "fsrel" $ map placementfsrel placements
    sqlSetList "page" $ map placementpage placements
    sqlSetList "tip" $ map placementtipside placements
    sqlResult "id"
  insertPlacementAnchors
    . concat
    . zipWith (\pl plid -> [(plid, anchor) | anchor <- placementanchors pl]) placements
    =<< fetchMany runIdentity
  where
    ids = map fst xs
    placements = map snd xs

insertPlacementAnchors :: MonadDB m => [(PlacementID, PlacementAnchor)] -> m ()
insertPlacementAnchors [] = return ()
insertPlacementAnchors xs = do
  runQuery_ . sqlInsert "placement_anchors" $ do
    sqlSetList "field_placement_id" ids
    sqlSetList "text" $ map placementanchortext anchors
    sqlSetList "index" $ map placementanchorindex anchors
  where
    ids = map fst xs
    anchors = map snd xs

insertDocumentTags :: MonadDB m => Bool -> DocumentID -> S.Set DocumentTag -> m (S.Set DocumentTag)
insertDocumentTags fetch did tags
  | S.null tags = return S.empty
  | otherwise = do
    let tags_list = S.toList tags
    runQuery_ . sqlInsert "document_tags" $ do
      sqlSet "document_id" did
      sqlSetList "name" $ tagname <$> tags_list
      sqlSetList "value" $ tagvalue <$> tags_list
      when fetch $ mapM_ sqlResult documentTagsSelectors
    if fetch
      then S.fromList <$> fetchMany toComposite
      else return S.empty

insertAuthorAttachments :: MonadDB m => DocumentID -> [AuthorAttachment] -> m ()
insertAuthorAttachments _ [] = return ()
insertAuthorAttachments did atts = runQuery_ . sqlInsert "author_attachments" $ do
  sqlSet "document_id" did
  sqlSetList "name" $ authorattachmentname <$> atts
  sqlSetList "required" $ authorattachmentrequired <$> atts
  sqlSetList "add_to_sealed_file" $ authorattachmentaddtosealedfile <$> atts
  sqlSetList "file_id" $ authorattachmentfileid <$> atts

insertMainFiles :: MonadDB m => DocumentID -> [MainFile] -> m ()
insertMainFiles _ [] = return ()
insertMainFiles documentid rfiles = do
  -- rfiles should be inserted with descending id: newer files come first in rfiles
  -- FIXME: this is too error prone, needs to be solved at the type level.
  let files = reverse rfiles
  runQuery_ . sqlInsert "main_files" $ do
    sqlSet "document_id" documentid
    sqlSetList "file_id" $ mainfileid <$> files
    sqlSetList "document_status" $ mainfiledocumentstatus <$> files
    sqlSetList "seal_status" $ mainfilesealstatus <$> files

insertSignatoryScreenshots
  :: ( CryptoRNG m, MonadBase IO m, MonadCatch m
     , MonadDB m, MonadFileStorage m, MonadLog m
     , MonadThrow m, MonadTime m )
  => [(SignatoryLinkID, SignatoryScreenshots)] -> m Int
insertSignatoryScreenshots l = do
  let (slids, types, times, ss) = unzip4 $ f "first" first
                                        <> f "signing" signing
                                        <> f "reference" getReferenceScreenshot
      f col part = [ (slid, col, Screenshot.time s, Screenshot.image s) | (slid, Just s) <- map (second part) l ]
  (fileids :: [FileID]) <- forM (zip types ss) $ \(t,s) -> do
    saveNewFile (t <> "_screenshot.jpeg") s
  if null slids then return 0 else
    runQuery . sqlInsert "signatory_screenshots" $ do
           sqlSetList "signatory_link_id" $ slids
           sqlSetList "type"              $ T.unpack <$> types
           sqlSetList "time"              $ times
           sqlSetList "file_id"           $ fileids

insertSignatoryConsentQuestions :: MonadDB m => [(SignatoryLinkID, SignatoryConsentQuestion)] -> m ()
insertSignatoryConsentQuestions [] = return ()
insertSignatoryConsentQuestions questions = runQuery_ . sqlInsert "signatory_link_consent_questions" $ do
  sqlSetList "signatory_link_id" $ map fst questions
  sqlSetList "position" $ take (length questions) (iterate (+1) (0 :: Int))
  sqlSetList "title" $ map (scqTitle . snd) questions
  sqlSetList "positive_option" $ map (scqPositiveOption . snd) questions
  sqlSetList "negative_option" $ map (scqNegativeOption . snd) questions
  sqlSetList "response" $ map (scqResponse . snd) questions
  sqlSetList "description_title" $ map (fmap fst . scqDescription . snd) questions
  sqlSetList "description_text" $ map (fmap snd . scqDescription . snd) questions

insertDocument :: (MonadLog m, MonadDB m, MonadThrow m)
               => Document -> m Document
insertDocument document@(Document{..}) = do
  runQuery_ . sqlInsert "documents" $ do
    sqlSet "title" documenttitle
    sqlSet "status" documentstatus
    sqlSet "type" documenttype
    sqlSet "ctime" documentctime
    sqlSet "mtime" documentmtime
    sqlSet "days_to_sign" documentdaystosign
    sqlSet "days_to_remind" documentdaystoremind
    sqlSet "timeout_time" documenttimeouttime
    sqlSet "invite_time" $ signtime <$> documentinvitetime
    sqlSet "invite_ip" $ signipnumber <$> documentinvitetime
    sqlSet "invite_text" documentinvitetext
    sqlSet "confirm_text" documentconfirmtext
    sqlSet "show_header" documentshowheader
    sqlSet "show_pdf_download" documentshowpdfdownload
    sqlSet "show_reject_option" documentshowrejectoption
    sqlSet "allow_reject_reason" documentallowrejectreason
    sqlSet "show_footer" documentshowfooter
    sqlSet "is_receipt" documentisreceipt
    sqlSet "lang" documentlang
    sqlSet "sharing" documentsharing
    sqlSet "object_version" documentobjectversion
    sqlSet "api_v1_callback_url" documentapiv1callbackurl
    sqlSet "api_v2_callback_url" documentapiv2callbackurl
    sqlSet "token" documentmagichash
    sqlSet "time_zone_name" documenttimezonename
    sqlSet "author_id" $ unsafeDocumentID 0
    sqlSet "shareable_link_hash" documentshareablelinkhash
    sqlSet "template_id" documenttemplateid
    sqlSet "from_shareable_link" documentfromshareablelink
    sqlSet "show_arrow" documentshowarrow
    sqlSet "folder_id" documentfolderid
    sqlResult "documents.id"
  did <- fetchOne runIdentity
  insertSignatoryLinks did documentsignatorylinks
  insertAuthorAttachments did documentauthorattachments
  insertMainFiles did documentmainfiles
  void $ insertDocumentTags False did documenttags
  newdocument <- dbQuery $ GetDocumentByDocumentID did
  assertEqualDocuments document newdocument
  return newdocument

----------------------------------------

insertNewDocument :: (MonadDB m, MonadThrow m, MonadLog m, CryptoRNG m) => Document -> m Document
insertNewDocument doc = do
  now <- currentTime
  magichash <- random
  let docWithTime = doc {documentmtime  = now, documentctime = now, documentmagichash = magichash}
  insertDocument docWithTime

-- Create new document based on existing one
newFromDocumentID :: (MonadDB m, MonadThrow m, MonadLog m, CryptoRNG m) => (Document -> Document) -> DocumentID -> m (Maybe Document)
newFromDocumentID f docid = do
  doc <- query $ GetDocumentByDocumentID docid
  Just <$> insertNewDocument (f doc)

newFromDocument :: (MonadDB m, MonadThrow m, MonadLog m, CryptoRNG m) => Document -> m (Maybe Document)
newFromDocument doc = Just <$> insertNewDocument doc

data AddCustomEvidenceEvent = AddCustomEvidenceEvent String Actor
instance
  (DocumentMonad m, TemplatesMonad m, MonadThrow m, MonadTime m) =>
  DBUpdate m AddCustomEvidenceEvent () where
  update (AddCustomEvidenceEvent text actor) = do
    void $ update $ InsertEvidenceEvent
      CustomEventEvidence
      (do F.value "text" text)
      actor

data ArchiveDocument = ArchiveDocument UserID Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m, MonadTime m) => DBUpdate m ArchiveDocument Bool where
  update (ArchiveDocument uid _actor) = updateDocumentWithID $ \did -> do
    now <- currentTime
    res <- runQuery $ sqlUpdate "signatory_links" $ do
        sqlSet "deleted" now

        sqlWhereExists $ sqlSelect "users" $ do
          sqlJoinOn "users AS same_usergroup_users" "(users.user_group_id = same_usergroup_users.user_group_id OR users.id = same_usergroup_users.id)"
          sqlWhere "signatory_links.user_id = users.id"

          sqlWhereUserIsDirectlyOrIndirectlyRelatedToDocument uid
          sqlWhereUserIsSelfOrCompanyAdmin

        sqlWhereExists $ sqlSelect "documents" $ do
          sqlWhere $ "signatory_links.document_id =" <?> did
          sqlWhere "documents.id = signatory_links.document_id"

          sqlWhereDocumentIsNotDeleted
          sqlWhereDocumentStatusIsOneOf [Preparation, Closed, Canceled, Timedout, Rejected, DocumentError]

    return (res > 0)

data ReallyDeleteDocument = ReallyDeleteDocument UserID Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m, MonadTime m) => DBUpdate m ReallyDeleteDocument Bool where
  update (ReallyDeleteDocument uid _actor) = updateDocumentWithID $ \did -> do
    now <- currentTime
    res <- runQuery $ sqlUpdate "signatory_links" $ do
        sqlSet "really_deleted" now

        sqlWhereExists $ sqlSelect "users" $ do
          sqlJoinOn "users AS same_usergroup_users" "(users.user_group_id = same_usergroup_users.user_group_id OR users.id = same_usergroup_users.id)"
          sqlWhere "signatory_links.user_id = users.id"

          sqlWhereUserIsDirectlyOrIndirectlyRelatedToDocument uid
          sqlWhereUserIsSelfOrCompanyAdmin

        sqlWhereExists $ sqlSelect "documents" $ do
          sqlWhere $ "signatory_links.document_id =" <?> did
          sqlWhere "documents.id = signatory_links.document_id"

          sqlWhereDocumentIsDeleted
          sqlWhereDocumentIsNotReallyDeleted
          sqlWhereDocumentStatusIsOneOf [Preparation, Closed, Canceled, Timedout, Rejected, DocumentError]

    return (res > 0)

-- | Attach a main file to a document associating it with preparation
-- status.  Any old main file in preparation status will be removed.
-- Can only be done on documents in preparation.
data AttachFile = AttachFile FileID Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m AttachFile () where
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
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m AppendSealedFile () where
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
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m AppendExtendedSealedFile () where
  update (AppendExtendedSealedFile fid status actor) = do
    updateDocumentWithID $ \did -> do
      appendSealedFile did fid status
    void $ update $ InsertEvidenceEvent
      AttachExtendedSealedFileEvidence
      (return ())
      actor

appendSealedFile :: (MonadDB m, MonadThrow m, TemplatesMonad m) => DocumentID -> FileID -> SealStatus -> m ()
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
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m FixClosedErroredDocument () where
  update (FixClosedErroredDocument _actor) = updateDocumentWithID $ \did -> do
    kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
        sqlSet "status" Closed
        sqlWhereEq "id" did
        sqlWhereEq "status" $ DocumentError

data CancelDocument = CancelDocument Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m CancelDocument () where
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

data ChangeSignatoryEmail = ChangeSignatoryEmail SignatoryLinkID (Maybe User) String Actor
instance (CryptoRNG m, DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m ChangeSignatoryEmail () where
  update (ChangeSignatoryEmail slid muser email actor) = do
    oldemail <- updateDocumentWithID $ const $ do
      oldemail :: String <- kRunAndFetch1OrThrowWhyNot runIdentity $ sqlUpdate "signatory_link_fields" $ do
             sqlFrom "signatory_link_fields AS signatory_link_fields_old"
             sqlWhere "signatory_link_fields.id = signatory_link_fields_old.id"
             sqlSet "value_text" email
             sqlResult "signatory_link_fields_old.value_text"
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
    update $ InvalidateSignatoryAccessTokens slid SignatoryAccessTokenForMailBeforeClosing
    update $ InvalidateSignatoryAccessTokens slid SignatoryAccessTokenForMailAfterClosing
    void $ update $ InsertEvidenceEvent
          ChangeSignatoryEmailEvidence
          (F.value "oldemail" oldemail >> F.value "newemail" email)
          actor

data ChangeSignatoryPhone = ChangeSignatoryPhone SignatoryLinkID String Actor
instance (CryptoRNG m, DocumentMonad m, TemplatesMonad m, MonadThrow m)
  => DBUpdate m ChangeSignatoryPhone () where
  update (ChangeSignatoryPhone slid phone actor) = do
    oldphone <- updateDocumentWithID $ const $ do
      oldphone :: String <- kRunAndFetch1OrThrowWhyNot runIdentity $ sqlUpdate "signatory_link_fields" $ do
             sqlFrom "signatory_link_fields AS signatory_link_fields_old"
             sqlWhere "signatory_link_fields.id = signatory_link_fields_old.id"
             sqlSet "value_text" phone
             sqlResult "signatory_link_fields_old.value_text"
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
    update $ InvalidateSignatoryAccessTokens slid SignatoryAccessTokenForSMSBeforeClosing
    update $ InvalidateSignatoryAccessTokens slid SignatoryAccessTokenForSMSAfterClosing
    void $ update $ InsertEvidenceEvent
          ChangeSignatoryPhoneEvidence
          (F.value "oldphone" oldphone >> F.value "newphone" phone)
          actor

data ChangeAuthenticationToViewMethod =
  ChangeAuthenticationToViewMethod
    SignatoryLinkID
    AuthenticationKind
    AuthenticationToViewMethod
    (Maybe Text)
    (Maybe Text)
    Actor

instance (DocumentMonad m, TemplatesMonad m, MonadThrow m, MonadTime m) =>
  DBUpdate m ChangeAuthenticationToViewMethod () where

  update (ChangeAuthenticationToViewMethod slid
          authKind newAuthToView
          mSSN mPhone actor) = do
    updateDocumentWithID $ const $ do
      -- Get the signatory link before the update
      sl <- (\did -> dbQuery $ GetSignatoryLinkByID did slid)
            =<< theDocumentID
      let ssnField    = getFieldByIdentity PersonalNumberFI $ signatoryfields sl
          mobileField = getFieldByIdentity MobileFI $ signatoryfields sl
      -- Update the authentication to view method
      kRun1OrThrowWhyNot $ sqlUpdate "signatory_links" $ do
        let authField = case authKind of
              AuthenticationToView         ->
                "authentication_to_view_method"
              AuthenticationToViewArchived ->
                "authentication_to_view_archived_method"
        sqlSet authField newAuthToView
        sqlWhereEq "id" slid
        sqlWhereSigningPartyHasNotSignedOrApproved
        sqlWhereSigningPartyHasNotAuthenticatedToView
        sqlWhereExists $ sqlSelect "documents" $ do
          sqlWhere "documents.id = signatory_links.document_id"
          sqlWhereDocumentStatusIs Pending
      -- If the AuthenticationToViewMethod does *not* need a personal number we
      -- need to check if it is still needed, otherwise make the field
      -- non-obligatory If it *does* need it, we need to make sure the field is
      -- there and maybe update the value
      let authToSign = signatorylinkauthenticationtosignmethod sl
          otherAuthToView = case authKind of
            AuthenticationToView         ->
              signatorylinkauthenticationtoviewarchivedmethod sl
            AuthenticationToViewArchived ->
              signatorylinkauthenticationtoviewmethod sl
      case authToViewNeedsPersonalNumber newAuthToView of
        -- When new authentication to view method doesn't need a personal
        -- number, we check if the authentication to view method of the other
        -- kind or authentication to sign need it or if there are placements for
        -- this field. We set it to optional if none of the above is true.
        False -> when (and
          [ not $ authToViewNeedsPersonalNumber otherAuthToView
          , not $ authToSignNeedsPersonalNumber authToSign
          , maybe False (null . fieldPlacements) ssnField
          ]) $
          runQuery_ $ sqlUpdate "signatory_link_fields" $ do
           sqlSet "obligatory" False
           sqlWhereEq "signatory_link_id" slid
           sqlWhereEq "type" PersonalNumberFT
        -- If the new AuthenticationToViewMethod needs a personal number,
        -- we either alter the existing field or add it to the signatory
        -- Maybe setting the value too along the way
        True -> do
          case ssnField of
            Nothing ->
              runQuery_ . sqlInsert "signatory_link_fields" $
              do sqlSet "signatory_link_id" slid
                 sqlSet "obligatory" True
                 sqlSet "value_text" $ fromMaybe "" mSSN
                 sqlSet "type" PersonalNumberFT
            Just _ ->
              kRun1OrThrowWhyNot $
              sqlUpdate "signatory_link_fields" $
              do sqlSet "obligatory" True
                 case mSSN of
                   Nothing  -> return ()
                   Just ssn -> sqlSet "value_text" ssn
                 sqlWhereEq "signatory_link_id" slid
                 sqlWhereEq "type" PersonalNumberFT
          -- Add event to the EvidenceLog if the new personal number is
          -- different to the old one.
          let oldSSN = fromMaybe "" $ fieldTextValue =<< ssnField
              newSSN = fromMaybe "" mSSN
          when (isJust mSSN && newSSN /= oldSSN) $ do
            sl' <- (\did -> dbQuery $ GetSignatoryLinkByID did slid)
                   =<< theDocumentID
            void $ update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
              UpdateFieldPersonalNumberEvidence
              (do F.value "value" newSSN
                  F.value "previousvalue" oldSSN
                  F.value "hide_pn" $ signatorylinkhidepn sl'
                  when (newSSN == "") (F.value "newblank" True)
                  when (oldSSN == "") (F.value "prvblank" True)
              )
              (Just sl')
              Nothing
              actor
      -- If the AuthenticationToViewMethod needs a mobile number field, check
      -- if it exists for the signatory and make sure it is there, optionally
      -- also update with the value supplied
      case authToViewNeedsMobileNumber newAuthToView of
        -- When new authentication to view method doesn't need a mobile number,
        -- we check if the authentication to view method of the other kind or
        -- authentication to sign need it or if there are placements for this
        -- field. We set it to optional if none of the above is true.
        False -> when (and
          [ not $ authToViewNeedsMobileNumber otherAuthToView
          , not $ authToSignNeedsMobileNumber authToSign
          , maybe False (null . fieldPlacements) mobileField
          ]) $
          runQuery_ $ sqlUpdate "signatory_link_fields" $ do
           sqlSet "obligatory" False
           sqlSet "editable_by_signatory" False
           sqlWhereEq "signatory_link_id" slid
           sqlWhereEq "type" MobileFT
        True -> do
          let oldPhone = fromMaybe "" $ fieldTextValue =<< mobileField
              newPhone = fromMaybe "" mPhone
          case mobileField of
            Nothing -> runQuery_ . sqlInsert "signatory_link_fields" $ do
              sqlSet "signatory_link_id" slid
              sqlSet "value_text" newPhone
              sqlSet "type" MobileFT
              case authKind of
                AuthenticationToView -> do
                  sqlSet "obligatory" False
                  sqlSet "editable_by_signatory" False
                AuthenticationToViewArchived -> do
                  -- Signatory needs to fill these out during signing in order to
                  -- be able to authenticate to view after.
                  sqlSet "obligatory" True
                  sqlSet "editable_by_signatory" True
            Just _ -> kRun1OrThrowWhyNot $ sqlUpdate "signatory_link_fields" $ do
              sqlSet "value_text" $ fromMaybe oldPhone mPhone
              sqlWhereEq "signatory_link_id" slid
              sqlWhereEq "type" MobileFT
              case authKind of
                AuthenticationToView         -> return ()
                AuthenticationToViewArchived -> do
                  -- Signatory needs to fill these out during signing in order to
                  -- be able to authenticate to view after.
                  sqlSet "obligatory" True
                  sqlSet "editable_by_signatory" True
          -- Add an EvidenceLog event if the value changed
          when (newPhone /= oldPhone) $ do
            sl' <- theDocumentID >>= \did ->
              dbQuery $ GetSignatoryLinkByID did slid
            void $ update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
              UpdateFieldMobileEvidence
              (do F.value "value" newPhone
                  F.value "previousvalue" oldPhone
                  when (newPhone == "") (F.value "newblank" True)
                  when (oldPhone == "") (F.value "prvblank" True)
              )
              (Just sl')
              Nothing
              actor

      -- Finally, add event in EvidenceLog for changed authentication
      sl' <- (\did -> dbQuery $ GetSignatoryLinkByID did slid)
             =<< theDocumentID
      addChangeAuthenticationToViewEvidenceEvent sl'
        (signatorylinkauthenticationtoviewmethod sl, newAuthToView)

    where
      insertEvidenceFor
        :: (DocumentMonad m, TemplatesMonad m, MonadThrow m)
        => SignatoryLink -> CurrentEvidenceEventType -> m ()
      insertEvidenceFor sl e = void . update $
        InsertEvidenceEventWithAffectedSignatoryAndMsg e (return ())
                                                       (Just sl) Nothing actor

      addChangeAuthenticationToViewEvidenceEvent
        :: (DocumentMonad m, TemplatesMonad m, MonadThrow m)
        => SignatoryLink
        -> (AuthenticationToViewMethod, AuthenticationToViewMethod)
        -> m ()
      addChangeAuthenticationToViewEvidenceEvent sl (authtoviewfrom, authtoviewto) = do
        mapM_ (insertEvidenceFor sl) $ case authKind of
              AuthenticationToView         -> authToViewChangeEvidence authtoviewfrom authtoviewto
              AuthenticationToViewArchived -> authToViewArchivedChangeEvidence authtoviewfrom authtoviewto

data ChangeAuthenticationToSignMethod = ChangeAuthenticationToSignMethod
  SignatoryLinkID
  AuthenticationToSignMethod
  (Maybe Text)
  (Maybe Text)
  Actor

instance (DocumentMonad m, TemplatesMonad m, MonadThrow m, MonadTime m) => DBUpdate m ChangeAuthenticationToSignMethod () where
  update (ChangeAuthenticationToSignMethod slid newAuthToSign mSSN mPhone actor) = do
    updateDocumentWithID $ const $ do
      -- Get the SignatoryLink before the updates
      sl <- theDocumentID >>= \did -> dbQuery $ GetSignatoryLinkByID did slid
      let oldAuthToSign = signatorylinkauthenticationtosignmethod sl
          slSSNField    = getFieldByIdentity PersonalNumberFI $ signatoryfields sl
          slMobileField = getFieldByIdentity MobileFI $ signatoryfields sl
          slAuthToView  = signatorylinkauthenticationtoviewmethod sl
          slAuthToViewArchived = signatorylinkauthenticationtoviewarchivedmethod sl
          maybeFieldHasPlacements :: Maybe SignatoryField -> Bool
          maybeFieldHasPlacements Nothing = False
          maybeFieldHasPlacements (Just sf) = not . null $ fieldPlacements sf
      -- Set the new authentication method in signatory_links
      kRun1OrThrowWhyNot $ sqlUpdate "signatory_links" $ do
        sqlSet "authentication_to_sign_method" newAuthToSign
        sqlWhereEq "id" slid
        sqlWhereSignatoryRoleIsSigningParty
        sqlWhereSigningPartyHasNotSignedOrApproved
        sqlWhereExists $ sqlSelect "documents" $ do
          sqlWhere "documents.id = signatory_links.document_id"
          sqlWhereDocumentStatusIs Pending
      -- Make previously obligatory fields (due to authentication to sign)
      -- non-obligatory when the new authentication is different and they don't
      -- have placements
      case (newAuthToSign == oldAuthToSign, oldAuthToSign) of
        -- Nothing to change!
        (True, _)                             -> return ()
        (False, StandardAuthenticationToSign) -> return ()
        -- Make PersonalNumber non-obligatory if conditions are satisfied
        (False, SEBankIDAuthenticationToSign) -> when
          ( maybeFieldHasPlacements slSSNField
            && (not $ authToViewNeedsPersonalNumber slAuthToView)
            && (not $ authToViewNeedsPersonalNumber slAuthToViewArchived)
          )
          ( kRun1OrThrowWhyNot $ sqlUpdate "signatory_link_fields" $ do
              sqlSet "obligatory" False
              sqlWhereEq "signatory_link_id" slid
              sqlWhereEq "type" PersonalNumberFT
          )
        -- Make PersonalNumber non-obligatory if conditions are satisfied
        (False, NOBankIDAuthenticationToSign) -> when
          ( maybeFieldHasPlacements slSSNField
            && (not $ authToViewNeedsPersonalNumber slAuthToView)
            && (not $ authToViewNeedsPersonalNumber slAuthToViewArchived)
          )
          ( kRun1OrThrowWhyNot $ sqlUpdate "signatory_link_fields" $ do
              sqlSet "obligatory" False
              sqlWhereEq "signatory_link_id" slid
              sqlWhereEq "type" PersonalNumberFT
          )
        -- Make PersonalNumber non-obligatory if conditions are satisfied
        (False, DKNemIDAuthenticationToSign) -> when
          ( maybeFieldHasPlacements slSSNField
            && (not $ authToViewNeedsPersonalNumber slAuthToView)
            && (not $ authToViewNeedsPersonalNumber slAuthToViewArchived)
          )
          ( kRun1OrThrowWhyNot $ sqlUpdate "signatory_link_fields" $ do
              sqlSet "obligatory" False
              sqlWhereEq "signatory_link_id" slid
              sqlWhereEq "type" PersonalNumberFT
          )
        -- Make MobileNumber non-obligatory if conditions are satisfied
        (False, SMSPinAuthenticationToSign) -> when
          ( maybeFieldHasPlacements slMobileField
            && (not $ authToViewNeedsMobileNumber slAuthToView)
            && (not $ authToViewNeedsMobileNumber slAuthToViewArchived)
          )
          ( kRun1OrThrowWhyNot $ sqlUpdate "signatory_link_fields" $ do
              sqlSet "obligatory" False
              sqlWhereEq "signatory_link_id" slid
              sqlWhereEq "type" MobileFT
          )
      -- If newAuthToSign needs PersonalNumber we need to make sure the field
      -- exists and is obligatory, and maybe set to the value provided
      when (authToSignNeedsPersonalNumber newAuthToSign) $ do
        let oldSSN = fromMaybe "" $ fieldTextValue =<< slSSNField
            newSSN = fromMaybe "" mSSN
        case slSSNField of
             Just _  -> kRun1OrThrowWhyNot $ sqlUpdate "signatory_link_fields" $ do
               case mSSN of
                    Just ssn -> sqlSet "value_text" ssn
                    Nothing -> return ()
               sqlSet "obligatory" True
               sqlWhereEq "signatory_link_id" slid
               sqlWhereEq "type" PersonalNumberFT
             -- Note: default in table for `obligatory` is true
             Nothing -> runQuery_ . sqlInsert "signatory_link_fields" $ do
               sqlSet "signatory_link_id" slid
               sqlSet "value_text" $ fromMaybe "" mSSN
               sqlSet "type" PersonalNumberFT
        -- Add an EvidenceLog event if the value changed
        when (isJust mSSN && newSSN /= oldSSN) $ do
          sl' <- theDocumentID >>= \did -> dbQuery $ GetSignatoryLinkByID did slid
          void $ update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
            UpdateFieldPersonalNumberEvidence
            (do F.value "value" newSSN
                F.value "previousvalue" oldSSN
                F.value "hide_pn" $ signatorylinkhidepn sl'
                when (newSSN == "") (F.value "newblank" True)
                when (oldSSN == "") (F.value "prvblank" True)
            )
            (Just sl')
            Nothing
            actor
      -- If newAuthToSign needs MobileNumber we need to make sure the field
      -- exists and is obligatory, and maybe set to the value provided
      when (authToSignNeedsMobileNumber newAuthToSign) $ do
        let oldPhone = fromMaybe "" $ fieldTextValue =<< slMobileField
            newPhone = fromMaybe "" mPhone
        case slMobileField of
             Just _  -> kRun1OrThrowWhyNot $ sqlUpdate "signatory_link_fields" $ do
               case mPhone of
                    Just phone -> sqlSet "value_text" phone
                    Nothing -> return ()
               sqlSet "obligatory" True
               sqlWhereEq "signatory_link_id" slid
               sqlWhereEq "type" MobileFT
             -- Note: default in table for `obligatory` is true
             Nothing -> runQuery_ . sqlInsert "signatory_link_fields" $ do
               sqlSet "signatory_link_id" slid
               sqlSet "value_text" $ fromMaybe "" mPhone
               sqlSet "type" MobileFT
               sqlSet "editable_by_signatory" $ False
        -- Add an EvidenceLog event if the value changed
        when (newPhone /= oldPhone) $ do
          sl' <- theDocumentID >>= \did -> dbQuery $ GetSignatoryLinkByID did slid
          void $ update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
            UpdateFieldMobileEvidence
            (do F.value "value" newPhone
                F.value "previousvalue" oldPhone
                when (newPhone == "") (F.value "newblank" True)
                when (oldPhone == "") (F.value "prvblank" True)
            )
            (Just sl')
            Nothing
            actor
      -- Update document mtime and object_version
      updateMTimeAndObjectVersion (actorTime actor)
      sl' <- theDocumentID >>= \did -> dbQuery $ GetSignatoryLinkByID did slid
      let insertEvidence e = void $ update $
            InsertEvidenceEventWithAffectedSignatoryAndMsg e (return ()) (Just sl') Nothing actor
      -- Add event for changing AuthenticationToSignMethod
      mapM_ insertEvidence (authToSignChangeEvidence oldAuthToSign newAuthToSign)

data PreparationToPending = PreparationToPending Actor TimeZoneName
instance (DocumentMonad m, TemplatesMonad m, MonadMask m) => DBUpdate m PreparationToPending () where
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
            let timestamp = formatTime' "%F" time <> " " <> (T.unpack $ TimeZoneName.toString tzn)
            -- Need to temporarily set session timezone to any one
            -- that recognizes daylight savings so that the day
            -- interval addition advances the time properly across DST changes
            -- (i.e., so that we stay on midnight)
            -- http://www.postgresql.org/docs/9.2/static/functions-datetime.html
            withTimeZone defaultTimeZoneName $ do
              lang :: Lang <- kRunAndFetch1OrThrowWhyNot runIdentity $ sqlUpdate "documents" $ do
                sqlSet "status" Pending
                sqlSetCmd "timeout_time" $ "cast (" <?> timestamp <+> "as timestamp with time zone)"
                            <+> "+ ((interval '1 day') * documents.days_to_sign) + (interval '23 hours 59 minutes 59 seconds')" -- This interval add almost one they from description above.
                sqlResult "lang"
                sqlWhereDocumentIDIs docid
                sqlWhereDocumentTypeIs Signable
                sqlWhereDocumentStatusIs Preparation

              runQuery_ . sqlUpdate "signatory_links" $ do
                sqlSet "csv_contents" (Nothing :: Maybe String)
                sqlWhereEq "document_id" docid

              -- It is possible for customer to choose an invalid settings for the document, when
              -- using Scrive-online, Salesforce or API. Documents with API or Pad delivery methods
              -- cannot be forwarded.
              -- Instead of reporting the error to customer we decided to just silently fix it.
              runQuery_ . sqlUpdate "signatory_links" $ do
                sqlSet "can_be_forwarded" False
                sqlWhereIn "delivery_method" [PadDelivery, APIDelivery]
                sqlWhereEq "document_id" docid

              runQuery_ $ "SELECT timeout_time FROM documents WHERE id =" <?> docid
              tot <- fetchOne runIdentity
              updateMTimeAndObjectVersion (actorTime actor)
              return (lang, tot)
    void $ update $ InsertEvidenceEvent
                PreparationToPendingEvidence
                (  F.value "timezone" (TimeZoneName.toString tzn)
                >> F.value "lang" (showt lang)
                >> F.value "timeouttime" (formatTimeUTC tot))
                actor

data CloseDocument = CloseDocument Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m CloseDocument () where
  update (CloseDocument actor) = do
    updateDocumentWithID $ \docid -> do
      kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
                   sqlSet "status" Closed
                   sqlWhereDocumentIDIs docid
                   sqlWhereDocumentTypeIs Signable
                   sqlWhereDocumentStatusIs Pending
                   sqlWhereAllSigningPartiesHaveSignedOrApproved
      updateMTimeAndObjectVersion (actorTime actor)
    void $ update $ InsertEvidenceEvent
                CloseDocumentEvidence
                (return ())
                actor

data DeleteSigAttachment = DeleteSigAttachment SignatoryLinkID SignatoryAttachment Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m DeleteSigAttachment () where
  update (DeleteSigAttachment slid sa actor) = do
    (saname::String) <- updateDocumentWithID $ const $ do
      kRunAndFetch1OrThrowWhyNot runIdentity $ sqlUpdate "signatory_attachments" $ do
        sqlFrom "signatory_links"
        sqlWhere "signatory_links.id = signatory_attachments.signatory_link_id"
        sqlSet "file_id" (Nothing :: Maybe FileID)
        sqlResult "signatory_attachments.name"
        sqlWhereEq "signatory_attachments.name" (signatoryattachmentname sa)
        sqlWhereSignatoryLinkIDIs slid
        sqlWhereSigningPartyHasNotSignedOrApproved

    void $ update $ InsertEvidenceEvent
                    DeleteSigAttachmentEvidence
                    (F.value "name" saname)
                    actor


data ErrorDocument = ErrorDocument CurrentEvidenceEventType (F.Fields Identity ()) Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m ErrorDocument () where
  update (ErrorDocument event textFields actor) = do
    updateDocumentWithID $ \docid -> do
      kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
        sqlSet "status" $ DocumentError
        sqlWhereDocumentIDIs docid
    void $ update $ InsertEvidenceEvent event textFields actor

data MarkDocumentSeen = MarkDocumentSeen SignatoryLinkID Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m MarkDocumentSeen () where
  update (MarkDocumentSeen slid actor) = updateDocumentWithID $ \did -> do
        let time = actorTime actor
            ipnumber = fromMaybe noIP $ actorIP actor
        kRun1OrThrowWhyNotAllowIgnore $ sqlUpdate "signatory_links" $ do
            sqlSet "seen_time" time
            sqlSet "seen_ip" ipnumber

            sqlWhereExists $ sqlSelect "documents" $ do
              sqlWhere "documents.id = signatory_links.document_id"
              sqlWhereDocumentIDIs did
              sqlWhereSignatoryLinkIDIs slid
              sqlWhereDocumentTypeIs (Signable)
              sqlIgnore $ sqlWhere "signatory_links.seen_time IS NULL"
              sqlIgnore $ sqlWhere "signatory_links.sign_time IS NULL"
              sqlWhereDocumentStatusIsOneOf [Pending, Timedout, Canceled, DocumentError, Rejected]

data MarkInvitationRead = MarkInvitationRead SignatoryLinkID Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m, MonadTime m) => DBUpdate m MarkInvitationRead Bool where
  update (MarkInvitationRead slid actor) = do
    success <- updateDocumentWithID $ \did -> do
        let time = actorTime actor
        runQuery01 . sqlUpdate "signatory_links" $ do
                      sqlSet "read_invitation" time
                      sqlWhereEq "id" slid
                      sqlWhereEq "document_id" did
                      sqlWhere "read_invitation IS NULL"
    sig <- theDocumentID >>= \did -> query $ GetSignatoryLinkByID did slid
    void $ update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
        MarkInvitationReadEvidence
        (return ())
        (Just sig)
        Nothing
        actor
    return success

data NewDocument = NewDocument User Text DocumentType TimeZoneName Int Actor (Maybe FolderID)
instance (CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m, TemplatesMonad m, MonadBase IO m) => DBUpdate m NewDocument Document where
  update (NewDocument user title documenttype timezone nrOfOtherSignatories actor desiredFolderId) = do
    let ctime = actorTime actor
    authorFields <- signatoryFieldsFromUser user
    let authorlink0 = signLinkFromDetails' authorFields True
                      SignatoryRoleSigningParty (SignOrder 1) []

    let authorlink = authorlink0 { maybesignatory = Just $ userid user }

    othersignatories <- sequence $ replicate nrOfOtherSignatories $ do
                          return $ signLinkFromDetails' emptySignatoryFields False
                                   SignatoryRoleSigningParty (SignOrder 2) []

    token <- random
    let doc = defaultDocument
                  { documenttitle                = title
                  , documentsignatorylinks       = authorlink : othersignatories
                  , documenttype                 = documenttype
                  , documentlang                 = getLang user
                  , documentctime                = ctime
                  , documentmtime                = ctime
                  , documentauthorattachments    = []
                  , documentmagichash            = token
                  , documenttimezonename         = timezone
                  , documentfolderid             = desiredFolderId <|> userhomefolderid user
                  }

    insertDocument doc


data ForwardSigning = ForwardSigning SignatoryLink (Maybe Text) [(FieldIdentity, Text)] Actor
instance (CryptoRNG m, DocumentMonad m, TemplatesMonad m, MonadThrow m, MonadLog m) =>
  DBUpdate m ForwardSigning SignatoryLinkID where
  update (ForwardSigning sl message fieldsWithVTexts actor) = do
    updateDocumentWithID $ \docid -> do
      let originalsl = signatorylinkid sl
      newslid <- insertAsAdditionalSignatoryLink docid $ sl {
            signatorylinkid = unsafeSignatoryLinkID 0
          , signatoryisauthor = False
          , maybesignatory = Nothing
          , maybesigninfo = Nothing
          , maybeseeninfo = Nothing
          , mailinvitationdeliverystatus = Unknown
          , smsinvitationdeliverystatus = Unknown
          , signatoryfields = updatedFields fieldsWithVTexts (signatoryfields sl)
        }

      kRun1OrThrowWhyNot $ sqlUpdate "signatory_links" $ do
        sqlSet "signatory_role" $ if isApprover sl
          then SignatoryRoleForwardedApprover
          else SignatoryRoleForwardedSigningParty
        sqlWhereSignatoryLinkIDIs originalsl

      runQuery_ $ sqlDelete "field_placements" $ do
        sqlWhereInSql "signatory_field_id" $ sqlSelect "signatory_link_fields" $ do
            sqlWhereEq "signatory_link_id" originalsl
            sqlResult "id"

      runQuery_ $ sqlDelete "signatory_link_fields" $ do
        sqlWhereEq "signatory_link_id" originalsl
        sqlWhereIn "type" [CheckboxFT, SignatureFT, RadioGroupFT]

      runQuery_ $ sqlDelete "signatory_attachments" $ do
        sqlWhereEq "signatory_link_id" originalsl

      nsl <- dbQuery $ GetSignatoryLinkByID docid newslid
      void $ update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
        ForwardedSigningEvidence (return ()) (Just nsl) message actor

      updateMTimeAndObjectVersion (actorTime actor)

      return newslid
    where
      updatedFields :: [(FieldIdentity, Text) ] -> [SignatoryField] -> [SignatoryField]
      updatedFields [] slfs = slfs
      updatedFields (tf:fs) slfs = updatedFields fs $ updateTextField tf slfs

      updateTextField :: (FieldIdentity, Text) -> [SignatoryField] -> [SignatoryField]
      updateTextField _ [] = []
      updateTextField (fi,t) (slf:slfs) =
        let rest = (updateTextField (fi,t) slfs)
        in if (fi == fieldIdentity slf)
          then (setTextValue t slf):rest
          else slf:rest

data RejectDocument = RejectDocument SignatoryLinkID Bool (Maybe Text) Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) =>
  DBUpdate m RejectDocument () where
  update (RejectDocument slid byApprover customtext actor) = do
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
        sqlWhereSignatoryRoleIsSigningPartyOrApprover
        sqlWhereSigningPartyHasNotSignedOrApproved
        sqlWhereDocumentIDIs docid
        sqlWhereSignatoryLinkIDIs slid
      updateMTimeAndObjectVersion (actorTime actor)
    let event = if byApprover
          then RejectDocumentByApproverEvidence
          else RejectDocumentEvidence
    void $ update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
      event (return ()) Nothing customtext actor

data RestartDocument = RestartDocument Document Actor
instance (CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m, TemplatesMonad m) => DBUpdate m RestartDocument (Maybe Document) where
  update (RestartDocument doc actor) = do
    mndoc <- tryToGetRestarted
    case mndoc of
      Right newdoc -> do
        let newSls d = filter (not . isForwarded) $ documentsignatorylinks d
        md <- newFromDocument $ newdoc {documentsignatorylinks = newSls newdoc}
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
        logAttention "Document restart failed" $ object [
            identifier $ documentid doc
          , "error" .= err
          ]
        return Nothing
   where

    tryToGetRestarted =
      if (documentstatus doc `notElem` [Canceled, Timedout, Rejected])
      then return $ Left $ "Can't restart document with " <> (showt $ documentstatus doc) <> " status"
      else do
             doc' <- clearSignInfofromDoc
             return $ Right doc'

    clearSignInfofromDoc = do
      newSignLinks <- forM (documentsignatorylinks doc) $ \sl -> do
            let newFields = for (signatoryfields sl) $ \f ->
                  case f of
                    SignatorySignatureField sf ->
                      SignatorySignatureField $ sf { ssfValue = Nothing }
                    SignatoryRadioGroupField rgf ->
                      SignatoryRadioGroupField $ rgf{srgfSelectedValue = Nothing}

                    _ -> f
            return $ defaultSignatoryLink
                 { signatorylinkid            = (unsafeSignatoryLinkID 0)
                 , signatoryfields            = newFields
                 , signatoryisauthor          = signatoryisauthor sl
                 , signatoryrole              = signatoryrole sl
                 , signatorysignorder         = signatorysignorder sl
                 , signatorylinkcsvupload       = signatorylinkcsvupload sl
                 , signatoryattachments         = signatoryattachments sl
                 , signatorylinksignredirecturl = signatorylinksignredirecturl sl
                 , signatorylinkrejectredirecturl = signatorylinkrejectredirecturl sl
                 , signatorylinkauthenticationtoviewmethod = signatorylinkauthenticationtoviewmethod sl
                 , signatorylinkauthenticationtosignmethod = signatorylinkauthenticationtosignmethod sl
                 , signatorylinkdeliverymethod       = signatorylinkdeliverymethod sl
                 , signatorylinkconfirmationdeliverymethod       = signatorylinkconfirmationdeliverymethod sl
                 , signatorylinknotificationdeliverymethod       = signatorylinknotificationdeliverymethod sl
                 , signatorylinkhidepn = signatorylinkhidepn sl
                 , signatorylinkcanbeforwarded = signatorylinkcanbeforwarded sl
                 , signatorylinkallowshighlighting = signatorylinkallowshighlighting sl
                 , maybesignatory = if (isAuthor sl) then maybesignatory sl else Nothing
                 }
      return doc {documentstatus = Preparation,
                  documenttimeouttime = Nothing,
                  documentsignatorylinks = newSignLinks
                 }

data RestoreArchivedDocument = RestoreArchivedDocument User Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m RestoreArchivedDocument () where
  update (RestoreArchivedDocument user _actor) = updateDocumentWithID $ \did -> do
    kRunManyOrThrowWhyNot $ sqlUpdate "signatory_links" $ do

      sqlSet "deleted" (Nothing :: Maybe UTCTime)

      sqlWhereExists $ sqlSelect "users" $ do
          sqlJoinOn "users AS same_usergroup_users" "(users.user_group_id = same_usergroup_users.user_group_id OR users.id = same_usergroup_users.id)"
          sqlWhere "signatory_links.user_id = users.id"

          sqlWhereUserIsDirectlyOrIndirectlyRelatedToDocument (userid user)
          sqlWhereUserIsSelfOrCompanyAdmin

      sqlWhereExists $ sqlSelect "documents" $ do
          sqlJoinOn "users AS same_usergroup_users" "TRUE"

          sqlWhere "documents.purged_time IS NULL"

          sqlWhere $ "signatory_links.document_id =" <?> did
          sqlWhere "documents.id = signatory_links.document_id"

{- |
    Links up a signatory link to a user account.  This should happen when
      \1. a document moves from preparation to pending more
      \2. a signer creates an account after signing to save their document
      \3. the email of a signatory is corrected to that of an existing user
-}
data SaveDocumentForUser = SaveDocumentForUser User SignatoryLinkID
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SaveDocumentForUser Bool where
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
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SaveSigAttachment () where
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

    void $ update $ InsertEvidenceEvent
        SaveSigAttachmentEvidence
        (do F.value "name" name
            F.value "description" $ signatoryattachmentdescription sigattach)
        actor

data SetHighlightingForPageAndSignatory = SetHighlightingForPageAndSignatory SignatoryLink Int32 FileID Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetHighlightingForPageAndSignatory () where
  update (SetHighlightingForPageAndSignatory sl page fid actor) = do
    updateDocumentWithID $ const $ do
      runQuery_ . sqlDelete "highlighted_pages" $ do
         sqlWhereEq "signatory_link_id" $ signatorylinkid sl
         sqlWhereEq "page" page
      runQuery_ . sqlInsert "highlighted_pages" $ do
         sqlSet "signatory_link_id" $ signatorylinkid sl
         sqlSet "page"  page
         sqlSet "file_id"  fid
      void $ update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
        PageHighlightingAdded
        (F.value "pageno" page)
        (Just sl)
        Nothing
        actor

data ClearHighlightingForPageAndSignatory = ClearHighlightingForPageAndSignatory SignatoryLink Int32 Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m ClearHighlightingForPageAndSignatory () where
  update (ClearHighlightingForPageAndSignatory sl page actor) = do
    updateDocumentWithID $ const $ do
      runQuery_ . sqlDelete "highlighted_pages" $ do
         sqlWhereEq "signatory_link_id" $ signatorylinkid sl
         sqlWhereEq "page" page
      void $ update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
        PageHighlightingCleared
        (F.value "pageno" page)
        (Just sl)
        Nothing
        actor

data SetFieldPlacements = SetFieldPlacements SignatoryFieldID [FieldPlacement]
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetFieldPlacements () where
  -- Delete existing fields and reinsert them
  update (SetFieldPlacements fieldid placements) = updateDocumentWithID $ const $ do
    runQuery_ . sqlDelete "field_placements" $ do
      sqlWhereEq "signatory_field_id" fieldid
    insertFieldPlacements [(fieldid, pl) | pl <- placements]

data SetDocumentTags = SetDocumentTags (S.Set DocumentTag) Actor
instance (DocumentMonad m, TemplatesMonad m) => DBUpdate m SetDocumentTags Bool where
  update (SetDocumentTags doctags _actor) = updateDocumentWithID $ \did -> do
    oldtags <- query $ GetDocumentTags did
    let changed = doctags /= oldtags
    if changed
      then do
        runQuery_ . sqlDelete "document_tags" $ do
          sqlWhereEq "document_id" did
        newtags <- insertDocumentTags True did doctags
        return $ S.size newtags == S.size doctags
      else
        return True


data SetDocumentInviteTime = SetDocumentInviteTime UTCTime Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetDocumentInviteTime () where
  update (SetDocumentInviteTime invitetime actor) = updateDocumentWithID $ \did -> do
    let ipaddress  = fromMaybe noIP $ actorIP actor
    kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
       sqlSet "invite_time" invitetime
       sqlSet "invite_ip" ipaddress
       sqlWhereDocumentIDIs did

data SetInviteText = SetInviteText Text Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetInviteText Bool where
  update (SetInviteText text _actor) = updateWithoutEvidence "invite_text" text

data SetConfirmText = SetConfirmText Text Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetConfirmText Bool where
  update (SetConfirmText text _actor) = updateWithoutEvidence "confirm_text" text



data SetShowHeader = SetShowHeader Bool Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetShowHeader Bool where
  update (SetShowHeader bool _actor) = updateWithoutEvidence "show_header" bool

data SetShowPDFDownload = SetShowPDFDownload Bool Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetShowPDFDownload Bool where
  update (SetShowPDFDownload bool _actor) = updateWithoutEvidence "show_pdf_download" bool

data SetShowRejectOption = SetShowRejectOption Bool Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetShowRejectOption Bool where
  update (SetShowRejectOption bool _actor) = updateWithoutEvidence "show_reject_option" bool

data SetAllowRejectReason = SetAllowRejectReason Bool Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetAllowRejectReason Bool where
  update (SetAllowRejectReason bool _actor) = updateWithoutEvidence "allow_reject_reason" bool

data SetIsReceipt = SetIsReceipt Bool Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetIsReceipt Bool where
  update (SetIsReceipt bool _actor) = updateWithoutEvidence "is_receipt" bool

data SetShowFooter = SetShowFooter Bool Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetShowFooter Bool where
  update (SetShowFooter bool _actor) = updateWithoutEvidence "show_footer" bool

data SetShowArrow = SetShowArrow Bool Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetShowArrow Bool where
  update (SetShowArrow bool _actor) = updateWithoutEvidence "show_arrow" bool

data SetDaysToSign = SetDaysToSign Int32 Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetDaysToSign Bool where
  update (SetDaysToSign days _actor) = updateWithoutEvidence "days_to_sign" days

data SetDaysToRemind = SetDaysToRemind (Maybe Int32) Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetDaysToRemind Bool where
  update (SetDaysToRemind days _actor) = updateWithoutEvidence "days_to_remind" days

data SetDocumentTitle = SetDocumentTitle Text Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetDocumentTitle Bool where
  update (SetDocumentTitle doctitle _actor) = updateWithoutEvidence "title" doctitle

data SetDocumentLang = SetDocumentLang Lang Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetDocumentLang Bool where
  update (SetDocumentLang lang _actor) = updateWithoutEvidence "lang" lang

data SetDocumentFolderID = SetDocumentFolderID FolderID Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetDocumentFolderID Bool where
  update (SetDocumentFolderID folderId _actor) = updateWithoutEvidence "folder_id" folderId

data SetEmailInvitationDeliveryStatus = SetEmailInvitationDeliveryStatus SignatoryLinkID DeliveryStatus Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m, MonadTime m) => DBUpdate m SetEmailInvitationDeliveryStatus Bool where
  update (SetEmailInvitationDeliveryStatus slid status actor) = do
    sig <- theDocumentID >>= \did -> query $ GetSignatoryLinkByID did slid
    updateDocumentWithID $ \did -> do
      kRun1OrThrowWhyNot $  sqlUpdate "signatory_links" $ do
          sqlFrom "documents"
          sqlJoin "signatory_links AS signatory_links_old"
          sqlWhere "signatory_links.id = signatory_links_old.id"
          sqlSet "mail_invitation_delivery_status" status
          sqlWhereSignatoryLinkIDIs slid
          sqlWhereDocumentIDIs did
          sqlWhereDocumentTypeIs Signable
    nsig <- theDocumentID >>= \did -> query $ GetSignatoryLinkByID did slid
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
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m, MonadTime m) => DBUpdate m SetSMSInvitationDeliveryStatus Bool where
  update (SetSMSInvitationDeliveryStatus slid status actor) = do
    sig <- theDocumentID >>= \did -> query $ GetSignatoryLinkByID did slid
    updateDocumentWithID $ \did -> do
      runQuery_ . sqlUpdate "signatory_links" $ do
          sqlFrom "documents"
          sqlJoin "signatory_links AS signatory_links_old"
          sqlWhere "signatory_links.id = signatory_links_old.id"
          sqlSet "sms_invitation_delivery_status" status
          sqlWhereSignatoryLinkIDIs slid
          sqlWhereDocumentIDIs did
          sqlWhereDocumentTypeIs Signable
    nsig <- theDocumentID >>= \did -> query $ GetSignatoryLinkByID did slid
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

data SetEmailConfirmationDeliveryStatus = SetEmailConfirmationDeliveryStatus SignatoryLinkID DeliveryStatus Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m, MonadTime m) => DBUpdate m SetEmailConfirmationDeliveryStatus Bool where
  update (SetEmailConfirmationDeliveryStatus slid status actor) = do
    sig <- theDocumentID >>= \did -> query $ GetSignatoryLinkByID did slid
    updateDocumentWithID $ \did -> do
      kRun1OrThrowWhyNot $  sqlUpdate "signatory_links" $ do
          sqlFrom "documents"
          sqlJoin "signatory_links AS signatory_links_old"
          sqlWhere "signatory_links.id = signatory_links_old.id"
          sqlSet "mail_confirmation_delivery_status" status
          sqlWhereSignatoryLinkIDIs slid
          sqlWhereDocumentIDIs did
          sqlWhereDocumentTypeIs Signable
    nsig <- theDocumentID >>= \did -> query $ GetSignatoryLinkByID did slid
    let changed =
          signatorylinkmailconfirmationdeliverystatus sig
          /= signatorylinkmailconfirmationdeliverystatus nsig

    when_ (changed && status == Delivered) $
      update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
        ConfirmationDeliveredByEmail
        (return ())
        (Just nsig)
        Nothing
        actor
    when_ (changed && status == Undelivered) $
      update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
        ConfirmationUndeliveredByEmail
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
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetDocumentUnsavedDraft () where
  update (SetDocumentUnsavedDraft flag) = updateDocumentWithID $ \did -> do
    kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
      sqlSet "unsaved_draft" flag
      sqlWhereDocumentIDIs did

data AddAcceptedAuthorAttachmentsEvents = AddAcceptedAuthorAttachmentsEvents SignatoryLink [FileID] [(Text,AuthorAttachment)] Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m, CryptoRNG m) => DBUpdate m AddAcceptedAuthorAttachmentsEvents () where
  update (AddAcceptedAuthorAttachmentsEvents sl acceptedAttachments allAuthorAttachments actor) = generateEvents allAuthorAttachments
    where
      generateEvents [] = return ()
      generateEvents ((attAcceptanceText, att) : atts) = do
        when (authorattachmentrequired att && (authorattachmentfileid att) `elem` acceptedAttachments) $ do
          void $ update $ InsertEvidenceEventWithAffectedSignatoryAndMsgs
              AuthorAttachmentAccepted
              (do
                F.value "attachment_name" (authorattachmentname att)
                F.value "attachment_acceptance_text" attAcceptanceText
              )
              (Just sl)
              (Just $ authorattachmentname att)
              (Just attAcceptanceText)
            actor
        generateEvents atts

data AddNotUploadedSignatoryAttachmentsEvents = AddNotUploadedSignatoryAttachmentsEvents SignatoryLink [(Text, Text)] Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m, CryptoRNG m) => DBUpdate m AddNotUploadedSignatoryAttachmentsEvents () where
  update (AddNotUploadedSignatoryAttachmentsEvents sl notUploadedSignatoryAttachmentsWithText actor) =
    forM_ notUploadedSignatoryAttachmentsWithText $ \(saName,saNothingToUploadText) ->
      update $ InsertEvidenceEventWithAffectedSignatoryAndMsgs
        SignatoryAttachmentNotUploaded
        (do
          F.value "attachment_name" saName
          F.value "attachment_nothing_to_upload_text" saNothingToUploadText
        )
        (Just sl)
        (Just saName)
        (Just saNothingToUploadText)
        actor

data ApproveDocument = ApproveDocument SignatoryLinkID Actor
instance ( DocumentMonad m, TemplatesMonad m
         , MonadThrow m, CryptoRNG m, MonadTime m ) =>
         DBUpdate m ApproveDocument () where
  update (ApproveDocument slid actor) = do
    updateDocumentWithID $ \docid -> do
      let ipnumber = fromMaybe noIP $ actorIP actor
          time     = actorTime actor
      kRun1OrThrowWhyNot $ sqlUpdate "signatory_links" $ do
        sqlFrom  "documents"
        -- We're reusing maybesigninfo for the approver role.
        sqlSet   "sign_ip"               ipnumber
        sqlSet   "sign_time"             time
        sqlWhere "documents.id = signatory_links.document_id"
        sqlWhereDocumentIDIs             docid
        sqlWhereSignatoryLinkIDIs        slid
        sqlWhereDocumentTypeIs           Signable
        sqlWhereDocumentStatusIs         Pending
        sqlWhereSignatoryRoleIsApprover
      updateMTimeAndObjectVersion (actorTime actor)
    sl <- theDocumentID >>=
          \docid -> query $ GetSignatoryLinkByID docid slid
    let signatureFields = return ()
    void $ update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
        ApprovedByApproverPartyEvidence
        signatureFields
        (Just sl)
        Nothing
        actor


data SignDocument = SignDocument SignatoryLinkID (Maybe ESignature) (Maybe Text) SignatoryScreenshots Actor
instance ( DocumentMonad m, CryptoRNG m, MonadBase IO m, MonadCatch m
         , MonadFileStorage m, MonadLog m, MonadThrow m, MonadTime m
         , TemplatesMonad m ) => DBUpdate m SignDocument () where
  update (SignDocument slid mesig mpin screenshots actor) = do
    let legacy_signature_error = unexpectedError "signing with legacy signatures is not possible"
        sqlWhereAuthSign = case (mesig, mpin) of
          (Just (CGISEBankIDSignature_        _), _) -> sqlWhereSignatoryAuthenticationToSignMethodIs SEBankIDAuthenticationToSign
          (Just (NetsNOBankIDSignature_       _), _) -> sqlWhereSignatoryAuthenticationToSignMethodIs NOBankIDAuthenticationToSign
          (Just (NetsDKNemIDSignature_        _), _) -> sqlWhereSignatoryAuthenticationToSignMethodIs DKNemIDAuthenticationToSign
          (Just (LegacyBankIDSignature_       _), _) -> legacy_signature_error
          (Just (LegacyTeliaSignature_        _), _) -> legacy_signature_error
          (Just (LegacyNordeaSignature_       _), _) -> legacy_signature_error
          (Just (LegacyMobileBankIDSignature_ _), _) -> legacy_signature_error
          (_, Just _) -> sqlWhereSignatoryAuthenticationToSignMethodIs SMSPinAuthenticationToSign -- We should check pin here, but for now we do it in controler
          (Nothing, Nothing) -> sqlWhereSignatoryAuthenticationToSignMethodIs StandardAuthenticationToSign

    updateDocumentWithID $ \docid -> do
      let ipnumber = fromMaybe noIP $ actorIP actor
          time     = actorTime actor
      kRun1OrThrowWhyNot $ sqlUpdate "signatory_links" $ do
           sqlFrom  "documents"
           sqlSet   "sign_ip"        ipnumber
           sqlSet   "sign_time"      time
           sqlWhere "documents.id = signatory_links.document_id"
           sqlWhereDocumentIDIs      docid
           sqlWhereSignatoryLinkIDIs slid
           sqlWhereDocumentTypeIs    Signable
           sqlWhereDocumentStatusIs  Pending
           sqlWhereSignatoryRoleIsSigningParty
           sqlWhereSigningPartyHasNotSignedOrApproved
           sqlWhereAuthSign
      updateMTimeAndObjectVersion (actorTime actor)
    sl <- theDocumentID >>= \docid -> query $ GetSignatoryLinkByID docid slid
    let signatureFields = case (mesig, mpin) of
          (Just LegacyBankIDSignature_{}, _)       -> legacy_signature_error
          (Just LegacyTeliaSignature_{}, _)        -> legacy_signature_error
          (Just LegacyNordeaSignature_{}, _)       -> legacy_signature_error
          (Just LegacyMobileBankIDSignature_{}, _) -> legacy_signature_error
          (Just (CGISEBankIDSignature_ CGISEBankIDSignature{..}), _) -> do
            F.value "hide_pn" $ signatorylinkhidepn sl
            F.value "eleg" True
            F.value "signatory_name" cgisebidsSignatoryName
            F.value "signatory_personal_number" cgisebidsSignatoryPersonalNumber
            F.value "signatory_ip" cgisebidsSignatoryIP
            F.value "signed_text" cgisebidsSignedText
            F.value "provider_sebankid" True
            F.value "signature" $ B64.encode cgisebidsSignature
            F.value "ocsp_response" $ B64.encode cgisebidsOcspResponse
          (Just (NetsNOBankIDSignature_ NetsNOBankIDSignature{..}), _) -> do
            F.value "hide_pn" $ signatorylinkhidepn sl
            F.value "eleg" True
            F.value "signatory_name" netsnoSignatoryName
            F.value "signatory_pid" netsnoSignatoryPID
            F.value "signed_text" netsnoSignedText
            F.value "provider_nobankid" True
            F.value "signature" $ netsnoB64SDO
          (Just (NetsDKNemIDSignature_ NetsDKNemIDSignature{..}), _) -> do
            F.value "hide_pn" $ signatorylinkhidepn sl
            F.value "eleg" True
            F.value "signatory_name" netsdkSignatoryName
            F.value "signed_text" netsdkSignedText
            F.value "provider_dknemid" True
            F.value "signature" $ netsdkB64SDO
            unless (T.null netsdkSignatorySSN) $
              F.value "signatory_personal_number" netsdkSignatorySSN
            F.value "signatory_personal_number_from_signlink" $ getPersonalNumber sl
            unless (T.null netsdkSignatoryIP) $
              F.value "signatory_ip" netsdkSignatoryIP
          (Nothing, Just _) -> do
            F.value "sms_pin" True
            F.value "phone" $ getMobile sl
          (Nothing, Nothing) -> return ()
    void $ update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
        SignDocumentEvidence
        signatureFields
        (Just sl)
        Nothing
        actor
    void $ insertSignatoryScreenshots [(slid, screenshots)]

-- For this to work well we assume that signatories are ordered: author first, then all with ids set, then all with id == 0
data ResetSignatoryDetails = ResetSignatoryDetails [SignatoryLink] Actor
instance (CryptoRNG m, MonadLog m, MonadThrow m, DocumentMonad m, TemplatesMonad m) => DBUpdate m ResetSignatoryDetails Bool where
  update (ResetSignatoryDetails signatories _actor) = updateDocumentWithID $ \documentid -> do
    document <- query $ GetDocumentByDocumentID documentid
    case checkResetSignatoryData document signatories of
          [] -> do
            runQuery_ $ "DELETE FROM signatory_links WHERE document_id =" <?> documentid
            siglinks <- forM signatories $ \sl -> do
                     return $ sl {  maybesignatory = if (isAuthor sl) then (maybesignatory sl) else Nothing
                                 }
            insertSignatoryLinks documentid siglinks
            return True

          errs -> do
            logAttention "Cannot reset signatory details on document" $ object [
                identifier documentid
              , "errors" .= errs
              ]
            return False

data CloneDocumentWithUpdatedAuthor
  = CloneDocumentWithUpdatedAuthor (Maybe User) Document Actor (Document -> Document)
instance (MonadDB m, MonadThrow m, MonadLog m, TemplatesMonad m, CryptoRNG m)
  => DBUpdate m CloneDocumentWithUpdatedAuthor (Maybe DocumentID) where
  update (CloneDocumentWithUpdatedAuthor mUser document actor f) = do
    siglinks <- forM (documentsignatorylinks document) $ \sl -> do
      sl' <-  case mUser of
        Just user | isAuthor sl -> do
          ugwp <- query . UserGroupGetWithParentsByUserID . userid $ user
          return $ replaceSignatoryUser sl user ugwp
        _ -> return sl
      return sl'
        { signatorylinkid = unsafeSignatoryLinkID 0
        }

    let mAuthorID = maybesignatory =<< (listToMaybe . filter isAuthor $ siglinks)
    mAuthorUser <- case mAuthorID of
      Nothing -> return Nothing
      Just authorID -> query . GetUserByID $ authorID
    -- mAuthorUser and mUser may coincide at this point

    mDoc <- flip newFromDocumentID (documentid document) $ f . \doc -> doc
      { documentstatus = Preparation
      , documentsharing = Private
      , documentsignatorylinks = siglinks
      -- FIXME: Need to remove authorfields?
      , documentctime = actorTime actor
      , documentmtime = actorTime actor
      , documentshareablelinkhash = Nothing
      , documentfolderid = (userhomefolderid =<< mAuthorUser) `mplus`
                           (documentfolderid doc)
      }

    return $ documentid <$> mDoc

data StoreDocumentForTesting = StoreDocumentForTesting Document
instance (MonadDB m, MonadThrow m, MonadLog m, TemplatesMonad m) => DBUpdate m StoreDocumentForTesting DocumentID where
  update (StoreDocumentForTesting document) = documentid <$> insertDocument document

{-
   FIXME: this is so wrong on so many different levels
   - should set mtime
   - should not change type or copy this doc into new doc
-}
data TemplateFromDocument = TemplateFromDocument Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m TemplateFromDocument () where
  update (TemplateFromDocument _actor) = updateDocumentWithID $ \did -> do
    kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
       sqlSet "status" Preparation
       sqlSet "type" Template
       sqlWhereDocumentIDIs did
       sqlWhereEq "status" Preparation


data DocumentFromTemplate = DocumentFromTemplate DocumentID Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m DocumentFromTemplate () where
  update (DocumentFromTemplate tplID _actor) = updateDocumentWithID $ \did -> do
    kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
       sqlSet "status" Preparation
       sqlSet "type" Signable
       sqlSet "template_id" $ Just tplID
       sqlWhereDocumentIDIs did
       sqlWhereEq "status" Preparation

data TimeoutDocument = TimeoutDocument Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m TimeoutDocument () where
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

data ProlongTimeoutedDocument = ProlongTimeoutedDocument Int32 TimeZoneName Actor
instance (DocumentMonad m, TemplatesMonad m, MonadMask m) => DBUpdate m ProlongTimeoutedDocument () where
  update (ProlongTimeoutedDocument days tzn actor) = do
    updateDocumentWithID $ \did -> do
      -- Whole TimeZome behaviour is a clone of what is happending with making document ready for signing.
      let time = actorTime actor
      let timestamp = formatTime' "%F" time <> " " <> (T.unpack $ TimeZoneName.toString tzn)
      withTimeZone defaultTimeZoneName $ kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
         sqlSet "status" Pending
         sqlSet "mtime" time
         sqlSetCmd "timeout_time" $ "cast (" <?> timestamp <+> "as timestamp with time zone)"
                              <+> "+ (interval '1 day') *" <?> days <+> "+ (interval '23 hours 59 minutes 59 seconds')"
         sqlWhereDocumentIDIs did
         sqlWhereDocumentTypeIs Signable
         sqlWhereDocumentStatusIs Timedout
      runQuery_ . sqlUpdate "signatory_links" $ do
         sqlSet "deleted" (Nothing :: Maybe UTCTime)
         sqlSet "really_deleted" (Nothing :: Maybe UTCTime)
         sqlWhereEq "document_id" did
    void $ update $ InsertEvidenceEvent
        ProlongDocumentEvidence
        (return ())
        actor

data ProlongPendingDocument = ProlongPendingDocument Int32 Actor
instance (DocumentMonad m, TemplatesMonad m, MonadMask m) => DBUpdate m ProlongPendingDocument () where
  update (ProlongPendingDocument days actor) = do
    updateDocumentWithID $ \did -> do
      kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
         sqlSet "mtime" $ actorTime actor
         sqlSetCmd "timeout_time" $ "timeout_time" <+> "+ (interval '1 day') *" <?> days
         sqlWhereDocumentIDIs did
         sqlWhereDocumentTypeIs Signable
         sqlWhereDocumentStatusIs Pending
      runQuery_ . sqlUpdate "signatory_links" $ do
         sqlSet "deleted" (Nothing :: Maybe UTCTime)
         sqlSet "really_deleted" (Nothing :: Maybe UTCTime)
         sqlWhereEq "document_id" did
    void $ update $ InsertEvidenceEvent
        ProlongDocumentEvidence
        (return ())
        actor

data SetDocumentAPICallbackURL = SetDocumentAPICallbackURL APIVersion (Maybe Text)
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetDocumentAPICallbackURL Bool where
  update (SetDocumentAPICallbackURL apiVersion mac) = updateDocumentWithID $ \did -> do
    let tableColumn = case apiVersion of
                           V1 -> "api_v1_callback_url"
                           V2 -> "api_v2_callback_url"
    runQuery01 . sqlUpdate "documents" $ do
      sqlSet tableColumn mac
      sqlWhereEq "id" did

data SetDocumentTimeZoneName = SetDocumentTimeZoneName TimeZoneName
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetDocumentTimeZoneName Bool where
  update (SetDocumentTimeZoneName timezone) = updateDocumentWithID $ \did -> do
    runQuery01 . sqlUpdate "documents" $ do
      sqlSet "time_zone_name" timezone
      sqlWhereEq "id" did



data PostReminderSend = PostReminderSend SignatoryLink (Maybe Text) Bool Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m PostReminderSend () where
   update (PostReminderSend sl mmsg automatic actor) = do
     updateDocument $ \doc -> do
       let docid = documentid doc
       kRun1OrThrowWhyNot $ sqlUpdate "signatory_links" $ do
         sqlFrom "documents"
         sqlSet "read_invitation" (Nothing :: Maybe UTCTime)
         sqlSet "mail_invitation_delivery_status" Unknown
         sqlSet "sms_invitation_delivery_status" Unknown
         sqlWhere "documents.id = signatory_links.document_id"

         sqlWhereDocumentIDIs docid
         sqlWhereSignatoryLinkIDIs (signatorylinkid sl)
         sqlWhereSigningPartyHasNotSignedOrApproved
         sqlWhereDocumentStatusIs Pending
       updateMTimeAndObjectVersion (actorTime actor)

     void $ update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
          (if automatic then AutomaticReminderSent else ReminderSend)
          (return ())
          (Just sl)
          mmsg
          actor

data UpdateFieldsForSigning = UpdateFieldsForSigning SignatoryLink [(FieldIdentity, FieldValue)] [(FileID, BS.ByteString)] Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m UpdateFieldsForSigning () where
  -- Document has to be in Pending state
  -- signatory could not have signed already
  update (UpdateFieldsForSigning sl fields signaturesContent actor) = updateDocumentWithID $ const $ do
    let slid = signatorylinkid sl
    let updateValue :: (FieldIdentity, FieldValue) -> m ()
        updateValue (fieldIdent, newValue) = do
          let custom_name = case fieldIdent of
                              TextFI xname   -> xname
                              CheckboxFI xname -> xname
                              SignatureFI xname -> xname
                              RadioGroupFI xname -> xname
                              _ -> ""
              oldField = getFieldByIdentity fieldIdent (signatoryfields sl)
          updated <- runQuery . sqlUpdate "signatory_link_fields" $ do
                   sqlSet "value_text" $ case newValue of
                                              StringFV s -> Just s
                                              _ -> Nothing
                   sqlSet "value_bool" $  case newValue of
                                              BoolFV b -> Just b
                                              _ -> Nothing
                   sqlSet "value_file_id" $ case newValue of
                                              FileFV f -> f
                                              _ -> Nothing
                   sqlWhereEq "signatory_link_id" slid
                   sqlWhereEq "custom_name" custom_name
                   case fieldIdent of
                     NameFI no -> sqlWhereEq "name_order" no
                     _ ->         sqlWhereIsNULL "name_order"
                   sqlWhereEq "type" $ fieldTypeFromFieldIdentity fieldIdent
                   sqlWhereAny
                       [ do
                           sqlWhereEq "value_text" (""::String)
                           sqlWhereIn "type" [TextFT, NameFT ,EmailFT,CompanyFT,PersonalNumberFT,PersonalNumberFT,CompanyNumberFT, MobileFT]
                       , sqlWhereIn "type" [CheckboxFT, SignatureFT, RadioGroupFT]
                       , do
                           sqlWhereIn "type" [EmailFT, MobileFT]
                           sqlWhereEq "editable_by_signatory" True
                       ]
                   sqlWhereExists $ sqlSelect "documents" $ do
                     sqlWhere "signatory_links.id = signatory_link_id"
                     sqlLeftJoinOn "signatory_links" "documents.id = signatory_links.document_id"
                     sqlWhereEq "documents.status" Pending
                     sqlWhere "signatory_links.sign_time IS NULL"

          let oldValue = case oldField of
                Just (SignatoryCheckboxField (chf@CheckboxField{})) -> BoolFV (schfValue chf)
                Just (SignatorySignatureField (sf@SignatureField{}))  -> FileFV (ssfValue sf)
                Just f -> StringFV $ fromMaybe "" $ fieldTextValue f
                _ -> StringFV ""
              changed = oldValue /= newValue
              emptyValue (StringFV s) = T.null s
              emptyValue (BoolFV False) = True
              emptyValue (BoolFV True) = False
              emptyValue (FileFV Nothing) = True
              emptyValue (FileFV _) = False

          when (updated/=0 && changed) $ do
            let eventEvidenceText = getEvidenceTextForUpdateField sl fieldIdent
            void $ update $ InsertEvidenceEvent eventEvidenceText
               (do F.value "value" $ case newValue of
                     StringFV s -> s
                     BoolFV False -> ""
                     BoolFV True -> "checked"
                     FileFV Nothing -> ""
                     FileFV (Just fi) -> case (lookup fi signaturesContent) of
                                                   Nothing -> ""
                                                   Just s -> imgEncodeRFC2397ToText $ s
                   F.value "previousvalue"$ case oldValue of
                     StringFV s -> s
                     BoolFV False  -> ""
                     BoolFV True -> "checked"
                     FileFV Nothing -> ""
                     FileFV (Just fi) -> case (lookup fi signaturesContent) of
                                                   Nothing -> ""
                                                   Just s -> imgEncodeRFC2397ToText $ s
                   when (emptyValue newValue) $
                       F.value "newblank" True
                   when (emptyValue oldValue) $
                       F.value "prvblank" True
                   case (oldField) of
                        Just (SignatoryTextField f) -> do
                          F.value "customfieldname" $ stfName f
                          F.value "fieldname" $ stfName f
                        Just (SignatoryPersonalNumberField _) -> do
                          F.value "hide_pn" $ signatorylinkhidepn sl
                        Just (SignatoryCheckboxField f) -> do
                          F.value "fieldname" $ schfName f
                          unless (emptyValue newValue) $ do
                            F.value "checked" $ schfName f
                        Just (SignatorySignatureField f) -> do
                          F.value "fieldname" $ ssfName f
                        Just (SignatoryRadioGroupField f) -> do
                          F.value "fieldname" $ srgfName f
                          F.value "fieldvalues" $ T.unpack $
                            T.intercalate ", " $ map (\v -> "\"" <> v <> "\"") $ srgfValues f
                        _ -> return ()
                   case oldField of
                     Just f | not (null ps) -> do
                       F.objects "placements" $ for ps $ \p -> do
                         F.value "page" $ placementpage p
                         F.value "x" $ show $ realFracToDecimal 3 $ placementxrel p
                         F.value "y" $ show $ realFracToDecimal 3 $ placementyrel p
                       where ps = fieldPlacements f
                     _ -> return ())
               actor

    forM_ fields updateValue

data UpdateConsentResponsesForSigning = UpdateConsentResponsesForSigning SignatoryLink SignatoryConsentResponsesForSigning Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m UpdateConsentResponsesForSigning () where
  update (UpdateConsentResponsesForSigning sl (SignatoryConsentResponsesForSigning responses) actor) =
    updateDocumentWithID $ const $ forM_ responses $ \(i, r) -> do
      runQuery_ $ sqlUpdate "signatory_link_consent_questions" $ do
        sqlSet "response" r
        sqlWhereSignatoryConsentQuestionIDIs i
        sqlWhereEq "signatory_link_id" (signatorylinkid sl)

      case find ((==i) . scqID) (signatorylinkconsentquestions sl) of
        Nothing ->
          throwM $ SomeDBExtraException $ SignatoryConsentQuestionDoesNotExist i
        Just SignatoryConsentQuestion{..} -> do
          let (eventType, tplFields) = case scqDescription of
                     Nothing -> (ConsentQuestionAnswered , return ())
                     Just (title, text) ->
                       ( ConsentQuestionAnsweredWithDescription
                       , do F.value "description_title" title
                            F.value "description_text" text
                       )
          update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
            eventType
            (do F.value "question" scqTitle
                F.value "response" $ if r then scqPositiveOption else scqNegativeOption
                tplFields)
            (Just sl) Nothing actor

data UpdatePhoneAfterIdentificationToView = UpdatePhoneAfterIdentificationToView SignatoryLink Text Text Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m UpdatePhoneAfterIdentificationToView () where
  update (UpdatePhoneAfterIdentificationToView sl oldphone newphone actor) = updateDocumentWithID $ const $ do
    success <- runQuery01 . sqlUpdate "signatory_link_fields" $ do
                   sqlSet "value_text" $ newphone
                   sqlWhereEq "signatory_link_id" $ signatorylinkid sl
                   sqlWhereEq "type" $ MobileFT
                   sqlWhereEq "value_text" (""::String) -- Note: we only let update of phone number if it's not set
                   sqlWhereExists $ sqlSelect "documents" $ do
                     sqlWhere "signatory_links.id = signatory_link_id"
                     sqlLeftJoinOn "signatory_links" "documents.id = signatory_links.document_id"
                     sqlWhereEq "documents.status" Pending
                     sqlWhere "signatory_links.sign_time IS NULL"
    unless success $ do
      unexpectedError "Failed to update phone number after identification to view"
    void $ update $ InsertEvidenceEventWithAffectedSignatoryAndMsg UpdateMobileAfterIdentificationToViewWithNets
               (F.value "oldphone" oldphone >> F.value "newphone" newphone) (Just sl) Nothing actor


data AddDocumentAttachment = AddDocumentAttachment Text Bool Bool FileID Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m AddDocumentAttachment Bool where
  update (AddDocumentAttachment aname arequired aaddedtosealedfile afid _actor) = updateDocumentWithID $ \did -> do
    runQuery01 . sqlInsertSelect "author_attachments" "" $ do
        sqlSet "document_id" did
        sqlSet "name" aname
        sqlSet "required" arequired
        sqlSet "add_to_sealed_file" aaddedtosealedfile
        sqlSet "file_id" afid
        sqlWhereExists $ sqlSelect "documents" $ do
          sqlWhereEq "id" did
          sqlWhereEq "status" Preparation

data RemoveDocumentAttachments = RemoveDocumentAttachments FileID Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m RemoveDocumentAttachments Bool where
  update (RemoveDocumentAttachments fid _actor) = updateDocumentWithID $ \did -> do
    count <- runQuery $ "DELETE FROM author_attachments WHERE document_id =" <?> did <+> "AND file_id =" <?> fid <+> "AND EXISTS (SELECT 1 FROM documents WHERE id = author_attachments.document_id AND status =" <?> Preparation <+> ")"
    return $ count > 0

data UpdateShareableLinkHash = UpdateShareableLinkHash (Maybe MagicHash)
instance (DocumentMonad m, MonadThrow m) => DBUpdate m UpdateShareableLinkHash () where
  update (UpdateShareableLinkHash mHash) = updateDocumentWithID $ \did ->
    runQuery01_ . sqlUpdate "documents" $ do
      sqlSet "shareable_link_hash" mHash
      sqlWhereEq "id" did

-- Remove unsaved drafts (older than 1 week) from db.
-- Uses chunking to not overload db when there's a lot of old drafts
data RemoveOldDrafts = RemoveOldDrafts Int32
instance (MonadDB m, MonadTime m) => DBUpdate m RemoveOldDrafts Int where
    update (RemoveOldDrafts limit) = do
      weekAgo <- (7 `daysBefore`) <$> currentTime
      runQuery $ smconcat [
          "DELETE FROM documents"
        , "WHERE id IN ("
        , "  SELECT id FROM documents"
        , "  WHERE unsaved_draft"
        , "    AND type =" <?> Signable
        , "    AND status =" <?> Preparation
        , "    AND mtime <" <?> weekAgo
        , "  LIMIT" <?> limit
        , ")"
        ]

data SetSigAttachments = SetSigAttachments SignatoryLinkID [SignatoryAttachment] Actor
instance (DocumentMonad m) => DBUpdate m SetSigAttachments () where
  update (SetSigAttachments slid sigatts _actor) = updateDocumentWithID $ const $ do
    void $ doDeleteAll
    forM_ sigatts doInsertOne
    where
     doDeleteAll = runQuery $ "DELETE FROM signatory_attachments WHERE signatory_link_id =" <?> slid
     doInsertOne SignatoryAttachment{..} = do
        runQuery . sqlInsert "signatory_attachments" $ do
            sqlSet "file_id" signatoryattachmentfile
            sqlSet "name" signatoryattachmentname
            sqlSet "description" signatoryattachmentdescription
            sqlSet "signatory_link_id" slid
            sqlSet "required" signatoryattachmentrequired


data ClearSignatoryEmail = ClearSignatoryEmail SignatoryLinkID
instance (DocumentMonad m) => DBUpdate m ClearSignatoryEmail () where
  update (ClearSignatoryEmail slid) = clearSignatoryFieldText slid EmailFT

data ClearSignatoryMobile = ClearSignatoryMobile SignatoryLinkID
instance (DocumentMonad m) => DBUpdate m ClearSignatoryMobile () where
  update (ClearSignatoryMobile slid) = clearSignatoryFieldText slid MobileFT

clearSignatoryFieldText ::  (DocumentMonad m) => SignatoryLinkID -> FieldType -> m ()
clearSignatoryFieldText slid ft = updateDocumentWithID $ const $ do
  runQuery_ $ sqlUpdate "signatory_link_fields" $ do
    sqlSet "value_text" ("" :: Text)
    sqlWhereEq "type" ft
    sqlWhereEq "signatory_link_id" slid

data UpdateDraft = UpdateDraft Document Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m UpdateDraft Bool where
  update (UpdateDraft document actor) = updateDocument $ const $ and <$> sequence [
      update $ SetDocumentTitle (documenttitle document) actor
    , update $ maybe (unexpectedError "folder_id should have a value") 
                       (\fid -> SetDocumentFolderID fid actor)
                       (documentfolderid document)
    , update $ SetDaysToSign (documentdaystosign document) actor
    , update $ SetDaysToRemind (documentdaystoremind document) actor
    , update $ SetDocumentLang (getLang document) actor
    , update $ SetInviteText (documentinvitetext document) actor
    , update $ SetConfirmText (documentconfirmtext document) actor
    , update $ SetShowHeader (documentshowheader document) actor
    , update $ SetShowPDFDownload (documentshowpdfdownload document) actor
    , update $ SetShowRejectOption (documentshowrejectoption document) actor
    , update $ SetAllowRejectReason (documentallowrejectreason document) actor
    , update $ SetShowFooter (documentshowfooter document) actor
    , update $ SetIsReceipt (documentisreceipt document) actor
    , update $ SetDocumentTags (documenttags document) actor
    , update $ SetShowArrow (documentshowarrow document) actor
    , update $ SetDocumentAPICallbackURL V1 (documentapiv1callbackurl document)
    , update $ SetDocumentAPICallbackURL V2 (documentapiv2callbackurl document)
    , update $ SetDocumentTimeZoneName (documenttimezonename document)
    , updateMTimeAndObjectVersion (actorTime actor) >> return True
    ]

-- | Connect signatories who saw or signed the document not earlier
-- than at a given time with supplied email address to a user.
data ConnectSignatoriesToUser = ConnectSignatoriesToUser Email UserID UTCTime
instance MonadDB m => DBUpdate m ConnectSignatoriesToUser () where
  -- Connect only signatory links that belong to a document that is
  -- not a template nor draft and are not already connected to an
  -- existing user.
  update (ConnectSignatoriesToUser email uid time) = runSQL_ $ smconcat [
      "WITH ids AS ("
    , "SELECT d.id AS doc_id, sl.id AS sl_id"
    , "  FROM documents d"
    , "  JOIN signatory_links sl"
    , "    ON d.id = sl.document_id"
    , "  JOIN signatory_link_fields slf"
    , "    ON sl.id = slf.signatory_link_id"
    , " WHERE d.type =" <?> Signable
    , "   AND d.status <>" <?> Preparation
    , "   AND sl.user_id IS NULL"
    , "   AND slf.type =" <?> EmailFT
    , "   AND slf.value_text =" <?> email
    , "   AND (sl.sign_time >=" <?> time <+> "OR sl.seen_time >=" <?> time <> ")"
    , "FOR UPDATE"
    , ")"
    , "UPDATE signatory_links"
    , "   SET user_id =" <?> uid
    , "  FROM ids"
    , " WHERE id = ids.sl_id"
    , "   AND document_id = ids.doc_id"
    ]

-- | We purge documents that:
-- 1) Were deleted by all signatories with an existing account
-- 2) Don't have an access token in an existing session.
-- 3) A month has passed (because document could be too big to be sent by email,
--    and confirmation email with links may still reference it)
data PurgeDocuments = PurgeDocuments Int32
instance (MonadDB m, MonadTime m) => DBUpdate m PurgeDocuments Int where
  update (PurgeDocuments savedDocumentLingerDays) = do
    now <- currentTime
    -- Unlink documents that were in thrash long enough.
    runQuery_ . sqlUpdate "signatory_links sl" $ do
      sqlSet "really_deleted" now
      -- Document belongs to somebody.
      sqlWhereIsNotNULL "sl.user_id"
      -- It was deleted sufficient amount of time ago or the user group's data
      -- retention policy is to delete things in the trash immediately.
      sqlWhereAny
        [ sqlWhere $
            "sl.deleted +" <?> idays savedDocumentLingerDays <+> "<=" <?> now
        , do
            sqlWhereIsNotNULL "sl.deleted"
            sqlWhereExists . sqlSelect "users u" $ do
              sqlJoinOn "user_group_settings ugs"
                        "ugs.user_group_id = u.user_group_id"
              sqlWhere "ugs.immediate_trash OR u.immediate_trash"
              sqlWhere "u.id = sl.user_id"
        ]
      -- It wasn't yet unlinked.
      sqlWhere "really_deleted IS NULL"

    rows <- runQuery . sqlUpdate "documents" $ do
      sqlWith "documents_to_purge" . sqlSelect "documents d" $ do
        sqlResult "d.id"

        -- Document wasn't purged yet.
        sqlWhere "d.purged_time IS NULL"

        -- Documents (not in preparation) are deleted if, for all signatories,
        -- ANY of the following is true
        -- a) she has deleted the document or
        -- b) her user group has been deleted or
        -- c) she is deleted but/and not the author. (*)
        --
        -- In other words, the document is NOT deleted if, there exists a
        -- signatory for which ALL of the following conditions are true:
        -- a) she hasn't deleted the document and
        -- b) her user group still exists and
        -- c) she is not deleted or is the author. (*)
        --
        -- (*) The condition (c) is for documents which have been created by
        -- another company where a user who happens to have a Scrive account
        -- is one of the signatory and this user has deleted her account.
        -- If the author is deleted and is the author, the company should still
        -- keep the document.
        sqlWhereNotExists . sqlSelect "signatory_links sl" $ do
          sqlJoinOn "users u" "sl.user_id = u.id"
          sqlJoinOn "user_groups ug" "u.user_group_id = ug.id"
          sqlWhere "sl.document_id = d.id"
          sqlWhereIsNotNULL "sl.user_id"
          sqlWhereIsNULL "sl.really_deleted" -- condition a
          sqlWhereIsNULL "ug.deleted" -- condition b
          sqlWhere "u.deleted IS NULL OR u.id = d.author_user_id" -- condition c
          sqlWhereNotEq "d.status" Preparation

        -- Documents in preparation are deleted if
        -- a) their author has been deleted but it is not a shared template OR
        -- b) their author has deleted the document OR
        -- c) their user groups has been deleted.
        --
        -- That is, the document is NOT deleted if
        -- a) the author is not deleted or it is a shared template AND
        -- b) the author has not deleted the document AND
        -- c) the document's author's user group is not deleted.
        sqlWhereNotExists . sqlSelect "signatory_links sl" $ do
          sqlJoinOn "users u" "sl.user_id = u.id"
          sqlJoinOn "user_groups ug" "u.user_group_id = ug.id"
          sqlWhere "d.author_id = sl.id"
           -- condition a
          sqlWhere $ "u.deleted IS NULL OR d.sharing =" <?> Shared
          sqlWhereIsNULL "sl.really_deleted" -- condition b
          sqlWhereIsNULL "ug.deleted" -- condition c
          sqlWhereEq "d.status" Preparation

        -- Document isn't pending (it's possible that there are 0
        -- signatories with user set, but the doc is still pending
        -- purging it would violate db constraints
        sqlWhereNotEq "d.status" Pending

        -- Document is not referenced by any session.
        sqlWhereNotExists . sqlSelect "signatory_links sl" $ do
          sqlJoinOn "document_session_tokens dst" "sl.id = dst.signatory_link_id"
          sqlWhere "sl.document_id = d.id"
        -- Document hasn't changed for a month
        -- so that `availabledate` in `documentAttachableFields` works
        sqlWhere $ "d.mtime" <+> "<=" <?> (30 `daysBefore` now)

      -- Blank out sensitive data.
      sqlWith "purged_signatory_links" . sqlUpdate "signatory_links" $ do
        sqlResult "id"
        sqlSet "sign_ip" (0::Int32)
        sqlSet "seen_ip" (0::Int32)
        sqlSet "csv_contents" (Nothing::Maybe [[String]])
        sqlSet "sign_redirect_url" (Nothing::Maybe String)
        sqlSet "rejection_reason" (Nothing::Maybe String)
        sqlSet "reject_redirect_url" (Nothing::Maybe String)
        sqlSet "consent_title" (Nothing::Maybe String)
        sqlWhere "document_id IN (SELECT id FROM documents_to_purge)"

      -- Blank out sensitive data in fields.
      sqlWith "purged_signatory_fields" . sqlUpdate "signatory_link_fields" $ do
        sqlSetCmd "value_text" "CASE WHEN value_text IS NULL THEN NULL ELSE '' END"
        sqlSetCmd "value_bool" "CASE WHEN value_bool IS NULL THEN NULL ELSE FALSE END"
        sqlSetCmd "value_file_id" "NULL"
        sqlWhere "signatory_link_id IN (SELECT id FROM purged_signatory_links)"

      -- Remove whole evidence log as it is sensitive data.
      sqlWith "purged_evidence_log" . sqlDelete "evidence_log" $ do
        sqlWhere "document_id IN (SELECT id FROM documents_to_purge)"

      -- Remove all document_tags.
      sqlWith "purged_document_tags" . sqlDelete "document_tags" $ do
        sqlWhere "document_id IN (SELECT id FROM documents_to_purge)"

      -- Remove all author attachments
      sqlWith "purged_author_attachments" . sqlDelete "author_attachments" $ do
        sqlWhere "document_id IN (SELECT id FROM documents_to_purge)"

      -- Remove all eid_authentications.
      sqlWith "purged_eid_authentications" . sqlDelete "eid_authentications" $ do
        sqlWhere "signatory_link_id IN (SELECT id FROM purged_signatory_links)"

      -- Remove all eid_authentications.
      sqlWith "purged_eid_signatures" . sqlDelete "eid_signatures" $ do
        sqlWhere "signatory_link_id IN (SELECT id FROM purged_signatory_links)"

      -- Remove all signatory_sms_pins.
      sqlWith "purged_signatory_sms_pins" . sqlDelete "signatory_sms_pins" $ do
        sqlWhere "signatory_link_id IN (SELECT id FROM purged_signatory_links)"

      -- Remove all signatory attachments
      sqlWith "purged_signatory_attachements" . sqlDelete "signatory_attachments" $ do
        sqlWhere "signatory_link_id IN (SELECT id FROM purged_signatory_links)"

      -- Remove all signatory consent questions
      sqlWith "purged_signatory_consent_questions" . sqlDelete "signatory_link_consent_questions" $ do
        sqlWhere "signatory_link_id IN (SELECT id FROM purged_signatory_links)"

      -- Clean sensitive data in document
      sqlSet "title" ("" :: String)
      sqlSet "api_v1_callback_url" (Nothing :: Maybe String)
      sqlSet "api_v2_callback_url" (Nothing :: Maybe String)
      sqlSet "invite_ip" (0::Int32)
      sqlSet "invite_text" ("" :: String)
      sqlSet "confirm_text" ("" :: String)

      -- Set purged_time on documents.
      sqlSet "purged_time" now
      sqlWhere "id IN (SELECT id FROM documents_to_purge)"



    return rows

{- | Archive (move to trash) idle documents for signatories.  A
document is idle for a signatory if

   1. the document is not archived for the signatory,
   2. the document is not a template and not pending,
   3. the document authors companys or the authors idle_doc_timeout_STATUS is set,
   4. the signatory belongs to the same company as the author, and
   5. it's been more than idle_doc_timeout_STATUS days since the document was modified.
-}
archiveIdleDocuments
  :: (MonadLog m, MonadDB m, MonadThrow m, MonadBase IO m)
  => UTCTime -> m Int
archiveIdleDocuments now = do
  (archived_from_groups, processed_ugids) <- expireUserDocumentsInGroupsWithDRP
  -- Previous step returns all the groups, which we have already processed. We don't want to
  -- process users in these groups again
  archived_from_users <- expireUserDocumentsWithTheirOwnDRP processed_ugids
  return $ archived_from_groups + archived_from_users

  where
    expireUserDocumentsInGroupsWithDRP
      :: (MonadLog m, MonadDB m, MonadThrow m) => m (Int, S.Set UserGroupID)
    expireUserDocumentsInGroupsWithDRP = do
      ugs <- dbQuery $ UserGroupsGetFiltered [UGWithAnyIdleDocTimeoutSet] Nothing
      -- all usergroups with any Retention Policy have ugSettings
      let ugs_settings = mapMaybe (\ug -> (ug,) <$> get ugSettings ug) ugs
      counts_and_ugids <- forM ugs_settings $ \(ug_with_drp, ug_settings) -> do
        let ug_drp = get ugsDataRetentionPolicy ug_settings
        -- Get recursive all children, who inherit this DRP property.
        -- This requires another level of queries, but it just means 2 queries per
        -- UserGroup (1 for getting children + 1 for getting users)
        children_ugs <- ugGetChildrenInheritingProperty (get ugID ug_with_drp) (get ugSettings)
        archived_counts <- forM (ug_with_drp:children_ugs) $ \ug_with_inherited_drp -> do
          users <- dbQuery $ UserGroupGetUsersIncludeDeleted $ get ugID ug_with_inherited_drp
          forM users $ expireUserDocuments (Just ug_drp)
        return (sum $ concat archived_counts, map (get ugID) $ ug_with_drp:children_ugs)
      let (archived_counts, ugidss_with_drp) = unzip counts_and_ugids
      return $ (sum archived_counts, S.fromList $ concat ugidss_with_drp)

    expireUserDocumentsWithTheirOwnDRP
      :: (MonadLog m, MonadDB m) => S.Set UserGroupID -> m Int
    expireUserDocumentsWithTheirOwnDRP processed_ugids = do
      users <- dbQuery $ GetUsers [UserFilterWithAnyDocumentRetentionPolicy]
      let users_not_yet_processed = filter (\u -> not $ S.member (usergroupid u) processed_ugids) users
      archived_counts_from_users <- forM users_not_yet_processed $ expireUserDocuments Nothing
      return $ sum archived_counts_from_users

    expireUserDocuments
      :: (MonadLog m, MonadDB m) => Maybe DataRetentionPolicy -> User -> m Int
    expireUserDocuments m_ug_drp user = do
      let uid      = userid user
          ugid     = usergroupid user
          user_drp = dataretentionpolicy $ usersettings user
          drp      = case m_ug_drp of
            Nothing -> user_drp
            Just ug_drp -> makeStricterDataRetentionPolicy ug_drp user_drp
      logInfo "archiveIdleDocuments starting for user in user_group" $ object [
          identifier ugid
        , identifier uid
        ]
      startTime <- currentTime
      archived_counts <- forM allDrpStatuses $ \status ->
        case get (drpIdleDocTimeout status) drp of
          Just timeoutDays ->
            dbUpdate $ ArchiveIdleDocumentsForUserInUserGroup
              uid ugid status timeoutDays now
          Nothing -> return 0
      commit
      finishTime <- currentTime
      logInfo "archiveIdleDocuments finished for user in user_group" $ object [
          identifier ugid
        , identifier uid
        , "elapsed_time" .= (realToFrac (diffUTCTime finishTime startTime) :: Double)
        , "signatory_links_archived" .= archived_counts
        ]
      return $ sum archived_counts

    allDrpStatuses :: [DocumentStatus]
    allDrpStatuses = [Preparation, Closed, Canceled, Timedout, Rejected, DocumentError]


data ArchiveIdleDocumentsForUserInUserGroup = ArchiveIdleDocumentsForUserInUserGroup UserID UserGroupID DocumentStatus Int16 UTCTime
instance MonadDB m => DBUpdate m ArchiveIdleDocumentsForUserInUserGroup Int where
  update (ArchiveIdleDocumentsForUserInUserGroup uid ugid status timeoutDays now) = do
    runSQL $ "WITH user_idle_docs AS ("
           <+> "SELECT d.id"
           <+> "FROM documents AS d"
           <+> "WHERE d.type =" <?> Signable
           <+> "AND d.author_user_id =" <?> uid
           <+> "AND d.status =" <?> status
           <+> "AND d.mtime + (interval '1 day') *" <?> timeoutDays <+> "<" <?> now
         <+> "),"
         <+> "doc_sigs_in_user_group AS ("
           <+> "  SELECT sl.id"
           <+> "  FROM signatory_links AS sl"
           <+> "  JOIN users AS u"
           <+> "  ON sl.user_id = u.id"
           <+> "  AND u.user_group_id =" <?> ugid
           <+> "  WHERE sl.document_id IN (SELECT * FROM user_idle_docs)"
         <+> ")"
         <+> "UPDATE signatory_links"
         <+> "SET deleted =" <?> now
         <+> "WHERE deleted IS NULL"
         <+> "AND id IN (SELECT * FROM doc_sigs_in_user_group)"

data UpdateAuthorUserID = UpdateAuthorUserID Int
instance (MonadDB m, MonadThrow m) => DBUpdate m UpdateAuthorUserID Int where
  update (UpdateAuthorUserID limit) = do
    runQuery . sqlUpdate "documents" $ do
      sqlWith "document_authors" . sqlSelect "documents d" $ do
        sqlResult "d.id as document_id"
        sqlResult "sl.user_id as author_id"
        sqlJoinOn "signatory_links sl" "d.author_id = sl.id"
        sqlWhereIsNULL "d.author_user_id"
        sqlWhereIsNotNULL "sl.user_id"
        sqlLimit limit
      sqlFrom "document_authors da"
      sqlSetCmd "author_user_id" "da.author_id"
      sqlWhere "id = da.document_id"

-- Update utilities
getEvidenceTextForUpdateField :: SignatoryLink -> FieldIdentity -> CurrentEvidenceEventType
getEvidenceTextForUpdateField sig (NameFI (NameOrder 1))
                            | hasOneNameField sig      = UpdateFieldNameEvidence
                            | otherwise                = UpdateFieldFirstNameEvidence
getEvidenceTextForUpdateField _ (NameFI (NameOrder 2)) = UpdateFieldLastNameEvidence
getEvidenceTextForUpdateField _ (NameFI (NameOrder _)) = unexpectedError "NameFT with nameorder different than 1 and 2"
getEvidenceTextForUpdateField _ CompanyFI              = UpdateFieldCompanyEvidence
getEvidenceTextForUpdateField _ PersonalNumberFI       = UpdateFieldPersonalNumberEvidence
getEvidenceTextForUpdateField _ CompanyNumberFI        = UpdateFieldCompanyNumberEvidence
getEvidenceTextForUpdateField _ EmailFI                = UpdateFieldEmailEvidence
getEvidenceTextForUpdateField _ (TextFI _)             = UpdateFieldCustomEvidence
getEvidenceTextForUpdateField _ MobileFI               = UpdateFieldMobileEvidence
getEvidenceTextForUpdateField _ (SignatureFI _)        = UpdateFieldSignatureEvidence
getEvidenceTextForUpdateField _ (CheckboxFI _)         = UpdateFieldCheckboxEvidence
getEvidenceTextForUpdateField _ (RadioGroupFI _)       = UpdateFieldRadioGroupEvidence

hasOneNameField :: SignatoryLink -> Bool
hasOneNameField sig = 1 == (length $ filter (\f -> NameFT == fieldType f) $ signatoryfields sig)

updateWithoutEvidence :: (DocumentMonad m, MonadThrow m, Show a, ToSQL a) => SQL -> a -> m Bool
updateWithoutEvidence col newValue = updateDocumentWithID $ \did -> do
  runQuery01 $ "UPDATE" <+> raw (tblName tableDocuments) <+> "SET" <+> (col <+> "=" <?> newValue <+> "WHERE id =" <?> did)

updateMTimeAndObjectVersion :: DocumentMonad m  => UTCTime -> m ()
updateMTimeAndObjectVersion mtime = updateDocumentWithID $ \did -> do
  runQuery_ . sqlUpdate "documents" $ do
       sqlSetInc "object_version"
       sqlSet "mtime" mtime
       sqlWhereEq "id" did

checkEqualBy :: TextShow b => (b -> b -> Bool) -> Text -> (a -> b) -> a -> a -> Maybe (Text, Text, Text)
checkEqualBy equals name f obj1 obj2
  | f obj1 `equals` f obj2 = Nothing
  | otherwise              = Just (name, showt (f obj1), showt (f obj2))

checkEqualSignatoryFields  :: Text ->[SignatoryField] -> [SignatoryField] -> Maybe (Text, Text, Text)
checkEqualSignatoryFields name (f:fs) (f':fs') = if fieldsAreAlmostEqual f f'
                                                   then checkEqualSignatoryFields name fs fs'
                                                   else Just (name, showt f, showt f')
checkEqualSignatoryFields name (f:_) [] = Just (name, showt f, "No field")
checkEqualSignatoryFields name [] (f:_) = Just (name,  "No field", showt f)
checkEqualSignatoryFields _ [] [] = Nothing

checkEqualByAllowSecondNothing :: TextShow b => (Maybe b -> Maybe b -> Bool) -> Text -> (a -> Maybe b) -> a -> a -> Maybe (Text, Text, Text)
checkEqualByAllowSecondNothing equals name f obj1 obj2
  | isNothing (f obj2)     = Nothing
  | f obj1 `equals` f obj2 = Nothing
  | otherwise              = Just (name, showt (f obj1), showt (f obj2))

assertEqualDocuments :: (MonadThrow m, MonadLog m) => Document -> Document -> m ()
assertEqualDocuments d1 d2 | null inequalities = return ()
                           | otherwise = do
  logInfo message $ object [
      "inequalities" .= (T.concat $ showInequality <$> inequalities)
    ]
  unexpectedError message
  where
    message = "Documents aren't equal"

    showInequality :: (Text, Text, Text) -> Text
    showInequality (name,obj1,obj2) = name <> ": \n" <> obj1 <> "\n" <> obj2 <> "\n"

    sl1 = documentsignatorylinks d1
    sl2 = documentsignatorylinks d2

    checkSigLink :: SignatoryLink -> SignatoryLink -> [Maybe (Text, Text, Text)]
    checkSigLink s1 s2 = map (\f -> f s1 s2) [
        checkEqualByAllowSecondNothing (==) "maybesignatory" maybesignatory
      , checkEqualBy eqMaybeSignInfo "maybesigninfo" maybesigninfo
      , checkEqualBy eqMaybeSignInfo "maybeseeninfo" maybeseeninfo
      , checkEqualBy eqMaybeUtcTime "maybereadinvite" maybereadinvite
      , checkEqualBy (==) "mailinvitationdeliverystatus" mailinvitationdeliverystatus
      , checkEqualBy (==) "smsinvitationdeliverystatus" smsinvitationdeliverystatus
      , checkEqualBy eqMaybeUtcTime "signatorylinkdeleted" signatorylinkdeleted
      , checkEqualBy eqMaybeUtcTime "signatorylinkreallydeleted" signatorylinkreallydeleted
      , checkEqualBy (==) "signatorylinkcsvupload" signatorylinkcsvupload
      , \s1' s2' -> checkEqualSignatoryFields "signatoryfields" (signatoryfields s1') (signatoryfields s2')
      , checkEqualBy (==) "signatoryisauthor" (signatoryisauthor)
      , checkEqualBy (==) "signatoryrole" (signatoryrole)
      , checkEqualBy (==) "signatorysignorder" (signatorysignorder)
      , checkEqualBy eqMaybeUtcTime "signatorylinkrejectiontime" signatorylinkrejectiontime
      , checkEqualBy (==) "signatorylinkrejectionreason" signatorylinkrejectionreason
      , checkEqualBy (==) "signatorylinkauthenticationtosignmethod" signatorylinkauthenticationtosignmethod
      , checkEqualBy (==) "signatorylinkdeliverymethod" signatorylinkdeliverymethod
      ]

    inequalities :: [(Text, Text, Text)]
    inequalities = catMaybes $ map (\f -> f d1 d2) [
        checkEqualBy (==) "documenttitle" documenttitle
      , checkEqualBy (==) "documentfiles" documentfile
      , checkEqualBy (==) "documentsealedfiles" documentsealedfile
      , checkEqualBy (==) "documentstatus" documentstatus
      , checkEqualBy (==) "documenttype" documenttype
      , checkEqualBy eqUtcTime "documentctime" documentctime
      , checkEqualBy eqUtcTime "documentmtime" documentmtime
      , checkEqualBy (==) "documentdaystosign" documentdaystosign
      , checkEqualBy (==) "documentdaystoremind" documentdaystoremind
      , checkEqualBy eqMaybeUtcTime "documenttimeouttime" documenttimeouttime
      , checkEqualBy eqMaybeSignInfo "documentinvitetime" documentinvitetime
      , checkEqualBy (==) "documentinvitetext" documentinvitetext
      , checkEqualBy (==) "documentconfirmtext" documentconfirmtext
      , checkEqualBy (==) "documentsharing" documentsharing
      , checkEqualBy (==) "documenttags" documenttags
      , checkEqualBy (==) "documentauthorattachments" (sort . documentauthorattachments)
      , checkEqualBy (==) "documentlang" documentlang
      , checkEqualBy (==) "documentapiv1callbackurl" documentapiv1callbackurl
      , checkEqualBy (==) "documentapiv2callbackurl" documentapiv2callbackurl
      , checkEqualBy (==) "documentsealstatus" documentsealstatus
      , checkEqualBy (==) "documentsignatorylinks count" (length . documentsignatorylinks)
      ] <> concat (zipWith checkSigLink sl1 sl2)

    -- getCurrentTime gives precision up to nanoseconds whereas PostgreSQL
    -- gives precision up to microseconds, so take that into account.
    eqUtcTime t1 t2 = abs (diffUTCTime t1 t2) < 0.00001

    eqMaybeUtcTime = liftEqMaybe eqUtcTime

    eqMaybeSignInfo = liftEqMaybe $ \si1 si2 -> and [
        signtime si1 `eqUtcTime`signtime si2
      , signipnumber si1 == signipnumber si2
      ]

    liftEqMaybe :: (t -> t -> Bool) -> Maybe t -> Maybe t -> Bool
    liftEqMaybe f (Just t1) (Just t2) = f t1 t2
    liftEqMaybe _ Nothing   Nothing   = True
    liftEqMaybe _ _         _         = False

data NewSignatoryAccessToken = NewSignatoryAccessToken
  SignatoryLinkID SignatoryAccessTokenReason (Maybe UTCTime)
instance (CryptoRNG m, MonadDB m, MonadThrow m, MonadTime m)
  => DBUpdate m NewSignatoryAccessToken MagicHash where
  update (NewSignatoryAccessToken slid reason mexptime) = do
    hash <- random
    update $ NewSignatoryAccessTokenWithHash slid reason mexptime hash

data NewSignatoryAccessTokenWithHash = NewSignatoryAccessTokenWithHash
  SignatoryLinkID SignatoryAccessTokenReason (Maybe UTCTime) MagicHash
instance (CryptoRNG m, MonadDB m, MonadThrow m, MonadTime m)
  => DBUpdate m NewSignatoryAccessTokenWithHash MagicHash where
  update (NewSignatoryAccessTokenWithHash slid reason mexptime hash) = do
    runQuery_ . sqlInsertSelect "signatory_access_tokens" "" $ do
      sqlSet "hash" hash
      sqlSet "signatory_link_id" slid
      sqlSet "reason" reason
      sqlSet "expiration_time" mexptime
      sqlResult "hash"
    fetchOne runIdentity

data PurgeTimeoutedSignatoryAccessTokens= PurgeTimeoutedSignatoryAccessTokens
instance (MonadDB m, MonadTime m)
  => DBUpdate m PurgeTimeoutedSignatoryAccessTokens () where
  update _ = do
    now <- currentTime
    runQuery_ . sqlDelete "signatory_access_tokens" $ do
      sqlWith "ids_to_purge" . sqlSelect "signatory_access_tokens sat" $ do
        sqlResult "sat.id"
        sqlWhere $ "sat.expiration_time IS NOT NULL"
        sqlWhere $ "sat.expiration_time <" <?> now
      sqlWhere "id IN (SELECT id FROM ids_to_purge)"

data InvalidateSignatoryAccessTokens
  = InvalidateSignatoryAccessTokens SignatoryLinkID SignatoryAccessTokenReason
instance (DocumentMonad m, MonadDB m)
  => DBUpdate m InvalidateSignatoryAccessTokens () where
  update (InvalidateSignatoryAccessTokens slid reason) = do
    runQuery_ . sqlDelete "signatory_access_tokens" $ do
      sqlWhereEq "reason" reason
      sqlWhereEq "signatory_link_id" slid

newtype ExtendSignatoryAccessTokensForAccessBeforeClosing
  = ExtendSignatoryAccessTokensForAccessBeforeClosing DocumentID
instance (MonadDB m, MonadTime m)
  => DBUpdate m ExtendSignatoryAccessTokensForAccessBeforeClosing () where
  update (ExtendSignatoryAccessTokensForAccessBeforeClosing did) = do
    now <- currentTime

    runQuery_ . sqlUpdate "signatory_access_tokens" $ do
      sqlWith "signatory_link_ids" . sqlSelect "signatory_links sl" $ do
        sqlResult "sl.id"
        sqlWhereEq "sl.document_id" did

      sqlSet "expiration_time" $ 1 `daysAfter` now
      sqlWhereIn "reason" [
          SignatoryAccessTokenForMailBeforeClosing
        , SignatoryAccessTokenForSMSBeforeClosing
        ]
      sqlWhere "signatory_link_id IN (SELECT id FROM signatory_link_ids)"

data SetDocumentApiCallbackResult = SetDocumentApiCallbackResult DocumentID (Maybe Text)
instance MonadDB m => DBUpdate m SetDocumentApiCallbackResult () where
  -- this should be written as proper upsert, but hpq does not support it yet
  -- not a problem, we don't care about any races for this
  update (SetDocumentApiCallbackResult did mtext) = do
    runQuery_ . sqlUpdate "api_callback_result" $ do
      sqlSet "callback_result" mtext
      sqlWhereEq "document_id" did
    runQuery_ . sqlInsertSelect "api_callback_result" "" $ do
      sqlSet "document_id" did
      sqlSet "callback_result" mtext
      sqlWhereNotExists . sqlSelect "api_callback_result acr" $ do
        sqlResult "1"
        sqlWhereEq "acr.document_id" did
