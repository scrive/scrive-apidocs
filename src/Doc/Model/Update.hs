module Doc.Model.Update
  ( AddDocumentAttachment(..)
  , ArchiveDocument(..)
  , AttachFile(..)
  , DetachFile(..)
  , AppendSealedFile(..)
  , AppendExtendedSealedFile(..)
  , CancelDocument(..)
  , ChangeSignatoryEmailWhenUndelivered(..)
  , ChangeSignatoryPhoneWhenUndelivered(..)
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
  , ArchiveIdleDocuments(..)
  , unsavedDocumentLingerDays
  , RejectDocument(..)
  , RemoveDocumentAttachment(..)
  , ResetSignatoryDetails(..)
  , RestartDocument(..)
  , ProlongDocument(..)
  , RestoreArchivedDocument(..)
  , ReallyDeleteDocument(..)
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

import Control.Arrow (second)
import Control.Monad.Catch
import Data.Decimal (realFracToDecimal)
import Data.Int
import Data.Maybe hiding (fromJust)
import Log
import Text.StringTemplates.Templates
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

import API.APIVersion
import Company.Model
import Control.Monad.Trans.Instances ()
import Crypto.RNG
import DB
import DB.TimeZoneName (TimeZoneName, withTimeZone, defaultTimeZoneName)
import Doc.Conditions
import Doc.DocStateCommon
import Doc.DocStateData
import Doc.DocumentID
import Doc.DocumentMonad (updateDocumentWithID, updateDocument, DocumentMonad, withDocument, theDocumentID)
import Doc.DocUtils
import Doc.Model.Query (GetSignatoryLinkByID(..), GetDocumentByDocumentID(..), GetDocumentTags(..), GetDocsSentBetween(..), GetDocsSent(..), GetSignatoriesByEmail(..))
import Doc.SealStatus (SealStatus(..), hasGuardtimeSignature)
import Doc.SignatoryFieldID
import Doc.SignatoryLinkID
import Doc.SignatoryScreenshots
import Doc.Tables
import EID.Signature.Model
import EvidenceLog.Model
import File.FileID
import File.Model
import IPAddress
import KontraPrelude
import Log.Identifier
import MagicHash
import MinutesTime
import User.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.Image
import Utils.Monad
import qualified DB.TimeZoneName as TimeZoneName
import qualified Doc.Screenshot as Screenshot

-- For this to work well we assume that signatories are ordered: author first, then all with ids set, then all with id == 0
-- FIXME: this assumption needs to be encoded in the type.
insertSignatoryLinks :: MonadDB m => DocumentID -> [SignatoryLink] -> m ()
insertSignatoryLinks _ [] = return ()
insertSignatoryLinks did links = do
  runQuery_ . sqlInsert "signatory_links" $ do
    sqlSet "document_id" did
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
    sqlSetList "csv_contents" $ fmap csvcontents <$> signatorylinkcsvupload <$> links
    sqlSetList "deleted" $ signatorylinkdeleted <$> links
    sqlSetList "really_deleted" $ signatorylinkreallydeleted <$> links
    sqlSetList "sign_redirect_url" $ signatorylinksignredirecturl <$> links
    sqlSetList "reject_redirect_url" $ signatorylinkrejectredirecturl <$> links
    sqlSetList "rejection_time" $ signatorylinkrejectiontime <$> links
    sqlSetList "rejection_reason" $ signatorylinkrejectionreason <$> links
    sqlSetList "authentication_to_view_method" $ signatorylinkauthenticationtoviewmethod <$> links
    sqlSetList "authentication_to_sign_method" $ signatorylinkauthenticationtosignmethod <$> links
    sqlSetList "delivery_method" $ signatorylinkdeliverymethod <$> links
    sqlSetList "confirmation_delivery_method" $ signatorylinkconfirmationdeliverymethod <$> links
    sqlResult "id"

  -- update ids
  links' <- zipWith (\sl slid -> sl { signatorylinkid = slid }) links
    <$> fetchMany runIdentity

  insertSignatoryAttachments
    [(signatorylinkid sl, att) | sl <- links', att <- signatoryattachments sl]

  insertSignatoryLinkFields
    [(signatorylinkid sl, fld) | sl <- links', fld <- signatoryfields sl]

insertSignatoryAttachments :: MonadDB m => [(SignatoryLinkID, SignatoryAttachment)] -> m ()
insertSignatoryAttachments [] = return ()
insertSignatoryAttachments atts = runQuery_ . sqlInsert "signatory_attachments" $ do
  sqlSetList "signatory_link_id" $ map fst atts
  sqlSetList "file_id" $ map (signatoryattachmentfile . snd) atts
  sqlSetList "name" $ map (signatoryattachmentname . snd) atts
  sqlSetList "description" $ map (signatoryattachmentdescription . snd) atts

insertSignatoryLinkFields :: MonadDB m => [(SignatoryLinkID, SignatoryField)] -> m ()
insertSignatoryLinkFields [] = return ()
insertSignatoryLinkFields fields = runQuery_ . sqlInsert "signatory_link_fields" $ do
  sqlSetList "signatory_link_id" $ map fst fields
  sqlSetList "type" $ map (fieldType . snd) fields
  sqlSetList "name_order" $ map (name_order . snd) fields
  sqlSetList "custom_name" $ map (custom_name . snd) fields
  sqlSetList "is_author_filled" $ map (author_filled . snd) fields
  sqlSetList "value_text" $ map (fieldTextValue . snd) fields
  sqlSetList "value_file_id" $ map (fieldFileValue . snd) fields
  sqlSetList "value_bool" $ map (fieldBoolValue . snd) fields
  sqlSetList "placements" $ map (fieldPlacements . snd) fields
  sqlSetList "obligatory" $ map (fieldIsObligatory . snd) fields
  sqlSetList "should_be_filled_by_author" $ map (fieldShouldBeFilledBySender . snd) fields
  where
    name_order field = case field of
      SignatoryNameField (NameField{snfNameOrder})-> Just snfNameOrder
      _ -> Nothing
    custom_name field = case field of
      SignatoryTextField (TextField{stfName})-> stfName
      SignatorySignatureField (SignatureField{ssfName})-> ssfName
      SignatoryCheckboxField (CheckboxField{schfName})-> schfName
      _ -> ""
    author_filled field = case field of
      SignatoryTextField (TextField{stfFilledByAuthor})-> stfFilledByAuthor
      _ -> False

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

insertSignatoryScreenshots :: (MonadDB m, MonadThrow m)
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
    sqlSet "show_footer" documentshowfooter
    sqlSet "lang" documentlang
    sqlSet "sharing" documentsharing
    sqlSet "object_version" documentobjectversion
    sqlSet "api_v1_callback_url" documentapiv1callbackurl
    sqlSet "api_v2_callback_url" documentapiv2callbackurl
    sqlSet "token" documentmagichash
    sqlSet "time_zone_name" documenttimezonename
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
  newFromDocument f doc

newFromDocument :: (MonadDB m, MonadThrow m, MonadLog m, CryptoRNG m) => (Document -> Document) -> Document -> m (Maybe Document)
newFromDocument f doc = do
  Just `liftM` insertNewDocument (f doc)

data ArchiveDocument = ArchiveDocument UserID Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m, MonadTime m) => DBUpdate m ArchiveDocument () where
  update (ArchiveDocument uid _actor) = updateDocumentWithID $ \did -> do
    now <- currentTime
    kRunManyOrThrowWhyNot $ sqlUpdate "signatory_links" $ do
        sqlSet "deleted" now

        sqlWhereExists $ sqlSelect "users" $ do
          sqlJoinOn "users AS same_company_users" "(users.company_id = same_company_users.company_id OR users.id = same_company_users.id)"
          sqlWhere "signatory_links.user_id = users.id"

          sqlWhereUserIsDirectlyOrIndirectlyRelatedToDocument uid
          sqlWhereUserIsSelfOrCompanyAdmin

        sqlWhereExists $ sqlSelect "documents" $ do
          sqlWhere $ "signatory_links.document_id = " <?> did
          sqlWhere "documents.id = signatory_links.document_id"

          sqlWhereDocumentIsNotDeleted
          sqlWhereDocumentStatusIsOneOf [Preparation, Closed, Canceled, Timedout, Rejected, DocumentError]

data ReallyDeleteDocument = ReallyDeleteDocument UserID Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m, MonadTime m) => DBUpdate m ReallyDeleteDocument () where
  update (ReallyDeleteDocument uid _actor) = updateDocumentWithID $ \did -> do
    now <- currentTime
    kRunManyOrThrowWhyNot $ sqlUpdate "signatory_links" $ do
        sqlSet "really_deleted" now

        sqlWhereExists $ sqlSelect "users" $ do
          sqlJoinOn "users AS same_company_users" "(users.company_id = same_company_users.company_id OR users.id = same_company_users.id)"
          sqlWhere "signatory_links.user_id = users.id"

          sqlWhereUserIsDirectlyOrIndirectlyRelatedToDocument uid
          sqlWhereUserIsSelfOrCompanyAdmin

        sqlWhereExists $ sqlSelect "documents" $ do
          sqlWhere $ "signatory_links.document_id = " <?> did
          sqlWhere "documents.id = signatory_links.document_id"

          sqlWhereDocumentIsDeleted
          sqlWhereDocumentIsNotReallyDeleted
          sqlWhereDocumentStatusIsOneOf [Preparation, Closed, Canceled, Timedout, Rejected, DocumentError]


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

data ChangeSignatoryEmailWhenUndelivered = ChangeSignatoryEmailWhenUndelivered SignatoryLinkID (Maybe User) String Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m ChangeSignatoryEmailWhenUndelivered () where
  update (ChangeSignatoryEmailWhenUndelivered slid muser email actor) = do
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
    void $ update $ InsertEvidenceEvent
          ChangeSignatoryEmailWhenUndeliveredEvidence
          (F.value "oldemail" oldemail >> F.value "newemail" email)
          actor

data ChangeSignatoryPhoneWhenUndelivered = ChangeSignatoryPhoneWhenUndelivered SignatoryLinkID String Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m ChangeSignatoryPhoneWhenUndelivered () where
  update (ChangeSignatoryPhoneWhenUndelivered slid phone actor) = do
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
    void $ update $ InsertEvidenceEvent
          ChangeSignatoryPhoneWhenUndeliveredEvidence
          (F.value "oldphone" oldphone >> F.value "newphone" phone)
          actor

data ChangeAuthenticationToSignMethod = ChangeAuthenticationToSignMethod SignatoryLinkID AuthenticationToSignMethod (Maybe String) Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m ChangeAuthenticationToSignMethod () where
  update (ChangeAuthenticationToSignMethod slid newAuth mValue actor) = do
    let extraInfoField StandardAuthenticationToSign = Nothing
        extraInfoField SEBankIDAuthenticationToSign     = Just PersonalNumberFI
        extraInfoField SMSPinAuthenticationToSign   = Just MobileFI
    (oldAuth, sig) <- updateDocumentWithID $ const $ do
      -- Set the new authentication method in signatory_links
      -- Return the old authentication method
      (oldAuth' :: AuthenticationToSignMethod) <- kRunAndFetch1OrThrowWhyNot runIdentity $ sqlUpdate "signatory_links" $ do
        sqlFrom "signatory_links AS signatory_links_old"
        sqlSet "authentication_to_sign_method" newAuth
        sqlResult "signatory_links_old.authentication_to_sign_method"
        sqlWhere "signatory_links.id = signatory_links_old.id"
        sqlWhereEq "signatory_links.id" slid
        sqlWhereExists $ sqlSelect "documents" $ do
          sqlWhere "documents.id = signatory_links.document_id"
          sqlWhereDocumentStatusIs Pending
      -- Get the `SignatoryLink`
      siglink <- theDocumentID >>= \did -> dbQuery $ GetSignatoryLinkByID did slid Nothing
      -- When the old authentication method needed extra info and the new one
      -- isin't the same type, then we need to make the field for the old
      -- authentication method non obligatory
      let signatoryLinkHasPlacementLike :: SignatoryLink -> FieldIdentity -> Bool
          signatoryLinkHasPlacementLike sl fi = isJust $ getFieldByIdentity fi $ signatoryfields sl
      case extraInfoField oldAuth' of
        Nothing -> return ()
        Just prvAuthField ->
          when ( oldAuth' /= newAuth
                 && signatoryLinkHasPlacementLike siglink prvAuthField
               )
               ( kRun1OrThrowWhyNot $ sqlUpdate "signatory_link_fields" $ do
                   sqlSet "obligatory" False
                   sqlWhereEq "signatory_link_id" slid
                   sqlWhereEq "type" $ fieldTypeFromFieldIdentity prvAuthField
               )
      -- When the new authentication method needs extra info then we need to
      -- either add it (if provided) or make obligatory for the signatory
      case extraInfoField newAuth of
        Nothing -> return ()
        Just authMethodField ->
          case getFieldByIdentity authMethodField (signatoryfields siglink) of
               Just _  -> kRun1OrThrowWhyNot $ sqlUpdate "signatory_link_fields" $ do
                 case mValue of
                      Just a  -> sqlSet "value_text" a
                      Nothing -> return ()
                 sqlSet "obligatory" True
                 sqlWhereEq "signatory_link_id" slid
                 sqlWhereEq "type" $ fieldTypeFromFieldIdentity authMethodField
               -- Note: default in table for `obligatory` is true
               Nothing -> runQuery_ . sqlInsert "signatory_link_fields" $ do
                 sqlSet "signatory_link_id" slid
                 sqlSet "value_text" $ fromMaybe "" mValue
                 sqlSet "type" $ fieldTypeFromFieldIdentity authMethodField
                 sqlSet "placements" ("[]"::String)
      updateMTimeAndObjectVersion (actorTime actor)
      return (oldAuth', siglink)
    -- Evidence Events
    -- One for changing the value, the other for changing authentication method
    case extraInfoField newAuth of
         Nothing -> return ()
         Just authMethodField -> do
           let previousValue = fromMaybe "" $ fieldTextValue =<< getFieldByIdentity authMethodField (signatoryfields sig)
               value         = fromMaybe "" mValue
           when (value /= previousValue)
                (void $ update $ InsertEvidenceEvent
                  (getEvidenceTextForUpdateField sig authMethodField)
                  (do F.value "value" value
                      F.value "previousvalue" previousValue
                      when (value == "") $
                          F.value "newblank" True
                      when (previousValue == "") $
                          F.value "prvblank" True
                  )
                  actor
                )
    let insertEvidence e = void $ update $ InsertEvidenceEvent e
          (F.value "signatory" (getSmartName sig)) actor
    case (oldAuth, newAuth) of
         (StandardAuthenticationToSign, SEBankIDAuthenticationToSign)   -> insertEvidence ChangeAuthenticationToSignMethodStandardToSEBankIDEvidence
         (StandardAuthenticationToSign, SMSPinAuthenticationToSign) -> insertEvidence ChangeAuthenticationToSignMethodStandardToSMSEvidence
         (SEBankIDAuthenticationToSign, StandardAuthenticationToSign)   -> insertEvidence ChangeAuthenticationToSignMethodSEBankIDToStandardEvidence
         (SEBankIDAuthenticationToSign, SMSPinAuthenticationToSign)     -> insertEvidence ChangeAuthenticationToSignMethodSEBankIDToSMSEvidence
         (SMSPinAuthenticationToSign, StandardAuthenticationToSign) -> insertEvidence ChangeAuthenticationToSignMethodSMSToStandardEvidence
         (SMSPinAuthenticationToSign, SEBankIDAuthenticationToSign)     -> insertEvidence ChangeAuthenticationToSignMethodSMSToSEBankIDEvidence
         _ -> return()

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
            let timestamp = formatTime' "%F" time ++ " " ++ TimeZoneName.toString tzn
            -- Need to temporarily set session timezone to any one
            -- that recognizes daylight savings so that the day
            -- interval addition advances the time properly across DST changes
            -- (i.e., so that we stay on midnight)
            -- http://www.postgresql.org/docs/9.2/static/functions-datetime.html
            withTimeZone defaultTimeZoneName $ do
              lang :: Lang <- kRunAndFetch1OrThrowWhyNot runIdentity $ sqlUpdate "documents" $ do
                sqlSet "status" Pending
                sqlSetCmd "timeout_time" $ "cast (" <?> timestamp <+> "as timestamp with time zone)"
                            <+> "+ ((interval '1 day') * documents.days_to_sign) + (interval '23 hours 59 minutes 59 seconds')" -- This interval add almoust one they from description above.
                sqlResult "lang"
                sqlWhereDocumentIDIs docid
                sqlWhereDocumentTypeIs Signable
                sqlWhereDocumentStatusIs Preparation

              runQuery_ . sqlUpdate "signatory_links" $ do
                sqlSet "csv_contents" (Nothing :: Maybe String)
                sqlWhereEq "document_id" docid

              runQuery_ $ "SELECT timeout_time FROM documents WHERE id =" <?> docid
              tot <- fetchOne runIdentity
              updateMTimeAndObjectVersion (actorTime actor)
              return (lang, tot)
    void $ update $ InsertEvidenceEvent
                PreparationToPendingEvidence
                (  F.value "timezone" (TimeZoneName.toString tzn)
                >> F.value "lang" (show lang)
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
                   sqlWhereAllSignatoriesHaveSigned
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
        sqlWhereSignatoryHasNotSigned

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

data MarkDocumentSeen = MarkDocumentSeen SignatoryLinkID MagicHash Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m MarkDocumentSeen () where
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
              sqlWhereDocumentStatusIsOneOf [Pending, Timedout, Canceled, DocumentError, Rejected]

data MarkInvitationRead = MarkInvitationRead SignatoryLinkID Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m MarkInvitationRead Bool where
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
instance (CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m, TemplatesMonad m) => DBUpdate m NewDocument Document where
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
    let doc = def
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

    insertDocument doc

data RejectDocument = RejectDocument SignatoryLinkID (Maybe String) Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m RejectDocument () where
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
instance (CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m, TemplatesMonad m) => DBUpdate m RestartDocument (Maybe Document) where
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
        logAttention "Document restart failed" $ object [
            identifier_ $ documentid doc
          , "error" .= err
          ]
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
                           let newFields = for (signatoryfields sl) $ \f ->
                                             case f of
                                               SignatorySignatureField sf -> SignatorySignatureField $ sf{ssfValue = Nothing}
                                               _ -> f
                           return $ def {
                                signatorylinkid            = (unsafeSignatoryLinkID 0)
                              , signatorymagichash = magichash
                              , signatoryfields            = newFields
                              , signatoryisauthor          = signatoryisauthor sl
                              , signatoryispartner         = signatoryispartner sl
                              , signatorysignorder         = signatorysignorder sl
                              , signatorylinkcsvupload       = signatorylinkcsvupload sl
                              , signatoryattachments         = signatoryattachments sl
                              , signatorylinksignredirecturl = signatorylinksignredirecturl sl
                              , signatorylinkrejectredirecturl = signatorylinkrejectredirecturl sl
                              , signatorylinkauthenticationtoviewmethod = signatorylinkauthenticationtoviewmethod sl
                              , signatorylinkauthenticationtosignmethod = signatorylinkauthenticationtosignmethod sl
                              , signatorylinkdeliverymethod       = signatorylinkdeliverymethod sl
                              , maybesignatory = if (isAuthor sl) then maybesignatory sl else Nothing
                          }
      return doc {documentstatus = Preparation,
                  documenttimeouttime = Nothing,
                  documentsignatorylinks = newSignLinks,
                  documentapiv1callbackurl = Nothing,
                  documentapiv2callbackurl = Nothing
                 }

data RestoreArchivedDocument = RestoreArchivedDocument User Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m RestoreArchivedDocument () where
  update (RestoreArchivedDocument user _actor) = updateDocumentWithID $ \did -> do
    kRunManyOrThrowWhyNot $ sqlUpdate "signatory_links" $ do

      sqlSet "deleted" (Nothing :: Maybe UTCTime)

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

data SetInviteText = SetInviteText String Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetInviteText Bool where
  update (SetInviteText text _actor) = updateWithoutEvidence "invite_text" text

data SetConfirmText = SetConfirmText String Actor
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

data SetShowFooter = SetShowFooter Bool Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetShowFooter Bool where
  update (SetShowFooter bool _actor) = updateWithoutEvidence "show_footer" bool

data SetDaysToSign = SetDaysToSign Int32 Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetDaysToSign Bool where
  update (SetDaysToSign days _actor) = updateWithoutEvidence "days_to_sign" days

data SetDaysToRemind = SetDaysToRemind (Maybe Int32) Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetDaysToRemind Bool where
  update (SetDaysToRemind days _actor) = updateWithoutEvidence "days_to_remind" days

data SetDocumentTitle = SetDocumentTitle String Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetDocumentTitle Bool where
  update (SetDocumentTitle doctitle _actor) = updateWithoutEvidence "title" doctitle

data SetDocumentLang = SetDocumentLang Lang Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetDocumentLang Bool where
  update (SetDocumentLang lang _actor) = updateWithoutEvidence "lang" lang

data SetEmailInvitationDeliveryStatus = SetEmailInvitationDeliveryStatus SignatoryLinkID DeliveryStatus Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetEmailInvitationDeliveryStatus Bool where
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
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetSMSInvitationDeliveryStatus Bool where
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
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m SetDocumentUnsavedDraft () where
  update (SetDocumentUnsavedDraft flag) = updateDocumentWithID $ \did -> do
    kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
      sqlSet "unsaved_draft" flag
      sqlWhereDocumentIDIs did

data SignDocument = SignDocument SignatoryLinkID MagicHash (Maybe ESignature) (Maybe String) SignatoryScreenshots Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m, CryptoRNG m) => DBUpdate m SignDocument () where
  update (SignDocument slid mh mesig mpin screenshots actor) = do
    updateDocumentWithID $ \docid -> do
      let ipnumber = fromMaybe noIP $ actorIP actor
          time     = actorTime actor
      kRun1OrThrowWhyNot $ sqlUpdate "signatory_links" $ do
           sqlFrom "documents"
           sqlSet "sign_ip"                            ipnumber
           sqlSet "sign_time"                          time
           sqlWhere "documents.id = signatory_links.document_id"
           sqlWhereDocumentIDIs docid
           sqlWhereSignatoryLinkIDIs slid
           sqlWhereDocumentTypeIs Signable
           sqlWhereDocumentStatusIs Pending
           sqlWhereSignatoryIsPartner
           sqlWhereSignatoryHasNotSigned
           case (mesig, mpin) of
             (Just _, _) -> sqlWhereSignatoryAuthenticationToSignMethodIs SEBankIDAuthenticationToSign
             (_, Just _) -> sqlWhereSignatoryAuthenticationToSignMethodIs SMSPinAuthenticationToSign -- We should check pin here, but for now we do it in controler
             (Nothing, Nothing) -> sqlWhereSignatoryAuthenticationToSignMethodIs StandardAuthenticationToSign
           sqlWhereSignatoryLinkMagicHashIs mh
      updateMTimeAndObjectVersion (actorTime actor)
    sl <- theDocumentID >>= \docid -> query $ GetSignatoryLinkByID docid slid Nothing
    let legacy_signature_error = $unexpectedError "signing with legacy signatures is not possible"
        signatureFields = case (mesig, mpin) of
          (Just LegacyBankIDSignature_{}, _) -> legacy_signature_error
          (Just LegacyTeliaSignature_{}, _) -> legacy_signature_error
          (Just LegacyNordeaSignature_{}, _) -> legacy_signature_error
          (Just LegacyMobileBankIDSignature_{}, _) -> legacy_signature_error
          (Just (BankIDSignature_ BankIDSignature{..}), _) -> do
            F.value "eleg" True
            F.value "signatory_name" bidsSignatoryName
            F.value "signatory_personal_number" bidsSignatoryPersonalNumber
            F.value "signed_text" bidsSignedText
            F.value "provider" ("Swedish BankID" :: String)
            F.value "signature" $ B64.encode . unBinary $ bidsSignature
            F.value "ocsp_response" $ B64.encode . unBinary $ bidsOcspResponse
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
            runQuery_ $ "DELETE FROM signatory_links WHERE document_id = " <?> documentid
            siglinks <- forM signatories $ \sl -> do
                     magichash <- random
                     return $ sl {  signatorymagichash = magichash,
                                    maybesignatory = if (isAuthor sl) then (maybesignatory sl) else Nothing
                                 }
            insertSignatoryLinks documentid siglinks
            return True

          errs -> do
            logAttention "Cannot reset signatory details on document" $ object [
                identifier_ documentid
              , "errors" .= errs
              ]
            return False

data CloneDocumentWithUpdatedAuthor = CloneDocumentWithUpdatedAuthor User Document Actor
instance (MonadDB m, MonadThrow m, MonadLog m, TemplatesMonad m, CryptoRNG m) => DBUpdate m CloneDocumentWithUpdatedAuthor (Maybe DocumentID) where
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


data DocumentFromTemplate = DocumentFromTemplate Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m DocumentFromTemplate () where
  update (DocumentFromTemplate _actor) = updateDocumentWithID $ \did -> do
    kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
       sqlSet "status" Preparation
       sqlSet "type" Signable
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

data ProlongDocument = ProlongDocument Int32 TimeZoneName Actor
instance (DocumentMonad m, TemplatesMonad m, MonadMask m) => DBUpdate m ProlongDocument () where
  update (ProlongDocument days tzn actor) = do
    updateDocumentWithID $ \did -> do
      -- Whole TimeZome behaviour is a clone of what is happending with making document ready for signing.
      let time = actorTime actor
      let timestamp = formatTime' "%F" time ++ " " ++ TimeZoneName.toString tzn
      withTimeZone defaultTimeZoneName $ kRun1OrThrowWhyNot $ sqlUpdate "documents" $ do
         sqlSet "status" Pending
         sqlSet "mtime" time
         sqlSetCmd "timeout_time" $ "cast (" <?> timestamp <+> "as timestamp with time zone)"
                              <+> "+ (interval '1 day') * " <?> days <+> " + (interval '23 hours 59 minutes 59 seconds')"
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

data SetDocumentAPICallbackURL = SetDocumentAPICallbackURL APIVersion (Maybe String)
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



data PostReminderSend = PostReminderSend SignatoryLink (Maybe String) Bool Actor
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
         sqlWhereSignatoryHasNotSigned
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
  update (UpdateFieldsForSigning sl fields signaturesContent actor) = updateDocumentWithID $ const $ do
    -- Document has to be in Pending state
    -- signatory could not have signed already
    let slid = signatorylinkid sl
    let updateValue :: (FieldIdentity, FieldValue) -> m ()
        updateValue (fieldIdent, newValue) = do
          let custom_name = case fieldIdent of
                              TextFI xname   -> xname
                              CheckboxFI xname -> xname
                              SignatureFI xname -> xname
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
                           sqlWhereEq "value_text" (""::String) -- Note: if we allow values to be overwritten, the evidence events need to be adjusted to reflect the old value.
                           sqlWhereIn "type" [TextFT, NameFT ,EmailFT,CompanyFT,PersonalNumberFT,PersonalNumberFT,CompanyNumberFT, MobileFT]
                       , sqlWhereIn "type" [CheckboxFT, SignatureFT]
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
              emptyValue (StringFV s) = null s
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
                                                   Just s -> BS.unpack $ imgEncodeRFC2397 $ s
                   F.value "previousvalue"$ case oldValue of
                     StringFV s -> s
                     BoolFV False  -> ""
                     BoolFV True -> "checked"
                     FileFV Nothing -> ""
                     FileFV (Just fi) -> case (lookup fi signaturesContent) of
                                                   Nothing -> ""
                                                   Just s -> BS.unpack $ imgEncodeRFC2397 $ s
                   when (emptyValue newValue) $
                       F.value "newblank" True
                   when (emptyValue oldValue) $
                       F.value "prvblank" True
                   case (oldField) of
                        Just (SignatoryTextField f) -> do
                          F.value "customfieldname" $ stfName f
                          F.value "fieldname" $ stfName f
                        Just (SignatoryCheckboxField f) -> do
                          F.value "fieldname" $ schfName f
                          when (not $ emptyValue newValue) $ do
                            F.value "checked" $ schfName f
                        Just (SignatorySignatureField f) -> do
                          F.value "fieldname" $ ssfName f
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

data AddDocumentAttachment = AddDocumentAttachment FileID Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m AddDocumentAttachment Bool where
  update (AddDocumentAttachment fid _actor) = updateDocumentWithID $ \did -> do
    runQuery01 . sqlInsertSelect "author_attachments" "" $ do
        sqlSet "document_id" did
        sqlSet "file_id" fid
        sqlWhereExists $ sqlSelect "documents" $ do
          sqlWhereEq "id" did
          sqlWhereEq "status" Preparation

data RemoveDocumentAttachment = RemoveDocumentAttachment FileID Actor
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m RemoveDocumentAttachment Bool where
  update (RemoveDocumentAttachment fid _actor) = updateDocumentWithID $ \did -> do
    runQuery01 $ "DELETE FROM author_attachments WHERE document_id =" <?> did <+> "AND file_id =" <?> fid <+> "AND EXISTS (SELECT 1 FROM documents WHERE id = author_attachments.document_id AND status = " <?> Preparation <+> ")"

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
        , "    AND type = " <?> Signable
        , "    AND status = " <?> Preparation
        , "    AND mtime <" <?> weekAgo
        , "  LIMIT " <?> limit
        , ")"
        ]

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
instance (DocumentMonad m, TemplatesMonad m, MonadThrow m) => DBUpdate m UpdateDraft Bool where
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
    , update $ SetDocumentAPICallbackURL V1 (documentapiv1callbackurl document)
    , update $ SetDocumentAPICallbackURL V2 (documentapiv2callbackurl document)
    , update $ SetDocumentTimeZoneName (documenttimezonename document)
    , updateMTimeAndObjectVersion (actorTime actor) >> return True
    ]

unsavedDocumentLingerDays :: Int
unsavedDocumentLingerDays = 30

data PurgeDocuments = PurgeDocuments Int Int
instance (MonadDB m, MonadTime m) => DBUpdate m PurgeDocuments Int where
  update (PurgeDocuments savedDocumentLingerDays unsavedDocumentLingerDays') = do
    now <- currentTime
    runQuery_ $ "UPDATE signatory_links"
            <+> "  SET really_deleted =" <?> now
            <+> "WHERE signatory_links.user_id IS NOT NULL" -- document belongs to somebody
            <+> "  AND signatory_links.deleted IS NOT NULL" -- somebody deleted that document long time ago
            <+> "  AND signatory_links.deleted +" <?> idays (fromIntegral savedDocumentLingerDays) <+> "<=" <?> now
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

        -- session is still in progress
        <+> "   AND NOT EXISTS(SELECT TRUE"
        <+> "                    FROM signatory_links"
        <+> "                   WHERE signatory_links.document_id = documents.id"
        <+> "                     AND EXISTS (SELECT TRUE"
        <+> "                                   FROM document_session_tokens"
        <+> "                                  WHERE document_session_tokens.signatory_link_id = signatory_links.id))"

        -- company settings require to wait time to allow saving (we
        -- wait even if there is nobody to wait for to make things
        -- simple and more predictable).
        <+> "   AND NOT EXISTS(SELECT TRUE"
        <+> "                    FROM companies, users, signatory_links"
        <+> "                   WHERE signatory_links.document_id = documents.id"
                                   -- linger time is allowed by author's company settings
        <+> "                     AND companies.allow_save_safety_copy"
                                   -- linger time hasn't elapsed yet
        <+> "                     AND documents.mtime +" <?> idays (fromIntegral unsavedDocumentLingerDays') <+> ">" <?> now
        <+> "                     AND signatory_links.is_author"
        <+> "                     AND users.id = signatory_links.user_id"
        <+> "                     AND users.company_id = companies.id)"

    -- set purged time on documents
    rows <- runSQL $ "UPDATE documents"
        <+> "   SET purged_time =" <?> now
        <+> " WHERE documents.id IN (SELECT id"
        <+> "                          FROM documents_to_purge)"

    -- blank out sensitive data
    runSQL_ $ "UPDATE signatory_links"
        <+> "   SET sign_ip = 0"
        <+> "     , seen_ip = 0"
        <+> " WHERE signatory_links.document_id IN (SELECT id FROM documents_to_purge)"

    -- blank out sensitive data in fields
    runSQL_ $ "UPDATE signatory_link_fields"
        <+> "   SET value_text   = CASE WHEN value_text   IS NULL THEN NULL ELSE '' END"
        <+> "   ,   value_bool = CASE WHEN value_bool IS NULL THEN NULL ELSE FALSE END"
        <+> "   ,   value_file_id = NULL"
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

{- | Archive (move to trash) idle documents for signatories.  A
document is idle for a signatory if

   1. the document is not archived for the signatory,
   2. the document is not a template and not pending,
   3. the document author's company's idle_doc_timeout is set,
   4. the signatory belongs to the same company as the author, and
   5. it's been more than idle_doc_timeout days since the document was modified.
-}
data ArchiveIdleDocuments = ArchiveIdleDocuments UTCTime
instance MonadDB m => DBUpdate m ArchiveIdleDocuments Int where
  update (ArchiveIdleDocuments now) = do
    runSQL $ "UPDATE signatory_links"
         <+> "   SET deleted =" <?> now
         <+> " WHERE deleted IS NULL"
         <+> "  AND EXISTS(SELECT TRUE"
         <+> "               FROM documents"
         <+> "               JOIN signatory_links AS author_sl"
         <+> "                 ON author_sl.document_id = documents.id"
         <+> "                AND author_sl.is_author"
         <+> "               JOIN users AS author"
         <+> "                 ON author.id = author_sl.user_id"
         <+> "               JOIN companies as author_company"
         <+> "                 ON author_company.id = author.company_id"
         <+> "                AND author_company.idle_doc_timeout IS NOT NULL"
         <+> "               JOIN users"
         <+> "                 ON users.company_id = author.company_id"
         <+> "              WHERE users.id = signatory_links.user_id"
         <+> "                AND signatory_links.document_id = documents.id"
         <+> "                AND documents.type =" <?> Signable
         <+> "                AND documents.status NOT IN (" <?> Pending <+> ")"
         <+> "                AND documents.mtime + (interval '1 day') * author_company.idle_doc_timeout <" <?> now <+> ")"

-- Update utilities
getEvidenceTextForUpdateField :: SignatoryLink -> FieldIdentity -> CurrentEvidenceEventType
getEvidenceTextForUpdateField sig (NameFI (NameOrder 1))
                            | hasOneNameField sig      = UpdateFieldNameEvidence
                            | otherwise                = UpdateFieldFirstNameEvidence
getEvidenceTextForUpdateField _ (NameFI (NameOrder 2)) = UpdateFieldLastNameEvidence
getEvidenceTextForUpdateField _ (NameFI (NameOrder _)) = $unexpectedError "NameFT with nameorder different than 1 and 2"
getEvidenceTextForUpdateField _ CompanyFI        = UpdateFieldCompanyEvidence
getEvidenceTextForUpdateField _ PersonalNumberFI = UpdateFieldPersonalNumberEvidence
getEvidenceTextForUpdateField _ CompanyNumberFI  = UpdateFieldCompanyNumberEvidence
getEvidenceTextForUpdateField _ EmailFI          = UpdateFieldEmailEvidence
getEvidenceTextForUpdateField _ (TextFI _)      = UpdateFieldCustomEvidence
getEvidenceTextForUpdateField _ MobileFI         = UpdateFieldMobileEvidence
getEvidenceTextForUpdateField _ (SignatureFI _)  = UpdateFieldSignatureEvidence
getEvidenceTextForUpdateField _ (CheckboxFI _)   = UpdateFieldCheckboxEvidence

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

checkEqualBy :: (Eq b, Show b) => String -> (a -> b) -> a -> a -> Maybe (String, String, String)
checkEqualBy name func obj1 obj2
  | func obj1 /= func obj2 = Just (name, show (func obj1), show (func obj2))
  | otherwise              = Nothing

checkEqualSignatoryFields  :: String ->[SignatoryField] -> [SignatoryField] -> Maybe (String, String, String)
checkEqualSignatoryFields name (f:fs) (f':fs') = if fieldsAreAlmoustEqual f f'
                                                   then checkEqualSignatoryFields name fs fs'
                                                   else Just (name, show f, show f')
checkEqualSignatoryFields name (f:_) [] = Just (name, show f, "No field")
checkEqualSignatoryFields name [] (f:_) = Just (name,  "No field", show f)
checkEqualSignatoryFields _ [] [] = Nothing

checkEqualByAllowSecondNothing :: (Eq b, Show b) => String -> (a -> Maybe b) -> a -> a -> Maybe (String, String, String)
checkEqualByAllowSecondNothing name func obj1 obj2
  | func obj1 /= func obj2 && (not (isNothing (func obj2))) = Just (name, show (func obj1), show (func obj2))
  | otherwise              = Nothing

assertEqualDocuments :: (MonadThrow m, MonadLog m) => Document -> Document -> m ()
assertEqualDocuments d1 d2 | null inequalities = return ()
                           | otherwise = do
  logInfo message $ object [
      "inequalities" .= concatMap showInequality inequalities
    ]
  $unexpectedErrorM $ T.unpack message
  where
    message = "Documents aren't equal"
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
                         , checkEqualBy "signatorylinkdeleted" signatorylinkdeleted
                         , checkEqualBy "signatorylinkreallydeleted" signatorylinkreallydeleted
                         , checkEqualBy "signatorylinkcsvupload" signatorylinkcsvupload
                         , \s1' s2' -> checkEqualSignatoryFields "signatoryfields" (signatoryfields s1') (signatoryfields s2')
                         , checkEqualBy "signatoryisauthor" (signatoryisauthor)
                         , checkEqualBy "signatoryispartner" (signatoryispartner)
                         , checkEqualBy "signatorysignorder" (signatorysignorder)
                         , checkEqualBy "signatorylinkrejectiontime" signatorylinkrejectiontime
                         , checkEqualBy "signatorylinkrejectionreason" signatorylinkrejectionreason
                         , checkEqualBy "signatorylinkauthenticationtosignmethod" signatorylinkauthenticationtosignmethod
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
                   , checkEqualBy "documentapiv1callbackurl" documentapiv1callbackurl
                   , checkEqualBy "documentapiv2callbackurl" documentapiv2callbackurl
                   , checkEqualBy "documentsealstatus" documentsealstatus
                   , checkEqualBy "documentsignatorylinks count" (length . documentsignatorylinks)
                   ] ++
                   concat (zipWith checkSigLink sl1 sl2)
