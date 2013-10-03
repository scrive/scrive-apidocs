{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.Migrations where

import Control.Monad
import Control.Monad.IO.Class
import Data.Int
import Data.Monoid
import Data.Maybe
import Database.HDBC
import Text.JSON
import Text.JSON.FromJSValue

import DB
import DB.SQL2
import Doc.Tables
import qualified Log
import Doc.DocumentID
import Utils.Default
import Doc.DocStateData
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BS
import EvidenceLog.Model
import Version
import Doc.SignatoryLinkID
import MinutesTime
import Utils.Read
import Control.Applicative
import Data.String.Utils (split)
import Utils.Prelude
import Text.JSON as JSON

default (SQL)

$(jsonableDeriveConvertible [t| [SignatoryField] |])

addSealStatusToDocument :: MonadDB m => Migration m
addSealStatusToDocument = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 23
  , mgrDo = kRunRaw "ALTER TABLE documents ADD COLUMN seal_status SMALLINT NULL"
}

setMandatoryExpirationTimeInDocument :: (MonadDB m, MonadIO m) => Migration m
setMandatoryExpirationTimeInDocument = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 12
  , mgrDo = do
    -- Fix documents that don't have days to sign or timeout set:
    --   Pending  => 90 days to sign, timeout_time to 90 days from time of migration
    --   Draft => default days to sign
    --   All other documents => set days to sign to 0
    let pendingDaysToSign = 90
    timeout <- (pendingDaysToSign `daysAfter`) `liftM` getMinutesTime
    kRun_ $ "UPDATE documents SET days_to_sign =" <?> documentdaystosign defaultValue
        <+> "WHERE status =" <?> Preparation <+> "AND days_to_sign IS NULL"
    kRun_ $ "UPDATE documents SET days_to_sign =" <?> pendingDaysToSign
                           <+> ", timeout_time =" <?> timeout
        <+> "WHERE status =" <?> Pending <+> "AND timeout_time IS NULL"
    kRun_ "UPDATE documents SET days_to_sign = 0 WHERE days_to_sign IS NULL"
    kRunRaw "ALTER TABLE documents ALTER days_to_sign SET NOT NULL"
}

removeDeletedFromDocuments :: MonadDB m => Migration m
removeDeletedFromDocuments = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 14
  , mgrDo = do
    kRunRaw "ALTER TABLE documents DROP COLUMN deleted"
}

removeSignatoryLinksInternalInsertOrder :: MonadDB m => Migration m
removeSignatoryLinksInternalInsertOrder = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 14
  , mgrDo = do
      kRunRaw $ "ALTER TABLE signatory_link_fields "
             <> "DROP CONSTRAINT fk_signatory_link_fields_signatory_links, "
             <> "ADD CONSTRAINT fk_signatory_link_fields_signatory_links FOREIGN KEY (signatory_link_id) "
             <> "REFERENCES signatory_links (id) MATCH SIMPLE "
             <> "ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE INITIALLY IMMEDIATE"

      kRunRaw $ "ALTER TABLE signatory_attachments "
             <> "DROP CONSTRAINT fk_signatory_attachments_signatory_links,  "
             <> "ADD CONSTRAINT fk_signatory_attachments_signatory_links FOREIGN KEY (signatory_link_id)  "
             <> "REFERENCES signatory_links (id) MATCH SIMPLE  "
             <> "ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE INITIALLY IMMEDIATE"

      -- This gigantic recursive query has to collect all
      -- signatory_links that are problematic and need to get a new
      -- id. Signatory links is problematic either because it itself
      -- is in wrong order or because some other link after it gets
      -- new id will make this link problematic. We iterativelly find
      -- all such links and put them in the table
      -- problematic_signatory_links. Then we assign new ids to
      -- problematic_signatory_links sorted according to
      -- internal_insert_order because this is the order we want to
      -- mimic. At the very end we use this new ids for
      -- signatory_links.
      --
      -- There is no way to make it in one swoop because UPDATE has
      -- unspecified order of updates and it cannot be made
      -- deterministic. Generator nextval('') has to be used in a
      -- SELECT query.
      kRunRaw $ "WITH RECURSIVE "
             <> "problematic_signatory_links(id,internal_insert_order,document_id) AS ( "
             <> "    SELECT signatory_links.id, signatory_links.internal_insert_order, signatory_links.document_id "
             <> "      FROM signatory_links, signatory_links AS sl2 "
             <> "     WHERE sl2.document_id = signatory_links.document_id "
             <> "       AND signatory_links.id < sl2.id "
             <> "       AND signatory_links.internal_insert_order > sl2.internal_insert_order "
             <> "UNION "
             <> "    SELECT signatory_links.id, signatory_links.internal_insert_order, signatory_links.document_id "
             <> "      FROM signatory_links, problematic_signatory_links "
             <> "     WHERE problematic_signatory_links.document_id = signatory_links.document_id "
             <> "       AND signatory_links.id > problematic_signatory_links.id "
             <> "       AND signatory_links.internal_insert_order > problematic_signatory_links.internal_insert_order "
             <> ") "
             <> "UPDATE signatory_links "
             <> "   SET id = problematic_signatory_links_with_new_ids.new_id "
             <> "  FROM (SELECT sorted_problematic_signatory_links.*, nextval('signatory_links_id_seq') AS new_id "
             <> "          FROM (SELECT * "
             <> "                  FROM problematic_signatory_links "
             <> "                 ORDER BY internal_insert_order) "
             <> "                    AS sorted_problematic_signatory_links) "
             <> "            AS problematic_signatory_links_with_new_ids "
             <> " WHERE signatory_links.id = problematic_signatory_links_with_new_ids.id "

      kRunRaw $ "ALTER TABLE signatory_links "
             <> "DROP COLUMN internal_insert_order"
}

removeSignatoryRoles :: MonadDB m => Migration m
removeSignatoryRoles = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 11
  , mgrDo = do
    kRunRaw "ALTER TABLE signatory_links ADD COLUMN is_author BOOL NULL"
    kRunRaw "ALTER TABLE signatory_links ADD COLUMN is_partner BOOL NULL"
    kRunRaw $ "UPDATE signatory_links SET"
      <> "  is_author  = (roles & 2)::BOOL"
      <> ", is_partner = (roles & 1)::BOOL"
    kRunRaw "ALTER TABLE signatory_links DROP COLUMN roles"
    kRunRaw "ALTER TABLE signatory_links ALTER is_author SET NOT NULL"
    kRunRaw "ALTER TABLE signatory_links ALTER is_partner SET NOT NULL"
}

addApiCallbackUrlToDocument :: MonadDB m => Migration m
addApiCallbackUrlToDocument = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 10
  , mgrDo = kRunRaw "ALTER TABLE documents ADD COLUMN api_callback_url TEXT NULL"
}

addUnsavedDraftToDocument :: MonadDB m => Migration m
addUnsavedDraftToDocument = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 15
  , mgrDo = do
      kRunRaw "ALTER TABLE documents ADD COLUMN unsaved_draft BOOL NOT NULL DEFAULT FALSE"
      kRunRaw "UPDATE documents SET unsaved_draft = true WHERE (title ILIKE 'Namnlös%' OR title ILIKE  'Untitled%') AND type = 1 AND status = 1"
}

dropTrustWeaverReferenceFromDocuments :: MonadDB m => Migration m
dropTrustWeaverReferenceFromDocuments = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 16
  , mgrDo = do
      kRunRaw "ALTER TABLE documents DROP COLUMN trust_weaver_reference"
}

moveRejectionInfoFromDocumentsToSignatoryLinks :: MonadDB m => Migration m
moveRejectionInfoFromDocumentsToSignatoryLinks = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 16
  , mgrDo = do
      kRunRaw $  "ALTER TABLE signatory_links"
              <+> "ADD COLUMN rejection_time    TIMESTAMPTZ,"
              <+> "ADD COLUMN rejection_reason  TEXT"
      kRunRaw $   "UPDATE signatory_links"
              <+> "   SET rejection_time = documents.rejection_time,"
              <+> "       rejection_reason = documents.rejection_reason"
              <+> "FROM documents"
              <+> "WHERE documents.rejection_signatory_link_id = signatory_links.id"
}

dropRejectionInfoFromDocuments :: MonadDB m => Migration m
dropRejectionInfoFromDocuments = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 17
  , mgrDo = do
      kRunRaw $ "ALTER TABLE documents"
              <+> "DROP COLUMN rejection_time,"
              <+> "DROP COLUMN rejection_reason,"
              <+> "DROP COLUMN rejection_signatory_link_id"
}

moveAuthenticationMethodFromDocumentsToSignatoryLinks :: MonadDB m => Migration m
moveAuthenticationMethodFromDocumentsToSignatoryLinks = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 17
  , mgrDo = do
      kRunRaw $   "ALTER TABLE signatory_links"
              <+> "ADD COLUMN authentication_method         SMALLINT     NULL"
      kRunRaw $   "UPDATE signatory_links"
              <+> "   SET authentication_method = (SELECT authentication_method FROM documents WHERE documents.id = signatory_links.document_id)"
      kRunRaw $   "ALTER TABLE signatory_links"
              <+> "ALTER COLUMN authentication_method SET NOT NULL"
}

dropAuthenticationMethodFromDocuments :: MonadDB m => Migration m
dropAuthenticationMethodFromDocuments = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 18
  , mgrDo = do
      kRunRaw $ "ALTER TABLE documents"
              <+> "DROP COLUMN authentication_method"
}

moveDeliveryMethodFromDocumentsToSignatoryLinks :: MonadDB m => Migration m
moveDeliveryMethodFromDocumentsToSignatoryLinks = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 19
  , mgrDo = do
      kRunRaw $   "ALTER TABLE signatory_links"
              <+> "ADD COLUMN delivery_method         SMALLINT     NULL"
      kRunRaw $   "UPDATE signatory_links"
              <+> "   SET delivery_method = (SELECT delivery_method FROM documents WHERE documents.id = signatory_links.document_id)"
      kRunRaw $   "ALTER TABLE signatory_links"
              <+> "ALTER COLUMN delivery_method SET NOT NULL"
}

dropDeliveryMethodFromDocuments :: MonadDB m => Migration m
dropDeliveryMethodFromDocuments = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 21
  , mgrDo = do
      kRunRaw $ "ALTER TABLE documents"
              <+> "DROP COLUMN delivery_method"
}

addObjectVersionToDocuments :: MonadDB m => Migration m
addObjectVersionToDocuments = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 22
  , mgrDo = do
      kRunRaw $   "ALTER TABLE documents"
              <+> "ADD COLUMN object_version BIGINT NOT NULL DEFAULT 0"
}


moveCancelationReasonFromDocumentsToSignatoryLinks :: MonadDB m => Migration m
moveCancelationReasonFromDocumentsToSignatoryLinks = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 18
  , mgrDo = do
      kRunRaw $   "ALTER TABLE signatory_links"
              <+> "ADD COLUMN eleg_data_mismatch_message          TEXT     NULL,"
              <+> "ADD COLUMN eleg_data_mismatch_first_name       TEXT     NULL,"
              <+> "ADD COLUMN eleg_data_mismatch_last_name        TEXT     NULL,"
              <+> "ADD COLUMN eleg_data_mismatch_personal_number  TEXT     NULL"
      _ <- kRun $ SQL "SELECT id, cancelation_reason FROM documents WHERE cancelation_reason LIKE '%ELegDataMismatch%'" []

      let fetch acc docid fieldsstr = v : acc
             where
               Ok value = decode fieldsstr
               v = fromJSValue1 (value :: JSValue)
               fromJSValue1 = do
                 g <- fromJSValueField "ELegDataMismatch"
                 case g of
                   Just (JSArray [ JSString message
                                 , slid
                                 , JSString first_name
                                 , JSString last_name
                                 , JSString personal_number
                                 ]) -> do
                                -- this should be array of 5 elements:
                                -- [message, slid, first_name, last_name, personal_number]
                                return ( docid
                                       , fromJust (fromJSValue slid), fromJSString message
                                       , fromJSString first_name, fromJSString last_name
                                       , fromJSString personal_number)
                   _ -> error $ "Could not parse what is in ELegDataMismatch: " ++ fieldsstr ++ ", value is " ++ show g

      values <- kFold fetch []
      forM_ values $ \v@( did :: Integer, slid :: Integer, message :: String
                        , first_name :: String, last_name :: String
                        , personal_number :: String) -> do
        r <- kRun $ sqlUpdate "signatory_links" $ do
          sqlSet "eleg_data_mismatch_message" message
          sqlSet "eleg_data_mismatch_first_name" first_name
          sqlSet "eleg_data_mismatch_last_name" last_name
          sqlSet "eleg_data_mismatch_personal_number" personal_number
          sqlWhereEq "id" slid
          sqlWhereEq "document_id" did
        when (r /= 1) $
          Log.debug $ "Migration failed at " ++ show v
      kRun_ $ sqlUpdate "documents" $ do
        sqlSet "cancelation_reason" SqlNull
        sqlWhere "cancelation_reason = '\"ManualCancel\"'"
      kRun_ $ sqlUpdate "documents" $ do
        sqlSet "cancelation_reason" SqlNull
        sqlWhereExists $ sqlSelect "signatory_links" $ do
          sqlWhere "signatory_links.document_id = documents.id"
          sqlWhere "signatory_links.eleg_data_mismatch_message IS NOT NULL"
}

dropCancelationReasonFromDocuments :: MonadDB m => Migration m
dropCancelationReasonFromDocuments = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 19
  , mgrDo = do
      let fetch acc ids title eleg = (ids :: Integer, title :: String, eleg :: String) : acc
      kRun_ $ sqlSelect "documents" $ do
                 sqlResult "id, title, cancelation_reason"
                 sqlWhere "cancelation_reason IS NOT NULL"
      values <- kFold fetch []
      mapM_ (\(a,b,c) -> Log.debug $ "ID: " ++ show a ++ " (" ++ b ++ "): " ++ c) $ values

      --when (not (null values)) $
      --     error "There are some useful cancelation_reason fields in documents still"

      kRunRaw $ "ALTER TABLE documents"
              <+> "DROP COLUMN cancelation_reason"
}

dropMailFooterFromDocuments :: MonadDB m => Migration m
dropMailFooterFromDocuments = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 20
  , mgrDo = do
      kRunRaw $ "ALTER TABLE documents"
              <+> "DROP COLUMN mail_footer"
}

dropCSVSignatoryIndexFromSignatoryLinks :: MonadDB m => Migration m
dropCSVSignatoryIndexFromSignatoryLinks = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 15
  , mgrDo = do
      kRunRaw "ALTER TABLE signatory_links DROP COLUMN csv_signatory_index"
}

addSequenceOwnerToDocumentsId :: MonadDB m => Migration m
addSequenceOwnerToDocumentsId = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 11
  , mgrDo = kRunRaw "ALTER SEQUENCE documents_id_seq OWNED BY documents.id"
}

addSequenceOwnerToSignatoryLinks :: MonadDB m => Migration m
addSequenceOwnerToSignatoryLinks = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 12
  , mgrDo = do
      kRunRaw "ALTER SEQUENCE signatory_links_internal_insert_order_seq OWNED BY signatory_links.internal_insert_order"
      kRunRaw "ALTER SEQUENCE signatory_links_id_seq OWNED BY signatory_links.id"
}

removeCompanyIdFromSignatoryLinks :: MonadDB m => Migration m
removeCompanyIdFromSignatoryLinks = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 13
  , mgrDo = do
      kRunRaw "ALTER TABLE signatory_links DROP COLUMN company_id"
}

removeServiceIDFromDocuments :: MonadDB m => Migration m
removeServiceIDFromDocuments = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 9
  , mgrDo = do
    -- check if service_id field is empty for all documents
    check <- getMany "SELECT DISTINCT service_id IS NULL FROM documents"
    case check of
      []     -> return () -- no records, ok
      [True] -> return () -- only nulls, ok
      _      -> error "Documents have rows with non-null service_id"
    kRunRaw "ALTER TABLE documents DROP CONSTRAINT fk_documents_services"
    kRunRaw "DROP INDEX idx_documents_service_id"
    kRunRaw "ALTER TABLE documents DROP COLUMN service_id"
}

addObligatoryColumnToSignatoryLinkFields :: MonadDB m => Migration m
addObligatoryColumnToSignatoryLinkFields = Migration {
    mgrTable = tableSignatoryLinkFields
  , mgrFrom = 1
  , mgrDo = do
    kRunRaw "ALTER TABLE signatory_link_fields ADD COLUMN obligatory BOOL NOT NULL DEFAULT TRUE"
    -- Change CheckboxOptionalFT to CheckboxFT and set obligatory to false
    kRunRaw "UPDATE signatory_link_fields SET obligatory = FALSE WHERE type = 9"
    -- Change CheckboxMandatoryFT to CheckboxFT and do not change obligatory (will stay true)
    kRunRaw "UPDATE signatory_link_fields SET type = 9 WHERE type = 10"
  }



dropPixelSizeFormSignatureSignatoryLinkFieldsAndNormalizeFields :: MonadDB m => Migration m
dropPixelSizeFormSignatureSignatoryLinkFieldsAndNormalizeFields = Migration {
    mgrTable = tableSignatoryLinkFields
  , mgrFrom = 2
  , mgrDo = do
    -- Normalize of fields jsons
    kRun_ $ sqlSelect "signatory_link_fields" $ do
                 sqlResult "id, placements"
                 sqlWhere "placements != '[]'"
    values <- kFold (\acc ids placements -> (ids :: Integer, placements :: String) : acc) []
    forM_ values $ \(fid, placements) -> do
       let placementsJSON = case (JSON.decode placements) of
                               Ok js -> js
                               _ -> JSNull
       let (placements' :: [FieldPlacement]) = fromMaybe (error $ "Could not parse placements from " ++ placements) $
                                               fromJSValueCustomMany parseFields placementsJSON
       kRun $ sqlUpdate "signatory_link_fields" $ do
          sqlSet "placements" placements'
          sqlWhereEq "id" fid


    -- Now clean checkboxes
    kRun_ $ sqlSelect "signatory_link_fields" $ do
                 sqlResult "id, placements"
                 sqlWhereEq "type" (CheckboxFT undefined)
    values' <- kFold (\acc ids placements -> (ids :: Integer, placements :: [FieldPlacement]) : acc) []
    forM_ values' $ \(fid,  placements) -> do
                 let placements' = for placements (\p -> if (placementwrel p > 0 || placementhrel p >0)
                                                      then p
                                                      else p {placementwrel = 12 / 943, placementhrel = 12 / 1335 }  )
                 kRun_ $ sqlUpdate "signatory_link_fields" $ do
                 sqlSet "placements" placements'
                 sqlWhereEq "id" fid

    -- Now clean images
    kRun_ $ sqlSelect "signatory_link_fields" $ do
                 sqlResult "id, value, placements"
                 sqlWhereEq "type" (SignatureFT undefined)
    values'' <- kFold (\acc ids value placements -> (ids :: Integer, value :: String, placements :: [FieldPlacement]) : acc) []
    forM_ values'' $ \(fid, value, placements) -> do
      case split "|" value of
        [ws,hs,content] ->
          case (maybeRead ws,maybeRead hs) of
               (Just (w :: Int),Just (h :: Int)) -> do
                 let placements' = for placements (\p -> if (placementwrel p > 0 || placementhrel p >0)
                                                      then p
                                                      else p {placementwrel = fromIntegral w / 943, placementhrel = fromIntegral h / 1335 }  )
                 kRun_ $ sqlUpdate "signatory_link_fields" $ do
                 sqlSet "placements" placements'
                 sqlSet "value" content
                 sqlWhereEq "id" fid
               _ ->  kRun_ $ sqlUpdate "signatory_link_fields" $ do
                        sqlSet "value" content
                        sqlWhereEq "id" fid
        _ -> return ()
  }
  where
    parseFields :: JSValue -> Maybe FieldPlacement
    parseFields js = msum $ fmap ($ js)
                     [ do xrel       <- fromJSValueField "xrel"
                          yrel       <- fromJSValueField "yrel"
                          wrel       <- fromJSValueField "wrel"
                          hrel       <- fromJSValueField "hrel"
                          fsrel      <- fromJSValueField "fsrel"
                          page       <- fromJSValueField "page"
                          side       <- fromJSValueField "tip"
                          return (FieldPlacement <$> xrel <*> yrel
                                                 <*> wrel <*> hrel <*> fsrel
                                                 <*> page <*> Just side)
                     , do x          <- fromJSValueField "x"
                          y          <- fromJSValueField "y"
                          page       <- fromJSValueField "page"
                          pagewidth  <- fromJSValueField "pagewidth"
                          pageheight <- fromJSValueField "pageheight"
                          side       <- fromJSValueField "tip"
                          let xrel  = (/) <$> x <*> pagewidth
                          let yrel  = (/) <$> y <*> pageheight
                          let wrel  = Just 0
                          let hrel  = Just 0
                          let fsrel = Just 0
                          return (FieldPlacement <$> xrel <*> yrel
                                                 <*> wrel <*> hrel <*> fsrel
                                                 <*> page <*> Just side)
                     , do x          <- fromJSValueField "placementx"
                          y          <- fromJSValueField "placementy"
                          page       <- fromJSValueField "placementpage"
                          pagewidth  <- fromJSValueField "placementpagewidth"
                          pageheight <- fromJSValueField "placementpageheight"
                          tipside    <- fromJSValueField "placementtipside"
                          let xrel  = (/) <$> x <*> pagewidth
                          let yrel  = (/) <$> y <*> pageheight
                          let wrel  = Just 0
                          let hrel  = Just 0
                          let fsrel = Just 0
                          return (FieldPlacement <$> xrel <*> yrel
                                                 <*> wrel <*> hrel <*> fsrel
                                                 <*> page <*> (Just $ Control.Monad.join $ maybeRead <$> tipside))
                     ]

addShouldBeFilledBySenderColumnToSignatoryLinkFields :: MonadDB m => Migration m
addShouldBeFilledBySenderColumnToSignatoryLinkFields = Migration {
    mgrTable = tableSignatoryLinkFields
  , mgrFrom = 3
  , mgrDo = kRunRaw "ALTER TABLE signatory_link_fields ADD COLUMN should_be_filled_by_author BOOL NOT NULL DEFAULT FALSE"
  }

splitIdentificationTypes :: MonadDB m => Migration m
splitIdentificationTypes = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 8
  , mgrDo = do
    kRunRaw "ALTER TABLE documents ADD COLUMN authentication_method SMALLINT NULL"
    kRunRaw "ALTER TABLE documents ADD COLUMN delivery_method SMALLINT NULL"
    kRun_ $ mconcat [
        SQL "UPDATE documents SET" []
      , SQL "  authentication_method = (CASE WHEN allowed_id_types = 0 THEN ? WHEN allowed_id_types = 1 THEN ? WHEN allowed_id_types = 2 THEN ? WHEN allowed_id_types = 4 THEN ? END)::SMALLINT" [
          toSql StandardAuthentication -- 0 (nothing, was defaulting to email)
        , toSql StandardAuthentication -- 1 (email)
        , toSql ELegAuthentication     -- 2 (eleg)
        , toSql StandardAuthentication -- 4 (pad, it implied email)
        ]
      , SQL ", delivery_method = (CASE WHEN allowed_id_types = 0 THEN ? WHEN allowed_id_types = 1 THEN ? WHEN allowed_id_types = 2 THEN ? WHEN allowed_id_types = 4 THEN ? END)::SMALLINT" [
          toSql EmailDelivery -- 0 (nothing, was defaulting to email)
        , toSql EmailDelivery -- 1 (email)
        , toSql EmailDelivery -- 2 (eleg, couldn't mix eleg with pad previously)
        , toSql PadDelivery   -- 4 (pad)
        ]
      ]
    kRunRaw "ALTER TABLE documents ALTER authentication_method SET NOT NULL"
    kRunRaw "ALTER TABLE documents ALTER delivery_method SET NOT NULL"
    kRunRaw "ALTER TABLE documents DROP COLUMN allowed_id_types"
}

addForeignKeyToDocumentTags :: MonadDB m => Migration m
addForeignKeyToDocumentTags = Migration {
    mgrTable = tableDocumentTags
  , mgrFrom = 1
  , mgrDo = kRunRaw $ "ALTER TABLE document_tags"
      <> " ADD CONSTRAINT fk_document_tags_document_id FOREIGN KEY(document_id)"
      <> " REFERENCES documents(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      <> " DEFERRABLE INITIALLY IMMEDIATE"
}

deprecateDocFunctionalityCol :: MonadDB m => Migration m
deprecateDocFunctionalityCol = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 5
  , mgrDo = do
    kRunRaw "ALTER TABLE documents DROP COLUMN functionality"
}

setCascadeOnSignatoryAttachments :: MonadDB m => Migration m
setCascadeOnSignatoryAttachments = Migration {
    mgrTable = tableSignatoryAttachments
  , mgrFrom = 3
  , mgrDo = do
    -- this is supposed to aid in the signatory_links renumeration step that follows
    kRunRaw $ "ALTER TABLE signatory_attachments"
              <> " DROP CONSTRAINT fk_signatory_attachments_signatory_links,"
              <> " ADD CONSTRAINT fk_signatory_attachments_signatory_links FOREIGN KEY(document_id,signatory_link_id)"
              <> " REFERENCES signatory_links(document_id,id) ON DELETE RESTRICT ON UPDATE CASCADE"
              <> " DEFERRABLE INITIALLY IMMEDIATE"
  }

renumerateSignatoryLinkIDS :: MonadDB m => Migration m
renumerateSignatoryLinkIDS = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 6
  , mgrDo = do
    kRunRaw $ "UPDATE signatory_links"
              <> " SET id = DEFAULT"
              <> " FROM signatory_links AS sl2"
              <> " WHERE signatory_links.id = sl2.id"
              <> " AND signatory_links.document_id <>"
              <> " sl2.document_id"
  }

dropSLForeignKeyOnSignatoryAttachments :: MonadDB m => Migration m
dropSLForeignKeyOnSignatoryAttachments = Migration {
    mgrTable = tableSignatoryAttachments
  , mgrFrom = 4
  , mgrDo = do
    kRunRaw $ "ALTER TABLE signatory_attachments"
           <> " DROP CONSTRAINT fk_signatory_attachments_signatory_links"
  }

setSignatoryLinksPrimaryKeyToIDOnly :: MonadDB m => Migration m
setSignatoryLinksPrimaryKeyToIDOnly = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 7
  , mgrDo = do
    kRunRaw $ "ALTER TABLE signatory_links"
              <> " DROP CONSTRAINT pk_signatory_links,"
              <> " ADD CONSTRAINT pk_signatory_links PRIMARY KEY (id)"
  }

setSignatoryAttachmentsForeignKeyToSLIDOnly :: MonadDB m => Migration m
setSignatoryAttachmentsForeignKeyToSLIDOnly = Migration {
    mgrTable = tableSignatoryAttachments
  , mgrFrom = 5
  , mgrDo = do
    kRunRaw $ "ALTER TABLE signatory_attachments"
      <> " ADD CONSTRAINT fk_signatory_attachments_signatory_links FOREIGN KEY(signatory_link_id)"
      <> " REFERENCES signatory_links(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      <> " DEFERRABLE INITIALLY IMMEDIATE"
  }

dropDocumentIDColumntFromSignatoryAttachments :: MonadDB m => Migration m
dropDocumentIDColumntFromSignatoryAttachments = Migration {
    mgrTable = tableSignatoryAttachments
  , mgrFrom = 6
  , mgrDo = do
    kRunRaw $ "ALTER TABLE signatory_attachments"
      <> " DROP COLUMN document_id"
  }


{-
- migrate padqueue - set fk referencing signatory_links to ON UPDATE CASCADE
- migrate signatory_attachments - set fk referencing signatory_links to ON UPDATE CASCADE
- migrate signatory_links - renumerate ids (references in padqueue/signatory_attachments are properly updated if necessary)
- migrate padqueue - drop fk referencing signatory_links
- migrate signatory_attachments - drop fk referencing signatory_links
- migrate signatory_links - change primary key
- migrate padqueue - add new fk referencing signatory_links
- migrate signatory_attachments - add new fk referencing signatory_links
-}

moveSignatoryLinkFieldsToSeparateTable :: MonadDB m => Migration m
moveSignatoryLinkFieldsToSeparateTable = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 8
  , mgrDo = do
    _ <- kRun $ SQL "SELECT id, fields FROM signatory_links WHERE fields <> '' AND fields <> '[]'" [];
    values <- kFold fetch []
    forM_ values $ \(slid, fields) -> do
      forM_ fields $ \field -> do
        let (xtypestr :: String, custom_name :: String, is_author_filled :: Bool) =
              case lookup "sfType" field of
                Just (JSString x_sfType) -> (fromJSString x_sfType,"",False)
                Just obj@(JSObject xcustom) ->
                  case lookup "CustomFT" (fromJSObject xcustom) of
                    Just (JSArray [JSString custname, JSBool authorfilled]) ->
                      ("CustomFT", fromJSString custname, authorfilled)
                    _ -> error $ "Custom field has unrecognized format: " ++ encode obj
                Just x -> error $ "Field type must be either string or object, found: " ++ encode x
                Nothing -> error $ "Field definition does not have sfType, whole def: " ++ encode field

            Just (JSString x_sfValue) = lookup "sfValue" field
            Just placement = lookup "sfPlacements" field
            (xtype :: Int) = case xtypestr of
                      "FirstNameFT"      -> 1
                      "LastNameFT"       -> 2
                      "CompanyFT"        -> 3
                      "PersonalNumberFT" -> 4
                      "CompanyNumberFT"  -> 5
                      "EmailFT"          -> 6
                      "CustomFT"         -> 7
                      "SignatureFT"      -> 8
                      _                  -> error $ "Unknown field type: " ++ xtypestr

        _ <- kRun $ sqlInsert "signatory_link_fields" $ do
                sqlSet "type" xtype
                sqlSet "value" $ fromJSString x_sfValue
                sqlSet "signatory_link_id" slid
                sqlSet "is_author_filled" is_author_filled
                sqlSet "custom_name" custom_name
                sqlSet "placements" $ encode placement

        return ()
      return ()
    kRunRaw $ "ALTER TABLE signatory_links DROP COLUMN fields"
  }
  where
    fetch acc slid fieldsstr = (slid :: Int64, fields) : acc
      where
        Ok (JSArray arr) = decode fieldsstr
        fromJSVal (JSObject obj) = fromJSObject obj
        fromJSVal x =
          error $ "moveSignatoryLinkFieldsToSeparateTable: expected valid object, got: " ++ encode x
        fields = map fromJSVal arr

moveDocumentTagsFromDocumentsTableToDocumentTagsTable :: MonadDB m => Migration m
moveDocumentTagsFromDocumentsTableToDocumentTagsTable = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 4
  , mgrDo = do
    _ <- kRun $ SQL "SELECT id, tags FROM documents WHERE tags <> '' AND tags <> '[]'" [];
    values <- kFold fetch []
    forM_ values $ \(docid, tags) -> do
      forM_ tags $ \tag -> do
        let Just (JSString tagname) = lookup "tagname" tag
            Just (JSString tagvalue) = lookup "tagvalue" tag
        kRun $ sqlInsert "document_tags" $ do
                   sqlSet "name" $ fromJSString tagname
                   sqlSet "value" $ fromJSString tagvalue
                   sqlSet "document_id" docid
      return ()
    kRunRaw $ "ALTER TABLE documents DROP COLUMN tags"
  }
  where
    fetch acc docid tagsstr = (docid :: Int64, tags) : acc
      where
        Ok (JSArray arr) = decode tagsstr
        fromJSVal (JSObject obj) = fromJSObject obj
        fromJSVal x =
          error $ "moveDocumentTagsFromDocumentsTableToDocumentTagsTable: expected {tagname:'',tagvalue:''}, got: " ++ encode x
        tags = map fromJSVal arr

updateDocumentStatusAfterRemovingAwaitingAuthor :: MonadDB m => Migration m
updateDocumentStatusAfterRemovingAwaitingAuthor = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 3
  , mgrDo = do
    -- change AwaitingAuthor to Pending
    kRunRaw "UPDATE documents SET status = 2 WHERE status = 7"
    -- update DocumentError so it has proper value
    kRunRaw "UPDATE documents SET status = 7 WHERE status = 8"
  }

removeOldSignatoryLinkIDFromCancelationReason :: MonadDB m => Migration m
removeOldSignatoryLinkIDFromCancelationReason = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 1
  , mgrDo = do
    _ <- kRun $ SQL "SELECT id, cancelation_reason FROM documents WHERE cancelation_reason LIKE ?" [toSql ("{\"ELegDataMismatch%"::String)]
    values <- kFold fetch []
    forM_ values $ \(slid, params) -> do
      let (x : JSObject link : xs) = params
          [("unSignatoryLinkID", newlink)] = fromJSObject link
          newparams = toJSObject [("ELegDataMismatch", x : newlink : xs)]
      1 <- kRun $ SQL "UPDATE documents SET cancelation_reason = ? WHERE id = ?"
        [toSql $ encode newparams, toSql slid]
      return ()
  }
  where
    fetch acc slid reason = (slid :: Int64, params) : acc
      where
        Ok (JSObject o) = decode reason
        [("ELegDataMismatch", JSArray params)] = fromJSObject o

addColumnToRecordInternalInsertionOrder :: MonadDB m => Migration m
addColumnToRecordInternalInsertionOrder =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 2
  , mgrDo = do
      kRunRaw "CREATE SEQUENCE signatory_links_internal_insert_order_seq"
      kRunRaw $ "ALTER TABLE signatory_links"
        <> " ADD COLUMN internal_insert_order BIGINT NOT NULL DEFAULT nextval('signatory_links_internal_insert_order_seq')"
      return ()
  }

addDocumentIdIndexOnSignatoryLinks :: MonadDB m => Migration m
addDocumentIdIndexOnSignatoryLinks =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 3
  , mgrDo = do
      kRunRaw $ "CREATE INDEX idx_signatory_links_document_id ON signatory_links(document_id)"
      return ()
  }

addIdSerialOnSignatoryLinks :: MonadDB m => Migration m
addIdSerialOnSignatoryLinks =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 4
  , mgrDo = do
      -- create the sequence
      _ <- kRunRaw $ "CREATE SEQUENCE signatory_links_id_seq"
      -- set start value to be one more than maximum already in the table or 1000 if table is empty
      Just n <- getOne $ SQL "SELECT setval('signatory_links_id_seq',(SELECT COALESCE(max(id)+1,1000) FROM signatory_links))" []
      Log.debug $ "Table signatory_links has yet " ++ show (maxBound - n :: Int64) ++ " values to go"
      -- and finally attach serial default value to files.id
      _ <- kRunRaw $ "ALTER TABLE signatory_links ALTER id SET DEFAULT nextval('signatory_links_id_seq')"
      return ()
  }

addIdSerialOnDocuments :: MonadDB m => Migration m
addIdSerialOnDocuments =
  Migration {
    mgrTable = tableDocuments
  , mgrFrom = 2
  , mgrDo = do
      -- create the sequence
      _ <- kRunRaw $ "CREATE SEQUENCE documents_id_seq"
      -- set start value to be one more than maximum already in the table or 1000 if table is empty
      Just n <- getOne $ SQL "SELECT setval('documents_id_seq',(SELECT COALESCE(max(id)+1,1000) FROM documents))" []
      Log.debug $ "Table documents has yet " ++ show (maxBound - n :: Int64) ++ " values to go"
      -- and finally attach serial default value to files.id
      _ <- kRunRaw $ "ALTER TABLE documents ALTER id SET DEFAULT nextval('documents_id_seq')"
      return ()
  }

addNameColumnInSignatoryAttachments :: MonadDB m => Migration m
addNameColumnInSignatoryAttachments =
  Migration {
    mgrTable = tableSignatoryAttachments
  , mgrFrom = 1
  , mgrDo = do
      kRunRaw "ALTER TABLE signatory_attachments ADD COLUMN name TEXT NOT NULL DEFAULT ''"
  }

addCSVUploadDataFromDocumentToSignatoryLink :: MonadDB m => Migration m
addCSVUploadDataFromDocumentToSignatoryLink =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 1
  , mgrDo = do
      kRunRaw $ "ALTER TABLE signatory_links"
        <> " ADD COLUMN csv_title TEXT NULL,"
        <> " ADD COLUMN csv_contents TEXT NULL,"
        <> " ADD COLUMN csv_signatory_index INTEGER NULL"
  }

addSignatoryLinkIdToSignatoryAttachment :: MonadDB m => Migration m
addSignatoryLinkIdToSignatoryAttachment =
  Migration {
    mgrTable = tableSignatoryAttachments
  , mgrFrom = 2
  , mgrDo = do
    kRunRaw $ "ALTER TABLE signatory_attachments"
      <> " ADD COLUMN signatory_link_id BIGINT NOT NULL DEFAULT 0"
    -- set the new column signatory_link_id from signatory_links that have the same email and document_id
    kRunRaw $ "UPDATE signatory_attachments "
      <> "SET signatory_link_id = sl.id "
      <> "FROM signatory_links sl "
      <> "WHERE sl.document_id = signatory_attachments.document_id "
      <> "AND regexp_replace(sl.fields, '^.*EmailFT\",\"sfValue\":\"([a-zA-Z0-9@-_.]+)\".*$', E'\\\\1') = signatory_attachments.email"
    kRunRaw $ "ALTER TABLE signatory_attachments DROP CONSTRAINT pk_signatory_attachments"
    -- delete attachments which have emails and document_id that don't exist in signatory_links
    logAndDeleteBadAttachments
    kRunRaw $ "ALTER TABLE signatory_attachments DROP COLUMN email"
    kRunRaw $ "ALTER TABLE signatory_attachments ADD CONSTRAINT pk_signatory_attachments PRIMARY KEY (document_id, signatory_link_id, name)"
    kRunRaw $ "ALTER TABLE signatory_attachments"
      <> " ADD CONSTRAINT fk_signatory_attachments_signatory_links FOREIGN KEY(signatory_link_id, document_id)"
      <> " REFERENCES signatory_links(id, document_id) ON DELETE CASCADE ON UPDATE RESTRICT"
      <> " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "CREATE INDEX idx_signatory_attachments_signatory_link_id ON signatory_attachments(signatory_link_id)"
  }
  where
    logAndDeleteBadAttachments = do
      kRunRaw $ "SELECT document_id, name, email, description FROM signatory_attachments WHERE signatory_link_id = 0"
      atts <- kFold decoder []
      kRunRaw $ "DELETE FROM signatory_attachments WHERE signatory_link_id = 0"
      mapM_ (\(d, n, e, s) ->
        Log.debug $ "Deleted bad attachment: document_id = " ++ show d
                 ++ ", name = " ++ show n
                 ++ ", email = " ++ show e
                 ++ ", description = " ++ show s) atts
      return ()
      where
        decoder acc docid name email desc = (docid :: Int64, name :: BS.ByteString, email :: BS.ByteString, desc :: BS.ByteString) : acc


fixSignatoryLinksSwedishChars :: MonadDB m => Migration m
fixSignatoryLinksSwedishChars =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 5
  , mgrDo = do
     _ <- kRun $ SQL "SELECT id, document_id, fields FROM signatory_links" []
     sls <- kFold decoder []
     forM_ sls $ \(sid,did,fields) -> do
       let fixedfields = fixSwedishChars fields
       when (fields /= fixedfields) $ do
         _ <- kRun $ SQL "UPDATE signatory_links SET fields = ? WHERE id = ? AND document_id = ?"
                [ toSql fixedfields
                , toSql sid
                , toSql did
                ]
         return ()

  }
    where
        decoder :: [(SignatoryLinkID, DocumentID, [SignatoryField])] ->  SignatoryLinkID ->  DocumentID -> [SignatoryField] -> [(SignatoryLinkID, DocumentID, [SignatoryField])]
        decoder !acc sid did fields = (sid,did,fields) : acc
        fixSwedishChars :: [SignatoryField] ->  [SignatoryField]
        fixSwedishChars = map fixSwedishCharsForAField
        fixSwedishCharsForAField :: SignatoryField -> SignatoryField
        fixSwedishCharsForAField f = f { sfType = fixSwedishCharsForAFieldType (sfType f)
                                       , sfValue = fixSwedishCharsForAString (sfValue f)
                                       }
        fixSwedishCharsForAFieldType (CustomFT s b) = CustomFT (fixSwedishCharsForAString s) b
        fixSwedishCharsForAFieldType a = a
        fixSwedishCharsForAString :: String -> String
        fixSwedishCharsForAString s =
          let value = BS.toString $ BSC.pack s
          in if value /= s && BS.replacement_char `notElem` value
             then value
             else s

addOCSPResponse :: MonadDB m => Migration m
addOCSPResponse =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 9
  , mgrDo = kRunRaw $ "ALTER TABLE signatory_links ADD COLUMN signinfo_ocsp_response VARCHAR NULL DEFAULT NULL"
  }

addSignRedirectURL :: MonadDB m => Migration m
addSignRedirectURL =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 10
  , mgrDo = do
      kRunRaw $ "ALTER TABLE signatory_links ADD COLUMN sign_redirect_url VARCHAR NULL DEFAULT NULL"
  }

moveAttachmentsFromDocumentsToAttachments :: MonadDB m => Migration m
moveAttachmentsFromDocumentsToAttachments =
  Migration
  { mgrTable = tableDocuments
  , mgrFrom = 6
  , mgrDo = do
      inserted <- kRun $ SQL ("INSERT INTO attachments(title,file_id,deleted,shared,ctime,mtime, user_id)"
                              <> " SELECT title, file_id, signatory_links.deleted, sharing=2, ctime, mtime, user_id"
                              <> " FROM documents JOIN signatory_links ON document_id = documents.id AND (roles&2)<>0 AND (documents.file_id IS NOT NULL)"
                              <> " WHERE type = 3") []
      deleted <- kRun $ SQL ("DELETE FROM documents WHERE type = 3") []
      when (deleted /= inserted) $
         Log.debug  $ "Migration from documents to attachments done. Migrated: " ++ show inserted ++ ". Lost attachments due to missing files: " ++ show (deleted - inserted)
  }

removeOldDocumentLog :: (MonadDB m, MonadIO m) => Migration m
removeOldDocumentLog =
  Migration
  { mgrTable = tableDocuments
  , mgrFrom = 7
  , mgrDo = do
      now <- getMinutesTime
      _ <- kRun $ SQL ("INSERT INTO evidence_log(document_id,time,text,event_type,version_id)"
                              <> " SELECT id, ?, log, ? , ? FROM documents") [toSql now ,  toSql OldDocumentHistory, toSql versionID]
      kRunRaw "ALTER TABLE documents DROP COLUMN log"
  }

changeRegionToLang :: MonadDB m => Migration m
changeRegionToLang =
  Migration
  { mgrTable = tableDocuments
  , mgrFrom = 13
  , mgrDo = kRunRaw "ALTER TABLE documents RENAME COLUMN region TO lang"
  }

removeStatsTables :: MonadDB m => Migration m
removeStatsTables =
  Migration
  { mgrTable = tableDocuments
  , mgrFrom = 24
  , mgrDo = kRunRaw "DROP TABLE doc_stat_events, sign_stat_events, user_stat_events CASCADE"
  }

removeProcessFromDocuments :: MonadDB m => Migration m
removeProcessFromDocuments =
  Migration
  { mgrTable = tableDocuments
  , mgrFrom = 25
  , mgrDo = kRunRaw "ALTER TABLE documents DROP COLUMN process"
  }

moveBinaryDataForSignatoryScreenshotsToFilesTable :: MonadDB m => Migration m
moveBinaryDataForSignatoryScreenshotsToFilesTable =
  Migration
  { mgrTable = tableSignatoryScreenshots
  , mgrFrom = 1
  , mgrDo = do
      kRunRaw "ALTER TABLE signatory_screenshots DROP COLUMN mimetype"
      kRunRaw "ALTER TABLE signatory_screenshots ADD COLUMN file_id BIGINT"
      Log.debug $ "This is a long running migration with O(n^2) complexity. Please wait!"
      kRunRaw "CREATE INDEX ON signatory_screenshots((digest(image,'sha1')))"
      filesInserted <- kRun $ sqlInsertSelect "files" "signatory_screenshots" $ do
          sqlSetCmd "content" "signatory_screenshots.image"
          sqlSetCmd "name" "signatory_screenshots.type || '_screenshot.jpeg'"
          sqlSetCmd "size" "octet_length(signatory_screenshots.image)"
          sqlSetCmd "checksum" "digest(signatory_screenshots.image,'sha1')"
          sqlDistinct
      screenshotsUpdated <- kRun $ sqlUpdate "signatory_screenshots" $ do

        sqlSetCmd "file_id" "(SELECT id FROM files WHERE content = signatory_screenshots.image AND name=signatory_screenshots.type || '_screenshot.jpeg' LIMIT 1)"

      kRunRaw "ALTER TABLE signatory_screenshots DROP COLUMN image"
      Log.debug $ "Moved " ++ show screenshotsUpdated ++ " into " ++ show filesInserted ++ " files (removing duplicates)"
  }

migrateSignatoryLinksDeletedTime :: MonadDB m => Migration m
migrateSignatoryLinksDeletedTime =
  Migration {
      mgrTable = tableSignatoryLinks
    , mgrFrom = 20
    , mgrDo = do
       _ <- kRunRaw $ "ALTER TABLE signatory_links"
                  <+> "ALTER deleted DROP NOT NULL,"
                  <+> "ALTER deleted DROP DEFAULT,"
                  <+> "ALTER deleted TYPE TIMESTAMPTZ USING (CASE WHEN deleted THEN now() ELSE NULL END),"
                  <+> "ALTER really_deleted DROP NOT NULL,"
                  <+> "ALTER really_deleted DROP DEFAULT,"
                  <+> "ALTER really_deleted TYPE TIMESTAMPTZ USING (CASE WHEN really_deleted THEN now() ELSE NULL END)"
       return ()
    }


migrateSeparateDeliveryStatuses :: MonadDB m => Migration m
migrateSeparateDeliveryStatuses =
  Migration {
      mgrTable = tableSignatoryLinks
    , mgrFrom = 21
    , mgrDo = do
       _ <- kRunRaw $ "ALTER TABLE signatory_links ADD COLUMN mail_invitation_delivery_status SMALLINT NOT NULL DEFAULT 3"
       _ <- kRunRaw $ "ALTER TABLE signatory_links ADD COLUMN sms_invitation_delivery_status  SMALLINT NOT NULL DEFAULT 3"
       _ <- kRunRaw $ "UPDATE signatory_links "
             <> "      SET mail_invitation_delivery_status = invitation_delivery_status "
             <> "      WHERE delivery_method = 1 OR delivery_method = 5"
       _ <- kRunRaw $ "UPDATE signatory_links "
             <> "      SET sms_invitation_delivery_status = invitation_delivery_status "
             <> "      WHERE delivery_method = 4 OR delivery_method = 5"
       _ <- kRunRaw $ "ALTER TABLE signatory_links DROP COLUMN invitation_delivery_status"
       return ()
    }

removeCSVStuffFromDocuments  :: MonadDB m => Migration m
removeCSVStuffFromDocuments =
  Migration {
      mgrTable = tableDocuments
    , mgrFrom = 26
    , mgrDo = do
       _ <- kRunRaw $ "ALTER TABLE documents DROP COLUMN csv_title, DROP COLUMN csv_contents, DROP COLUMN csv_signatory_index"
       return ()
    }

migrateDocumentsAddPurgedTime :: MonadDB m => Migration m
migrateDocumentsAddPurgedTime =
  Migration {
      mgrTable = tableDocuments
    , mgrFrom = 27
    , mgrDo = do
       _ <- kRunRaw $ "ALTER TABLE documents"
                  <+> "ADD COLUMN purged_time TIMESTAMPTZ"
       return ()
    }

addRejectRedirectURL :: MonadDB m => Migration m
addRejectRedirectURL =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 22
  , mgrDo = do
      kRunRaw $ "ALTER TABLE signatory_links ADD COLUMN reject_redirect_url VARCHAR NULL DEFAULT NULL"
  }

migrateDocumentsMoveFilesToMainFilesTable :: MonadDB m => Migration m
migrateDocumentsMoveFilesToMainFilesTable =
  Migration
    { mgrTable = tableDocuments
    , mgrFrom = 28
    , mgrDo = do
        docsWithFile <- kRun $ sqlInsertSelect "main_files" "documents" $ do
          sqlSetCmd "document_id" "id"
          sqlSetCmd "file_id" "file_id"
          sqlSet "document_status" Preparation
          sqlSetCmd "seal_status" "NULL"
          sqlWhere "file_id IS NOT NULL"
        docsWithSealedFile <- kRun $ sqlInsertSelect "main_files" "documents" $ do
          sqlSetCmd "document_id" "id"
          sqlSetCmd "file_id" "sealed_file_id"
          sqlSet "document_status" Closed
          sqlSetCmd "seal_status" "seal_status"
          sqlWhere "sealed_file_id IS NOT NULL"

        -- Sanity checks: for each documents.file_id, there should be
        -- a matching element in main_files, and similar for documents.sealed_file_id

        Just migratedFiles <- getOne $ sqlSelect "main_files" $ do
          sqlResult "count(*)"
          sqlWhereEq "document_status" Preparation
          sqlWhereExists $ sqlSelect "documents" $ do
            sqlWhere "documents.id = main_files.document_id"
            sqlWhere "documents.file_id = main_files.file_id"
        Just migratedSealedFiles <- getOne $ sqlSelect "main_files" $ do
          sqlResult "count(*)"
          sqlWhereEq "document_status" Closed
          sqlWhereExists $ sqlSelect "documents" $ do
            sqlWhere "documents.id = main_files.document_id"
            sqlWhere "documents.sealed_file_id = main_files.file_id"
            sqlWhere "documents.seal_status = main_files.seal_status"
        when (migratedFiles /= docsWithFile) $ do
          fail $ "Doc.Migrations.migrateDocumentsRemoveFiles: #migratedFiles is not #docsWithFile: " ++ show (migratedFiles, docsWithFile)
        when (migratedSealedFiles /= docsWithSealedFile) $ do
          fail $ "Doc.Migrations.migrateDocumentsRemoveFiles: #migratedSealedFiles is not #docsWithSealedFile: " ++ show (migratedSealedFiles, docsWithSealedFile)

        -- Drop redundant columns

        kRun_ $ "ALTER TABLE documents DROP COLUMN file_id"
        kRun_ $ "ALTER TABLE documents DROP COLUMN sealed_file_id"
        kRun_ $ "ALTER TABLE documents DROP COLUMN seal_status"
    }
