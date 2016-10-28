{-# OPTIONS_GHC -fno-warn-orphans #-}

module Doc.Signing.Model (
    ScheduleDocumentSigning(..)
  , IsDocumentSigningInProgress(..)
  , CleanAllScheduledDocumentSigning(..)
  , GetDocumentSigningLastCheckStatus(..)
  , UpdateDocumentSigning(..)
  ) where

import Control.Monad.Catch
import Data.Int
import Data.Unjson as Unjson
import Database.PostgreSQL.PQTypes
import Log.Class
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import BrandedDomain.Model
import DB
import Doc.API.V2.JSON.Fields
import Doc.API.V2.JSON.Misc (unjsonSignatoryScreenshots)
import Doc.DocumentMonad
import Doc.SignatoryLinkID
import Doc.SignatoryScreenshots
import File.FileID
import IPAddress
import KontraPrelude
import User.Lang

instance PQFormat SignatoryFieldsValuesForSigning where
  pqFormat = const $ pqFormat ($undefined::JSON BS.ByteString)

instance FromSQL SignatoryFieldsValuesForSigning where
  type PQBase SignatoryFieldsValuesForSigning = PQBase (JSON BS.ByteString)
  fromSQL mbase = do
    JSON s <- fromSQL mbase
    case Aeson.eitherDecode s of
      Left _ -> hpqTypesError $ "fromSQL (SignatoryFieldsValuesForSigning): can't parse json"
      Right ae -> case (Unjson.parse unjsonSignatoryFieldsValuesForSigning ae) of
        (Result res []) -> return $ res
        (Result _ _) -> hpqTypesError $ "fromSQL (SignatoryFieldsValuesForSigning): can't parse SignatoryFieldsValuesForSigning"

instance ToSQL SignatoryFieldsValuesForSigning where
  type PQDest SignatoryFieldsValuesForSigning = PQDest (JSON BS.ByteString)
  toSQL s = toSQL (unjsonToByteStringLazy' (Options { pretty = False, indent = 0, nulls = True }) unjsonSignatoryFieldsValuesForSigning s)

instance PQFormat SignatoryScreenshots where
  pqFormat = const $ pqFormat ($undefined::JSON BS.ByteString)

instance FromSQL SignatoryScreenshots where
  type PQBase SignatoryScreenshots = PQBase (JSON BS.ByteString)
  fromSQL mbase = do
    JSON s <- fromSQL mbase
    case Aeson.eitherDecode s of
      Left _ -> hpqTypesError $ "fromSQL (SignatoryScreenshots): can't parse json"
      Right ae -> case (Unjson.parse unjsonSignatoryScreenshots ae) of
        (Result res []) -> return $ res
        (Result _ _) -> hpqTypesError $ "fromSQL (SignatoryScreenshots): can't parse SignatoryScreenshots"

instance ToSQL SignatoryScreenshots where
  type PQDest SignatoryScreenshots = PQDest (JSON BS.ByteString)
  toSQL s = toSQL (unjsonToByteStringLazy' (Options { pretty = False, indent = 0, nulls = True }) unjsonSignatoryScreenshots s)

data ScheduleDocumentSigning = ScheduleDocumentSigning SignatoryLinkID BrandedDomainID UTCTime IPAddress (Maybe UTCTime) (Maybe String) Lang SignatoryFieldsValuesForSigning [FileID] SignatoryScreenshots
instance (MonadDB m, DocumentMonad m, MonadLog m, MonadMask m) => DBUpdate m ScheduleDocumentSigning () where
  update (ScheduleDocumentSigning slid bdid st cip mct mcn sl sf saas ss) = do
    runQuery_ . sqlInsert "document_signing_jobs" $ do
      sqlSet "id" slid
      sqlSet "branded_domain_id" bdid
      sqlSet "time" st
      sqlSet "client_ip_v4" cip
      sqlSet "client_time" mct
      sqlSet "client_name" mcn
      sqlSet "lang" sl
      sqlSet "fields" sf
      sqlSet "accepted_attachments" $ Array1 saas
      sqlSet "screenshots" ss
      sqlSetCmd "run_at" "now()"
      sqlSet "attempts" (0::Int32)

data IsDocumentSigningInProgress = IsDocumentSigningInProgress SignatoryLinkID
instance (MonadDB m, DocumentMonad m, MonadLog m, MonadMask m) => DBQuery m IsDocumentSigningInProgress Bool where
  query (IsDocumentSigningInProgress slid) = do
    runQuery_ . sqlSelect "document_signing_jobs" $ do
      sqlWhereEq "id" slid
      sqlWhere "NOT cancelled"
      sqlResult "TRUE"
    result <- fetchMaybe runIdentity
    return $ result == Just True

data GetDocumentSigningLastCheckStatus = GetDocumentSigningLastCheckStatus SignatoryLinkID
instance (MonadDB m, DocumentMonad m, MonadLog m, MonadMask m) => DBQuery m GetDocumentSigningLastCheckStatus (Maybe T.Text) where
  query (GetDocumentSigningLastCheckStatus slid) = do
    runQuery_ . sqlSelect "document_signing_jobs" $ do
      sqlWhereEq "id" slid
      sqlResult "last_check_status"
    result <- fetchMaybe runIdentity
    return $ join $ result

data UpdateDocumentSigning = UpdateDocumentSigning SignatoryLinkID Bool T.Text
instance (MonadDB m, DocumentMonad m, MonadLog m, MonadMask m) => DBUpdate m UpdateDocumentSigning () where
  update (UpdateDocumentSigning slid cancelled text) = do
    runQuery_ . sqlUpdate  "document_signing_jobs" $ do
      sqlSet "cancelled" cancelled
      sqlSet "last_check_status" (Just text)
      sqlWhereEq "id" slid

data CleanAllScheduledDocumentSigning = CleanAllScheduledDocumentSigning SignatoryLinkID
instance (MonadDB m, DocumentMonad m, MonadLog m, MonadMask m) => DBUpdate m CleanAllScheduledDocumentSigning () where
  update (CleanAllScheduledDocumentSigning slid) = do
    runQuery_ . sqlDelete  "document_signing_jobs" $ do
      sqlWhereEq "id" slid
