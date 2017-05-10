module AppDBMigrations (
    kontraMigrations
  ) where

import Control.Monad.Catch
import Log

import BrandedDomain.Migrations
import Chargeable.Migrations
import Company.Migrations
import Cron.Migrations
import DB
import Doc.API.Callback.Migrations
import Doc.Migrations
import Doc.Sealing.Migrations
import Doc.Signing.Migrations
import EID.Authentication.Migrations
import EID.CGI.GRP.Transaction.Migrations
import EID.Signature.Migrations
import EvidenceLog.Migrations
import File.Migrations
import KontraPrelude
import Mails.Migrations
import Partner.Migrations
import SMS.Migrations
import Theme.Migrations
import User.Migrations

-- Note: ALWAYS append new migrations TO THE END of this list.
-- (mailerMigrations always stay at the end though. They are
-- disjoint with kontrakcja, so it can be done that way).
kontraMigrations :: (MonadDB m, MonadThrow m, MonadLog m) => [Migration m]
kontraMigrations = [
    addAllowSaveSafetyCopyToCompanies
  , addIdleDocTimeout
  , createChargeableItemsTable
  , signatoryLinkFieldsAddBinaryValue
  , createCgiGrpTransactionsTable
  , createEIDSignaturesTable
  , signatoryLinksMoveSignatures
  , companiesAddCgiDisplayName
  , addMtimeStatusIndexes
  , evidenceLogAddActor
  , createThemesTable
  , createThemeOwnersTable
  , addThemesToBrandedDomainAndMainDomain
  , makeAssociatedDomainObligatoryForUsers
  , addThemesAndOthersToCompanyUIs
  , removeSMSOriginatorFromCompany
  , changeScriveLoginLogo
  , createTableDocumentApiCallbackConsumers
  , updateApiCallbacksForNewConsumer
  , dropHTMLFromInvitationAndConfirmationMessages
  , dropHTMLFromMessagesInEvidenceLog
  , apiCallbacksAddIDColumn
  , addNameToCallbackConsumers
  , addNameOrderToFieldsAndMigrateNames
  , moveSignaturesToFilesAndAddBoolValueForFields
  , addFileNameToMainFiles
  , addFileNameToAuthorAttachments
  , dropErrorTextFromDocuments
  , dropCSVTitleFromSignatories
  , dropAPIVersionFromDocuments
  , addAPIV2CallbackAndRenameExisting
  , addAPIVersionToDocumentApiCallbacks
  , extendCgiGrpTransactionsWithAuthRequest
  , createEIDAuthenticationTable
  , addSignatoryAuthenticationToView
  , fixPurgedPendingDocumentsAndAddConstraint
  , addNetsFieldsToEIDAuthentication
  , addAuthenticatedToViewToCompositeType
  , createTableFieldPlacements
  , createTablePlacementAnchors
  , unjsonFieldPlacements
  , filesAddPurgeAtColumn
  , filesAddIndexesToSpeedUpQueries
  , createIndexOnEmailFields
  , addSMSProviderToCompanies
  , changeScriveSignviewLogo
  , addRequiredAndNameToAuthorAttachments
  , companiesAddCgiServiceID
  , documentsAddAuthorID
  , addUniqueConstraintForAuthorCheck
  , createDocumentSealingConsumersTable
  , createDocumentSealingJobsTable
  , dropBrandedDomainContact
  , addFileNameToSignatoryAttachment
  , allowManyCopiesOfAuthorAttachmentForSameDocument
  , addAddedToSealedFileToAuthorAttachment
  , evidenceLogAddAdditionalText
  , addAllowRejectReasonToDocuments
  , changeRestOfScriveLogos
  , changeScriveFavicon
  , createDocumentSigningConsumersTable
  , createDocumentSigningJobsTable
  , removeAmazonBucketColumn
  , documentSigningJobsUseJson
  , addIsReceiptToDocument
  , createTablePartners
  , companiesAddPartnerID
  , createTablePartnerAdmins
  , addAllowsHighlightingToSignatories
  , createHighlightedPagesTable
  , normalizeCheckboxesSize
  , normalizeCheckboxesFSRel
  , companiesAddPadAppModeAndEArchiveEnabled
  , companiesAddPaymentPlan
  , createIndexesForChargeableItems
  ] ++ cronMigrations
    ++ mailerMigrations
    ++ messengerMigrations
