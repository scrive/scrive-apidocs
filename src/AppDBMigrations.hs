module AppDBMigrations (
    kontraMigrations
  ) where

import DB
import ActionQueue.Migrations
import Company.Migrations
import CompanyAccounts.Migrations
import Doc.Migrations
import Doc.AutomaticReminder.Tables
import Doc.API.Callback.Migrations
import User.Migrations
import File.Migrations
import PadQueue.Migrations
import Mails.Migrations
import OAuth.Migrations
import SMS.Migrations
import EvidenceLog.Migrations
import Payments.Migrations
import Attachment.Migrations
import ThirdPartyStats.Migrations
import User.CallbackScheme.Migrations
import Session.Migrations
import BrandedDomain.Migrations
import qualified Log
import Doc.SMSPin.Tables

-- Note: ALWAYS append new migrations TO THE END of this list.
-- (mailerMigrations always stay at the end though. They are
-- disjoint with kontrakcja, so it can be done that way).
kontraMigrations :: (MonadDB m, Log.MonadLog m) => [Migration m]
kontraMigrations = [
    addRegionToUserSettings
  , removeSystemServer
  , addUserCustomFooter
  , addNameColumnInSignatoryAttachments
  , addCSVUploadDataFromDocumentToSignatoryLink
  , addColumnToRecordInternalInsertionOrder
  , addEmailBrandingToCompany
  , removeOldSignatoryLinkIDFromCancelationReason
  , addDocumentIdIndexOnSignatoryLinks
  , addSignatoryLinkIdToSignatoryAttachment
  , addTextColourToEmailBranding
  , addFileIdSequence
  , addIdSerialOnSignatoryLinks
  , addIdSerialOnDocuments
  , addIdSerialOnCompanies
  , addIdSerialOnUsers
  , addEmailDomainOnCompanies
  , addCompanyNameNumberOnUsers
  , updateDocumentStatusAfterRemovingAwaitingAuthor
  , moveDocumentTagsFromDocumentsTableToDocumentTagsTable
  , fixSignatoryLinksSwedishChars
  , setCascadeOnPadQueue
  , setCascadeOnSignatoryAttachments
  , renumerateSignatoryLinkIDS
  , dropSLForeignKeyOnPadQueue
  , dropSLForeignKeyOnSignatoryAttachments
  , setSignatoryLinksPrimaryKeyToIDOnly
  , setPadQueueForeignKeyToSLIDOnly
  , setSignatoryAttachmentsForeignKeyToSLIDOnly
  , dropDocumentIDColumntFromSignatoryAttachments
  , addCheckLowercaseEmailsUsers
  , moveSignatoryLinkFieldsToSeparateTable
  , migrateTempCredentialRemoveEmail -- for oauth
  , deprecateDocFunctionalityCol
  , removePreferedDesignMode
  , addDefaultEmptyStringsToSomeColumnsInCompaniesTable
  , addOCSPResponse
  , addCryptoColumnsToFilesTable
  , addForeignKeyToDocumentTags
  , addIsFree
  , moveAttachmentsFromDocumentsToAttachments
  , removeOldDocumentLog
  , splitIdentificationTypes
  , addSignRedirectURL
  , removeServiceIDFromCompanies
  , removeServiceIDFromDocuments
  , removeServiceIDFromUsers
  , removeDiskPathAndMakeNewColumnsNotNull
  , addApiCallbackUrlToDocument
  , removeSignatoryRoles
  , addSequenceOwnerToDocumentsId
  , addSequenceOwnerToSignatoryLinks
  , addBillingEndDateCache
  , setMandatoryExpirationTimeInDocument
  , removeRegionFromUsers
  , changeRegionToLang
  , removeCompanyIdFromSignatoryLinks
  , removeDeletedFromDocuments
  , attachUniqueContraintsToPaymentPlansColumns
  , removeSignatoryLinksInternalInsertOrder
  , addUnsavedDraftToDocument
  , expandEventsWithAffectedSignatoryAndTextMessage
  , addObligatoryColumnToSignatoryLinkFields
  , addIPAddressMaskListToCompanies
  , dropTrustWeaverReferenceFromDocuments
  , dropCSVSignatoryIndexFromSignatoryLinks
  , moveRejectionInfoFromDocumentsToSignatoryLinks
  , dropRejectionInfoFromDocuments
  , moveAuthenticationMethodFromDocumentsToSignatoryLinks
  , dropAuthenticationMethodFromDocuments
  , moveCancelationReasonFromDocumentsToSignatoryLinks
  , dropCancelationReasonFromDocuments
  , addNewCompanyBrandingOptions
  , addSignviewBrandingOptions
  , dropCustomFooterFromUsers
  , dropMailFooterFromDocuments
  , moveDeliveryMethodFromDocumentsToSignatoryLinks
  , dropDeliveryMethodFromDocuments
  , dropPixelSizeFormSignatureSignatoryLinkFieldsAndNormalizeFields
  , addObjectVersionToDocuments
  , addShouldBeFilledBySenderColumnToSignatoryLinkFields
  , addCustomBrandingOptions
  , addAssociatedDomainToUsers
  , dropMobileFromUsers
  , removeExternalIDFromCompanies
  , addSealStatusToDocument
  , removeStatsTables
  , removeEmailDomainFromCompany
  , asyncEventQueueChangePrimaryKeyToBigSerial
  , removeProcessFromDocuments
  , moveCompanyUIsToSeparateTable
  , removeIsFree
  , setProperOwnerOnFilesIDSequence
  , moveBinaryDataForSignatoryScreenshotsToFilesTable
  , allUsersMustHaveCompany
  , paymentsPlansOnlyForCompanies
  , paymentsStatsOnlyForCompanies
  , migrateUsersDeletedTime
  , migrateSignatoryLinksDeletedTime
  , migrateSeparateDeliveryStatuses
  , removeCSVStuffFromDocuments
  , addPurgedTimeToFiles
  , migrateDocumentsAddPurgedTime
  , addRejectRedirectURL
  , createMainFilesTable
  , migrateDocumentsMoveFilesToMainFilesTable
  , removeDuplicateIndexFromPaymentPlans
  , removeDuplicateIndexFromAccessNewAccounts
  , removeDuplicateIndexFromPasswordReminders
  , removeDuplicateIndexFromEmailChangeRequests
  , removeDuplicateIndexFromUserAccountRequests
  , removeDuplicateIndexFromDocumentApiCallbacks
  , removeDuplicateIndexFromUsersCallbackScheme
  , removeDuplicateIndexFromCompanyUIs
  , addClientTimeNameToEvidenceLog
  , fixSignatureFieldsWithAnySize
  , migrateUsersUniqueIndexOnEmail
  , makeSealStatusNonNullInMainFiles
  , createDocumentAutomaticRemindersTable
  , migrateDocumentsAddDaysToRemind
  , normalizeCompanyInvites
  , addProbablyMissingIndexesOnAttachments
  , addPrimaryAndSecondaryColoursToCompanyUIs
  , evidenceLogFixColumns
  , signatoryLinksChangeVarcharColumnsToText
  , tempCredentialChangeVarcharColumnsToText
  , migrateDocumentsAddSignviewSettings
  , migrateDocumentsAddDocumentToken
  , addDomainToSession
  , addConfirmTextToDocuments
  , addConfirmationDeliveryMethodToSignatoryLinks
  , changeSomeStandardFieldsToOptional
  , createSignatorySMSPinsTable
  , createBrandedDomainsTable
  , usersTableChangeAssociatedDomainToForeignKey
  ] ++ mailerMigrations
    ++ messengerMigrations
