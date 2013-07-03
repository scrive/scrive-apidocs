module AppDB (
    kontraFunctions
  , kontraMigrations
  , kontraTables
  ) where

import DB.Core
import DB.Model
import DB.SQLFunction
import Control.Monad.IO.Class

import ActionQueue.Tables
import Company.Tables
import Company.Migrations
import CompanyAccounts.Tables
import Doc.Tables
import Doc.Migrations
import Doc.API.Callback.Tables
import Doc.Tokens.Tables
import HostClock.Tables
import User.Migrations
import User.Tables
import User.History.Tables
import File.Tables
import File.Migrations
import Mails.Tables
import PadQueue.Tables
import PadQueue.Migrations
import Session.Tables
import Mails.Migrations
import OAuth.Tables
import ScriveByMail.Tables
import ELegitimation.ELegTransaction.Tables
import EvidenceLog.Tables
import EvidenceLog.Migrations
import Payments.Tables
import Payments.Migrations
import Attachment.Tables
import ThirdPartyStats.Tables
import User.CallbackScheme.Tables

kontraFunctions :: [SQLFunction]
kontraFunctions = [
    insertDocumentSessionToken
  , mergeELegTransaction
  ]

-- Note: ALWAYS append new migrations TO THE END of this list.
-- (mailerMigrations always stay at the end though. They are
-- disjoint with kontrakcja, so it can be done that way).
kontraMigrations :: (MonadDB m, MonadIO m) => [Migration m]
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
  ] ++ mailerMigrations

kontraTables :: [Table]
kontraTables = [
    tableUsers
  , tableUserMailAPIs
  , tableUserInviteInfos
  , tableUsersHistory
  , tableCompanies
  , tableCompanyInvites
  , tableFiles
  , tableDocuments
  , tableSignatoryLinks
  , tableAuthorAttachments
  , tableSignatoryAttachments
  , tableEvidenceLog
  , tablePadQueue
  , tableCompanyMailAPIs
  , tableDocumentTags
  , tableSignatoryLinkFields
  , tableSignatoryScreenshots
  , tableTempCredential
  , tableTempPrivileges
  , tableAPIToken
  , tableAccessToken
  , tablePrivilege
  , tablePasswordReminders
  , tableAccessNewAccounts
  , tableEmailChangeRequests
  , tableUserAccountRequests
  , tablePaymentPlans
  , tableAttachments
  , tablePaymentStats
  , tableDocumentApiCallbacks
  , tableSessions
  , tableDocumentSessionTokens
  , tableELegTransactions
  , tableAsyncEventQueue
  , tableHostClock
  , tableUsersCallbackScheme
  ] ++ mailerTables
