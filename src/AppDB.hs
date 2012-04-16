module AppDB (
    kontraMigrations
  , kontraTables
  ) where

import DB.Core
import DB.Model

import API.Service.Tables
import Company.Tables
import Company.Migrations
import CompanyAccounts.Tables
import Doc.Tables
import Doc.Migrations
import User.Migrations
import User.Tables
import User.History.Tables
import Stats.Tables
import Stats.Migrations
import File.Tables
import File.Migrations
import Mails.Tables
import PadQueue.Tables
import Mails.Migrations
import ScriveByMail.Tables
import EvidenceLog.Tables

-- Note: ALWAYS append new migrations TO THE END of this list.
-- (mailerMigrations always stay at the end though. They are
-- disjoint with kontrakcja, so it can be done that way).
kontraMigrations :: MonadDB m => [Migration m]
kontraMigrations = [
    addRegionToUserSettings
  , addServiceAndCompanyToStats
  , removeSystemServer
  , addUserCustomFooter
  , makeUserStatsRepeatableByChangingPK
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
  , addAPIStringDocStats
  ] ++ mailerMigrations 

kontraTables :: [Table]
kontraTables = [
    tableUsers
  , tableUserFriends
  , tableUserMailAPIs
  , tableUserInviteInfos
  , tableUsersHistory
  , tableServices
  , tableCompanies
  , tableCompanyInvites
  , tableDocStatEvents
  , tableUserStatEvents
  , tableSignStatEvents
  , tableFiles
  , tableDocuments
  , tableSignatoryLinks
  , tableAuthorAttachments
  , tableSignatoryAttachments
  , tableEvidenceLog
  , tablePadQueue
  , tableCompanyMailAPIs
  , tableUserRequest
  , tableMailAPIDelay
  , tableDocumentTags
  ] ++ mailerTables
