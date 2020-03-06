module AppDBTables (
    kontraComposites
  , kontraDomains
  , kontraExtensions
  , kontraFunctions
  , kontraTables
  , kontraTriggers
  ) where

import Database.PostgreSQL.PQTypes.Model

import AccessControl.Tables
import Attachment.Tables
import BrandedDomain.Tables
import Chargeable.Tables
import Cron.Tables
import DB.SQLFunction
import DB.SQLTrigger
import Doc.API.Callback.Tables
import Doc.AutomaticReminder.Tables
import Doc.Extending.Tables
import Doc.Functions
import Doc.Sealing.Tables
import Doc.Signing.Tables
import Doc.SMSPin.Tables
import Doc.Tables
import Doc.Tokens.Tables
import Doc.Trigger
import EID.Authentication.Tables
import EID.CGI.GRP.Transaction.Tables
import EID.EIDService.Tables
import EID.Nets.Tables
import EID.Signature.Tables
import EvidenceLog.Tables
import FeatureFlags.Tables
import File.Tables
import Folder.Tables
import HostClock.Tables
import Mails.FromKontra.Tables
import Mails.Tables
import OAuth.Tables
import Session.Tables
import SMS.FromKontra.Tables
import SMS.Tables
import Tag.Tables
import Theme.Tables
import ThirdPartyStats.Tables
import User.APILog.Tables
import User.CallbackScheme.Tables
import User.EmailChangeRequest.Tables
import User.History.Tables
import User.PasswordReminder.Tables
import User.Tables
import User.UserAccountRequest.Tables
import UserGroup.Tables
import UserGroupAccounts.Tables

kontraComposites :: [CompositeType]
kontraComposites =
  [ ctAuthorAttachment
    , ctSignatoryAttachment
    , ctPlacementAnchor
    , ctFieldPlacement
    , ctHighlightedPage
    , ctSignatoryField
    , ctDocumentTag
    , ctMainFile
    , ctSignatoryConsentQuestion
    , ctSignatoryAccessToken
    , ctSignatoryLink
    , ctDocument
    , ctFeatureFlags
    , ctFeatureFlags4
    , ctFeatureFlags3

  -- user group composites
    , ctUserGroupInvoicing
    , ctUserGroupSettings
    , ctUserGroupSettings1
    , ctUserGroupSettings2
    , ctUserGroupSettings3
    , ctUserGroupSettings4
    , ctUserGroupSettings5
    , ctUserGroupSettings6
    , ctUserGroupUI
    , ctUserGroupAddress
    , ctUserGroupAddress1
    , ctTag
    ]
    ++ mailerComposites

kontraDomains :: [Domain]
kontraDomains = [domainColor, domainFont]

kontraExtensions :: [Extension]
kontraExtensions = ["pgcrypto"]

kontraFunctions :: [SQLFunction]
kontraFunctions =
  [archiveSearchTerms, postProcessSearchString, extractEmails, splitEmail]

kontraTriggers :: [SQLTrigger]
kontraTriggers = [searchUpdateDocs, searchUpdateSignatoryLinkFields]

kontraTables :: [Table]
kontraTables =
  [ tableThemes
    , tableBrandedDomains
    , tableFolders
    , tableUserGroups
    , tableUsers
    , tableUsersHistory
    , tableCompanyInvites
    , tableFiles
    , tableDocuments
    , tableAccessControl
    , tableMainFiles
    , tableSignatoryLinks
    , tableAuthorAttachments
    , tableSignatoryAttachments
    , tableEvidenceLog
    , tableDocumentTags
    , tableSignatoryLinkFields
    , tableFieldPlacements
    , tablePlacementAnchors
    , tableSignatoryScreenshots
    , tableAPIToken
    , tableAccessToken
    , tablePrivilege
    , tableTempCredential
    , tableTempPrivileges
    , tablePasswordReminders
    , tableEmailChangeRequests
    , tableUserAccountRequests
    , tableAttachments
    , tableDocumentApiCallbackConsumers
    , tableDocumentApiCallbacks
    , tableDocumentExtendingConsumers
    , tableDocumentExtendingJobs
    , tableDocumentSealingConsumers
    , tableDocumentSealingJobs
    , tableDocumentSigningConsumers
    , tableDocumentSigningJobs
    , tableSessions
    , tableDocumentSessionTokens
    , tableCgiGrpTransactions
    , tableEIDSignatures
    , tableAsyncEventQueue
    , tableHostClock
    , tableUsersCallbackScheme
    , tableDocumentAutomaticReminders
    , tableSignatorySMSPins
    , tableChargeableItems
    , tableThemeOwnership
    , tableEIDAuthentications
    , tableHighlightedPages
    , tableFeatureFlags
    , tableNetsSignOrders
    , tableEIDServiceTransactions
    , tableUserTags
    ]
    ++ cronTables
    ++ mailerTables
    ++ messengerTables
    ++ [ tableKontraInfoForMails
       , tableKontraInfoForSMSes
       , tableAPILogs
       , tableSignatoryLinkConsentQuestions
       , tableUserGroupSettings
       , tableUserGroupInvoicings
       , tableUserGroupUIs
       , tableUserGroupAddresses
       , tableUserGroupFreeDocumentTokens
       , tableUserGroupTags
       , tableFilePurgeConsumers
       , tableFilePurgeJobs
       , tableTemporaryLoginTokens
       , tableApiCallbackResult
       , tableSignatoryAccessTokens
       ]
