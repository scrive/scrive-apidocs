module AppDBTables (
    kontraFunctions
  , kontraTables
  ) where

import ActionQueue.Tables
import Attachment.Tables
import BrandedDomain.Tables
import Chargeable.Tables
import Company.Tables
import CompanyAccounts.Tables
import Cron.Tables
import DB.Model
import DB.SQLFunction
import Doc.API.Callback.Tables
import Doc.AutomaticReminder.Tables
import Doc.SMSPin.Tables
import Doc.Tables
import Doc.Tokens.Tables
import EID.CGI.GRP.Transaction.Tables
import EID.Signature.Tables
import EvidenceLog.Tables
import File.Tables
import HostClock.Tables
import Mails.Tables
import OAuth.Tables
import Payments.Tables
import Session.Tables
import SMS.Tables
import ThirdPartyStats.Tables
import User.CallbackScheme.Tables
import User.History.Tables
import User.Tables

kontraFunctions :: [SQLFunction]
kontraFunctions = [
    insertDocumentSessionToken
  , mergeCgiGrpTransaction
  ]

kontraTables :: [Table]
kontraTables =
  [ tableCompanies
  , tableBrandedDomains
  , tableUsers
  , tableUsersHistory
  , tableCompanyInvites
  , tableFiles
  , tableDocuments
  , tableMainFiles
  , tableSignatoryLinks
  , tableAuthorAttachments
  , tableSignatoryAttachments
  , tableEvidenceLog
  , tableDocumentTags
  , tableSignatoryLinkFields
  , tableSignatoryScreenshots
  , tableAPIToken
  , tableAccessToken
  , tablePrivilege
  , tableTempCredential
  , tableTempPrivileges
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
  , tableCgiGrpTransactions
  , tableEIDSignatures
  , tableAsyncEventQueue
  , tableHostClock
  , tableUsersCallbackScheme
  , tableCompanyUIs
  , tableDocumentAutomaticReminders
  , tableSignatorySMSPins
  , tableChargeableItems
  ] ++ cronTables
    ++ mailerTables
    ++ messengerTables
