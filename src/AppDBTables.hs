module AppDBTables (
    kontraFunctions
  , kontraTables
  ) where

import DB.Model
import DB.SQLFunction

import ActionQueue.Tables
import Company.Tables
import CompanyAccounts.Tables
import Doc.Tables
import Doc.API.Callback.Tables
import Doc.Tokens.Tables
import HostClock.Tables
import User.Tables
import User.History.Tables
import File.Tables
import Mails.Tables
import PadQueue.Tables
import Session.Tables
import OAuth.Tables
import SMS.Tables
import ELegitimation.ELegTransaction.Tables
import EvidenceLog.Tables
import Payments.Tables
import Attachment.Tables
import ThirdPartyStats.Tables
import User.CallbackScheme.Tables
import Doc.AutomaticReminder.Tables
import Doc.SMSPin.Tables
import BrandedDomain.Tables

kontraFunctions :: [SQLFunction]
kontraFunctions = [
    insertDocumentSessionToken
  , mergeELegTransaction
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
  , tablePadQueue
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
  , tableELegTransactions
  , tableAsyncEventQueue
  , tableHostClock
  , tableUsersCallbackScheme
  , tableCompanyUIs
  , tableDocumentAutomaticReminders
  , tableSignatorySMSPins
  ] ++ mailerTables
    ++ messengerTables
