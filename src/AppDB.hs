{-# LANGUAGE CPP #-}
module AppDB (
    kontraMigrations
  , kontraTables
  ) where

import DB.Model

import API.Service.Tables
import Company.Tables
import CompanyAccounts.Tables
import Doc.Tables
import Doc.Migrations
import User.Migrations
import User.Tables
import User.History.Tables
import Stats.Tables
import Stats.Migrations
import File.Tables
import Mails.Tables
import Mails.Migrations

import OAuth.Tables
import EvidenceLog.Tables

-- Note: ALWAYS append new migrations TO THE END of this list.
kontraMigrations :: [Migration]
kontraMigrations = [
    addRegionToUserSettings
  , addServiceAndCompanyToStats
  , removeSystemServer
  , addUserCustomFooter
  , makeUserStatsRepeatableByChangingPK
  , addNameColumnInSignatoryAttachments
  , addCSVUploadDataFromDocumentToSignatoryLink
  , addColumnToRecordInternalInsertionOrder
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
  , tableTempCredential
  , tableTempPrivileges
  , tableAPIToken
  ] ++ mailerTables
