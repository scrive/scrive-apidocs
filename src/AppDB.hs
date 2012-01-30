{-# LANGUAGE CPP #-}
module AppDB (
    kontraMigrations
  , kontraTables
  ) where

import DB.Model

import API.Service.Tables
import Company.Tables
import CompanyAccounts.Tables
#ifdef DOCUMENTS_IN_POSTGRES
import Doc.Tables
import Doc.Migrations
#endif
import User.Migrations
import User.Tables
import User.History.Tables
import Stats.Tables
import Stats.Migrations
import File.Tables
import Mails.Tables
import Mails.Migrations

-- Note: ALWAYS append new migrations TO THE END of this list.
kontraMigrations :: [Migration]
kontraMigrations = [
    addRegionToUserSettings
  , addServiceAndCompanyToStats
  , removeSystemServer
  , addUserCustomFooter
  , makeUserStatsRepeatableByChangingPK
#ifdef DOCUMENTS_IN_POSTGRES
  , addNameColumnInSignatoryAttachments
  , addCSVUploadDataFromDocumentToSignatoryLink
#endif
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
#ifdef DOCUMENTS_IN_POSTGRES
  , tableDocuments
  , tableSignatoryLinks
  , tableAuthorAttachments
  , tableSignatoryAttachments
#endif
  ] ++ mailerTables
