{-# LANGUAGE CPP #-}
module DB.Migrations (
    migrationsList
  , tablesList
  ) where

import DB.Model
import DB.Versions

import API.Service.Tables
import Company.Tables
import CompanyAccounts.Tables
#ifdef DOCUMENTS_IN_POSTGRES
import Doc.Tables
import Doc.Migrations
#endif
import User.Migrations
import User.Tables
import Stats.Tables
import Stats.Migrations
import File.Tables

-- Note: ALWAYS append new migrations TO THE END of this list.
migrationsList :: [Migration]
migrationsList = [
    addRegionToUserSettings
  , addServiceAndCompanyToStats
  , removeSystemServer
  , addUserCustomFooter
#ifdef DOCUMENTS_IN_POSTGRES
  , addNameColumnInSignatoryAttachments
#endif
  ]

tablesList :: [Table]
tablesList = [
    tableVersions
  , tableUsers
  , tableUserFriends
  , tableUserMailAPIs
  , tableUserInviteInfos
  , tableServices
  , tableCompanies
  , tableCompanyInvites
  , tableDocStatEvents
  , tableUserStatEvents
  , tableFiles
#ifdef DOCUMENTS_IN_POSTGRES
  , tableDocuments
  , tableSignatoryLinks
  , tableAuthorAttachments
  , tableSignatoryAttachments
#endif
  ]
