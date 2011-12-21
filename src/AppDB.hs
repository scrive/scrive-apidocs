{-# LANGUAGE CPP #-}
module AppDB (
    kontraMigrations
  , kontraTables
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
kontraMigrations :: [Migration]
kontraMigrations = [
    addRegionToUserSettings
  , addServiceAndCompanyToStats
  , removeSystemServer
  , addUserCustomFooter
#ifdef DOCUMENTS_IN_POSTGRES
  , addNameColumnInSignatoryAttachments
#endif
  ]

kontraTables :: [Table]
kontraTables = [
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
