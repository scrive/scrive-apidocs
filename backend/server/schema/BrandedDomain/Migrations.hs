module BrandedDomain.Migrations (
    brandedDomainDropNoreplyEmail
) where

import Control.Monad.Catch

import BrandedDomain.Tables
import DB

brandedDomainDropNoreplyEmail :: (MonadThrow m, MonadDB m) => Migration m
brandedDomainDropNoreplyEmail = Migration
  { mgrTableName = tblName tableBrandedDomains
  , mgrFrom      = 11
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable (tblName tableBrandedDomains)
                                               [sqlDropColumn "noreply_email"]
  }
