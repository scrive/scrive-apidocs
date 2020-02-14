module BrandedDomain.Migrations (
    brandedDomainDropNoreplyEmail
  , brandedDomainChangeLightLogo
) where

import Control.Monad.Catch
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.UTF8 as BS

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

brandedDomainChangeLightLogo :: (MonadThrow m, MonadDB m) => Migration m
brandedDomainChangeLightLogo = Migration
  { mgrTableName = tblName tableBrandedDomains
  , mgrFrom      = 12
  , mgrAction    =
    StandardMigration $ do
      let mainTheme themeName mextra = sqlSelect "branded_domains" $ do
            sqlResult themeName
            sqlWhereEq "main_domain" True
            fromMaybe (return ()) mextra
      runQuery_ $ sqlUpdate "themes" $ do
        sqlSet "logo" $ B64.decodeLenient $ BS.fromString lightTextLogo
        sqlWhereInSql "id" $ mainTheme "service_theme" $ Just $ sqlUnion
          [mainTheme "login_theme" Nothing, mainTheme "signview_theme" Nothing]
  }
