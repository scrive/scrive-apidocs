{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Company.CompanyUI (
    CompanyUI(..)
  , SetCompanyUI(..)
  , GetCompanyUI(..)
  ) where

import Control.Monad.Catch
import Control.Monad.State
import Data.Typeable
import qualified Data.ByteString.Char8 as BS

import Company.CompanyID
import DB
import OurPrelude
import Theme.Model

data CompanyUI = CompanyUI
  { companyuicompanyid                :: !CompanyID
  , companyMailTheme                  :: !(Maybe ThemeID)
  , companySignviewTheme              :: !(Maybe ThemeID)
  , companyServiceTheme               :: !(Maybe ThemeID)
  , companyBrowserTitle               :: !(Maybe String)
  , companySmsOriginator              :: !(Maybe String)
  , companyFavicon                    :: !(Maybe (Binary BS.ByteString))
} deriving (Eq, Ord, Show, Typeable)

data GetCompanyUI = GetCompanyUI CompanyID
instance (MonadDB m, MonadThrow m) => DBQuery m GetCompanyUI CompanyUI where
  query (GetCompanyUI cid) = do
    runQuery_ . sqlSelect "company_uis" $ do
      sqlWhereEq "company_id" cid
      selectCompanyUIsSelectors
    fetchOne fetchCompanyUI

data SetCompanyUI = SetCompanyUI CompanyID CompanyUI
instance (MonadDB m, MonadThrow m) => DBUpdate m SetCompanyUI Bool where
  update (SetCompanyUI cid cui) = do
    runQuery01 . sqlUpdate "company_uis" $ do
      sqlSet "mail_theme" $ companyMailTheme cui
      sqlSet "signview_theme" $ companySignviewTheme cui
      sqlSet "service_theme" $ companyServiceTheme cui
      sqlSet "browser_title" $ companyBrowserTitle cui
      sqlSet "sms_originator" $ companySmsOriginator cui
      sqlSet "favicon" $ companyFavicon cui
      sqlWhereEq "company_id" cid

      when (isJust $ companyMailTheme cui) $ do
        sqlWhereExists $ sqlSelect "theme_owners" $ do
          sqlWhereEq "company_id" cid
          sqlWhereEq "theme_id" (companyMailTheme cui)

      when (isJust $ companySignviewTheme cui) $ do
        sqlWhereExists $ sqlSelect "theme_owners" $ do
          sqlWhereEq "company_id" cid
          sqlWhereEq "theme_id" (companySignviewTheme cui)

      when (isJust $ companyServiceTheme cui) $ do
        sqlWhereExists $ sqlSelect "theme_owners" $ do
          sqlWhereEq "company_id" cid
          sqlWhereEq "theme_id" (companyServiceTheme cui)

selectCompanyUIsSelectors :: (SqlResult command) => State command ()
selectCompanyUIsSelectors = do
  sqlResult "company_uis.company_id"
  sqlResult "company_uis.mail_theme"
  sqlResult "company_uis.signview_theme"
  sqlResult "company_uis.service_theme"
  sqlResult "browser_title"
  sqlResult "sms_originator"
  sqlResult "favicon"

fetchCompanyUI :: (CompanyID, Maybe ThemeID, Maybe ThemeID, Maybe ThemeID, Maybe String, Maybe String, Maybe (Binary BS.ByteString)) -> CompanyUI
fetchCompanyUI (company_id,mail_theme,signview_theme,service_theme,browser_title,sms_originator,favicon) = CompanyUI {
  companyuicompanyid = company_id
, companyMailTheme = mail_theme
, companySignviewTheme = signview_theme
, companyServiceTheme = service_theme
, companyBrowserTitle = browser_title
, companySmsOriginator = sms_originator
, companyFavicon = faviconFromBinary favicon
}
  where
    -- We should interpret empty logos as no logos.
    faviconFromBinary (Just f) = if (BS.null $ unBinary f) then Nothing else Just f
    faviconFromBinary Nothing = Nothing
