module Company.CompanyUI.Model (
    module Company.CompanyUI.Data
  , GetCompanyUI(..)
  ) where

import Control.Monad.Catch
import Control.Monad.State
import qualified Data.ByteString.Char8 as BS

import Company.CompanyID
import Company.CompanyUI.Data
import DB
import Theme.Model

data GetCompanyUI = GetCompanyUI CompanyID
instance (MonadDB m, MonadThrow m) => DBQuery m GetCompanyUI CompanyUI where
  query (GetCompanyUI cid) = do
    runQuery_ . sqlSelect "company_uis" $ do
      sqlWhereEq "company_id" cid
      selectCompanyUIsSelectors
    fetchOne fetchCompanyUI

selectCompanyUIsSelectors :: (SqlResult command) => State command ()
selectCompanyUIsSelectors = do
  sqlResult "company_uis.company_id"
  sqlResult "company_uis.mail_theme"
  sqlResult "company_uis.signview_theme"
  sqlResult "company_uis.service_theme"
  sqlResult "browser_title"
  sqlResult "sms_originator"
  sqlResult "favicon"

fetchCompanyUI :: (CompanyID, Maybe ThemeID, Maybe ThemeID, Maybe ThemeID, Maybe String, Maybe String, Maybe BS.ByteString) -> CompanyUI
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
    faviconFromBinary (Just f) = if (BS.null f) then Nothing else Just f
    faviconFromBinary Nothing = Nothing
