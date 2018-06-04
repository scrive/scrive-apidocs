module Company.CompanyUI.Model (
    module Company.CompanyUI.Data
  , SetCompanyUI(..)
  , GetCompanyUI(..)
  ) where

import Control.Monad.Catch
import Control.Monad.State
import Log
import qualified Data.ByteString.Char8 as BS

import Company.CompanyID
import Company.CompanyUI.Data
import Company.Model
import DB
import Log.Identifier
import Theme.Model
import UserGroup.Data
import UserGroup.Model

data GetCompanyUI = GetCompanyUI CompanyID
instance (MonadDB m, MonadThrow m) => DBQuery m GetCompanyUI CompanyUI where
  query (GetCompanyUI cid) = do
    runQuery_ . sqlSelect "company_uis" $ do
      sqlWhereEq "company_id" cid
      selectCompanyUIsSelectors
    fetchOne fetchCompanyUI

data SetCompanyUI = SetCompanyUI CompanyID CompanyUI
instance (MonadDB m, MonadThrow m, MonadLog m) => DBUpdate m SetCompanyUI Bool where
  update (SetCompanyUI cid cui) = do
    result <- runQuery01 . sqlUpdate "company_uis" $ do
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
    when result $ do
      dbQuery (GetCompany cid) >>= \case
        Nothing -> logAttention "SetCompanyUI company doesn't exist" $ object [
            identifier_ cid
          ]
        Just c -> do
          whenJust (companyusergroupid c) $ \ugid ->
            dbQuery (UserGroupGet ugid) >>= \case
              Nothing -> logAttention "UserGroup doesn't exist during SetCompanyUI" $ object [
                  identifier_ cid
                , identifier_ ugid
                ]
              Just ug -> do
                -- check, that all themes are already owned by this user group
                let newthemeids = catMaybes [companyMailTheme cui, companySignviewTheme cui, companyServiceTheme cui]
                runQuery_ . sqlSelect "theme_owners" $ do
                  sqlResult "theme_id"
                  sqlWhereEq "user_group_id" ugid
                ownedthemeids <- fetchMany runIdentity
                case all (`elem` ownedthemeids) newthemeids of
                  False -> logAttention "UserGroup doesn't own assigned themes during SetCompanyUI" $ object [
                      identifier_ cid
                    , identifier_ ugid
                    ]
                  True  -> dbUpdate . UserGroupUpdate . set ugUI (toUserGroupUI cui) $ ug
    return result


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
