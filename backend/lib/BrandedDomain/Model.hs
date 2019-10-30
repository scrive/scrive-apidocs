module BrandedDomain.Model
  (   BrandedDomainID
    , BrandedDomain(..)
    , GetBrandedDomains(..)
    , GetMainBrandedDomain(..)
    , GetBrandedDomainByURL(..)
    , GetBrandedDomainByUserID(..)
    , GetBrandedDomainByID(..)
    , UpdateBrandedDomain(..)
    , NewBrandedDomain(..)
    , SetMainDomainURL(..)
  ) where

import Control.Monad.Catch
import Log
import qualified Data.ByteString.Char8 as BS

import BrandedDomain.BrandedDomain
import BrandedDomain.BrandedDomain.Internal
import DB
import Theme.Model
import User.UserID

fetchBrandedDomain
  :: ( BrandedDomainID
     , Bool
     , Text
     , Text
     , Text
     , ThemeID
     , ThemeID
     , ThemeID
     , ThemeID
     , Text
     , BS.ByteString
     , Text
     , Text
     , Text
     , Text
     , Text
     , Text
     , Text
     , Text
     , Text
     , Text
     , Text
     , Text
     , Text
     , Text
     )
  -> BrandedDomain
fetchBrandedDomain (xid, maindomain, url, smsoriginator, emailoriginator, mail_theme, signview_theme, service_theme, login_theme, browser_title, favicon, participant_color_1, participant_color_2, participant_color_3, participant_color_4, participant_color_5, participant_color_6, draft_color, cancelled_color, initiated_color, sent_color, delivered_color, opened_color, reviewed_color, signed_color)
  = BrandedDomain { bdid                = xid
                  , bdMainDomain        = maindomain
                  , bdUrl               = url
                  , bdSmsOriginator     = smsoriginator
                  , bdEmailOriginator   = emailoriginator
                  , bdMailTheme         = mail_theme
                  , bdSignviewTheme     = signview_theme
                  , bdServiceTheme      = service_theme
                  , bdLoginTheme        = login_theme
                  , bdBrowserTitle      = browser_title
                  , bdFavicon           = favicon
                  , bdParticipantColor1 = participant_color_1
                  , bdParticipantColor2 = participant_color_2
                  , bdParticipantColor3 = participant_color_3
                  , bdParticipantColor4 = participant_color_4
                  , bdParticipantColor5 = participant_color_5
                  , bdParticipantColor6 = participant_color_6
                  , bdDraftColor        = draft_color
                  , bdCancelledColor    = cancelled_color
                  , bdInitatedColor     = initiated_color
                  , bdSentColor         = sent_color
                  , bdDeliveredColor    = delivered_color
                  , bdOpenedColor       = opened_color
                  , bdReviewedColor     = reviewed_color
                  , bdSignedColor       = signed_color
                  }

brandedDomainSelector :: [SQL]
brandedDomainSelector =
  [ "id"
  , "main_domain"
  , "url"
  , "sms_originator"
  , "email_originator"
  , "mail_theme"
  , "signview_theme"
  , "service_theme"
  , "login_theme"
  , "browser_title"
  , "favicon"
  , "participant_color_1"
  , "participant_color_2"
  , "participant_color_3"
  , "participant_color_4"
  , "participant_color_5"
  , "participant_color_6"
  , "draft_color"
  , "cancelled_color"
  , "initiated_color"
  , "sent_color"
  , "delivered_color"
  , "opened_color"
  , "reviewed_color"
  , "signed_color"
  ]


data GetBrandedDomains = GetBrandedDomains (Maybe Text)
instance (MonadDB m, MonadLog m) => DBQuery m GetBrandedDomains [BrandedDomain] where
  query (GetBrandedDomains murlpart) = do
    runQuery_ . sqlSelect "branded_domains" $ do
      mapM_ sqlResult $ brandedDomainSelector
      when (isJust murlpart) $ do
        sqlWhereILike "url" ("%" <> fromMaybe "" murlpart <> "%")
      sqlOrderBy "id"
    fetchMany fetchBrandedDomain

data GetMainBrandedDomain =  GetMainBrandedDomain
instance (MonadDB m,  MonadThrow m, MonadLog m) => DBQuery m GetMainBrandedDomain BrandedDomain where
  query (GetMainBrandedDomain) = do
    runQuery_ . sqlSelect "branded_domains" $ do
      mapM_ sqlResult $ brandedDomainSelector
      sqlWhere "branded_domains.main_domain"
      sqlLimit 1
    fetchOne fetchBrandedDomain


data GetBrandedDomainByURL = GetBrandedDomainByURL Text
instance (MonadDB m, MonadThrow m, MonadLog m) => DBQuery m GetBrandedDomainByURL BrandedDomain where
  query (GetBrandedDomainByURL url) = do
    runQuery_ . sqlSelect "branded_domains" $ do
      mapM_ sqlResult $ brandedDomainSelector
      sqlWhere ("" <?> url <> "ILIKE (branded_domains.url || '%')")
      sqlWhere "branded_domains.url <> ''"
      sqlOrderBy "branded_domains.id"
      sqlLimit 1
    mdomain <- fetchMaybe fetchBrandedDomain
    case mdomain of
      Just d  -> return d
      Nothing -> query $ GetMainBrandedDomain

data GetBrandedDomainByUserID = GetBrandedDomainByUserID UserID
instance (MonadDB m,  MonadThrow m, MonadLog m) => DBQuery m GetBrandedDomainByUserID BrandedDomain where
  query (GetBrandedDomainByUserID uid) = do
    runQuery_ . sqlSelect "branded_domains" $ do
      mapM_ sqlResult $ brandedDomainSelector
      sqlWhereExists $ sqlSelect "users" $ do
        sqlWhereEq "users.id" uid
        sqlWhere "users.associated_domain_id = branded_domains.id"
    fetchOne fetchBrandedDomain


data GetBrandedDomainByID = GetBrandedDomainByID BrandedDomainID
instance (MonadDB m, MonadThrow m, MonadLog m) => DBQuery m GetBrandedDomainByID BrandedDomain where
  query (GetBrandedDomainByID uid) = do
    runQuery_ . sqlSelect "branded_domains" $ do
      mapM_ sqlResult $ brandedDomainSelector
      sqlWhereEq "id" uid
    fetchOne fetchBrandedDomain

data UpdateBrandedDomain = UpdateBrandedDomain BrandedDomain
instance (MonadDB m) => DBUpdate m UpdateBrandedDomain () where
  update (UpdateBrandedDomain bd) = do
    runQuery_ . sqlUpdate "branded_domains" $ do
      sqlSet "url" $ bdUrl bd
      sqlSet "sms_originator" $ bdSmsOriginator bd
      sqlSet "email_originator" $ bdEmailOriginator bd
      sqlSet "mail_theme" $ bdMailTheme bd
      sqlSet "signview_theme" $ bdSignviewTheme bd
      sqlSet "service_theme" $ bdServiceTheme bd
      sqlSet "login_theme" $ bdLoginTheme bd
      sqlSet "browser_title" $ bdBrowserTitle bd
      sqlSet "favicon" $ bdFavicon bd
      sqlSet "participant_color_1" $ bdParticipantColor1 bd
      sqlSet "participant_color_2" $ bdParticipantColor2 bd
      sqlSet "participant_color_3" $ bdParticipantColor3 bd
      sqlSet "participant_color_4" $ bdParticipantColor4 bd
      sqlSet "participant_color_5" $ bdParticipantColor5 bd
      sqlSet "participant_color_6" $ bdParticipantColor6 bd
      sqlSet "draft_color" $ bdDraftColor bd
      sqlSet "cancelled_color" $ bdCancelledColor bd
      sqlSet "initiated_color" $ bdInitatedColor bd
      sqlSet "sent_color" $ bdSentColor bd
      sqlSet "delivered_color" $ bdDeliveredColor bd
      sqlSet "opened_color" $ bdOpenedColor bd
      sqlSet "reviewed_color" $ bdReviewedColor bd
      sqlSet "signed_color" $ bdSignedColor bd
      sqlWhereEq "id" (bdid bd)
      sqlWhereNotEq "main_domain" True

data NewBrandedDomain = NewBrandedDomain
instance (MonadDB m, MonadThrow m, MonadLog m) => DBUpdate m NewBrandedDomain BrandedDomainID where
  update (NewBrandedDomain) = do
    mbd          <- dbQuery $ GetMainBrandedDomain
    newmailtheme <-
      (dbUpdate . UnsafeInsertNewThemeWithoutOwner)
        =<< (dbQuery $ GetTheme $ bdMailTheme mbd)
    newsignviewtheme <-
      (dbUpdate . UnsafeInsertNewThemeWithoutOwner)
        =<< (dbQuery $ GetTheme $ bdSignviewTheme mbd)
    newservicetheme <-
      (dbUpdate . UnsafeInsertNewThemeWithoutOwner)
        =<< (dbQuery $ GetTheme $ bdServiceTheme mbd)
    newlogintheme <-
      (dbUpdate . UnsafeInsertNewThemeWithoutOwner)
        =<< (dbQuery $ GetTheme $ bdLoginTheme mbd)
    runQuery_ . sqlInsert "branded_domains" $ do
      sqlSet "url"         ("" :: String)
      sqlSet "main_domain" False -- One can not create new main domain
      sqlSet "sms_originator" $ bdSmsOriginator mbd
      sqlSet "email_originator" $ bdEmailOriginator mbd
      sqlSet "mail_theme" $ themeID newmailtheme
      sqlSet "signview_theme" $ themeID newsignviewtheme
      sqlSet "service_theme" $ themeID newservicetheme
      sqlSet "login_theme" $ themeID newlogintheme
      sqlSet "browser_title" ("Scrive" :: String)
      sqlSet "favicon" $ bdFavicon mbd
      sqlSet "participant_color_1" $ bdParticipantColor1 mbd
      sqlSet "participant_color_2" $ bdParticipantColor2 mbd
      sqlSet "participant_color_3" $ bdParticipantColor3 mbd
      sqlSet "participant_color_4" $ bdParticipantColor4 mbd
      sqlSet "participant_color_5" $ bdParticipantColor5 mbd
      sqlSet "participant_color_6" $ bdParticipantColor6 mbd
      sqlSet "draft_color" $ bdDraftColor mbd
      sqlSet "cancelled_color" $ bdCancelledColor mbd
      sqlSet "initiated_color" $ bdInitatedColor mbd
      sqlSet "sent_color" $ bdSentColor mbd
      sqlSet "delivered_color" $ bdDeliveredColor mbd
      sqlSet "opened_color" $ bdOpenedColor mbd
      sqlSet "reviewed_color" $ bdReviewedColor mbd
      sqlSet "signed_color" $ bdSignedColor mbd
      sqlResult "id"
    newdomainID <- fetchOne runIdentity
    dbUpdate $ MakeThemeOwnedByDomain newdomainID (themeID newmailtheme)
    dbUpdate $ MakeThemeOwnedByDomain newdomainID (themeID newsignviewtheme)
    dbUpdate $ MakeThemeOwnedByDomain newdomainID (themeID newservicetheme)
    dbUpdate $ MakeThemeOwnedByDomain newdomainID (themeID newlogintheme)
    return newdomainID

data SetMainDomainURL = SetMainDomainURL String
instance (MonadDB m, MonadThrow m) => DBUpdate m SetMainDomainURL () where
  update (SetMainDomainURL url) = do
    n <- runQuery . sqlUpdate "branded_domains" $ do
      sqlSet "url" url
      sqlWhere "main_domain"
    when (n /= 1) $ do
      unexpectedError "Main domain doesn't exist"
