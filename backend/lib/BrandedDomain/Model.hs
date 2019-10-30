module BrandedDomain.Model
  (   BrandedDomainID
    , BrandedDomain
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
import DB
import Theme.Model
import User.UserID
import qualified BrandedDomain.BrandedDomain.Internal as I

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
fetchBrandedDomain ( xid
                   , maindomain
                   , url
                   , smsoriginator
                   , emailoriginator
                   , mail_theme
                   , signview_theme
                   , service_theme
                   , login_theme
                   , browser_title
                   , favicon
                   , participant_color_1
                   , participant_color_2
                   , participant_color_3
                   , participant_color_4
                   , participant_color_5
                   , participant_color_6
                   , draft_color
                   , cancelled_color
                   , initiated_color
                   , sent_color
                   , delivered_color
                   , opened_color
                   , reviewed_color
                   , signed_color
                   )
  = I.BrandedDomain { bdid                = xid
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
      sqlSet "url" $ bd ^. #bdUrl
      sqlSet "sms_originator" $ bd ^. #bdSmsOriginator
      sqlSet "email_originator" $ bd ^. #bdEmailOriginator
      sqlSet "mail_theme" $ bd ^. #bdMailTheme
      sqlSet "signview_theme" $ bd ^. #bdSignviewTheme
      sqlSet "service_theme" $ bd ^. #bdServiceTheme
      sqlSet "login_theme" $ bd ^. #bdLoginTheme
      sqlSet "browser_title" $ bd ^. #bdBrowserTitle
      sqlSet "favicon" $ bd ^. #bdFavicon
      sqlSet "participant_color_1" $ bd ^. #bdParticipantColor1
      sqlSet "participant_color_2" $ bd ^. #bdParticipantColor2
      sqlSet "participant_color_3" $ bd ^. #bdParticipantColor3
      sqlSet "participant_color_4" $ bd ^. #bdParticipantColor4
      sqlSet "participant_color_5" $ bd ^. #bdParticipantColor5
      sqlSet "participant_color_6" $ bd ^. #bdParticipantColor6
      sqlSet "draft_color" $ bd ^. #bdDraftColor
      sqlSet "cancelled_color" $ bd ^. #bdCancelledColor
      sqlSet "initiated_color" $ bd ^. #bdInitatedColor
      sqlSet "sent_color" $ bd ^. #bdSentColor
      sqlSet "delivered_color" $ bd ^. #bdDeliveredColor
      sqlSet "opened_color" $ bd ^. #bdOpenedColor
      sqlSet "reviewed_color" $ bd ^. #bdReviewedColor
      sqlSet "signed_color" $ bd ^. #bdSignedColor
      sqlWhereEq "id" $ bd ^. #bdid
      sqlWhereNotEq "main_domain" True

data NewBrandedDomain = NewBrandedDomain
instance (MonadDB m, MonadThrow m, MonadLog m) => DBUpdate m NewBrandedDomain BrandedDomainID where
  update (NewBrandedDomain) = do
    mbd          <- dbQuery $ GetMainBrandedDomain
    newmailtheme <-
      (dbUpdate . UnsafeInsertNewThemeWithoutOwner)
        =<< (dbQuery $ GetTheme $ mbd ^. #bdMailTheme)
    newsignviewtheme <-
      (dbUpdate . UnsafeInsertNewThemeWithoutOwner)
        =<< (dbQuery $ GetTheme $ mbd ^. #bdSignviewTheme)
    newservicetheme <-
      (dbUpdate . UnsafeInsertNewThemeWithoutOwner)
        =<< (dbQuery $ GetTheme $ mbd ^. #bdServiceTheme)
    newlogintheme <-
      (dbUpdate . UnsafeInsertNewThemeWithoutOwner)
        =<< (dbQuery $ GetTheme $ mbd ^. #bdLoginTheme)
    runQuery_ . sqlInsert "branded_domains" $ do
      sqlSet "url"         ("" :: String)
      sqlSet "main_domain" False -- One can not create new main domain
      sqlSet "sms_originator" $ mbd ^. #bdSmsOriginator
      sqlSet "email_originator" $ mbd ^. #bdEmailOriginator
      sqlSet "mail_theme" $ themeID newmailtheme
      sqlSet "signview_theme" $ themeID newsignviewtheme
      sqlSet "service_theme" $ themeID newservicetheme
      sqlSet "login_theme" $ themeID newlogintheme
      sqlSet "browser_title" ("Scrive" :: String)
      sqlSet "favicon" $ mbd ^. #bdFavicon
      sqlSet "participant_color_1" $ mbd ^. #bdParticipantColor1
      sqlSet "participant_color_2" $ mbd ^. #bdParticipantColor2
      sqlSet "participant_color_3" $ mbd ^. #bdParticipantColor3
      sqlSet "participant_color_4" $ mbd ^. #bdParticipantColor4
      sqlSet "participant_color_5" $ mbd ^. #bdParticipantColor5
      sqlSet "participant_color_6" $ mbd ^. #bdParticipantColor6
      sqlSet "draft_color" $ mbd ^. #bdDraftColor
      sqlSet "cancelled_color" $ mbd ^. #bdCancelledColor
      sqlSet "initiated_color" $ mbd ^. #bdInitatedColor
      sqlSet "sent_color" $ mbd ^. #bdSentColor
      sqlSet "delivered_color" $ mbd ^. #bdDeliveredColor
      sqlSet "opened_color" $ mbd ^. #bdOpenedColor
      sqlSet "reviewed_color" $ mbd ^. #bdReviewedColor
      sqlSet "signed_color" $ mbd ^. #bdSignedColor
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
