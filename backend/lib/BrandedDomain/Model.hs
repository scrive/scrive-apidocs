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
fetchBrandedDomain (xid, maindomain, url, smsoriginator, emailoriginator, mail_theme, signview_theme, service_theme, login_theme, browser_title, favicon, participant_color_1, participant_color_2, participant_color_3, participant_color_4, participant_color_5, participant_color_6, draft_color, cancelled_color, initiated_color, sent_color, delivered_color, opened_color, reviewed_color, signed_color)
  = I.BrandedDomain { id                = xid
                    , mainDomain        = maindomain
                    , url               = url
                    , smsOriginator     = smsoriginator
                    , emailOriginator   = emailoriginator
                    , mailTheme         = mail_theme
                    , signviewTheme     = signview_theme
                    , serviceTheme      = service_theme
                    , loginTheme        = login_theme
                    , browserTitle      = browser_title
                    , favicon           = favicon
                    , participantColor1 = participant_color_1
                    , participantColor2 = participant_color_2
                    , participantColor3 = participant_color_3
                    , participantColor4 = participant_color_4
                    , participantColor5 = participant_color_5
                    , participantColor6 = participant_color_6
                    , draftColor        = draft_color
                    , cancelledColor    = cancelled_color
                    , initatedColor     = initiated_color
                    , sentColor         = sent_color
                    , deliveredColor    = delivered_color
                    , openedColor       = opened_color
                    , reviewedColor     = reviewed_color
                    , signedColor       = signed_color
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


newtype GetBrandedDomains = GetBrandedDomains (Maybe Text)
instance (MonadDB m, MonadLog m) => DBQuery m GetBrandedDomains [BrandedDomain] where
  query (GetBrandedDomains murlpart) = do
    runQuery_ . sqlSelect "branded_domains" $ do
      mapM_ sqlResult brandedDomainSelector
      when (isJust murlpart) $ do
        sqlWhereILike "url" ("%" <> fromMaybe "" murlpart <> "%")
      sqlOrderBy "id"
    fetchMany fetchBrandedDomain

data GetMainBrandedDomain =  GetMainBrandedDomain
instance (MonadDB m,  MonadThrow m, MonadLog m) => DBQuery m GetMainBrandedDomain BrandedDomain where
  query GetMainBrandedDomain = do
    runQuery_ . sqlSelect "branded_domains" $ do
      mapM_ sqlResult brandedDomainSelector
      sqlWhere "branded_domains.main_domain"
      sqlLimit 1
    fetchOne fetchBrandedDomain


newtype GetBrandedDomainByURL = GetBrandedDomainByURL Text
instance (MonadDB m, MonadThrow m, MonadLog m) => DBQuery m GetBrandedDomainByURL BrandedDomain where
  query (GetBrandedDomainByURL url) = do
    runQuery_ . sqlSelect "branded_domains" $ do
      mapM_ sqlResult brandedDomainSelector
      sqlWhere ("" <?> url <> "ILIKE (branded_domains.url || '%')")
      sqlWhere "branded_domains.url <> ''"
      sqlOrderBy "branded_domains.id"
      sqlLimit 1
    mdomain <- fetchMaybe fetchBrandedDomain
    case mdomain of
      Just d  -> return d
      Nothing -> query GetMainBrandedDomain

newtype GetBrandedDomainByUserID = GetBrandedDomainByUserID UserID
instance (MonadDB m,  MonadThrow m, MonadLog m) => DBQuery m GetBrandedDomainByUserID BrandedDomain where
  query (GetBrandedDomainByUserID uid) = do
    runQuery_ . sqlSelect "branded_domains" $ do
      mapM_ sqlResult brandedDomainSelector
      sqlWhereExists . sqlSelect "users" $ do
        sqlWhereEq "users.id" uid
        sqlWhere "users.associated_domain_id = branded_domains.id"
    fetchOne fetchBrandedDomain


newtype GetBrandedDomainByID = GetBrandedDomainByID BrandedDomainID
instance (MonadDB m, MonadThrow m, MonadLog m) => DBQuery m GetBrandedDomainByID BrandedDomain where
  query (GetBrandedDomainByID uid) = do
    runQuery_ . sqlSelect "branded_domains" $ do
      mapM_ sqlResult brandedDomainSelector
      sqlWhereEq "id" uid
    fetchOne fetchBrandedDomain

newtype UpdateBrandedDomain = UpdateBrandedDomain BrandedDomain
instance (MonadDB m) => DBUpdate m UpdateBrandedDomain () where
  update (UpdateBrandedDomain bd) = do
    runQuery_ . sqlUpdate "branded_domains" $ do
      sqlSet "url" $ bd ^. #url
      sqlSet "sms_originator" $ bd ^. #smsOriginator
      sqlSet "email_originator" $ bd ^. #emailOriginator
      sqlSet "mail_theme" $ bd ^. #mailTheme
      sqlSet "signview_theme" $ bd ^. #signviewTheme
      sqlSet "service_theme" $ bd ^. #serviceTheme
      sqlSet "login_theme" $ bd ^. #loginTheme
      sqlSet "browser_title" $ bd ^. #browserTitle
      sqlSet "favicon" $ bd ^. #favicon
      sqlSet "participant_color_1" $ bd ^. #participantColor1
      sqlSet "participant_color_2" $ bd ^. #participantColor2
      sqlSet "participant_color_3" $ bd ^. #participantColor3
      sqlSet "participant_color_4" $ bd ^. #participantColor4
      sqlSet "participant_color_5" $ bd ^. #participantColor5
      sqlSet "participant_color_6" $ bd ^. #participantColor6
      sqlSet "draft_color" $ bd ^. #draftColor
      sqlSet "cancelled_color" $ bd ^. #cancelledColor
      sqlSet "initiated_color" $ bd ^. #initatedColor
      sqlSet "sent_color" $ bd ^. #sentColor
      sqlSet "delivered_color" $ bd ^. #deliveredColor
      sqlSet "opened_color" $ bd ^. #openedColor
      sqlSet "reviewed_color" $ bd ^. #reviewedColor
      sqlSet "signed_color" $ bd ^. #signedColor
      sqlWhereEq "id" $ bd ^. #id
      sqlWhereNotEq "main_domain" True

data NewBrandedDomain = NewBrandedDomain
instance (MonadDB m, MonadThrow m, MonadLog m) => DBUpdate m NewBrandedDomain BrandedDomainID where
  update NewBrandedDomain = do
    mbd          <- dbQuery GetMainBrandedDomain
    newmailtheme <- (dbUpdate . UnsafeInsertNewThemeWithoutOwner)
      =<< dbQuery (GetTheme $ mbd ^. #mailTheme)
    newsignviewtheme <- (dbUpdate . UnsafeInsertNewThemeWithoutOwner)
      =<< dbQuery (GetTheme $ mbd ^. #signviewTheme)
    newservicetheme <- (dbUpdate . UnsafeInsertNewThemeWithoutOwner)
      =<< dbQuery (GetTheme $ mbd ^. #serviceTheme)
    newlogintheme <- (dbUpdate . UnsafeInsertNewThemeWithoutOwner)
      =<< dbQuery (GetTheme $ mbd ^. #loginTheme)
    runQuery_ . sqlInsert "branded_domains" $ do
      sqlSet "url"         ("" :: String)
      sqlSet "main_domain" False -- One can not create new main domain
      sqlSet "sms_originator" $ mbd ^. #smsOriginator
      sqlSet "email_originator" $ mbd ^. #emailOriginator
      sqlSet "mail_theme" $ themeID newmailtheme
      sqlSet "signview_theme" $ themeID newsignviewtheme
      sqlSet "service_theme" $ themeID newservicetheme
      sqlSet "login_theme" $ themeID newlogintheme
      sqlSet "browser_title" ("Scrive" :: String)
      sqlSet "favicon" $ mbd ^. #favicon
      sqlSet "participant_color_1" $ mbd ^. #participantColor1
      sqlSet "participant_color_2" $ mbd ^. #participantColor2
      sqlSet "participant_color_3" $ mbd ^. #participantColor3
      sqlSet "participant_color_4" $ mbd ^. #participantColor4
      sqlSet "participant_color_5" $ mbd ^. #participantColor5
      sqlSet "participant_color_6" $ mbd ^. #participantColor6
      sqlSet "draft_color" $ mbd ^. #draftColor
      sqlSet "cancelled_color" $ mbd ^. #cancelledColor
      sqlSet "initiated_color" $ mbd ^. #initatedColor
      sqlSet "sent_color" $ mbd ^. #sentColor
      sqlSet "delivered_color" $ mbd ^. #deliveredColor
      sqlSet "opened_color" $ mbd ^. #openedColor
      sqlSet "reviewed_color" $ mbd ^. #reviewedColor
      sqlSet "signed_color" $ mbd ^. #signedColor
      sqlResult "id"
    newdomainID <- fetchOne runIdentity
    dbUpdate $ MakeThemeOwnedByDomain newdomainID (themeID newmailtheme)
    dbUpdate $ MakeThemeOwnedByDomain newdomainID (themeID newsignviewtheme)
    dbUpdate $ MakeThemeOwnedByDomain newdomainID (themeID newservicetheme)
    dbUpdate $ MakeThemeOwnedByDomain newdomainID (themeID newlogintheme)
    return newdomainID

newtype SetMainDomainURL = SetMainDomainURL String
instance (MonadDB m, MonadThrow m) => DBUpdate m SetMainDomainURL () where
  update (SetMainDomainURL url) = do
    n <- runQuery . sqlUpdate "branded_domains" $ do
      sqlSet "url" url
      sqlWhere "main_domain"
    when (n /= 1) $ do
      unexpectedError "Main domain doesn't exist"
