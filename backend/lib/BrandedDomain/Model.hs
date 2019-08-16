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
  :: ( BrandedDomainID,Bool
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
fetchBrandedDomain (xid, maindomain, url, smsoriginator, emailoriginator, mail_theme, signview_theme,service_theme,login_theme, browser_title,
                    favicon, participant_color_1, participant_color_2, participant_color_3, participant_color_4, participant_color_5, participant_color_6, draft_color, cancelled_color,
                    initiated_color, sent_color, delivered_color, opened_color, reviewed_color, signed_color)
       = BrandedDomain
         { _bdid                          = xid
         , _bdMainDomain                  = maindomain
         , _bdUrl                         = url
         , _bdSmsOriginator               = smsoriginator
         , _bdEmailOriginator             = emailoriginator
         , _bdMailTheme                   = mail_theme
         , _bdSignviewTheme               = signview_theme
         , _bdServiceTheme                = service_theme
         , _bdLoginTheme                  = login_theme
         , _bdBrowserTitle                = browser_title
         , _bdFavicon                     = favicon
         , _bdParticipantColor1           = participant_color_1
         , _bdParticipantColor2           = participant_color_2
         , _bdParticipantColor3           = participant_color_3
         , _bdParticipantColor4           = participant_color_4
         , _bdParticipantColor5           = participant_color_5
         , _bdParticipantColor6           = participant_color_6
         , _bdDraftColor                  = draft_color
         , _bdCancelledColor              = cancelled_color
         , _bdInitatedColor               = initiated_color
         , _bdSentColor                   = sent_color
         , _bdDeliveredColor              = delivered_color
         , _bdOpenedColor                 = opened_color
         , _bdReviewedColor               = reviewed_color
         , _bdSignedColor                 = signed_color
       }

brandedDomainSelector :: [SQL]
brandedDomainSelector = [
    "id"
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


data GetBrandedDomains = GetBrandedDomains
instance (MonadDB m, MonadLog m) => DBQuery m GetBrandedDomains [BrandedDomain] where
  query (GetBrandedDomains) = do
    runQuery_ . sqlSelect "branded_domains" $ do
      mapM_ sqlResult $ brandedDomainSelector
      sqlOrderBy "id"
    fetchMany fetchBrandedDomain

data GetMainBrandedDomain =  GetMainBrandedDomain
instance (MonadDB m,  MonadThrow m, MonadLog m) => DBQuery m GetMainBrandedDomain BrandedDomain where
  query (GetMainBrandedDomain ) = do
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
         Just d -> return d
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
      sqlSet "url"                 $ get bdUrl                bd
      sqlSet "sms_originator"      $ get bdSmsOriginator      bd
      sqlSet "email_originator"    $ get bdEmailOriginator    bd
      sqlSet "mail_theme"          $ get bdMailTheme          bd
      sqlSet "signview_theme"      $ get bdSignviewTheme      bd
      sqlSet "service_theme"       $ get bdServiceTheme       bd
      sqlSet "login_theme"         $ get bdLoginTheme         bd
      sqlSet "browser_title"       $ get bdBrowserTitle       bd
      sqlSet "favicon"             $ get bdFavicon            bd
      sqlSet "participant_color_1" $ get bdParticipantColor1  bd
      sqlSet "participant_color_2" $ get bdParticipantColor2  bd
      sqlSet "participant_color_3" $ get bdParticipantColor3  bd
      sqlSet "participant_color_4" $ get bdParticipantColor4  bd
      sqlSet "participant_color_5" $ get bdParticipantColor5  bd
      sqlSet "participant_color_6" $ get bdParticipantColor6  bd
      sqlSet "draft_color"         $ get bdDraftColor         bd
      sqlSet "cancelled_color"     $ get bdCancelledColor     bd
      sqlSet "initiated_color"     $ get bdInitatedColor      bd
      sqlSet "sent_color"          $ get bdSentColor          bd
      sqlSet "delivered_color"     $ get bdDeliveredColor     bd
      sqlSet "opened_color"        $ get bdOpenedColor        bd
      sqlSet "reviewed_color"      $ get bdReviewedColor      bd
      sqlSet "signed_color"        $ get bdSignedColor        bd
      sqlWhereEq "id" (get bdid bd)
      sqlWhereNotEq "main_domain" True

data NewBrandedDomain = NewBrandedDomain
instance (MonadDB m, MonadThrow m, MonadLog m) => DBUpdate m NewBrandedDomain BrandedDomainID where
  update (NewBrandedDomain) = do
    mbd <- dbQuery $ GetMainBrandedDomain
    newmailtheme <- (dbUpdate . UnsafeInsertNewThemeWithoutOwner) =<<
      (dbQuery $ GetTheme $ get bdMailTheme mbd)
    newsignviewtheme <- (dbUpdate . UnsafeInsertNewThemeWithoutOwner) =<<
      (dbQuery $ GetTheme $ get bdSignviewTheme mbd)
    newservicetheme <-  (dbUpdate . UnsafeInsertNewThemeWithoutOwner) =<<
      (dbQuery $ GetTheme $ get bdServiceTheme mbd)
    newlogintheme <-  (dbUpdate . UnsafeInsertNewThemeWithoutOwner) =<<
      (dbQuery $ GetTheme $ get bdLoginTheme mbd)
    runQuery_ . sqlInsert "branded_domains" $ do
      sqlSet "url" ("" :: String)
      sqlSet "main_domain" False -- One can not create new main domain
      sqlSet "sms_originator"      $ get bdSmsOriginator   mbd
      sqlSet "email_originator"    $ get bdEmailOriginator mbd
      sqlSet "mail_theme"          $ themeID           newmailtheme
      sqlSet "signview_theme"      $ themeID           newsignviewtheme
      sqlSet "service_theme"       $ themeID           newservicetheme
      sqlSet "login_theme"         $ themeID           newlogintheme
      sqlSet "browser_title"         ("Scrive":: String)
      sqlSet "favicon"             $ get bdFavicon           mbd
      sqlSet "participant_color_1" $ get bdParticipantColor1 mbd
      sqlSet "participant_color_2" $ get bdParticipantColor2 mbd
      sqlSet "participant_color_3" $ get bdParticipantColor3 mbd
      sqlSet "participant_color_4" $ get bdParticipantColor4 mbd
      sqlSet "participant_color_5" $ get bdParticipantColor5 mbd
      sqlSet "participant_color_6" $ get bdParticipantColor6 mbd
      sqlSet "draft_color"         $ get bdDraftColor        mbd
      sqlSet "cancelled_color"     $ get bdCancelledColor    mbd
      sqlSet "initiated_color"     $ get bdInitatedColor     mbd
      sqlSet "sent_color"          $ get bdSentColor         mbd
      sqlSet "delivered_color"     $ get bdDeliveredColor    mbd
      sqlSet "opened_color"        $ get bdOpenedColor       mbd
      sqlSet "reviewed_color"      $ get bdReviewedColor     mbd
      sqlSet "signed_color"        $ get bdSignedColor       mbd
      sqlResult "id"
    newdomainID <- fetchOne runIdentity
    dbUpdate $  MakeThemeOwnedByDomain newdomainID (themeID newmailtheme)
    dbUpdate $  MakeThemeOwnedByDomain newdomainID (themeID newsignviewtheme)
    dbUpdate $  MakeThemeOwnedByDomain newdomainID (themeID newservicetheme)
    dbUpdate $  MakeThemeOwnedByDomain newdomainID (themeID newlogintheme)
    return newdomainID

data SetMainDomainURL = SetMainDomainURL String
instance (MonadDB m, MonadThrow m) => DBUpdate m SetMainDomainURL () where
  update (SetMainDomainURL url) = do
    n <- runQuery . sqlUpdate "branded_domains" $ do
      sqlSet "url" url
      sqlWhere "main_domain"
    when (n /= 1) $ do
      unexpectedError "Main domain doesn't exist"
