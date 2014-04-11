module BrandedDomain.Model
  (   GetBrandedDomains(..)
    , GetBrandedDomainByURL(..)
    , GetBrandedDomainByUserID(..)
    , GetBrandedDomainByID(..)
    , UpdateBrandedDomain(..)
    , NewBrandedDomain(..)
  ) where

import BrandedDomain.BrandedDomainID
import BrandedDomain.BrandedDomain
import BrandedDomain.Tables
import DB
import qualified Log
import Data.Monoid
import User.UserID



fetchBrandedDomain :: (BrandedDomainID, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String) -> BrandedDomain
fetchBrandedDomain (_id, url, logolink, barscolour, barstextcolour, barssecondarycolour, backgroundcolour, backgroundcolorexternal, mailsbackgroundcolor, mailsbuttoncolor, mailstextcolor, signviewprimarycolour, signviewprimarytextcolour, signviewsecondarycolour, signviewsecondarytextcolour, buttonclass, servicelinkcolour, externaltextcolour, headercolour, textcolour, pricecolour, smsoriginator, emailoriginator, contactemail)
       = BrandedDomain
         { bdurl                         = url
         , bdlogolink                    = logolink
         , bdbarscolour                  = barscolour
         , bdbarstextcolour              = barstextcolour
         , bdbarssecondarycolour         = barssecondarycolour
         , bdbackgroundcolour            = backgroundcolour
         , bdbackgroundcolorexternal     = backgroundcolorexternal
         , bdmailsbackgroundcolor        = mailsbackgroundcolor
         , bdmailsbuttoncolor            = mailsbuttoncolor
         , bdmailstextcolor              = mailstextcolor
         , bdsignviewprimarycolour       = signviewprimarycolour
         , bdsignviewprimarytextcolour   = signviewprimarytextcolour
         , bdsignviewsecondarycolour     = signviewsecondarycolour
         , bdsignviewsecondarytextcolour = signviewsecondarytextcolour
         , bdbuttonclass                 = buttonclass
         , bdservicelinkcolour           = servicelinkcolour
         , bdexternaltextcolour          = externaltextcolour
         , bdheadercolour                = headercolour
         , bdtextcolour                  = textcolour
         , bdpricecolour                 = pricecolour
         , bdsmsoriginator               = smsoriginator
         , bdemailoriginator             = emailoriginator
         , bdcontactemail                = contactemail
         }


data GetBrandedDomains = GetBrandedDomains
instance (MonadDB m, Log.MonadLog m) => DBQuery m GetBrandedDomains [BrandedDomain] where
  query (GetBrandedDomains) = do
    runQuery_ . sqlSelect "branded_domains" $ do
      mapM_ (sqlResult . raw . colName) (tblColumns tableBrandedDomains)
    fetchMany fetchBrandedDomain

data GetBrandedDomainByURL = GetBrandedDomainByURL String
instance (MonadDB m, Log.MonadLog m) => DBQuery m GetBrandedDomainByURL (Maybe BrandedDomain) where
  query (GetBrandedDomainByURL url) = do
    runQuery_ . sqlSelect "branded_domains" $ do
      mapM_ (sqlResult . raw . colName) (tblColumns tableBrandedDomains)
      sqlWhere ("" <?> url <> "ILIKE (branded_domains.url || '%')")
    fetchMaybe fetchBrandedDomain

data GetBrandedDomainByUserID = GetBrandedDomainByUserID UserID
instance (MonadDB m, Log.MonadLog m) => DBQuery m GetBrandedDomainByUserID (Maybe BrandedDomain) where
  query (GetBrandedDomainByUserID uid) = do
    runQuery_ . sqlSelect "branded_domains" $ do
      mapM_ (sqlResult . raw . colName) (tblColumns tableBrandedDomains)
      sqlWhereExists $ sqlSelect "users" $ do
        sqlWhereEq "users.id" uid
        sqlWhere "users.associated_domain = branded_domains.url"
    fetchMaybe fetchBrandedDomain

data GetBrandedDomainByID = GetBrandedDomainByID BrandedDomainID
instance (MonadDB m, Log.MonadLog m) => DBQuery m GetBrandedDomainByID (Maybe BrandedDomain) where
  query (GetBrandedDomainByID uid) = do
    runQuery_ . sqlSelect "branded_domains" $ do
      mapM_ (sqlResult . raw . colName) (tblColumns tableBrandedDomains)
      sqlWhereEq "id" uid
    fetchMaybe fetchBrandedDomain

data UpdateBrandedDomain = UpdateBrandedDomain BrandedDomainID BrandedDomain
instance (MonadDB m, Log.MonadLog m) => DBUpdate m UpdateBrandedDomain () where
  update (UpdateBrandedDomain uid bd) = do
    runQuery_ . sqlUpdate "branded_domains" $ do
      sqlSet "url" $ bdurl bd
      sqlSet "logolink" $ bdlogolink bd
      sqlSet "bars_color" $ bdbarscolour bd
      sqlSet "bars_text_color" $ bdbarstextcolour bd
      sqlSet "bars_secondary_color" $ bdbarssecondarycolour bd
      sqlSet "background_color" $ bdbackgroundcolour bd
      sqlSet "background_color_external" $ bdbackgroundcolorexternal bd
      sqlSet "mails_background_color" $ bdmailsbackgroundcolor bd
      sqlSet "mails_button_color" $ bdmailsbuttoncolor bd
      sqlSet "mails_text_color" $ bdmailstextcolor bd
      sqlSet "signview_primary_color" $ bdsignviewprimarycolour bd
      sqlSet "signview_primary_text_color" $ bdsignviewprimarytextcolour bd
      sqlSet "signview_secondary_color" $ bdsignviewsecondarycolour bd
      sqlSet "signview_secondary_text_color" $ bdsignviewsecondarytextcolour bd
      sqlSet "button_class" $ bdbuttonclass bd
      sqlSet "service_link_color" $ bdservicelinkcolour bd
      sqlSet "external_text_color" $ bdexternaltextcolour bd
      sqlSet "header_color" $ bdheadercolour bd
      sqlSet "text_color" $ bdtextcolour bd
      sqlSet "price_color" $ bdpricecolour bd
      sqlSet "sms_originator" $ bdsmsoriginator bd
      sqlSet "email_originator" $ bdemailoriginator bd
      sqlSet "contact_email" $ bdcontactemail bd
      sqlWhereEq "id" uid

data NewBrandedDomain = NewBrandedDomain
instance (MonadDB m, Log.MonadLog m) => DBUpdate m NewBrandedDomain BrandedDomainID where
  update (NewBrandedDomain) = do
    runQuery_ . sqlInsert "branded_domains" $ do
      -- skip first column, that is branded_domains.id, it will default from sequence
      mapM_ (\col -> sqlSet (raw (colName col)) (""::String)) (tail $ tblColumns tableBrandedDomains)
      sqlResult "id"
    fetchOne unSingle

