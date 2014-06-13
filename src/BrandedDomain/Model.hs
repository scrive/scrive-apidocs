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
import DB
import qualified Log
import Data.Monoid
import User.UserID
import qualified Data.ByteString.Char8 as BS



fetchBrandedDomain :: (BrandedDomainID, String, Maybe (Binary BS.ByteString), String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String) -> BrandedDomain
fetchBrandedDomain (xid, url, logo, barscolour, barstextcolour, barssecondarycolour, backgroundcolour, backgroundcolorexternal, mailsbackgroundcolor, mailsbuttoncolor, mailstextcolor, signviewprimarycolour, signviewprimarytextcolour, signviewsecondarycolour, signviewsecondarytextcolour, buttonclass, servicelinkcolour, externaltextcolour, headercolour, textcolour, pricecolour, smsoriginator, emailoriginator, contactemail)
       = BrandedDomain
         { bdid                          = xid
         , bdurl                         = url
         , bdlogo                        = logo
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

brandedDomainSelector :: [SQL]
brandedDomainSelector = [
    "id"
  , "url"
  , "logo"
  , "bars_color"
  , "bars_text_color"
  , "bars_secondary_color"
  , "background_color"
  , "background_color_external"
  , "mails_background_color"
  , "mails_button_color"
  , "mails_text_color"
  , "signview_primary_color"
  , "signview_primary_text_color"
  , "signview_secondary_color"
  , "signview_secondary_text_color"
  , "button_class"
  , "service_link_color"
  , "external_text_color"
  , "header_color"
  , "text_color"
  , "price_color"
  , "sms_originator"
  , "email_originator"
  , "contact_email"
  ]


data GetBrandedDomains = GetBrandedDomains
instance (MonadDB m, Log.MonadLog m) => DBQuery m GetBrandedDomains [BrandedDomain] where
  query (GetBrandedDomains) = do
    runQuery_ . sqlSelect "branded_domains" $ do
      mapM_ sqlResult $ brandedDomainSelector
      sqlOrderBy "id"
    fetchMany fetchBrandedDomain

data GetBrandedDomainByURL = GetBrandedDomainByURL String
instance (MonadDB m, Log.MonadLog m) => DBQuery m GetBrandedDomainByURL (Maybe BrandedDomain) where
  query (GetBrandedDomainByURL url) = do
    runQuery_ . sqlSelect "branded_domains" $ do
      mapM_ sqlResult $ brandedDomainSelector
      sqlWhere ("" <?> url <> "ILIKE (branded_domains.url || '%')")
      sqlWhere "branded_domains.url <> ''"
      sqlOrderBy "branded_domains.id"
      sqlLimit 1
    fetchMaybe fetchBrandedDomain

data GetBrandedDomainByUserID = GetBrandedDomainByUserID UserID
instance (MonadDB m, Log.MonadLog m) => DBQuery m GetBrandedDomainByUserID (Maybe BrandedDomain) where
  query (GetBrandedDomainByUserID uid) = do
    runQuery_ . sqlSelect "branded_domains" $ do
      mapM_ sqlResult $ brandedDomainSelector
      sqlWhereExists $ sqlSelect "users" $ do
        sqlWhereEq "users.id" uid
        sqlWhere "users.associated_domain_id = branded_domains.id"
    fetchMaybe fetchBrandedDomain

data GetBrandedDomainByID = GetBrandedDomainByID BrandedDomainID
instance (MonadDB m, Log.MonadLog m) => DBQuery m GetBrandedDomainByID (Maybe BrandedDomain) where
  query (GetBrandedDomainByID uid) = do
    runQuery_ . sqlSelect "branded_domains" $ do
      mapM_ sqlResult $ brandedDomainSelector
      sqlWhereEq "id" uid
    fetchMaybe fetchBrandedDomain

data UpdateBrandedDomain = UpdateBrandedDomain BrandedDomain
instance (MonadDB m, Log.MonadLog m) => DBUpdate m UpdateBrandedDomain () where
  update (UpdateBrandedDomain bd) = do
    runQuery_ . sqlUpdate "branded_domains" $ do
      sqlSet "url" $ bdurl bd
      sqlSet "logo" $ bdlogo bd
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
      sqlWhereEq "id" (bdid bd)

data NewBrandedDomain = NewBrandedDomain
instance (MonadDB m, Log.MonadLog m) => DBUpdate m NewBrandedDomain BrandedDomainID where
  update (NewBrandedDomain) = do
    runQuery_ . sqlInsert "branded_domains" $ do
      -- skip all columns, they should get a default value
      sqlResult "id"
    fetchOne unSingle
