module BrandedDomain.Model
  (   GetBrandedDomains(..)
    , GetBrandedDomainByURL(..)
    , GetBrandedDomainByUserID(..)
    , GetBrandedDomainByID(..)
    , UpdateBrandedDomain(..)
    , NewBrandedDomain(..)
    , InitBrandedDomainsFromConf(..)
  ) where

import BrandedDomain.BrandedDomainID
import BrandedDomain.BrandedDomain
import qualified BrandedDomain.BrandedDomain.Legacy as Legacy
import BrandedDomain.Tables
import DB
import qualified Log
import Data.Monoid
import User.UserID
import Control.Applicative



fetchBrandedDomain :: (BrandedDomainID, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String) -> BrandedDomain
fetchBrandedDomain (xid, url, logolink, barscolour, barstextcolour, barssecondarycolour, backgroundcolour, backgroundcolorexternal, mailsbackgroundcolor, mailsbuttoncolor, mailstextcolor, signviewprimarycolour, signviewprimarytextcolour, signviewsecondarycolour, signviewsecondarytextcolour, buttonclass, servicelinkcolour, externaltextcolour, headercolour, textcolour, pricecolour, smsoriginator, emailoriginator, contactemail)
       = BrandedDomain
         { bdid                          = xid
         , bdurl                         = url
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
      sqlOrderBy "id"
    fetchMany fetchBrandedDomain

data GetBrandedDomainByURL = GetBrandedDomainByURL String
instance (MonadDB m, Log.MonadLog m) => DBQuery m GetBrandedDomainByURL (Maybe BrandedDomain) where
  query (GetBrandedDomainByURL url) = do
    runQuery_ . sqlSelect "branded_domains" $ do
      mapM_ (sqlResult . raw . colName) (tblColumns tableBrandedDomains)
      sqlWhere ("" <?> url <> "ILIKE (branded_domains.url || '%')")
      sqlWhere "branded_domains.url <> ''"
      sqlOrderBy "branded_domains.id"
      sqlLimit 1
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

data UpdateBrandedDomain = UpdateBrandedDomain BrandedDomain
instance (MonadDB m, Log.MonadLog m) => DBUpdate m UpdateBrandedDomain () where
  update (UpdateBrandedDomain bd) = do
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
      sqlWhereEq "id" (bdid bd)

data NewBrandedDomain = NewBrandedDomain
instance (MonadDB m, Log.MonadLog m) => DBUpdate m NewBrandedDomain BrandedDomainID where
  update (NewBrandedDomain) = do
    runQuery_ . sqlInsert "branded_domains" $ do
      -- skip first column, that is branded_domains.id, it will default from sequence
      mapM_ (\col -> sqlSet (raw (colName col)) (""::String)) (tail $ tblColumns tableBrandedDomains)
      sqlResult "id"
    fetchOne unSingle



data InitBrandedDomainsFromConf = InitBrandedDomainsFromConf [Legacy.BrandedDomain]
instance (MonadDB m, Log.MonadLog m) => DBUpdate m InitBrandedDomainsFromConf Bool where
  update (InitBrandedDomainsFromConf []) = return False
  update (InitBrandedDomainsFromConf bds) = do
    -- Note that here we insert old configuration branded domains into
    -- new structures in the database. Some information might need to
    -- be invented from thin air or taken from somewhere else (like
    -- contents of files)
    runQuery_ $ ("SELECT EXISTS (SELECT * FROM branded_domains)" :: SQL)
    thereAreBrandedDomainsAlready <- fetchOne unSingle
    if not thereAreBrandedDomainsAlready
      then do
        runQuery_ . sqlInsert "branded_domains" $ do
          sqlSetList "url" $ Legacy.bdurl <$> bds
          sqlSetList "logolink" $ Legacy.bdlogolink <$> bds
          sqlSetList "bars_color" $ Legacy.bdbarscolour <$> bds
          sqlSetList "bars_text_color" $ Legacy.bdbarstextcolour <$> bds
          sqlSetList "bars_secondary_color" $ Legacy.bdbarssecondarycolour <$> bds
          sqlSetList "background_color" $ Legacy.bdbackgroundcolour <$> bds
          sqlSetList "background_color_external" $ Legacy.bdbackgroundcolorexternal <$> bds
          sqlSetList "mails_background_color" $ Legacy.bdmailsbackgroundcolor <$> bds
          sqlSetList "mails_button_color" $ Legacy.bdmailsbuttoncolor <$> bds
          sqlSetList "mails_text_color" $ Legacy.bdmailstextcolor <$> bds
          sqlSetList "signview_primary_color" $ Legacy.bdsignviewprimarycolour <$> bds
          sqlSetList "signview_primary_text_color" $ Legacy.bdsignviewprimarytextcolour <$> bds
          sqlSetList "signview_secondary_color" $ Legacy.bdsignviewsecondarycolour <$> bds
          sqlSetList "signview_secondary_text_color" $ Legacy.bdsignviewsecondarytextcolour <$> bds
          sqlSetList "button_class" $ Legacy.bdbuttonclass <$> bds
          sqlSetList "service_link_color" $ Legacy.bdservicelinkcolour <$> bds
          sqlSetList "external_text_color" $ Legacy.bdexternaltextcolour <$> bds
          sqlSetList "header_color" $ Legacy.bdheadercolour <$> bds
          sqlSetList "text_color" $ Legacy.bdtextcolour <$> bds
          sqlSetList "price_color" $ Legacy.bdpricecolour <$> bds
          sqlSetList "sms_originator" $ Legacy.bdsmsoriginator <$> bds
          sqlSetList "email_originator" $ Legacy.bdemailoriginator <$> bds
          sqlSetList "contact_email" $ Legacy.bdcontactemail <$> bds
        return True
      else do
        return False
