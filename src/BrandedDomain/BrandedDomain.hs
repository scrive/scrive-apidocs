module BrandedDomain.BrandedDomain
  (   BrandedDomains
    , BrandedDomain(..)
    , GetBrandedDomains(..)
    , GetBrandedDomainByURL(..)
    , GetBrandedDomainByUserID(..)
  ) where

import BrandedDomain.BrandedDomainID
import BrandedDomain.Tables
import DB
import qualified Log
import Data.Monoid
import User.UserID

type BrandedDomains = [BrandedDomain]

data BrandedDomain = BrandedDomain {
                          bdurl :: String
                        , bdlogolink :: String
                        , bdbarscolour :: String
                        , bdbarstextcolour :: String
                        , bdbarssecondarycolour :: String
                        , bdbackgroundcolour :: String
                        , bdbackgroundcolorexternal :: String
                        , bdmailsbackgroundcolor :: String
                        , bdmailsbuttoncolor :: String
                        , bdmailstextcolor :: String
                        , bdsignviewprimarycolour :: String
                        , bdsignviewprimarytextcolour :: String
                        , bdsignviewsecondarycolour :: String
                        , bdsignviewsecondarytextcolour :: String
                        , bdbuttonclass    :: String
                        , bdservicelinkcolour :: String
                        , bdexternaltextcolour :: String
                        , bdheadercolour :: String
                        , bdtextcolour :: String
                        , bdpricecolour :: String
                        , bdsmsoriginator :: String
                        , bdemailoriginator :: String
                        , bdcontactemail :: String
                      } deriving (Read, Eq, Ord, Show)


fetchBrandedDomain :: (BrandedDomainID, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String) -> BrandedDomain
fetchBrandedDomain (_id, url, logolink, barscolour, barstextcolour, barssecondarycolour, backgroundcolour, backgroundcolorexternal, mailsbackgroundcolor, mailsbuttoncolor, mailstextcolor, signviewprimarycolour, signviewprimarytextcolour, signviewsecondarycolour, signviewsecondarytextcolour, buttonclass, servicelinkcolour, headercolour, textcolour, pricecolour, smsoriginator, emailoriginator, contactemail)
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
