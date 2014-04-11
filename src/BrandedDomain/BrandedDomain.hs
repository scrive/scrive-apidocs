module BrandedDomain.BrandedDomain
  (   BrandedDomains
    , BrandedDomain(..)
  ) where

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
