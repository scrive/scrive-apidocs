module BrandedDomain.BrandedDomain.Legacy
  ( BrandedDomain(..)
  ) where

--
-- This module is temporary, so that we can import old configuration
-- files into the database functionality should be removed when we
-- decide to kill brandedDomains from kontrakcja.conf
--
-- Please do not add new fields here. Use the structure in
-- BrandedDomain.BrandedDomain.

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
