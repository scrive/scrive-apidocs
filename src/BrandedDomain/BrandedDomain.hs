module BrandedDomain.BrandedDomain
  (   BrandedDomains
    , BrandedDomain(..)
  ) where

import BrandedDomain.BrandedDomainID
import DB
import qualified Data.ByteString.Char8 as BS

type BrandedDomains = [BrandedDomain]

data BrandedDomain = BrandedDomain {
                          bdid :: BrandedDomainID
                        , bdurl :: String
                        , bdlogo :: Maybe (Binary BS.ByteString)
                        , bdbarscolour :: String
                        , bdbarstextcolour :: String
                        , bdbarssecondarycolour :: String
                        , bdbackgroundcolour :: String
                        , bdbackgroundcolorexternal :: String
                        , bdmailsbackgroundcolor :: String
                        , bdmailsbuttoncolor :: String
                        , bdmailstextcolor :: String
                        , bdmailsbordercolor :: String
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
                        , bdnoreplyemail  :: String
                      } deriving (Eq, Ord, Show)
