module BrandedDomains
  (   BrandedDomains
    , BrandedDomain(..)
    , findBrandedDomain
  ) where

import Data.List

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
                        , bdbuttonclass    :: String
                        , bdservicelinkcolour :: String
                        , bdheadercolour :: String
                        , bdtextcolour :: String
                        , bdpricecolour :: String
                      } deriving (Read, Eq, Ord, Show)

findBrandedDomain :: String -> BrandedDomains -> Maybe BrandedDomain
findBrandedDomain s = find (\d -> bdurl d `isPrefixOf` s)