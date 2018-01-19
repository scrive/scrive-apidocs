module Company.CompanyUI.Data (
    CompanyUI(..)
  ) where

import Data.Typeable
import qualified Data.ByteString.Char8 as BS

import Company.CompanyID
import Theme.ThemeID

data CompanyUI = CompanyUI
  { companyuicompanyid                :: !CompanyID
  , companyMailTheme                  :: !(Maybe ThemeID)
  , companySignviewTheme              :: !(Maybe ThemeID)
  , companyServiceTheme               :: !(Maybe ThemeID)
  , companyBrowserTitle               :: !(Maybe String)
  , companySmsOriginator              :: !(Maybe String)
  , companyFavicon                    :: !(Maybe BS.ByteString)
} deriving (Eq, Ord, Show, Typeable)
