module BrandedDomain.BrandedDomain
  (   BrandedDomainID
    , BrandedDomain(..)
  ) where

import qualified Data.ByteString.Char8 as BS

import BrandedDomain.BrandedDomainID
import DB
import Theme.ThemeID

data BrandedDomain = BrandedDomain {
                          bdid :: !BrandedDomainID
                        , bdMainDomain :: !Bool
                        , bdUrl :: !String
                        , bdSmsOriginator :: !String
                        , bdEmailOriginator :: !String
                        , bdContactEmail :: !String
                        , bdNoreplyEmail  :: !String
                        , bdMailTheme     :: !ThemeID
                        , bdSignviewTheme :: !ThemeID
                        , bdServiceTheme  :: !ThemeID
                        , bdLoginTheme    :: !ThemeID
                        , bdBrowserTitle  :: !String
                        , bdFavicon       :: !(Binary BS.ByteString)
                        , bdParticipantColor1 :: !String
                        , bdParticipantColor2 :: !String
                        , bdParticipantColor3 :: !String
                        , bdParticipantColor4 :: !String
                        , bdParticipantColor5 :: !String
                        , bdParticipantColor6 :: !String
                        , bdDraftColor        :: !String
                        , bdCancelledColor    :: !String
                        , bdInitatedColor     :: !String
                        , bdSentColor         :: !String
                        , bdDeliveredColor    :: !String
                        , bdOpenedColor       :: !String
                        , bdReviewedColor     :: !String
                        , bdSignedColor       :: !String
} deriving (Eq, Ord, Show)

