module BrandedDomain.BrandedDomain.Internal where

import qualified Data.ByteString.Char8 as BS

import BrandedDomain.BrandedDomainID
import KontraPrelude
import Theme.ThemeID

data BrandedDomain = BrandedDomain {
                          _bdid :: !BrandedDomainID
                        , _bdMainDomain :: !Bool
                        , _bdUrl :: !String
                        , _bdSmsOriginator :: !String
                        , _bdEmailOriginator :: !String
                        , _bdMailTheme     :: !ThemeID
                        , _bdSignviewTheme :: !ThemeID
                        , _bdServiceTheme  :: !ThemeID
                        , _bdLoginTheme    :: !ThemeID
                        , _bdBrowserTitle  :: !String
                        , _bdFavicon       :: !BS.ByteString
                        , _bdParticipantColor1 :: !String
                        , _bdParticipantColor2 :: !String
                        , _bdParticipantColor3 :: !String
                        , _bdParticipantColor4 :: !String
                        , _bdParticipantColor5 :: !String
                        , _bdParticipantColor6 :: !String
                        , _bdDraftColor        :: !String
                        , _bdCancelledColor    :: !String
                        , _bdInitatedColor     :: !String
                        , _bdSentColor         :: !String
                        , _bdDeliveredColor    :: !String
                        , _bdOpenedColor       :: !String
                        , _bdReviewedColor     :: !String
                        , _bdSignedColor       :: !String
} deriving (Eq, Ord, Show)
