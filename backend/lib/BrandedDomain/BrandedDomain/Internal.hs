module BrandedDomain.BrandedDomain.Internal where

import qualified Data.ByteString.Char8 as BS

import BrandedDomain.BrandedDomainID
import Theme.ThemeID

data BrandedDomain = BrandedDomain
  { _bdid :: !BrandedDomainID
  , _bdMainDomain :: !Bool
  , _bdUrl :: !Text
  , _bdSmsOriginator :: !Text
  , _bdEmailOriginator :: !Text
  , _bdMailTheme     :: !ThemeID
  , _bdSignviewTheme :: !ThemeID
  , _bdServiceTheme  :: !ThemeID
  , _bdLoginTheme    :: !ThemeID
  , _bdBrowserTitle  :: !Text
  , _bdFavicon       :: !BS.ByteString
  , _bdParticipantColor1 :: !Text
  , _bdParticipantColor2 :: !Text
  , _bdParticipantColor3 :: !Text
  , _bdParticipantColor4 :: !Text
  , _bdParticipantColor5 :: !Text
  , _bdParticipantColor6 :: !Text
  , _bdDraftColor        :: !Text
  , _bdCancelledColor    :: !Text
  , _bdInitatedColor     :: !Text
  , _bdSentColor         :: !Text
  , _bdDeliveredColor    :: !Text
  , _bdOpenedColor       :: !Text
  , _bdReviewedColor     :: !Text
  , _bdSignedColor       :: !Text
  } deriving (Eq, Ord, Show)
