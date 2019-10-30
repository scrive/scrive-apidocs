{-# LANGUAGE TemplateHaskell #-}
module BrandedDomain.BrandedDomain.Internal where

import Optics.TH
import qualified Data.ByteString.Char8 as BS

import BrandedDomain.BrandedDomainID
import Theme.ThemeID

data BrandedDomain = BrandedDomain
  { bdid :: !BrandedDomainID
  , bdMainDomain :: !Bool
  , bdUrl :: !Text
  , bdSmsOriginator :: !Text
  , bdEmailOriginator :: !Text
  , bdMailTheme     :: !ThemeID
  , bdSignviewTheme :: !ThemeID
  , bdServiceTheme  :: !ThemeID
  , bdLoginTheme    :: !ThemeID
  , bdBrowserTitle  :: !Text
  , bdFavicon       :: !BS.ByteString
  , bdParticipantColor1 :: !Text
  , bdParticipantColor2 :: !Text
  , bdParticipantColor3 :: !Text
  , bdParticipantColor4 :: !Text
  , bdParticipantColor5 :: !Text
  , bdParticipantColor6 :: !Text
  , bdDraftColor        :: !Text
  , bdCancelledColor    :: !Text
  , bdInitatedColor     :: !Text
  , bdSentColor         :: !Text
  , bdDeliveredColor    :: !Text
  , bdOpenedColor       :: !Text
  , bdReviewedColor     :: !Text
  , bdSignedColor       :: !Text
  } deriving (Eq, Ord, Show)

makeFieldLabelsWith noPrefixFieldLabels ''BrandedDomain
