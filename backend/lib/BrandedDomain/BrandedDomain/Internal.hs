{-# LANGUAGE TemplateHaskell #-}
module BrandedDomain.BrandedDomain.Internal where

import Optics.TH
import qualified Data.ByteString.Char8 as BS

import BrandedDomain.BrandedDomainID
import Theme.ThemeID

data BrandedDomain = BrandedDomain
  { id                :: BrandedDomainID
  , mainDomain        :: Bool
  , url               :: Text
  , smsOriginator     :: Text
  , emailOriginator   :: Text
  , mailTheme         :: ThemeID
  , signviewTheme     :: ThemeID
  , serviceTheme      :: ThemeID
  , loginTheme        :: ThemeID
  , browserTitle      :: Text
  , favicon           :: BS.ByteString
  , participantColor1 :: Text
  , participantColor2 :: Text
  , participantColor3 :: Text
  , participantColor4 :: Text
  , participantColor5 :: Text
  , participantColor6 :: Text
  , draftColor        :: Text
  , cancelledColor    :: Text
  , initatedColor     :: Text
  , sentColor         :: Text
  , deliveredColor    :: Text
  , openedColor       :: Text
  , reviewedColor     :: Text
  , signedColor       :: Text
  } deriving (Eq, Ord, Show)

makeFieldLabelsWith noPrefixFieldLabels ''BrandedDomain
