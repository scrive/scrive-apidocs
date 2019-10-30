{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module BrandedDomain.BrandedDomain.Labels () where

import Optics.TH

import BrandedDomain.BrandedDomain.Internal

makeFieldLabelsWith noPrefixFieldLabels ''BrandedDomain
