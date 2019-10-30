{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module TestEnvSt.Labels () where

import Optics.TH

import TestEnvSt.Internal

makeFieldLabelsWith noPrefixFieldLabels ''TestEnvSt
makeFieldLabelsWith noPrefixFieldLabels ''TestEnvStRW
