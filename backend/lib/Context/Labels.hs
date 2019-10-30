{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Context.Labels () where

import Optics.TH

import Context.Internal

makeFieldLabelsWith noPrefixFieldLabels ''Context
