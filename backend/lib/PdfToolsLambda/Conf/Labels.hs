{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module PdfToolsLambda.Conf.Labels () where

import Optics.TH

import PdfToolsLambda.Conf.Internal

makeFieldLabelsWith noPrefixFieldLabels ''PdfToolsLambdaConf
makeFieldLabelsWith noPrefixFieldLabels ''PdfToolsLambdaEnv
