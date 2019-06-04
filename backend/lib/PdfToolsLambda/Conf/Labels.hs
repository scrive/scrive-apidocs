{-# LANGUAGE TemplateHaskell #-}
module PdfToolsLambda.Conf.Labels where

import Data.Label (mkLabel)

import PdfToolsLambda.Conf.Internal

mkLabel ''PdfToolsLambdaConf
mkLabel ''PdfToolsLambdaEnv
