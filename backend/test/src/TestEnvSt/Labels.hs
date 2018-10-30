{-# LANGUAGE TemplateHaskell #-}
module TestEnvSt.Labels where

import Data.Label (mkLabel)

import TestEnvSt.Internal

mkLabel ''TestEnvSt

mkLabel ''TestEnvStRW
