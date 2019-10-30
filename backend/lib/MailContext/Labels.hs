{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module MailContext.Labels () where

import Optics.TH

import MailContext.Internal

makeFieldLabelsWith noPrefixFieldLabels ''MailContext
