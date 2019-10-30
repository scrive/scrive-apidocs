{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module UserGroup.Labels () where

import Optics.TH

import UserGroup.Internal

makeFieldLabelsWith noPrefixFieldLabels ''UserGroup
makeFieldLabelsWith noPrefixFieldLabels ''UserGroupSettings
makeFieldLabelsWith noPrefixFieldLabels ''UserGroupUI
makeFieldLabelsWith noPrefixFieldLabels ''UserGroupAddress
makeFieldLabelsWith noPrefixFieldLabels ''UserGroupWithChildren
