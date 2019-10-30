{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Folder.Labels () where

import Optics.TH

import Folder.Internal

makeFieldLabelsWith noPrefixFieldLabels ''Folder
makeFieldLabelsWith noPrefixFieldLabels ''FolderWithChildren
