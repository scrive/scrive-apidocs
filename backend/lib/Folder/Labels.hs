{-# LANGUAGE TemplateHaskell #-}
module Folder.Labels where

import Data.Label

import Folder.Internal

mkLabel ''Folder
mkLabel ''FolderWithChildren

