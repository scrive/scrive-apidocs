{-# LANGUAGE NoImplicitPrelude #-}

-- | A custom Prelude for use in scripts that allows us to avoid CPP.
module ScriptsPrelude (
  module Control.Applicative,
  module Prelude
) where

import Control.Applicative
import Prelude

