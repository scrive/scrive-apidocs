{-# LANGUAGE TemplateHaskell #-}
module Flow.Server.DocFiles
  ( docFiles
  ) where

import Data.ByteString
import Data.FileEmbed

docFiles :: [(FilePath, ByteString)]
docFiles = $(embedDir "backend/flow/docs")
