module Flow.Process
  ( Process
  , fromProcess
  , unsafeProcess
  ) where

import Flow.Process.Internal

fromProcess :: Process -> Text
fromProcess (Process p) = p

unsafeProcess :: Text -> Process
unsafeProcess = Process
