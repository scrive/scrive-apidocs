module OpenDocument (
    openDocument
  ) where

import Control.Monad
import System.Exit
import System.Process
import qualified Control.Exception as E

openDocument :: String -> IO Bool
openDocument filename = foldM run False [
    openDocumentMac
  , openDocumentGnome
  , openDocumentKDE
  , openDocumentWindows
  ]
  where
    openDocumentMac = rawSystem "open" [filename]
    openDocumentGnome = rawSystem "gnome-open" [filename]
    openDocumentKDE = rawSystem "kde-open" [filename]
    openDocumentWindows = rawSystem "cmd" ["/c", filename]

    run True _ = return True
    run _ action = do
      res <- E.try action :: IO (Either E.IOException ExitCode)
      case res of
        Right ExitSuccess -> return True
        _                 -> return False
