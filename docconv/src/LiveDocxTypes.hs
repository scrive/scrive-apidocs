module LiveDocxTypes (
  FileFormat(..)
  , LiveDocxError(..)
) where

import Control.Exception

data FileFormat = DOC | DOCX | RTF | TXD
  deriving (Eq,Ord,Show,Read)

data LiveDocxError = LiveDocxIOError IOException
                     | LiveDocxSoapError String
  deriving (Show)
