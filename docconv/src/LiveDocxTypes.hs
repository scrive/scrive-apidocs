module LiveDocxTypes (
  FileFormat(..)
  , LiveDocxError(..)
) where

import Control.Exception

import KontraPrelude

data FileFormat = DOC | DOCX | RTF | TXD
  deriving (Eq,Ord,Show,Read)

data LiveDocxError = LiveDocxIOError IOException
                     | LiveDocxSoapError String
  deriving (Show)
