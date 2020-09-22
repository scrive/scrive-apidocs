module Doc.Types.MainFile (
    MainFile(..)
  , mainFilesSelectors
  ) where

import Doc.SealStatus

import DB
import Doc.Tables
import Doc.Types.DocumentStatus
import File.FileID

-- FIXME: why it has DocumentStatus, if the
-- only values used are Preparation and Closed?
-- Separate enum should be used for this.
data MainFile = MainFile
  { mainfileid             :: FileID         -- ^ pointer to the actual file
  , mainfiledocumentstatus :: DocumentStatus -- ^ Preparation if and only if this is not a sealed file
  , mainfilesealstatus     :: SealStatus     -- ^ for files in Preparation: Missing.
  , mainfilename           :: String         -- ^ name of file. Copied from files table
  } deriving (Eq, Ord, Show)

---------------------------------

mainFilesSelectors :: [SQL]
mainFilesSelectors =
  [ "main_files.file_id"
  , "main_files.document_status"
  , "main_files.seal_status"
  , "files.name"
  ]

type instance CompositeRow MainFile = (FileID, DocumentStatus, SealStatus, String)

instance PQFormat MainFile where
  pqFormat = compositeTypePqFormat ctMainFile

instance CompositeFromSQL MainFile where
  toComposite (fid, document_status, seal_status, file_name) = MainFile
    { mainfileid             = fid
    , mainfiledocumentstatus = document_status
    , mainfilesealstatus     = seal_status
    , mainfilename           = file_name
    }
