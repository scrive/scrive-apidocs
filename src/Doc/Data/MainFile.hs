module Doc.Data.MainFile (
    MainFile(..)
  , mainFilesSelectors
  ) where

import DB
import Doc.Data.DocumentStatus
import Doc.SealStatus
import File.FileID

-- FIXME: why it has DocumentStatus, if the
-- only values used are Preparation and Closed?
-- Separate enum should be used for this.
data MainFile = MainFile {
  mainfileid             :: !FileID         -- ^ pointer to the actual file
, mainfiledocumentstatus :: !DocumentStatus -- ^ Preparation if and only if this is not a sealed file
, mainfilesealstatus     :: !SealStatus     -- ^ for files in Preparation: Missing.
} deriving (Eq, Ord, Show)

---------------------------------

mainFilesSelectors :: [SQL]
mainFilesSelectors = [
    "main_files.file_id"
  , "main_files.document_status"
  , "main_files.seal_status"
  ]

type instance CompositeRow MainFile = (FileID, DocumentStatus, SealStatus)

instance PQFormat MainFile where
  pqFormat _ = "%main_file"

instance CompositeFromSQL MainFile where
  toComposite (fid, document_status, seal_status) = MainFile {
    mainfileid = fid
  , mainfiledocumentstatus = document_status
  , mainfilesealstatus = seal_status
  }
