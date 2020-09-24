module Doc.Types.DocumentFile (
    DocumentFile(..)
  , documentFileWithIDQuery
  , DocumentFileID
  , DocumentFileWithID(..)
  , DigitallySignedFile(..)
  , unsafeDocumentFileID
  ) where

import Data.Int

import DB
import Doc.DigitalSignatureStatus
import Doc.Tables
import Doc.Types.DocumentStatus
import File.Tables
import File.Types

-- | The DocumentFile IDs are just the postgres ids in "main_files"; we are
-- using them for versioning, exploiting the fact that they are monotonically
-- increasing.
newtype DocumentFileID = DocumentFileID Int64
  deriving (Ord, Eq, Show)

unsafeDocumentFileID :: Int64 -> DocumentFileID
unsafeDocumentFileID = DocumentFileID

instance PQFormat DocumentFileID where
  pqFormat = pqFormat @Int64

instance FromSQL DocumentFileID where
  type PQBase DocumentFileID = PQBase Int64
  fromSQL mbase = DocumentFileID <$> fromSQL mbase

data DigitallySignedFile = DigitallySignedFile
  { digitallySignedFile :: File
  , digitalSignatureStatus :: DigitalSignatureStatus
  } deriving (Show, Eq)

data DocumentFile
  = InputFile
      { mainfile :: File
      }
  | ClosedFile
      { mainfileWithEvidence :: DigitallySignedFile
      -- ^ Upon closing we attach the evidence log, verification page etc.
      }
  | PendingVerimiQesFile
      { mainfileWithSomeQesSignatures :: File
      -- ^ Each signatory adds their QES signature to the pdf in turn.
      }
  | ClosedVerimiQesFile
      { mainfileWithQesSignatures :: File
      -- ^ The pdf is QES-signed by each signatory.
      , evidenceFile :: DigitallySignedFile
      -- ^ Since we can't modify the pdf file after it has been QES signed, we
      -- compile the evidence package into a separate PDF file.
      }
  deriving (Show, Eq)

data DocumentFileWithID = DocumentFileWithID DocumentFileID DocumentFile


{- SQL Deserialisation -}

documentFileWithIDQuery :: SQL -> SQL
documentFileWithIDQuery document_id =
  toSQLCommand . sqlSelect "main_files as document_files" $ do
    sqlWhere $ "document_files.document_id = " <> document_id
    sqlJoinOn "files main_file" "main_file.id = document_files.file_id"
    sqlLeftJoinOn "files evidence_file"
                  "evidence_file.id = document_files.evidence_file_id"

    let main_file_selectors = map ("main_file." <>) filesSelectors
        main_file_result =
          parenthesize (mintercalate ", " main_file_selectors) <> "::" <> raw
            (ctName ctFile)

    let evidence_file_selectors = map ("evidence_file." <>) filesSelectors
        evidence_file_result =
          parenthesize (mintercalate ", " evidence_file_selectors) <> "::" <> raw
            (ctName ctFile)

    let result_selectors =
          [ "document_files.id"
          , "document_files.document_status"
          , "document_files.seal_status"
          , main_file_result
          -- If `evidence_file_id` is null, then `evidence_file_result` results
          -- in '(,,,,,,)'::file_c1 (i.e. all of the columns are `null`), but we
          -- need it to be `null` in that case; hence the use of `NULLIF`, which
          -- returns `NULL` if its arguments match.
          , "NULLIF" <> parenthesize
            (evidence_file_result <> "," <> "'(,,,,,,)'::" <> raw (ctName ctFile))
          ]
    sqlResult $ parenthesize (mintercalate ", " result_selectors) <> "::" <> raw
      (ctName ctMainFile)


type instance CompositeRow DocumentFileWithID
  = ( DocumentFileID
    , DocumentStatus
    , DigitalSignatureStatus
    , Composite File
    , Maybe (Composite File)
    )

instance PQFormat DocumentFileWithID where
  pqFormat = compositeTypePqFormat ctMainFile

instance CompositeFromSQL DocumentFileWithID where
  toComposite (id, document_status, signature_status, Composite main_file, mevidence_file)
    = DocumentFileWithID id document_file
    where
      document_file = case document_status of
        Preparation -> InputFile { mainfile = main_file }

        Closed | Nothing <- mevidence_file -> ClosedFile
          { mainfileWithEvidence = DigitallySignedFile
                                     { digitallySignedFile    = main_file
                                     , digitalSignatureStatus = signature_status
                                     }
          }

        Pending -> PendingVerimiQesFile { mainfileWithSomeQesSignatures = main_file }

        Closed | Just (Composite evidence_file) <- mevidence_file -> ClosedVerimiQesFile
          { mainfileWithQesSignatures = main_file
          , evidenceFile = DigitallySignedFile { digitallySignedFile    = evidence_file
                                               , digitalSignatureStatus = signature_status
                                               }
          }

        _ -> unexpectedError $ "Failed to deserialise DocumentFileWithID: " <> showt
          (id, document_status, signature_status, main_file)
