module Doc.API.V2.MockTestUtils (
-- * Re-export
  jsonTestRequestHelper
-- * MockDoc Utils
, mockDocTestRequestHelper
, mockDocFromValue
, mockDocToInput
, mockDocToCompare
-- * Get and set other values
, mockDocAuthorAttachmentNumber
, documentIDFromMockDoc
, signatoryLinkIDFromMockDoc
, fileIDFromMockDocFile
, fileIDFromMockAuthorAttachment
, setForSigNumberFromMockDoc
, getForSigNumberFromMockDoc
, getFieldValueOfTypeForSigNumberFromMockDoc
, getFieldValueOfTypeForSigNumberFromMockDoc'
) where

import Data.Aeson
import Data.Unjson
import Happstack.Server

import Context
import Doc.API.V2.AesonTestUtils (jsonTestRequestHelper)
import Doc.API.V2.MockUnjson
import Doc.DocumentID (DocumentID)
import Doc.SignatoryLinkID (SignatoryLinkID)
import File.FileID
import Kontra (Kontra)
import KontraPrelude
import TestKontra

-- | For conveniently doing a test API call that returns Document JSON.
-- Uses `jsonTestRequestHelper` but parses the result using `mockDocFromValue`
mockDocTestRequestHelper :: Context -> Method -> [(String, Input)]
                         -> Kontra Response -> Int -> TestEnv MockDoc
mockDocTestRequestHelper c m p f i = mockDocFromValue <$> jsonTestRequestHelper c m p f i

mockDocFromValue :: Value -> MockDoc
mockDocFromValue v = case parse mockDocUnjson v of
  (Result md []) -> md
  (Result _ e) -> $unexpectedError $
    "Could not parse MockDoc from JSON: " ++ (concat $ map show e)

mockDocToInput :: MockDoc -> Input
mockDocToInput md = inTextBS $ unjsonToByteStringLazy mockDocUnjson md

-- | Remove things which are not comparable between Documents so that we can
-- check for equality between two `MockDoc`
--
-- NOTE: Only checked for current usage, you may find more things need to be
--       added here in some cases
mockDocToCompare :: MockDoc -> MockDoc
mockDocToCompare md = md { mockDocId = ""
                         , mockDocParties = map mockSigLinkToCompare $ mockDocParties md
                         , mockDocCTime = ""
                         , mockDocMTime = ""
                         , mockDocTimeoutTime = Nothing
                         , mockDocStatus = ""
                         , mockDocObjectVersion = 1
                         , mockDocAccessToken = Nothing
                         , mockDocViewer = mockDocViewerToCompare . mockDocViewer $ md
                         }
  where
    mockSigLinkToCompare :: MockSigLink -> MockSigLink
    mockSigLinkToCompare msl = msl { mockSigLinkId = "" }
    mockDocViewerToCompare :: MockDocViewer -> MockDocViewer
    mockDocViewerToCompare mdv = mdv { mockDocViewerSigId = Nothing }

mockDocAuthorAttachmentNumber :: Int -> MockDoc -> MockAuthorAttachment
mockDocAuthorAttachmentNumber i md
  | i > length (mockDocAuthorAttachments md) = $unexpectedError
    $ "mockDocAuthorAttachment could not get index " ++ show i ++ " from MockDoc:\n" ++ show md
  | otherwise = (mockDocAuthorAttachments md) !! (i-1)

documentIDFromMockDoc :: MockDoc -> DocumentID
documentIDFromMockDoc md = case (maybeRead $ mockDocId md) of
  Just did -> did
  Nothing -> $unexpectedError $
    "Could not read DocumentID from MockDoc:\n" ++ show md

signatoryLinkIDFromMockDoc :: Int -> MockDoc -> SignatoryLinkID
signatoryLinkIDFromMockDoc i md = case (maybeRead $ mockSigLinkId $ getMockSigLinkNumber md i) of
  Just slid -> slid
  Nothing -> $unexpectedError $
    "Could not read SignatoryLinkID for signatory " ++ show i
    ++ "using `maybeRead` from MockDoc:\n" ++ show md

fileIDFromMockDocFile :: MockDoc -> FileID
fileIDFromMockDocFile md = case maybeRead (mockMainFileId mockFile) of
  Just fid -> fid
  Nothing -> $unexpectedError $
    "fileIDFromMockDocFile could not read FileID using maybeRead, MockDoc was:\n"
    ++ show md
  where mockFile = case mockDocFile md of
          Just f -> f
          Nothing -> $unexpectedError $ "No mockDocFile for MockDoc:\n" ++ show md

fileIDFromMockAuthorAttachment :: MockAuthorAttachment -> FileID
fileIDFromMockAuthorAttachment maa = case maybeRead (mockAuthorAttachmentFileId maa) of
  Just fid -> fid
  Nothing -> $unexpectedError $
    "fileIDFromMockAuthorAttachment could not read FileID using maybeRead, MockAuthorAttachment was:\n"
    ++ show maa

setForSigNumberFromMockDoc :: (MockSigLink -> MockSigLink) -> MockDoc -> Int -> MockDoc
setForSigNumberFromMockDoc f md i = md { mockDocParties = updatedSigLinks
                                       , mockDocObjectVersion = mockDocObjectVersion md + 1
                                       }
  where originalSL = getMockSigLinkNumber md i
        updatedSL = f originalSL
        updatedSigLinks = insert updatedSL . delete originalSL . sort . mockDocParties $ md

getForSigNumberFromMockDoc :: (MockSigLink -> a) -> MockDoc -> Int -> a
getForSigNumberFromMockDoc f md i = f $ getMockSigLinkNumber md i

getFieldValueOfTypeForSigNumberFromMockDoc :: MockDoc -> Int -> String -> Maybe String
getFieldValueOfTypeForSigNumberFromMockDoc md i ft =
  case getMockFieldsOfType ft . flip getMockSigLinkNumber i $ md of
    f:_ -> mockSigFieldValue f
    [] -> $unexpectedError $
      "Could not get field of type " ++ ft ++ " for signatory " ++ show i
      ++ " from MockDoc:\n" ++ show md
  where
    getMockFieldsOfType :: String -> MockSigLink -> [MockSigField]
    getMockFieldsOfType fts msl = filter (\f -> mockSigFieldType f == fts) (mockSigLinkFields msl)

getFieldValueOfTypeForSigNumberFromMockDoc' :: MockDoc -> Int -> String -> String
getFieldValueOfTypeForSigNumberFromMockDoc' md i ft =
  case getFieldValueOfTypeForSigNumberFromMockDoc md i ft of
    Just s -> s
    Nothing -> $unexpectedError $
      "Field of type " ++ ft ++ " for signatory " ++ show i
      ++ " was empty! From MockDoc:\n" ++ show md

-- * Internal Functions

getMockSigLinkNumber :: MockDoc -> Int -> MockSigLink
getMockSigLinkNumber md i
  | i > length (mockDocParties  md) = $unexpectedError $
      "getMockSigLinkIndex could not get index " ++ show i ++ " from MockDoc:\n" ++ show md
  | otherwise = (mockDocParties  md) !! (i-1)
