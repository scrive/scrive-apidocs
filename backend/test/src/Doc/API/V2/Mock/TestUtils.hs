-- Utility functions for working with document JSON for V2 API tests
--
-- NOTE:
--   MocDoc structure represents Document JSON for V2 API calls
--   This is why internals of MocDoc are NOT exported - because that are ~ JSON operations,
--   and our tests should be based on more Scrive related semantic.
--

module Doc.API.V2.Mock.TestUtils (
-- * Re-export
  MockDoc -- DON'T EXPOSE INTERNALS
, testRequestHelper
, jsonTestRequestHelper
-- * MockDoc Helper Functions
, mockDocTestRequestHelper
, mockDocFromValue
, mockDocToInput
, cleanMockDocForComparison
-- * MockDoc Accessor & Setter Functions
, getMockDocId
, getMockDocTitle
, getMockDocStatus
, getMockDocDaysToSign
, getMockDocHasFile
, getMockDocFileId
, getMockDocAuthorAttachmentLength
, getMockDocAuthorAttachmentName
, getMockDocAuthorAttachmentRequired
, getMockAuthorAttachmentAddedToSealedFile
, getMockDocAuthorAttachmentHasFile
, getMockDocAuthorAttachmentFileId
, getMockDocHasAutoRemindTime
, getMockDocIsTemplate
, getMockDocIsShared
, getMockDocIsTrashed
, getMockDocIsDeleted
, getMockDocViewerRole
, getMockDocSigLinkId
, getMockDocSigLinkHasSigned
, getMockDocSigLinkHasRejected
, getMockDocSigLinkAuthToViewMethod
, getMockDocSigLinkAuthToSignMethod
, setMockDocSigLinkAuthToSignMethod
, getMockDocSigLinkPersonalNumber
, getMockDocSigLinkMobileNumber
, getMockDocSigLinkAttachmentsLength
, getMockDocSigLinkAttachmentHasFile
, setMockDocSigLinkAttachments
) where

import Data.Aeson
import Data.Unjson
import Happstack.Server

import Context
import Doc.API.V2.AesonTestUtils (testRequestHelper, jsonTestRequestHelper)
import Doc.API.V2.Mock.MockDocInternal
import Doc.Data.DocumentStatus (DocumentStatus(..))
import Doc.Data.SignatoryLink (AuthenticationToViewMethod(..), AuthenticationToSignMethod(..))
import Doc.DocumentID (DocumentID)
import Doc.SignatoryLinkID (SignatoryLinkID)
import File.FileID
import Kontra (Kontra)
import KontraPrelude
import TestKontra

-- * MockDoc Helper Functions
-----------------------------

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

-- | Remove things which are not comparable between MockDoc so that we can
-- check for equality
cleanMockDocForComparison :: MockDoc -> MockDoc
cleanMockDocForComparison md = md { mockDocId = ""
                         , mockDocParties = map cleanMockSigLinkForComparison $ mockDocParties md
                         , mockDocCTime = ""
                         , mockDocMTime = ""
                         , mockDocTimeoutTime = Nothing
                         , mockDocStatus = ""
                         , mockDocObjectVersion = 1
                         , mockDocAccessToken = Nothing
                         , mockDocViewer = cleanMockViewerForComparison . mockDocViewer $ md
                         }
  where
    cleanMockSigLinkForComparison :: MockSigLink -> MockSigLink
    cleanMockSigLinkForComparison msl = msl { mockSigLinkId = "" }
    cleanMockViewerForComparison :: MockViewer -> MockViewer
    cleanMockViewerForComparison mdv = mdv { mockViewerSigId = Nothing }

-- * MockDoc Accessor & Setter Functions
----------------------------------------

getMockDocId :: MockDoc -> DocumentID
getMockDocId md = case (maybeRead $ mockDocId md) of
  Just did -> did
  Nothing -> $unexpectedError $
    "Could not read DocumentID from MockDoc:\n" ++ show md

getMockDocTitle :: MockDoc -> String
getMockDocTitle = mockDocTitle

getMockDocStatus :: MockDoc -> DocumentStatus
getMockDocStatus md = case mockDocStatus md of
  "preparation"    -> Preparation
  "pending"        -> Pending
  "closed"         -> Closed
  "canceled"       -> Canceled
  "timedout"       -> Timedout
  "rejected"       -> Rejected
  "document_error" -> DocumentError
  _ -> $unexpectedError $ "Could not parse MockDoc status to DocumentStatus: " ++ mockDocStatus md

getMockDocDaysToSign :: MockDoc -> Int
getMockDocDaysToSign = mockDocDaysToSign

getMockDocHasFile :: MockDoc -> Bool
getMockDocHasFile = isJust . mockDocFile

getMockDocFileId :: MockDoc -> FileID
getMockDocFileId md = case maybeRead (mockMainFileId mockFile) of
  Just fid -> fid
  Nothing -> $unexpectedError $
    "getMockDocFileId could not read FileID using maybeRead, MockDoc was:\n"
    ++ show md
  where mockFile = case mockDocFile md of
          Just f -> f
          Nothing -> $unexpectedError $ "No mockDocFile for MockDoc:\n" ++ show md

getMockDocAuthorAttachmentLength :: MockDoc -> Int
getMockDocAuthorAttachmentLength md = length . mockDocAuthorAttachments $ md

getMockDocAuthorAttachmentName :: Int -> MockDoc -> String
getMockDocAuthorAttachmentName i md = mockAuthorAttachmentName . mockDocAuthorAttachmentNumber i $ md

getMockDocAuthorAttachmentRequired :: Int -> MockDoc -> Bool
getMockDocAuthorAttachmentRequired i md = mockAuthorAttachmentRequired . mockDocAuthorAttachmentNumber i $ md

getMockAuthorAttachmentAddedToSealedFile :: Int -> MockDoc -> Bool
getMockAuthorAttachmentAddedToSealedFile i md = mockAuthorAttachmentAddedToSealedFile . mockDocAuthorAttachmentNumber i $ md

getMockDocAuthorAttachmentHasFile :: Int -> MockDoc -> Bool
getMockDocAuthorAttachmentHasFile i md = not . null . mockAuthorAttachmentFileId . mockDocAuthorAttachmentNumber i $ md

getMockDocAuthorAttachmentFileId :: Int -> MockDoc -> FileID
getMockDocAuthorAttachmentFileId i md = case maybeRead (mockAuthorAttachmentFileId maa) of
  Just fid -> fid
  Nothing -> $unexpectedError $
    "fileIDFromMockAuthorAttachment could not read FileID using maybeRead, MockAuthorAttachment was:\n"
    ++ show maa
  where maa = mockDocAuthorAttachmentNumber i md

getMockDocHasAutoRemindTime :: MockDoc -> Bool
getMockDocHasAutoRemindTime = isJust . mockDocAutoRemindTime

getMockDocIsTemplate :: MockDoc -> Bool
getMockDocIsTemplate = mockDocIsTemplate

getMockDocIsShared :: MockDoc -> Bool
getMockDocIsShared = mockDocIsShared

getMockDocIsTrashed :: MockDoc -> Bool
getMockDocIsTrashed = mockDocIsTrashed

getMockDocIsDeleted :: MockDoc -> Bool
getMockDocIsDeleted = mockDocIsDeleted

getMockDocViewerRole :: MockDoc -> String
getMockDocViewerRole = mockViewerRole . mockDocViewer

getMockDocSigLinkId :: Int -> MockDoc -> SignatoryLinkID
getMockDocSigLinkId i md = case (maybeRead $ mockSigLinkId $ getMockSigLinkNumber i md) of
  Just slid -> slid
  Nothing -> $unexpectedError $
    "Could not read SignatoryLinkID for signatory " ++ show i
    ++ "using `maybeRead` from MockDoc:\n" ++ show md

getMockDocSigLinkHasSigned :: Int -> MockDoc -> Bool
getMockDocSigLinkHasSigned i md = isJust . mockSigLinkSignTime . getMockSigLinkNumber i $ md

getMockDocSigLinkHasRejected :: Int -> MockDoc -> Bool
getMockDocSigLinkHasRejected i md = isJust . mockSigLinkRejectedTime . getMockSigLinkNumber i $ md

getMockDocSigLinkAuthToViewMethod :: Int -> MockDoc -> AuthenticationToViewMethod
getMockDocSigLinkAuthToViewMethod i md = case mockSigLinkAuthMethodToView . getMockSigLinkNumber i $ md of
  "standard"  -> StandardAuthenticationToView
  "se_bankid" -> SEBankIDAuthenticationToView
  "no_bankid" -> NOBankIDAuthenticationToView
  _ -> $unexpectedError $ "Could not parse AuthenticationToViewMethod from MockDoc:\n" ++ show md

getMockDocSigLinkAuthToSignMethod :: Int -> MockDoc -> AuthenticationToSignMethod
getMockDocSigLinkAuthToSignMethod i md = case mockSigLinkAuthMethodToSign . getMockSigLinkNumber i $ md of
  "standard"  -> StandardAuthenticationToSign
  "se_bankid" -> SEBankIDAuthenticationToSign
  "sms_pin"   -> SMSPinAuthenticationToSign
  _ -> $unexpectedError $ "Could not parse AuthenticationToSignMethod from MockDoc:\n" ++ show md

setMockDocSigLinkAuthToSignMethod :: Int -> AuthenticationToSignMethod -> MockDoc -> MockDoc
setMockDocSigLinkAuthToSignMethod i auth = setForSigNumberFromMockDoc i
  (\msl -> msl { mockSigLinkAuthMethodToSign = toStrAuth auth })
  where toStrAuth StandardAuthenticationToSign = "standard"
        toStrAuth SEBankIDAuthenticationToSign = "se_bankid"
        toStrAuth SMSPinAuthenticationToSign   = "sms_pin"

getMockDocSigLinkPersonalNumber :: Int -> MockDoc -> String
getMockDocSigLinkPersonalNumber i = getFieldValueOfTypeForSigNumberFromMockDoc i "personal_number"

getMockDocSigLinkMobileNumber :: Int -> MockDoc -> String
getMockDocSigLinkMobileNumber i = getFieldValueOfTypeForSigNumberFromMockDoc i "mobile"

getMockDocSigLinkAttachmentsLength :: Int -> MockDoc -> Int
getMockDocSigLinkAttachmentsLength i = length . mockSigLinkAttachments . getMockSigLinkNumber i

getMockDocSigLinkAttachmentHasFile :: Int -> Int -> MockDoc -> Bool
getMockDocSigLinkAttachmentHasFile si ai md
  | ai > length atts = $unexpectedError
    $ "getMockDocSigLinkAttachmentHasFile could not get index " ++ show ai ++ " from MockDoc:\n" ++ show md
  | otherwise = isJust . mockSigAttachmentFileId $ atts !! (ai-1)
  where atts = mockSigLinkAttachments . getMockSigLinkNumber si $ md

setMockDocSigLinkAttachments :: Int -> [(String, String)] -> MockDoc -> MockDoc
setMockDocSigLinkAttachments i namesdesc = setForSigNumberFromMockDoc i
  (\msl -> msl { mockSigLinkAttachments = for namesdesc
                  (\(n,d) -> MockSigAttachment { mockSigAttachmentName = n
                                               , mockSigAttachmentDescription = d
                                               , mockSigAttachmentFileId = Nothing
                                               , mockSigAttachmentFileName = Nothing
                                               }
                  )
               }
  )

-- * Internal use only!
-----------------------

-- | Internal use only
mockDocAuthorAttachmentNumber :: Int -> MockDoc -> MockAuthorAttachment
mockDocAuthorAttachmentNumber num mockdoc
  | num > length (mockDocAuthorAttachments mockdoc) = $unexpectedError
    $ "mockDocAuthorAttachment could not get index " ++ show num ++ " from MockDoc:\n" ++ show mockdoc
  | otherwise = (mockDocAuthorAttachments mockdoc) !! (num-1)

-- | Internal use only
getMockSigLinkNumber :: Int -> MockDoc -> MockSigLink
getMockSigLinkNumber num mockdoc
  | num > length (mockDocParties  mockdoc) = $unexpectedError $
      "getMockSigLinkIndex could not get index " ++ show num ++ " from MockDoc:\n" ++ show mockdoc
  | otherwise = (mockDocParties mockdoc) !! (num-1)

-- | Internal use only
getFieldValueOfTypeForSigNumberFromMockDoc :: Int -> String -> MockDoc -> String
getFieldValueOfTypeForSigNumberFromMockDoc i ft md =
  case getMockFieldsOfType ft . getMockSigLinkNumber i $ md of
    f:_ -> fromMaybe "" $ mockSigFieldValue f
    [] -> $unexpectedError $
      "Could not get field of type " ++ ft ++ " for signatory " ++ show i
      ++ " from MockDoc:\n" ++ show md
  where
    getMockFieldsOfType :: String -> MockSigLink -> [MockSigField]
    getMockFieldsOfType fts msl = filter (\f -> mockSigFieldType f == fts) (mockSigLinkFields msl)

-- | Internal use only
setForSigNumberFromMockDoc :: Int -> (MockSigLink -> MockSigLink) -> MockDoc -> MockDoc
setForSigNumberFromMockDoc i f md = md { mockDocParties = updatedSigLinks
                                       , mockDocObjectVersion = mockDocObjectVersion md + 1
                                       }
  where originalSL = getMockSigLinkNumber i md
        updatedSL = f originalSL
        updatedSigLinks = insert updatedSL . delete originalSL . sort . mockDocParties $ md
