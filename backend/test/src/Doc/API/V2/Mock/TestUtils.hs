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
, MockSigLink
, defaultMockSigLink
, testRequestHelper
, jsonTestRequestHelper
-- * MockDoc Helper Functions
, mockDocTestRequestHelper
, mockDocTestRequestHelperMultiple
, mockDocFromValue
, mockDocToInput
, mockDocIsShared
, cleanMockDocForComparison
-- * MockDoc Accessor & Setter Functions
, getMockDocId
, getMockDocTitle
, getMockDocStatus
, getMockDocDaysToSign
, getMockDocFolderId
, getMockDocHasFile
, getMockDocFileId
, getMockDocFileName
, getMockDocAuthorAttachmentLength
, getMockDocAuthorAttachmentName
, getMockDocAuthorAttachmentRequired
, getMockAuthorAttachmentAddedToSealedFile
, getMockDocAuthorAttachmentHasFile
, getMockDocAuthorAttachmentFileId
, getMockDocHasAutoRemindTime
, getMockDocAccessToken
, getMockDocIsTemplate
, getMockDocIsShared
, getMockDocIsTrashed
, getMockDocIsDeleted
, getMockDocViewerRole
, getMockDocSigLinkId
, getMockDocSigLinkHasSigned
, getMockDocSigLinkHasRejected
, getMockDocSigLinkAuthToViewMethod
, getMockDocSigLinkAuthToViewArchivedMethod
, getMockDocSigLinkAuthToSignMethod
, setMockDocSigLinkAuthToSignMethod
, getMockDocSigLinkPersonalNumber
, getMockDocSigLinkEmail
, getMockDocSigLinkMobileNumber
, getMockDocSigLinkAttachmentsLength
, getMockDocSigLinkAttachmentHasFile
, setMockDocSigLinkConfirmationDeliveryMethod
, setMockDocSigLinkDeliveryMethod
, setMockDocSigLinkAttachments
, setMockDocSigLinkSignatoryRole
, setMockSigLinkStandardField
, setMockDocSigLinkStandardField
, getMockDocUserSigLinkId
, addStandardSigLinksToMockDoc
, addSigLinksToMockDoc
, moveMockDoc
) where

import Data.Aeson
import Data.Unjson
import Happstack.Server
import qualified Data.Text as T

import Context
import Doc.API.V2.AesonTestUtils
  ( jsonTestRequestHelper, lookupObjectArray, testRequestHelper )

import Doc.API.V2.Mock.MockDocInternal
import Doc.DocumentID (DocumentID)
import Doc.SignatoryLinkID (SignatoryLinkID)
import Doc.Types.DocumentStatus (DocumentStatus(..))
import Doc.Types.SignatoryLink
  ( AuthenticationToSignMethod(..), AuthenticationToViewMethod(..)
  , SignatoryRole(..) )

import File.FileID
import Folder.Types
import TestKontra

-- * MockDoc Helper Functions
-----------------------------

-- | For conveniently doing a test API call that returns Document JSON.
-- Uses `jsonTestRequestHelper` but parses the result using `mockDocFromValue`
mockDocTestRequestHelper
  :: Context -> Method -> [(Text, Input)] -> KontraTest Response -> Int -> TestEnv MockDoc
mockDocTestRequestHelper c m p f i = mockDocFromValue <$> jsonTestRequestHelper c m p f i

-- | For conveniently doing a test API call that returns a list of Document JSON.
-- Uses `jsonTestRequestHelper` but parses the result using `mockDocFromValue`
mockDocTestRequestHelperMultiple
  :: Context
  -> Method
  -> [(Text, Input)]
  -> KontraTest Response
  -> Int
  -> TestEnv [MockDoc]
mockDocTestRequestHelperMultiple c m p f i = do
  listJSON  <- jsonTestRequestHelper c m p f i
  listArray <- lookupObjectArray "documents" listJSON
  forM listArray (return . mockDocFromValue)

mockDocFromValue :: Value -> MockDoc
mockDocFromValue v = case parse mockDocUnjson v of
  (Result md []) -> md
  (Result _  e ) -> unexpectedError $ "Could not parse MockDoc from JSON: " <> showt e

mockDocToInput :: MockDoc -> Input
mockDocToInput md = inTextBS $ unjsonToByteStringLazy mockDocUnjson md

-- | Remove things which are not comparable between MockDoc so that we can
-- check for equality
cleanMockDocForComparison :: MockDoc -> MockDoc
cleanMockDocForComparison md = md
  { mockDocId            = ""
  , mockDocParties       = map cleanMockSigLinkForComparison $ mockDocParties md
  , mockDocCTime         = ""
  , mockDocMTime         = ""
  , mockDocTimeoutTime   = Nothing
  , mockDocStatus        = ""
  , mockDocObjectVersion = 1
  , mockDocAccessToken   = Nothing
  , mockDocViewer        = cleanMockViewerForComparison . mockDocViewer $ md
  }
  where
    cleanMockSigLinkForComparison :: MockSigLink -> MockSigLink
    cleanMockSigLinkForComparison msl = msl { mockSigLinkId = "" }

    cleanMockViewerForComparison :: MockViewer -> MockViewer
    cleanMockViewerForComparison mdv = mdv { mockViewerSigId = Nothing }

-- * MockDoc Accessor & Setter Functions
----------------------------------------

getMockDocId :: MockDoc -> DocumentID
getMockDocId md = case (maybeRead $ T.pack $ mockDocId md) of
  Just did -> did
  Nothing  -> unexpectedError $ "Could not read DocumentID from MockDoc:\n" <> showt md

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
  _ ->
    unexpectedError
      $  T.pack
      $  "Could not parse MockDoc status to DocumentStatus: "
      <> mockDocStatus md

getMockDocFolderId :: MockDoc -> Maybe FolderID
getMockDocFolderId = mockDocFolderId

getMockDocDaysToSign :: MockDoc -> Int
getMockDocDaysToSign = mockDocDaysToSign

getMockDocHasFile :: MockDoc -> Bool
getMockDocHasFile = isJust . mockDocFile

getMockDocFileName :: MockDoc -> String
getMockDocFileName md = case (mockMainFileName <$> (mockDocFile md)) of
  Just n  -> n
  Nothing -> unexpectedError $ "No mockDocFile for MockDoc:\n" <> showt md

getMockDocFileId :: MockDoc -> FileID
getMockDocFileId md = case maybeRead $ T.pack $ mockMainFileId mockFile of
  Just fid -> fid
  Nothing ->
    unexpectedError
      $  "getMockDocFileId could not read FileID using maybeRead, MockDoc was:\n"
      <> showt md
  where
    mockFile = case mockDocFile md of
      Just f  -> f
      Nothing -> unexpectedError $ "No mockDocFile for MockDoc:\n" <> showt md

getMockDocAuthorAttachmentLength :: MockDoc -> Int
getMockDocAuthorAttachmentLength md = length . mockDocAuthorAttachments $ md

getMockDocAuthorAttachmentName :: Int -> MockDoc -> String
getMockDocAuthorAttachmentName i md =
  mockAuthorAttachmentName . mockDocAuthorAttachmentNumber i $ md

getMockDocAuthorAttachmentRequired :: Int -> MockDoc -> Bool
getMockDocAuthorAttachmentRequired i md =
  mockAuthorAttachmentRequired . mockDocAuthorAttachmentNumber i $ md

getMockAuthorAttachmentAddedToSealedFile :: Int -> MockDoc -> Bool
getMockAuthorAttachmentAddedToSealedFile i md =
  mockAuthorAttachmentAddedToSealedFile . mockDocAuthorAttachmentNumber i $ md

getMockDocAuthorAttachmentHasFile :: Int -> MockDoc -> Bool
getMockDocAuthorAttachmentHasFile i md =
  not . null . mockAuthorAttachmentFileId . mockDocAuthorAttachmentNumber i $ md

getMockDocAuthorAttachmentFileId :: Int -> MockDoc -> FileID
getMockDocAuthorAttachmentFileId i md =
  case maybeRead $ T.pack $ (mockAuthorAttachmentFileId maa) of
    Just fid -> fid
    Nothing ->
      unexpectedError
        $  "fileIDFromMockAuthorAttachment could not read FileID using maybeRead, "
        <> "MockAuthorAttachment was:\n"
        <> showt maa
  where maa = mockDocAuthorAttachmentNumber i md

getMockDocHasAutoRemindTime :: MockDoc -> Bool
getMockDocHasAutoRemindTime = isJust . mockDocAutoRemindTime

getMockDocAccessToken :: MockDoc -> String
getMockDocAccessToken md = case mockDocAccessToken md of
  Just t  -> t
  Nothing -> unexpectedError $ "No access token for MockDoc: " <> showt md

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
getMockDocSigLinkId i md =
  case (maybeRead $ T.pack $ mockSigLinkId $ getMockSigLinkNumber i md) of
    Just slid -> slid
    Nothing ->
      unexpectedError
        $  "Could not read SignatoryLinkID for signatory "
        <> showt i
        <> "using `maybeRead` from MockDoc:\n"
        <> showt md

getMockDocUserSigLinkId :: Int -> MockDoc -> SignatoryLinkID
getMockDocUserSigLinkId userId md =
  case (maybeRead $ T.pack $ mockSigLinkId $ getMockSigLinkFromUserId userId md) of
    Just slid -> slid
    Nothing ->
      unexpectedError
        $  "Could not read SignatoryLinkID for user "
        <> showt userId
        <> "using `maybeRead` from MockDoc:\n"
        <> showt md


getMockDocSigLinkHasSigned :: Int -> MockDoc -> Bool
getMockDocSigLinkHasSigned i md =
  isJust . mockSigLinkSignTime . getMockSigLinkNumber i $ md

getMockDocSigLinkHasRejected :: Int -> MockDoc -> Bool
getMockDocSigLinkHasRejected i md =
  isJust . mockSigLinkRejectedTime . getMockSigLinkNumber i $ md

getMockDocSigLinkAuthToViewMethod :: Int -> MockDoc -> AuthenticationToViewMethod
getMockDocSigLinkAuthToViewMethod i md =
  case
      authenticationToViewMethodFromString
      $ mockSigLinkAuthMethodToView
      . getMockSigLinkNumber i
      $ md
    of
      Just authMethod -> authMethod
      Nothing ->
        unexpectedError
          $  "Could not parse AuthenticationToViewMethod from MockDoc:\n"
          <> showt md

getMockDocSigLinkAuthToViewArchivedMethod :: Int -> MockDoc -> AuthenticationToViewMethod
getMockDocSigLinkAuthToViewArchivedMethod i md =
  case
      authenticationToViewMethodFromString
      $ mockSigLinkAuthMethodToViewArchived
      . getMockSigLinkNumber i
      $ md
    of
      Just authMethod -> authMethod
      Nothing ->
        unexpectedError
          $  "Could not parse AuthenticationToViewArchivedMethod from MockDoc:\n"
          <> showt md

authenticationToViewMethodFromString :: String -> Maybe AuthenticationToViewMethod
authenticationToViewMethodFromString "standard"  = Just StandardAuthenticationToView
authenticationToViewMethodFromString "se_bankid" = Just SEBankIDAuthenticationToView
authenticationToViewMethodFromString "no_bankid" = Just NOBankIDAuthenticationToView
authenticationToViewMethodFromString "dk_nemid"  = Just DKNemIDAuthenticationToView
authenticationToViewMethodFromString "fi_tupas"  = Just FITupasAuthenticationToView
authenticationToViewMethodFromString "sms_pin"   = Just SMSPinAuthenticationToView
authenticationToViewMethodFromString _           = Nothing

getMockDocSigLinkAuthToSignMethod :: Int -> MockDoc -> AuthenticationToSignMethod
getMockDocSigLinkAuthToSignMethod i md =
  case mockSigLinkAuthMethodToSign . getMockSigLinkNumber i $ md of
    "standard"  -> StandardAuthenticationToSign
    "se_bankid" -> SEBankIDAuthenticationToSign
    "no_bankid" -> NOBankIDAuthenticationToSign
    "dk_nemid"  -> DKNemIDAuthenticationToSign
    "sms_pin"   -> SMSPinAuthenticationToSign
    _ ->
      unexpectedError
        $  "Could not parse AuthenticationToSignMethod from MockDoc:\n"
        <> showt md

setMockDocSigLinkAuthToSignMethod
  :: Int -> AuthenticationToSignMethod -> MockDoc -> MockDoc
setMockDocSigLinkAuthToSignMethod i auth = setForSigNumberFromMockDoc
  i
  (\msl -> msl { mockSigLinkAuthMethodToSign = toStrAuth auth })
  where
    toStrAuth StandardAuthenticationToSign = "standard"
    toStrAuth SEBankIDAuthenticationToSign = "se_bankid"
    toStrAuth NOBankIDAuthenticationToSign = "no_bankid"
    toStrAuth DKNemIDAuthenticationToSign  = "dk_nemid"
    toStrAuth SMSPinAuthenticationToSign   = "sms_pin"

getMockDocSigLinkPersonalNumber :: Int -> MockDoc -> String
getMockDocSigLinkPersonalNumber i =
  getFieldValueOfTypeForSigNumberFromMockDoc i "personal_number"

getMockDocSigLinkEmail :: Int -> MockDoc -> String
getMockDocSigLinkEmail i = getFieldValueOfTypeForSigNumberFromMockDoc i "email"

getMockDocSigLinkMobileNumber :: Int -> MockDoc -> String
getMockDocSigLinkMobileNumber i = getFieldValueOfTypeForSigNumberFromMockDoc i "mobile"

getMockDocSigLinkAttachmentsLength :: Int -> MockDoc -> Int
getMockDocSigLinkAttachmentsLength i =
  length . mockSigLinkAttachments . getMockSigLinkNumber i

getMockDocSigLinkAttachmentHasFile :: Int -> Int -> MockDoc -> Bool
getMockDocSigLinkAttachmentHasFile si ai md
  | ai > length atts
  = unexpectedError
    $  "getMockDocSigLinkAttachmentHasFile could not get index "
    <> showt ai
    <> " from MockDoc:\n"
    <> showt md
  | otherwise
  = isJust . mockSigAttachmentFileId $ atts !! (ai - 1)
  where atts = mockSigLinkAttachments . getMockSigLinkNumber si $ md

setMockDocSigLinkAttachments :: Int -> [(String, String)] -> MockDoc -> MockDoc
setMockDocSigLinkAttachments i namesdesc = setForSigNumberFromMockDoc
  i
  (\msl -> msl
    { mockSigLinkAttachments = for
                                 namesdesc
                                 (\(n, d) -> MockSigAttachment
                                   { mockSigAttachmentName        = n
                                   , mockSigAttachmentDescription = d
                                   , mockSigAttachmentFileId      = Nothing
                                   , mockSigAttachmentFileName    = Nothing
                                   }
                                 )
    }
  )

setMockDocSigLinkDeliveryMethod :: String -> MockSigLink -> MockSigLink
setMockDocSigLinkDeliveryMethod deliveryMethod sigLink =
  sigLink { mockSigLinkDeliveryMethod = deliveryMethod }

setMockDocSigLinkConfirmationDeliveryMethod :: String -> MockSigLink -> MockSigLink
setMockDocSigLinkConfirmationDeliveryMethod deliveryMethod sigLink =
  sigLink { mockSigLinkConfirmationDelivery = deliveryMethod }

setMockDocSigLinkSignatoryRole :: SignatoryRole -> MockSigLink -> MockSigLink
setMockDocSigLinkSignatoryRole role sigLink = sigLink { mockSigLinkSignatoryRole = role }

setMockSigLinkStandardField :: String -> String -> MockSigLink -> MockSigLink
setMockSigLinkStandardField fieldType value msl = msl
  { mockSigLinkFields = newField : oldFieldsWithoutFieldType msl
  }
  where
    newField =
      defaultMockSigField { mockSigFieldType = fieldType, mockSigFieldValue = Just value }
    oldFieldsWithoutFieldType oldMsl =
      filter (\slf -> mockSigFieldType slf /= fieldType) (mockSigLinkFields oldMsl)


setMockDocSigLinkStandardField :: Int -> String -> String -> MockDoc -> MockDoc
setMockDocSigLinkStandardField i fieldType value = setForSigNumberFromMockDoc i
  $ \msl -> msl { mockSigLinkFields = newField : oldFieldsWithoutFieldType msl }
  where
    newField =
      defaultMockSigField { mockSigFieldType = fieldType, mockSigFieldValue = Just value }
    oldFieldsWithoutFieldType msl =
      filter (\slf -> mockSigFieldType slf /= fieldType) (mockSigLinkFields msl)

addStandardSigLinksToMockDoc :: Int -> MockDoc -> MockDoc
addStandardSigLinksToMockDoc i md =
  addSigLinksToMockDoc (replicate i defaultMockSigLink) md

addSigLinksToMockDoc :: [MockSigLink] -> MockDoc -> MockDoc
addSigLinksToMockDoc newParties md =
  md { mockDocParties = mockDocParties md <> newParties }

-- * Internal use only!
-----------------------

-- | Internal use only
mockDocAuthorAttachmentNumber :: Int -> MockDoc -> MockAuthorAttachment
mockDocAuthorAttachmentNumber num mockdoc
  | num > length (mockDocAuthorAttachments mockdoc)
  = unexpectedError
    $  "mockDocAuthorAttachment could not get index "
    <> showt num
    <> " from MockDoc:\n"
    <> showt mockdoc
  | otherwise
  = (mockDocAuthorAttachments mockdoc) !! (num - 1)

-- | Internal use only
getMockSigLinkNumber :: Int -> MockDoc -> MockSigLink
getMockSigLinkNumber num mockdoc
  | num > length (mockDocParties mockdoc)
  = unexpectedError
    $  "getMockSigLinkNumber could not get index "
    <> showt num
    <> " from MockDoc:\n"
    <> showt mockdoc
  | otherwise
  = (mockDocParties mockdoc) !! (num - 1)

-- | Internal use only
getMockSigLinkFromUserId :: Int -> MockDoc -> MockSigLink
getMockSigLinkFromUserId userId mockDoc =
  case
      find (\link -> elem (show userId) (mockSigLinkUserId link)) (mockDocParties mockDoc)
    of
      Just link -> link
      Nothing ->
        unexpectedError
          $  "getMockSigLinkFromUserId could not find a link for user "
          <> showt userId
          <> " from MockDoc:\n"
          <> showt mockDoc

-- | Internal use only
getFieldValueOfTypeForSigNumberFromMockDoc :: Int -> String -> MockDoc -> String
getFieldValueOfTypeForSigNumberFromMockDoc i ft md =
  case getMockFieldsOfType ft . getMockSigLinkNumber i $ md of
    f : _ -> fromMaybe "" $ mockSigFieldValue f
    [] ->
      unexpectedError
        $  "Could not get field of type "
        <> T.pack ft
        <> " for signatory "
        <> showt i
        <> " from MockDoc:\n"
        <> showt md

-- | Internal use only
getMockFieldsOfType :: String -> MockSigLink -> [MockSigField]
getMockFieldsOfType fts msl =
  filter (\f -> mockSigFieldType f == fts) (mockSigLinkFields msl)

-- | Internal use only
setForSigNumberFromMockDoc :: Int -> (MockSigLink -> MockSigLink) -> MockDoc -> MockDoc
setForSigNumberFromMockDoc i f md = md
  { mockDocParties       = updatedSigLinks
  , mockDocObjectVersion = mockDocObjectVersion md + 1
  }
  where
    originalSL      = getMockSigLinkNumber i md
    updatedSL       = f originalSL
    updatedSigLinks = insert updatedSL . delete originalSL . sort . mockDocParties $ md

moveMockDoc :: FolderID -> MockDoc -> MockDoc
moveMockDoc fid doc = doc { mockDocFolderId = Just fid }
