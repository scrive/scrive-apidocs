module PdfToolsLambda.Spec (
    sealSpecToLambdaSpec
  , presealSpecToLambdaSpec
  , addImageSpecToLambdaSpec
) where

import Control.Monad.Base
import Log
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import DB
import Doc.AddImageSpec
import Doc.SealSpec

sealSpecToLambdaSpec ::
  (MonadDB m, MonadLog m, MonadBase IO m) =>
  SealSpec -> m BSL.ByteString
sealSpecToLambdaSpec spec = do
  mfc <- liftBase $  BS.readFile $ T.unpack $ input spec
  persons_ <- mapM sealSpecForPerson (persons spec)
  secretaries_ <- mapM sealSpecForPerson (secretaries spec)
  initiator_ <- mapM sealSpecForPerson (maybeToList $ initiator spec)
  attachments_ <- mapM sealSpecForSealAttachment (attachments spec)
  files_ <- mapM sealSpecForFile(filesList spec)
  return $ Aeson.encode $ Aeson.object $ [
      "preseal" .= False
    , "persons" .= persons_
    , "secretaries" .= secretaries_
    , "attachments" .= attachments_
    , "filesList" .= files_
    ] ++
    ((\i -> "initiator" .= i) <$> (initiator_)) ++ [
      "mainFileInput" .= Aeson.object [
        "base64Content" .= (T.decodeUtf8 $ B64.encode mfc)
      ]
    , "documentNumberText" .= documentNumberText spec
    , "initialsText" .= initialsText spec
    , "dontAddFooterToEveryPage" .= disableFooter spec
    , "staticTexts" .= staticTextsJSON (staticTexts spec)
    ]

presealSpecToLambdaSpec ::
  (MonadDB m, MonadLog m, MonadBase IO m) =>
  PreSealSpec -> m BSL.ByteString
presealSpecToLambdaSpec spec = do
  mfc <- liftBase $ BS.readFile $ T.unpack $ pssInput spec
  fields_ <- mapM sealSpecForField (pssFields spec)
  return $ Aeson.encode $ Aeson.object $ [
      "preseal" .= True
    , "fields" .= fields_
    , "mainFileInput" .= Aeson.object [
        "base64Content" .= (T.decodeUtf8 $ B64.encode mfc)
      ]
    ]

sealSpecForPerson :: (MonadDB m, MonadLog m, MonadBase IO m) =>
                     Person -> m Aeson.Value
sealSpecForPerson person = do
  highlightedImages_ <- mapM sealSpecForHighlightedImage (highlightedImages person)
  fields_ <- mapM sealSpecForField (fields person)
  return $ Aeson.object [
      "fullname" .= fullname person
    , "company" .= company person
    , "personalnumber" .= personalnumber person
    , "companynumber" .= companynumber person
    , "email" .= email person
    , "phone" .= phone person
    , "fullnameverified" .= fullnameverified person
    , "companyverified" .= companyverified person
    , "numberverified" .= numberverified person
    , "emailverified" .= emailverified person
    , "phoneverified" .= phoneverified person
    , "signtime" .= signtime person
    , "signedAtText" .= signedAtText person
    , "personalNumberText" .= personalNumberText person
    , "companyNumberText" .= companyNumberText person
    , "identifiedNameText" .= identifiedNameText person
    , "nameFromText" .= nameFromText person
    , "highlightedImages" .= highlightedImages_
    , "fields" .= fields_
    ]


sealSpecForHighlightedImage :: (MonadDB m, MonadLog m, MonadBase IO m) =>
                               HighlightedImage -> m Aeson.Value
sealSpecForHighlightedImage hi = do
  return  $ Aeson.object [
      "page" .= hiPage hi
    , "fileInput" .= Aeson.object [
        "base64Content" .= (T.decodeUtf8 $ B64.encode $ hiImage hi)
      ]
    ]

sealSpecForField :: (MonadDB m, MonadLog m, MonadBase IO m) =>
                    Field -> m Aeson.Value
sealSpecForField field = do
  return  $ Aeson.object $ [
      "x" .= x field
    , "y" .= y field
    , "page" .= page field
    , "includeInSummary" .= includeInSummary field
    ] ++
    case field of
      Field{} -> [
          "value" .= value field
        , "fontSize" .= fontSize field
        , "greyed".= greyed field
        ]
      FieldJPG{} -> [
          "image_w" .= image_w field
        , "image_h" .= image_h field
        , "onlyForSummary" .= image_h field
        , "fileInput" .= Aeson.object [
            "base64Content" .= (T.decodeUtf8 $ B64.encode $ valueBinary field)
          ]
        ]

sealSpecForSealAttachment :: (MonadDB m, MonadLog m, MonadBase IO m) =>
                             SealAttachment -> m Aeson.Value
sealSpecForSealAttachment a = do
  return  $ Aeson.object [
      "fileName" .= fileName a
    , "mimeType" .= mimeType a
    , "fileInput" .= Aeson.object [
        "base64Content" .= (T.decodeUtf8 $ B64.encode $ fileContent a)
      ]
    ]

sealSpecForFile:: (MonadDB m, MonadLog m, MonadBase IO m) =>
                  FileDesc -> m Aeson.Value
sealSpecForFile fd = do
  mfc <- case (fileInput fd) of
           Just fn -> Just <$> liftBase (BS.readFile $ T.unpack fn)
           Nothing -> return Nothing
  return  $ Aeson.object $ [
      "title" .= fileTitle fd
    , "role" .= fileRole fd
    , "pagesText" .= filePagesText fd
    , "attachedBy" .= fileAttachedBy fd
    , "sealedOn" .= fileSealedOn fd
    , "attachedToSealedFileText" .= fileAttachedToSealedFileText fd
    ] ++
    case (mfc) of
      Just fc -> [
        "fileInput" .= Aeson.object [
          "base64Content" .= (T.decodeUtf8 $ B64.encode $ fc)
          ]
        ]
      _ -> []


addImageSpecToLambdaSpec ::
  (MonadDB m, MonadLog m, MonadBase IO m)
  => AddImageSpec -> m BSL.ByteString
addImageSpecToLambdaSpec spec = do
  mfc <- liftBase $  BS.readFile $ addImageInput spec
  return $ Aeson.encode $ Aeson.object $ [
      "documentNumberText" .= addImageDocumentNumberText spec
    , "image" .= Aeson.object [
        "x" .= addImageX spec
      , "y" .= addImageY spec
      , "page" .= addImagePage spec
      , "fileInput" .= Aeson.object [
          "base64Content" .= (T.decodeUtf8 $ B64.encode $ addImageImageBinary spec)
        ]
      ]
    , "mainFileInput" .= Aeson.object [
        "base64Content" .= (T.decodeUtf8 $ B64.encode mfc)
      ]
    ]

staticTextsJSON :: SealingTexts -> Aeson.Value
staticTextsJSON st =  Aeson.object [
     "verificationTitle" .= verificationTitle st
   , "partnerText" .= partnerText st
   , "initiatorText" .= initiatorText st
   , "documentText" .= documentText st
   , "verificationPageDescription" .= verificationPageDescription st
   , "hiddenAttachmentText" .=  hiddenAttachmentText st
   , "onePageText" .= onePageText st
   ]
