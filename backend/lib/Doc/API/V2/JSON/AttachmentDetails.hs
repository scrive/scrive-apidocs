{-# LANGUAGE TemplateHaskell #-}
module Doc.API.V2.JSON.AttachmentDetails (
    AttachmentDetails(..)
  , unjsonAttachmentDetails
) where

import Data.Functor.Invariant
import Data.Unjson

import File.FileID (FileID)
import Utils.TH

data AttachmentDetails = AttachmentDetails {
    aadName :: Text,
    aadRequired :: Bool,
    aadAddToSealedFile :: Bool,
    aadFileOrFileParam :: Either FileID Text
  }

data AttachmentDetailsWithFileIDOrFileParam =
    AttachmentDetailsWithFileID Text Bool Bool FileID
  | AttachmentDetailsWithFileParam Text Bool Bool Text

nameFromAttachmentDetailsWithFileIDOrFileParam
  :: AttachmentDetailsWithFileIDOrFileParam -> Text
nameFromAttachmentDetailsWithFileIDOrFileParam (AttachmentDetailsWithFileID n _ _ _) = n
nameFromAttachmentDetailsWithFileIDOrFileParam (AttachmentDetailsWithFileParam n _ _ _) =
  n

requiredFromAttachmentDetailsWithFileIDOrFileParam
  :: AttachmentDetailsWithFileIDOrFileParam -> Bool
requiredFromAttachmentDetailsWithFileIDOrFileParam (AttachmentDetailsWithFileID _ r _ _)
  = r
requiredFromAttachmentDetailsWithFileIDOrFileParam (AttachmentDetailsWithFileParam _ r _ _)
  = r

addToSealedFileFromAttachmentDetailsWithFileIDOrFileParam
  :: AttachmentDetailsWithFileIDOrFileParam -> Bool
addToSealedFileFromAttachmentDetailsWithFileIDOrFileParam (AttachmentDetailsWithFileID _ _ a _)
  = a
addToSealedFileFromAttachmentDetailsWithFileIDOrFileParam (AttachmentDetailsWithFileParam _ _ a _)
  = a

fileIDFromAttachmentDetailsWithFileIDOrFileParam
  :: AttachmentDetailsWithFileIDOrFileParam -> FileID
fileIDFromAttachmentDetailsWithFileIDOrFileParam (AttachmentDetailsWithFileID _ _ _ fid)
  = fid
fileIDFromAttachmentDetailsWithFileIDOrFileParam _ =
  unexpectedError "Trying to fetch file id from file param"

fileParamAttachmentDetailsWithFileIDOrFileParam
  :: AttachmentDetailsWithFileIDOrFileParam -> Text
fileParamAttachmentDetailsWithFileIDOrFileParam (AttachmentDetailsWithFileParam _ _ _ fp)
  = fp
fileParamAttachmentDetailsWithFileIDOrFileParam _ =
  unexpectedError "Trying to fetch file param from file id"

toAttachmentDetails :: AttachmentDetailsWithFileIDOrFileParam -> AttachmentDetails
toAttachmentDetails (AttachmentDetailsWithFileID n r a fid) =
  AttachmentDetails n r a (Left $ fid)
toAttachmentDetails (AttachmentDetailsWithFileParam n r a fp) =
  AttachmentDetails n r a (Right fp)

fromAttachmentDetails :: AttachmentDetails -> AttachmentDetailsWithFileIDOrFileParam
fromAttachmentDetails ad = case ad of
  (AttachmentDetails n r a (Left  fid)) -> AttachmentDetailsWithFileID n r a fid
  (AttachmentDetails n r a (Right fp )) -> AttachmentDetailsWithFileParam n r a fp

unjsonAttachmentDetails :: UnjsonDef AttachmentDetails
unjsonAttachmentDetails = invmap toAttachmentDetails fromAttachmentDetails $ unionOf
  [ ( $(isConstr 'AttachmentDetailsWithFileID)
    , pure AttachmentDetailsWithFileID
    <*> field "name"
              nameFromAttachmentDetailsWithFileIDOrFileParam
              "Name of author attachment"
    <*> field "required"
              requiredFromAttachmentDetailsWithFileIDOrFileParam
              "Is signatory required to read attachmnet before signing"
    <*> field "add_to_sealed_file"
              addToSealedFileFromAttachmentDetailsWithFileIDOrFileParam
              "Should attachment be added to sealed pdf"
    <*> field "file_id"
              fileIDFromAttachmentDetailsWithFileIDOrFileParam
              "Id of attachment file"
    )
  , ( $(isConstr 'AttachmentDetailsWithFileParam)
    , pure AttachmentDetailsWithFileParam
    <*> field "name"
              nameFromAttachmentDetailsWithFileIDOrFileParam
              "Name of author attachment"
    <*> field "required"
              requiredFromAttachmentDetailsWithFileIDOrFileParam
              "Is signatory required to read attachmnet before signing"
    <*> field "add_to_sealed_file"
              addToSealedFileFromAttachmentDetailsWithFileIDOrFileParam
              "Should attachment be added to sealed pdf"
    <*> field "file_param"
              fileParamAttachmentDetailsWithFileIDOrFileParam
              "Name of parameter with file"
    )
  ]
