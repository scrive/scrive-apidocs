module Doc.API.V2.JSON.AttachmentDetails (
    AttachmentDetails(..)
  , unjsonAttachmentDetails
) where

import Data.Data
import Data.Functor.Invariant
import Data.Int
import Data.Unjson
import qualified Data.Text as T

import File.FileID (FileID, fromFileID, unsafeFileID)
import KontraPrelude

data AttachmentDetails = AttachmentDetails {
    aadName :: T.Text,
    aadRequired :: Bool,
    aadFileOrFileParam :: Either FileID T.Text
  }

data AttachmentDetailsWithFileIDOrFileParam =
    AttachmentDetailsWithFileID T.Text Bool Int64
  | AttachmentDetailsWithFileParam T.Text Bool T.Text
  deriving (Typeable,Data)

nameFromAttachmentDetailsWithFileIDOrFileParam :: AttachmentDetailsWithFileIDOrFileParam -> T.Text
nameFromAttachmentDetailsWithFileIDOrFileParam (AttachmentDetailsWithFileID n _ _) = n
nameFromAttachmentDetailsWithFileIDOrFileParam (AttachmentDetailsWithFileParam n _ _) = n

requiredFromAttachmentDetailsWithFileIDOrFileParam :: AttachmentDetailsWithFileIDOrFileParam -> Bool
requiredFromAttachmentDetailsWithFileIDOrFileParam (AttachmentDetailsWithFileID _ r _) = r
requiredFromAttachmentDetailsWithFileIDOrFileParam (AttachmentDetailsWithFileParam _ r _) = r

fileIDFromAttachmentDetailsWithFileIDOrFileParam :: AttachmentDetailsWithFileIDOrFileParam -> Int64
fileIDFromAttachmentDetailsWithFileIDOrFileParam (AttachmentDetailsWithFileID _ _ fid) = fid
fileIDFromAttachmentDetailsWithFileIDOrFileParam _ = $unexpectedError "Trying to fetch file id from file param"

fileParamAttachmentDetailsWithFileIDOrFileParam :: AttachmentDetailsWithFileIDOrFileParam -> T.Text
fileParamAttachmentDetailsWithFileIDOrFileParam (AttachmentDetailsWithFileParam _ _ fp) = fp
fileParamAttachmentDetailsWithFileIDOrFileParam _ = $unexpectedError "Trying to fetch file param from file id"

toAttachmentDetails :: AttachmentDetailsWithFileIDOrFileParam -> AttachmentDetails
toAttachmentDetails (AttachmentDetailsWithFileID n r fid) = AttachmentDetails n r (Left $ unsafeFileID fid)
toAttachmentDetails (AttachmentDetailsWithFileParam n r fp)  = AttachmentDetails n r (Right fp)

fromAttachmentDetails :: AttachmentDetails -> AttachmentDetailsWithFileIDOrFileParam
fromAttachmentDetails ad = case ad of
  (AttachmentDetails n r (Left fid)) -> AttachmentDetailsWithFileID n r (fromFileID fid)
  (AttachmentDetails n r (Right fp)) -> AttachmentDetailsWithFileParam n r fp

unjsonAttachmentDetails :: UnjsonDef AttachmentDetails
unjsonAttachmentDetails = invmap toAttachmentDetails fromAttachmentDetails $ unionOf [
    (unjsonIsConstrByName "AttachmentDetailsWithFileID", pure AttachmentDetailsWithFileID
      <*> field "name" nameFromAttachmentDetailsWithFileIDOrFileParam "Name of author attachment"
      <*> field "required" requiredFromAttachmentDetailsWithFileIDOrFileParam "Is signatory required to read attachmnet before signing"
      <*> field "file_id" fileIDFromAttachmentDetailsWithFileIDOrFileParam "Id of attachment file"
    ),
    (unjsonIsConstrByName "AttachmentDetailsWithFileParam", pure AttachmentDetailsWithFileParam
      <*> field "name" nameFromAttachmentDetailsWithFileIDOrFileParam "Name of author attachment"
      <*> field "required" requiredFromAttachmentDetailsWithFileIDOrFileParam "Is signatory required to read attachmnet before signing"
      <*> field "file_param" fileParamAttachmentDetailsWithFileIDOrFileParam "Name of parameter with file"
    )
  ]
