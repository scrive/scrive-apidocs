module Folder.JSON (
    encodeFolder
  , encodeFolderWithChildren
  , updateFolderWithFolderFromRequest
) where

import Data.Aeson
import Data.Aeson.Encoding
import Data.Unjson

import Folder.Types
import InputValidation

encodeFolder :: Folder -> Encoding
encodeFolder folder = pairs $
     "id"        .= get folderID       folder
  <> "parent_id" .= get folderParentID folder
  <> "name"      .= get folderName     folder

encodeFolderWithChildren :: FolderWithChildren -> Encoding
encodeFolderWithChildren fdrwc = pairs $
     "id"        .= get folderID       folder
  <> "parent_id" .= get folderParentID folder
  <> "name"      .= get folderName     folder
  <> pair "children" (list encodeChild children)
  where
    folder = get fwcFolder fdrwc
    children = get fwcFolder <$> get fwcChildren fdrwc
    encodeChild childFolder = pairs $
         "id"   .= get folderID   childFolder
      <> "name" .= get folderName childFolder

unjsonFolderForUpdate :: UnjsonDef (Maybe FolderID, Text)
unjsonFolderForUpdate = objectOf $ pure (,)
  <*> fieldOpt "parent_id" fst "Parent folder ID"
  <*> fieldBy "name" snd "Folder name"
      (unjsonWithValidationOrEmptyText asValidCompanyName)

updateFolderWithFolderFromRequest :: Folder -> Value -> Maybe Folder
updateFolderWithFolderFromRequest folder folderChanges = do
  let folderReq = (get folderParentID folder, get folderName folder)
  case update folderReq unjsonFolderForUpdate folderChanges of
    (Result ugUpdated []) -> Just $ folder
      { _folderParentID = fst ugUpdated
      , _folderName     = snd ugUpdated
      }
    (Result _ _) -> Nothing
