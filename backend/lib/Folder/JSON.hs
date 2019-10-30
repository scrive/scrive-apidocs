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
encodeFolder folder =
  pairs
    $  "id"
    .= folderID folder
    <> "parent_id"
    .= folderParentID folder
    <> "name"
    .= folderName folder

encodeFolderWithChildren :: FolderWithChildren -> Encoding
encodeFolderWithChildren fdrwc =
  pairs
    $  "id"
    .= folderID folder
    <> "parent_id"
    .= folderParentID folder
    <> "name"
    .= folderName folder
    <> pair "children" (list encodeChild children)
  where
    folder   = fwcFolder fdrwc
    children = fwcFolder <$> fwcChildren fdrwc
    encodeChild childFolder =
      pairs $ "id" .= folderID childFolder <> "name" .= folderName childFolder

unjsonFolderForUpdate :: UnjsonDef (Maybe FolderID, Text)
unjsonFolderForUpdate =
  objectOf $ pure (,) <*> fieldOpt "parent_id" fst "Parent folder ID" <*> fieldBy
    "name"
    snd
    "Folder name"
    (unjsonWithValidationOrEmptyText asValidCompanyName)

updateFolderWithFolderFromRequest :: Folder -> Value -> Maybe Folder
updateFolderWithFolderFromRequest folder folderChanges = do
  let folderReq = (folderParentID folder, folderName folder)
  case update folderReq unjsonFolderForUpdate folderChanges of
    (Result ugUpdated []) ->
      Just $ folder { folderParentID = fst ugUpdated, folderName = snd ugUpdated }
    (Result _ _) -> Nothing
