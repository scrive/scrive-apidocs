module Folder.JSON (
    encodeFolderWithChildren
  , updateFolderWithFolderFromRequest
) where

import Data.Aeson
import Data.Aeson.Encoding
import Data.Unjson
import Optics (folded)

import Folder.Types
import InputValidation

encodeFolderWithChildren :: FolderWithChildren -> Encoding
encodeFolderWithChildren fdrwc =
  pairs
    $  ("id" .= (folder ^. #id))
    <> ("parent_id" .= (folder ^. #parentID))
    <> ("name" .= (folder ^. #name))
    <> pair "children" (list encodeChild children)
  where
    folder   = fdrwc ^. #folder
    children = fdrwc ^.. #children % folded % #folder
    encodeChild childFolder =
      pairs $ "id" .= (childFolder ^. #id) <> "name" .= (childFolder ^. #name)

unjsonFolderForUpdate :: UnjsonDef (Maybe FolderID, Text)
unjsonFolderForUpdate =
  objectOf $ pure (,) <*> fieldOpt "parent_id" fst "Parent folder ID" <*> fieldBy
    "name"
    snd
    "Folder name"
    (unjsonWithValidationOrEmptyText asValidCompanyName)

updateFolderWithFolderFromRequest :: Folder -> Value -> Maybe Folder
updateFolderWithFolderFromRequest folder folderChanges = do
  let folderReq = (folder ^. #parentID, folder ^. #name)
  case update folderReq unjsonFolderForUpdate folderChanges of
    (Result ugUpdated []) ->
      Just $ folder & (#parentID .~ fst ugUpdated) & (#name .~ snd ugUpdated)
    (Result _ _) -> Nothing
