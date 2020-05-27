module Folder.JSON (
    encodeFolderWithChildren
  , updateFolderWithFolderFromRequest
) where

import Data.Aeson
import Data.Aeson.Encoding
import Data.Unjson

import Folder.Types
import InputValidation

encodeFolderWithChildren :: Bool -> FolderWithChildren -> Encoding
encodeFolderWithChildren recursive fdrwc =
  pairs
    $  commonFields (fdrwc ^. #folder)
    <> ("parent_id" .= (fdrwc ^. #folder % #parentID))
    <> pair "children" (list encodeChild (fdrwc ^. #children))
  where
    commonFields folder =
      ("id" .= (folder ^. #id))
        <> ("name" .= (folder ^. #name))
        <> ("home_for_user" .= (folder ^. #homeForUser))
        <> ("home_for_user_group" .= (folder ^. #homeForUserGroup))
    encodeChild child =
      pairs
        $  commonFields (child ^. #folder)
        <> (if recursive
             then pair "children" (list encodeChild (child ^. #children))
             else mempty
           )

unjsonFolderForUpdate :: UnjsonDef (Maybe FolderID, Text)
unjsonFolderForUpdate =
  objectOf $ (,) <$> fieldOpt "parent_id" fst "Parent folder ID" <*> fieldBy
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
