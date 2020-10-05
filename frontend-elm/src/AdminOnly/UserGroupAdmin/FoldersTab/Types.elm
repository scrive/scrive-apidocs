module AdminOnly.UserGroupAdmin.FoldersTab.Types exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as DP
import List as L
import Maybe as M
import Utils exposing (..)


type alias Folder =
    { id : String
    , parentID : Maybe String
    , name : String
    , homeForUser : Maybe String
    , homeForUserGroup : Maybe String
    }


type FolderTree node
    = FolderTree node (List (FolderTree node))


folderDecoder : Decoder Folder
folderDecoder =
    D.succeed Folder
        |> DP.required "id" D.string
        |> DP.optional "parent_id" (D.nullable D.string) Nothing
        |> DP.required "name" D.string
        |> DP.required "home_for_user" (D.nullable D.string)
        |> DP.required "home_for_user_group" (D.nullable D.string)


folderTreeDecoder : Decoder (FolderTree Folder)
folderTreeDecoder =
    D.map2 FolderTree
        folderDecoder
        (D.map (M.withDefault [])
            (D.maybe <| D.field "children" <| D.list <| D.lazy (\_ -> folderTreeDecoder))
        )


getFolder : FolderTree node -> node
getFolder (FolderTree node _) =
    node


getParentFolder : Folder -> FolderTree Folder -> Maybe Folder
getParentFolder folder (FolderTree node childTrees) =
    if L.member folder (L.map getFolder childTrees) then
        Just node

    else
        L.head <| L.filterMap (getParentFolder folder) childTrees


folderIsRoot : Folder -> FolderTree Folder -> Bool
folderIsRoot folder tree =
    (folder.parentID == Nothing) && (getParentFolder folder tree == Nothing)


getHomeFolderForUserGroup : String -> FolderTree Folder -> Maybe Folder
getHomeFolderForUserGroup ugid (FolderTree node childTrees) =
    if Just ugid == node.homeForUserGroup then
        Just node

    else
        L.head <| L.filterMap (getHomeFolderForUserGroup ugid) childTrees


folderDisplayName : Folder -> String
folderDisplayName folder =
    case stringNonEmpty folder.name of
        Just name ->
            folder.name ++ " (ID: " ++ folder.id ++ ")"

        Nothing ->
            "FolderID: " ++ folder.id


folderDisplayNameForTree : Folder -> String
folderDisplayNameForTree folder =
    case stringNonEmpty folder.name of
        Just name ->
            folder.name

        Nothing ->
            "FolderID: " ++ folder.id
