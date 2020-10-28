module AdminOnly.UserGroupAdmin.FoldersTab.FoldersTab exposing
    ( Model
    , Msg
    , init
    , setUserGroupID
    , tabName
    , update
    , view
    )

import AdminOnly.Types.UserGroup as UserGroup exposing (UserGroup)
import AdminOnly.Types.UserGroup.Cmd as Cmd
import AdminOnly.UserAdmin.DetailsTab.User as User exposing (User)
import AdminOnly.UserGroupAdmin.FoldersTab.AddModal as AddModal
import AdminOnly.UserGroupAdmin.FoldersTab.DeleteModal as DeleteModal
import AdminOnly.UserGroupAdmin.FoldersTab.MakeRootModal as MakeRootModal
import AdminOnly.UserGroupAdmin.FoldersTab.MoveModal as MoveModal
import AdminOnly.UserGroupAdmin.FoldersTab.RenameModal as RenameModal
import AdminOnly.UserGroupAdmin.FoldersTab.Types exposing (..)
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import FlashMessage
import Html exposing (Html, a, div, h1, h4, li, p, strong, text, ul)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Http
import List as L
import Maybe as M
import Maybe.Extra as M
import Return exposing (..)
import Utils exposing (..)


type alias Model =
    { ugid : String
    , mUserGroup : Maybe UserGroup
    , mUserGroupForActiveFolder : Maybe UserGroup
    , mUserForActiveFolder : Maybe User
    , sFolderTree : Status (FolderTree Folder)
    , mActiveFolder : Maybe Folder
    , mAddModal : Maybe AddModal.Model
    , mDeleteModal : Maybe DeleteModal.Model
    , mMakeRootModal : Maybe MakeRootModal.Model
    , mMoveModal : Maybe MoveModal.Model
    , mRenameModal : Maybe RenameModal.Model
    }


type Msg
    = GotUserGroup (Result Http.Error UserGroup)
    | GotUserGroupForActiveFolder (Result Http.Error UserGroup)
    | GotUserForActiveFolder (Result Http.Error User)
    | GotFolderTree (Result Http.Error (FolderTree Folder))
    | FolderClicked Folder
    | AddClicked
    | AddModalMsg AddModal.Msg
    | DeleteClicked
    | DeleteModalMsg DeleteModal.Msg
    | MakeRootClicked
    | MakeRootModalMsg MakeRootModal.Msg
    | MoveClicked
    | MoveModalMsg MoveModal.Msg
    | RenameClicked
    | RenameModalMsg RenameModal.Msg


tabName : String
tabName =
    "folders"


init : (Msg -> msg) -> String -> Return msg Model
init embed ugid =
    let
        model =
            { ugid = ugid
            , mUserGroup = Nothing
            , mUserGroupForActiveFolder = Nothing
            , mUserForActiveFolder = Nothing
            , sFolderTree = Loading
            , mActiveFolder = Nothing
            , mAddModal = Nothing
            , mDeleteModal = Nothing
            , mMakeRootModal = Nothing
            , mMoveModal = Nothing
            , mRenameModal = Nothing
            }
    in
    return model <| Cmd.map embed <| Cmd.getUserGroup GotUserGroup ugid


getFolderTreeCmd : String -> Cmd Msg
getFolderTreeCmd fid =
    Http.get
        { url = "/api/frontend/folders/" ++ fid ++ "?recursive=true"
        , expect = Http.expectJson GotFolderTree folderTreeDecoder
        }


getUserGroupForActiveFolderCmd : String -> Cmd Msg
getUserGroupForActiveFolderCmd ugid =
    Http.get
        { url = "/adminonly/companyadmin/details/" ++ ugid
        , expect = Http.expectJson GotUserGroupForActiveFolder UserGroup.decoder
        }


getUserForActiveFolderCmd : String -> Cmd Msg
getUserForActiveFolderCmd uid =
    Http.get
        { url = "/adminonly/useradmin/details/" ++ uid
        , expect = Http.expectJson GotUserForActiveFolder User.decoder
        }


setUserGroupID : (Msg -> msg) -> String -> Model -> Return msg Model
setUserGroupID embed ugid model =
    let
        cmd =
            -- Prevent reloading the user group (and its home folder) when navigating between tabs.
            -- The user may have activated another folder so we want to persist that.
            if model.ugid == ugid then
                Cmd.none

            else
                Cmd.map embed <| Cmd.getUserGroup GotUserGroup ugid
    in
    return { model | ugid = ugid } cmd


update : (Msg -> msg) -> Globals msg -> Msg -> Model -> Return msg Model
update embed globals msg model =
    case msg of
        GotUserGroup result ->
            case result of
                Ok ug ->
                    return { model | mUserGroup = Just ug } <|
                        case ug.homeFolderID of
                            Just homeFolderID ->
                                Cmd.map embed <| getFolderTreeCmd homeFolderID

                            Nothing ->
                                globals.flashMessage <|
                                    FlashMessage.error <|
                                        "Unable to display folders: this user group does not have a home folder."

                Err _ ->
                    singleton { model | mUserGroup = Nothing, sFolderTree = Failure }

        GotUserGroupForActiveFolder result ->
            singleton <|
                case result of
                    Ok ug ->
                        { model | mUserGroupForActiveFolder = Just ug }

                    Err _ ->
                        { model | mUserGroupForActiveFolder = Nothing }

        GotUserForActiveFolder result ->
            singleton <|
                case result of
                    Ok user ->
                        { model | mUserForActiveFolder = Just user }

                    Err _ ->
                        { model | mUserForActiveFolder = Nothing }

        -- The folder tree is loaded using the following sequence:
        --
        -- 1. Request the user group by model.ugid to get its homeFolderID (-> GotUserGroup)
        -- 2. Request the folder by the homeFolderID (-> GotFolderTree)
        -- 3. If the folder is a root folder then we are happy
        --    otherwise request the parent folder and go to step 3
        --
        -- Note: If the home folder is deeply nested this means several requests
        -- to the View Folder API endpoint.
        GotFolderTree result ->
            case result of
                Ok folderTree ->
                    case getFolder folderTree |> .parentID of
                        -- Folder is not root, load the parent folder
                        Just parentID ->
                            return model <| Cmd.map embed <| getFolderTreeCmd parentID

                        -- We have the root folder, initialise
                        Nothing ->
                            let
                                activeFolder =
                                    getHomeFolderForUserGroup model.ugid folderTree
                                        |> M.withDefault (getFolder folderTree)

                                mUserGroupForActiveFolder =
                                    if activeFolder.homeForUserGroup == M.map .id model.mUserGroup then
                                        model.mUserGroup

                                    else
                                        Nothing

                                userForActiveFolderCmd =
                                    case activeFolder.homeForUser of
                                        Just uid ->
                                            Cmd.map embed <| getUserForActiveFolderCmd uid

                                        Nothing ->
                                            Cmd.none

                                ( addModal, addModalCmd ) =
                                    AddModal.init activeFolder

                                ( deleteModal, deleteModalCmd ) =
                                    DeleteModal.init activeFolder

                                ( makeRootModal, makeRootModalCmd ) =
                                    MakeRootModal.init activeFolder

                                ( moveModal, moveModalCmd ) =
                                    MoveModal.init activeFolder

                                ( renameModal, renameModalCmd ) =
                                    RenameModal.init activeFolder
                            in
                            return
                                { model
                                    | sFolderTree = Success folderTree
                                    , mActiveFolder = Just activeFolder
                                    , mUserGroupForActiveFolder = mUserGroupForActiveFolder
                                    , mUserForActiveFolder = Nothing
                                    , mAddModal = Just addModal
                                    , mDeleteModal = Just deleteModal
                                    , mMakeRootModal = Just makeRootModal
                                    , mMoveModal = Just moveModal
                                    , mRenameModal = Just renameModal
                                }
                            <|
                                Cmd.batch
                                    [ addModalCmd
                                    , deleteModalCmd
                                    , makeRootModalCmd
                                    , moveModalCmd
                                    , renameModalCmd
                                    , userForActiveFolderCmd
                                    ]

                Err _ ->
                    singleton { model | sFolderTree = Failure, mActiveFolder = Nothing }

        FolderClicked folder ->
            let
                userCmd =
                    case folder.homeForUser of
                        Just uid ->
                            Cmd.map embed (getUserForActiveFolderCmd uid)

                        Nothing ->
                            Cmd.none

                userGroupCmd =
                    case folder.homeForUserGroup of
                        Just ugid ->
                            Cmd.map embed (getUserGroupForActiveFolderCmd ugid)

                        Nothing ->
                            Cmd.none
            in
            return
                { model
                    | mActiveFolder = Just folder
                    , mUserForActiveFolder = Nothing
                    , mUserGroupForActiveFolder = Nothing
                }
            <|
                Cmd.batch [ userCmd, userGroupCmd ]

        -- ----------------   Add folder   ----------------
        --
        AddClicked ->
            singleton <|
                case ( model.mAddModal, model.mActiveFolder ) of
                    ( Just addModal, Just activeFolder ) ->
                        let
                            newAddModal =
                                AddModal.show { addModal | parentFolder = activeFolder }
                        in
                        { model | mAddModal = Just newAddModal }

                    _ ->
                        model

        AddModalMsg modalMsg ->
            let
                onSuccessCmd =
                    Cmd.map embed <| Cmd.getUserGroup GotUserGroup model.ugid

                updateAddModal =
                    AddModal.update (embed << AddModalMsg) globals modalMsg onSuccessCmd

                ( newAddModal, cmd ) =
                    maybeUpdate updateAddModal model.mAddModal
            in
            return { model | mAddModal = newAddModal } cmd

        -- ----------------   Delete folder   ----------------
        --
        DeleteClicked ->
            singleton <|
                case ( model.mDeleteModal, model.mActiveFolder ) of
                    ( Just deleteModal, Just activeFolder ) ->
                        let
                            newDeleteModal =
                                DeleteModal.show { deleteModal | folder = activeFolder }
                        in
                        { model | mDeleteModal = Just newDeleteModal }

                    _ ->
                        model

        DeleteModalMsg modalMsg ->
            let
                onSuccessCmd =
                    Cmd.map embed <| Cmd.getUserGroup GotUserGroup model.ugid

                updateDeleteModal =
                    DeleteModal.update (embed << DeleteModalMsg) globals modalMsg onSuccessCmd

                ( newDeleteModal, cmd ) =
                    maybeUpdate updateDeleteModal model.mDeleteModal
            in
            return { model | mDeleteModal = newDeleteModal } cmd

        -- ----------------   Make folder root   ----------------
        --
        MakeRootClicked ->
            singleton <|
                case ( model.mMakeRootModal, model.mActiveFolder ) of
                    ( Just makeRootModal, Just activeFolder ) ->
                        let
                            newMakeRootModal =
                                MakeRootModal.show { makeRootModal | folder = activeFolder }
                        in
                        { model | mMakeRootModal = Just newMakeRootModal }

                    _ ->
                        model

        MakeRootModalMsg modalMsg ->
            let
                onSuccessCmd =
                    Cmd.map embed <| Cmd.getUserGroup GotUserGroup model.ugid

                updateMakeRootModal =
                    MakeRootModal.update (embed << MakeRootModalMsg) globals modalMsg onSuccessCmd

                ( newMakeRootModal, cmd ) =
                    maybeUpdate updateMakeRootModal model.mMakeRootModal
            in
            return { model | mMakeRootModal = newMakeRootModal } cmd

        -- ----------------   Move folder   ----------------
        --
        MoveClicked ->
            case ( model.mMoveModal, model.mActiveFolder ) of
                ( Just moveModal, Just activeFolder ) ->
                    let
                        mParentID =
                            fromStatus model.sFolderTree
                                |> M.andThen (getParentFolder activeFolder)
                                |> M.map .id

                        ( newMoveModal, cmd ) =
                            MoveModal.show (embed << MoveModalMsg)
                                { moveModal
                                    | folder = activeFolder
                                    , mOrigParentID = mParentID
                                    , newParentID = M.withDefault "" mParentID
                                }
                    in
                    return { model | mMoveModal = Just newMoveModal } cmd

                _ ->
                    singleton model

        MoveModalMsg modalMsg ->
            let
                onSuccessCmd =
                    Cmd.map embed <| Cmd.getUserGroup GotUserGroup model.ugid

                updateMoveModal =
                    MoveModal.update (embed << MoveModalMsg) globals modalMsg onSuccessCmd

                ( newMoveModal, cmd ) =
                    maybeUpdate updateMoveModal model.mMoveModal
            in
            return { model | mMoveModal = newMoveModal } cmd

        -- ----------------   Rename folder   ----------------
        --
        RenameClicked ->
            singleton <|
                case ( model.mRenameModal, model.mActiveFolder ) of
                    ( Just renameModal, Just activeFolder ) ->
                        let
                            newRenameModal =
                                RenameModal.show
                                    { renameModal
                                        | folder = activeFolder
                                        , newFolderName = activeFolder.name
                                    }
                        in
                        { model | mRenameModal = Just newRenameModal }

                    _ ->
                        model

        RenameModalMsg modalMsg ->
            let
                onSuccessCmd =
                    Cmd.map embed <| Cmd.getUserGroup GotUserGroup model.ugid

                updateRenameModal =
                    RenameModal.update (embed << RenameModalMsg) globals modalMsg onSuccessCmd

                ( newRenameModal, cmd ) =
                    maybeUpdate updateRenameModal model.mRenameModal
            in
            return { model | mRenameModal = newRenameModal } cmd


view : (Msg -> msg) -> Model -> Html msg
view embed model =
    div [] <|
        case model.sFolderTree of
            Loading ->
                [ h4 [] [ text "Loading ..." ] ]

            Failure ->
                [ h4 [] [ text "Failure ..." ] ]

            Success folders ->
                [ Grid.row []
                    [ Grid.col [ Col.sm12, Col.md6 ]
                        [ ul [ class "folder-list" ]
                            [ Html.map embed <| viewFolderList model folders ]
                        ]
                    , Grid.col [ Col.sm12, Col.md6, Col.orderXsFirst, Col.orderMdLast ]
                        (case model.mActiveFolder of
                            Just folder ->
                                [ Html.map embed <|
                                    viewFolderDetails
                                        folder
                                        folders
                                        model.mUserGroupForActiveFolder
                                        model.mUserForActiveFolder
                                ]

                            Nothing ->
                                []
                        )
                    ]
                ]
                    ++ M.values
                        [ Maybe.map (AddModal.view <| embed << AddModalMsg) model.mAddModal
                        , Maybe.map (DeleteModal.view <| embed << DeleteModalMsg) model.mDeleteModal
                        , Maybe.map (MakeRootModal.view <| embed << MakeRootModalMsg) model.mMakeRootModal
                        , Maybe.map (MoveModal.view <| embed << MoveModalMsg) model.mMoveModal
                        , Maybe.map (RenameModal.view <| embed << RenameModalMsg) model.mRenameModal
                        ]


viewFolderDetails : Folder -> FolderTree Folder -> Maybe UserGroup -> Maybe User -> Html Msg
viewFolderDetails folder tree mUserGroup mUser =
    let
        mLinkToUser =
            M.map
                (\user ->
                    p []
                        [ text "Home folder for user "
                        , a [ href ("/adminonly/page/useradmin/" ++ user.id) ]
                            [ text <|
                                M.withDefault ("UserID: " ++ user.id)
                                    (stringNonEmpty <| String.trim <| user.firstName ++ " " ++ user.secondName)
                            ]
                        ]
                )
                mUser

        mLinkToUserGroup =
            M.map
                (\ug ->
                    p []
                        [ text "Home folder for user group "
                        , a [ href ("/adminonly/page/companyadmin/" ++ ug.id) ]
                            [ text <| M.withDefault ("UserGroupID: " ++ ug.id) (stringNonEmpty ug.name) ]
                        ]
                )
                mUserGroup

        mMakeRootButton =
            if not (folderIsRoot folder tree) then
                Just <|
                    Button.button
                        [ Button.danger
                        , Button.attrs [ class "ml-sm-2", onClick MakeRootClicked ]
                        ]
                        [ text "Make root" ]

            else
                Nothing
    in
    div [ class "mb-3" ] <|
        M.values
            [ Just <| h1 [ class "mb-3" ] [ text <| folderDisplayName folder ]
            , mLinkToUser
            , mLinkToUserGroup
            , Just <|
                Button.button
                    [ Button.primary
                    , Button.attrs [ onClick AddClicked ]
                    ]
                    [ text "Add subfolder" ]
            , Just <|
                Button.button
                    [ Button.primary
                    , Button.attrs [ class "ml-sm-2", onClick MoveClicked ]
                    ]
                    [ text "Move" ]
            , Just <|
                Button.button
                    [ Button.primary
                    , Button.attrs [ class "ml-sm-2", onClick RenameClicked ]
                    ]
                    [ text "Rename" ]
            , mMakeRootButton
            , Just <|
                Button.button
                    [ Button.danger
                    , Button.attrs [ class "ml-sm-2", onClick DeleteClicked ]
                    ]
                    [ text "Delete" ]
            ]


viewFolderList : Model -> FolderTree Folder -> Html Msg
viewFolderList model (FolderTree node children) =
    let
        url =
            "/adminonly/page/companyadmin/" ++ model.ugid ++ "#folders"

        displayItem name =
            if Just node == model.mActiveFolder then
                strong [] [ text name ]

            else
                a [ href url, onClick (FolderClicked node) ] [ text name ]
    in
    li [] <|
        [ text <| ite (L.isEmpty children) "• " "▼ " -- \u{2022} \u{25BC}
        , displayItem <| folderDisplayNameForTree node
        ]
            ++ ite (L.isEmpty children)
                []
                [ ul [] <| L.map (viewFolderList model) children ]
