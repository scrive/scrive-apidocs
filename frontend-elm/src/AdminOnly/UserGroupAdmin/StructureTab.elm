module AdminOnly.UserGroupAdmin.StructureTab exposing
    ( Model
    , Msg
    , init
    , setUserGroupID
    , tabName
    , update
    , view
    )

import AdminOnly.Types.UserGroup.Cmd as Cmd
import AdminOnly.Types.UserGroup.Structure as Structure exposing (UserGroupNode, UserGroupStructure(..))
import AdminOnly.UserGroupAdmin.CreateUserGroupModal as CreateUserGroupModal
import Bootstrap.Alert exposing (children)
import Bootstrap.Button as Button
import Bootstrap.Form.Radio exposing (create)
import Debug
import FlashMessage
import Html exposing (Html, a, button, div, h4, hr, li, span, strong, text, ul)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D exposing (Decoder)
import List as L
import Maybe as M
import Return exposing (..)
import Utils exposing (..)


type alias Model =
    { ugid : String
    , sStructure : Status UserGroupStructure
    , createUserGroupModal : CreateUserGroupModal.Model
    }


type Msg
    = GotStructure (Result Http.Error UserGroupStructure)
    | CreateChildGroupClicked
    | CreateUserGroupMsg CreateUserGroupModal.Msg
    | CreateUserGroupGotResponse Cmd.UserGroupCreated
    | PromoteGroupClicked


tabName : String
tabName =
    "structure"


init : (Msg -> msg) -> String -> Return msg Model
init embed ugid =
    let
        model =
            { ugid = ugid
            , sStructure = Loading
            , createUserGroupModal = CreateUserGroupModal.init
            }
    in
    return model <| Cmd.map embed <| Cmd.getUserGroupStructure GotStructure ugid


setUserGroupID : (Msg -> msg) -> String -> Model -> Return msg Model
setUserGroupID embed ugid model =
    return { model | ugid = ugid } <| Cmd.map embed <| Cmd.getUserGroupStructure GotStructure ugid


update : (Msg -> msg) -> Globals msg -> Msg -> Model -> Return msg Model
update embed globals msg model =
    case msg of
        GotStructure result ->
            singleton <|
                case result of
                    Ok structure ->
                        { model | sStructure = Success structure }

                    Err _ ->
                        { model | sStructure = Failure }

        CreateUserGroupMsg modalMsg ->
            let
                ( modalModel, modalCmd ) =
                    CreateUserGroupModal.update (embed << CreateUserGroupMsg) (embed << CreateUserGroupGotResponse) globals modalMsg model.createUserGroupModal
            in
            return { model | createUserGroupModal = modalModel } modalCmd

        CreateUserGroupGotResponse result ->
            case result of
                Ok response ->
                    return model <|
                        Cmd.batch
                            [ globals.flashMessage <| FlashMessage.success <| "User group was created"
                            , Cmd.map embed <| Cmd.getUserGroupStructure GotStructure model.ugid
                            ]

                Err err ->
                    return model <|
                        globals.flashMessage
                            (FlashMessage.error <| "Failed to create user group: " ++ err)

        CreateChildGroupClicked ->
            let
                modal =
                    CreateUserGroupModal.showWithConfig <| CreateUserGroupModal.CreateChildUserGroup model.ugid
            in
            singleton { model | createUserGroupModal = modal }

        PromoteGroupClicked ->
            singleton <|
                case model.sStructure of
                    Success structure ->
                        let
                            mCurrentUserGroup =
                                Structure.lookupUserGroup model.ugid structure

                            name =
                                "ROOT - "
                                    ++ (mCurrentUserGroup
                                            |> M.map .name
                                            |> M.withDefault ""
                                       )

                            modal =
                                CreateUserGroupModal.showWithConfig <| CreateUserGroupModal.CreateRootUserGroup model.ugid name
                        in
                        { model | createUserGroupModal = modal }

                    _ ->
                        model


view : (Msg -> msg) -> Model -> Html msg
view embed model =
    div [] <|
        case model.sStructure of
            Loading ->
                [ h4 [] [ text "Loading ..." ] ]

            Failure ->
                [ h4 [] [ text "Failure ..." ] ]

            Success structure ->
                let
                    isCurrentGroupRoot =
                        (Structure.getRootUserGroup structure).ugid == model.ugid

                    isCurrentGroupBillable =
                        (Structure.getRootUserGroup structure).isBillable

                    promoteButton =
                        Button.button [ Button.warning, Button.attrs [ onClick PromoteGroupClicked, class "mr-2" ] ] [ text "Upgrade to billable company" ]

                    createChildButton =
                        Button.button [ Button.success, Button.attrs [ onClick CreateChildGroupClicked, class "mr-2" ] ] [ text "Create child user group" ]

                    actionButtons =
                        case ( isCurrentGroupRoot, isCurrentGroupBillable ) of
                            ( True, False ) ->
                                promoteButton

                            ( True, True ) ->
                                createChildButton

                            _ ->
                                span [] [ createChildButton, promoteButton ]
                in
                [ Html.map embed <|
                    div []
                        [ actionButtons
                        , hr [] []
                        , ul [ class "user-group-structure" ]
                            [ viewStructure model structure
                            ]
                        , CreateUserGroupModal.view CreateUserGroupMsg model.createUserGroupModal
                        ]
                ]


viewStructure : Model -> UserGroupStructure -> Html msg
viewStructure model (UserGroupStructure node children) =
    let
        url =
            "/adminonly/page/companyadmin/" ++ node.ugid ++ "#details"

        displayItem item =
            if node.ugid == model.ugid then
                strong [] [ text item ]

            else
                a [ href url ] [ text item ]

        isBillable =
            if node.isBillable then
                " [billable]"

            else
                ""
    in
    li [] <|
        [ text <| ite (L.isEmpty children) "• " "▼ " -- \u{2022} \u{25BC}
        , displayItem <| node.name ++ " (" ++ node.ugid ++ ")" ++ isBillable
        ]
            ++ (if L.isEmpty children then
                    []

                else
                    [ ul [] <| L.map (viewStructure model) children ]
               )
