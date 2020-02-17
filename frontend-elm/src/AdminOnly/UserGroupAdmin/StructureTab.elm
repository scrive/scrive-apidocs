module AdminOnly.UserGroupAdmin.StructureTab exposing
    ( Model
    , Msg
    , init
    , setUserGroupID
    , tabName
    , update
    , view
    )

import Html exposing (Html, a, div, h4, li, strong, text, ul)
import Html.Attributes exposing (class, href)
import Http
import Json.Decode as D exposing (Decoder)
import List as L
import Utils exposing (Status(..), ite)


type alias Model =
    { ugid : String
    , sStructure : Status (Structure Node)
    }


type Structure node
    = Structure node (List (Structure node))


type alias Node =
    { name : String
    , ugid : String
    }


type Msg
    = GotStructure (Result Http.Error (Structure Node))


tabName : String
tabName =
    "structure"


init : String -> ( Model, Cmd Msg )
init ugid =
    let
        model =
            { ugid = ugid
            , sStructure = Loading
            }
    in
    ( model, getStructureCmd model )


getStructureCmd : Model -> Cmd Msg
getStructureCmd model =
    Http.get
        { url = "/adminonly/companyadmin/getstructure/" ++ model.ugid
        , expect =
            Http.expectJson GotStructure
                (D.field "user_group_structure" structureDecoder)
        }


setUserGroupID : String -> Model -> ( Model, Cmd Msg )
setUserGroupID ugid model0 =
    let
        model =
            { model0 | ugid = ugid }
    in
    ( model, getStructureCmd model )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GotStructure result ->
            case result of
                Ok structure ->
                    ( { model | sStructure = Success structure }, Cmd.none )

                Err _ ->
                    ( { model | sStructure = Failure }, Cmd.none )


view : Model -> Html Msg
view model =
    div [] <|
        case model.sStructure of
            Loading ->
                [ h4 [] [ text "Loading ..." ] ]

            Failure ->
                [ h4 [] [ text "Failure ..." ] ]

            Success structure ->
                [ ul [ class "user-group-structure" ] [ viewStructure model structure ] ]


viewStructure : Model -> Structure Node -> Html Msg
viewStructure model (Structure node children) =
    let
        url =
            "/adminonly/page/companyadmin/" ++ node.ugid ++ "#structure"

        displayItem item =
            if node.ugid == model.ugid then
                strong [] [ text item ]

            else
                a [ href url ] [ text item ]
    in
    li [] <|
        [ text <| ite (L.isEmpty children) "• " "▼ " -- \u{2022} \u{25BC}
        , displayItem <| node.name ++ "(" ++ node.ugid ++ ")"
        ]
            ++ (if L.isEmpty children then
                    []

                else
                    [ ul [] <| L.map (viewStructure model) children ]
               )


structureDecoder : Decoder (Structure Node)
structureDecoder =
    D.map2 Structure
        (D.field "group" <|
            D.map2 Node
                (D.field "name" D.string)
                (D.field "user_group_id" D.string)
        )
        (D.field "children" <| D.list <| D.lazy (\_ -> structureDecoder))
