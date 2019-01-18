module Component.Input.Icon exposing (Config, Image, Msg(..), State, initialize, selectFileMsg, update, view)

import Bootstrap.Button as Button
import Either exposing (Either(..))
import File exposing (File)
import File.Select as FileSelect
import Html exposing (Html, text)
import Html.Attributes exposing (type_)
import Html.Events exposing (onClick)
import Task


type alias Image =
    String


type alias Config =
    State


type Msg
    = ImageMsg Image
    | FileMsg File
    | SelectFileMsg


type alias State =
    Image


selectFileMsg : Msg
selectFileMsg =
    SelectFileMsg


initialize : Config -> ( State, Cmd Msg )
initialize state =
    ( state, Cmd.none )


update : Msg -> State -> ( State, Cmd (Either Never Msg) )
update msg1 state1 =
    case msg1 of
        ImageMsg image ->
            ( image, Cmd.none )

        FileMsg file ->
            let
                cmd =
                    Task.perform (Right << ImageMsg) <|
                        File.toUrl file
            in
            ( state1, cmd )

        SelectFileMsg ->
            let
                cmd =
                    FileSelect.files [ "image/png" ] <|
                        \f _ -> Right <| FileMsg f
            in
            ( state1, cmd )


view : State -> Html Msg
view _ =
    Button.button
        [ Button.secondary
        , Button.attrs
            [ onClick SelectFileMsg
            , type_ "button"
            ]
        ]
        [ text "Select image" ]
