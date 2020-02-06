module Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields.FaviconField exposing (Config, Msg, State, initialize, selectFileMsg, update, view)

import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Component.Input.Icon as InputImage
import Either exposing (Either(..))
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src, style)


type alias Config =
    InputImage.Config


type alias Msg =
    InputImage.Msg


type alias State =
    InputImage.State


selectFileMsg : Msg
selectFileMsg =
    InputImage.selectFileMsg


initialize : Config -> ( State, Cmd Msg )
initialize =
    InputImage.initialize


update : Msg -> State -> ( State, Cmd (Either Never Msg) )
update =
    InputImage.update


view : State -> Html Msg
view image =
    Form.row
        []
        [ Form.colLabel
            [ Col.sm3, Col.md3, Col.lg3 ]
            [ text "Favicon" ]
        , Form.col
            [ Col.sm9, Col.md9, Col.lg9 ]
            [ InputImage.view image
            , div
                [ class "mt-sm-2"
                ]
                [ img
                    [ class "p-sm-2"
                    , style "max-width" "100%"
                    , style "max-height" "100px"
                    , src image
                    ]
                    []
                ]
            ]
        ]
