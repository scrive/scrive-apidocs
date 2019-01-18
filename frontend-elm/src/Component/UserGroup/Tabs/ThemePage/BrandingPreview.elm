module Component.UserGroup.Tabs.ThemePage.BrandingPreview exposing (Config, Init, Msg, State, UpdateHandler, ViewHandler, initialize, previewHandlers, update, view)

import Component.Preview.Email as Email
import Component.Preview.Service as Service
import Component.Preview.SignView as SignView
import Component.Preview.Theme as Preview
import Component.Theme.Data exposing (Theme)
import Either exposing (Either(..))
import Html exposing (Html)


type alias Config =
    ()


type alias State =
    Preview.State


type alias Msg =
    Preview.Msg


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either Never Msg) )


type alias ViewHandler =
    Theme -> State -> Html Msg


type alias Init =
    Config -> ( State, Cmd Msg )


previewHandlers : List ( String, Preview.PreviewHandler )
previewHandlers =
    [ ( "Email", Email.view )
    , ( "Signview", SignView.view )
    , ( "Service", Service.view )
    ]


initialize : Init
initialize () =
    Preview.initialize
        { previewHandlers = previewHandlers
        }


update : UpdateHandler
update =
    Preview.update


view : ViewHandler
view =
    Preview.view
