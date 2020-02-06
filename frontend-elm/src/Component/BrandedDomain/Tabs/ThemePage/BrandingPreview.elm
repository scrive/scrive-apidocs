module Component.BrandedDomain.Tabs.ThemePage.BrandingPreview exposing (Config, Init, Msg, State, UpdateHandler, ViewHandler, initialize, previewHandlers, update, view)

import Component.Preview.Login as Login
import Component.Preview.Theme as Preview
import Component.Theme.Data exposing (Theme)
import Component.UserGroup.Tabs.ThemePage.BrandingPreview as UserGroup
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
    UserGroup.previewHandlers
        ++ [ ( "Login", Login.view )
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
