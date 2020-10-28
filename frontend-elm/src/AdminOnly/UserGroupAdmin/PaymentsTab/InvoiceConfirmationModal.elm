module AdminOnly.UserGroupAdmin.PaymentsTab.InvoiceConfirmationModal exposing (Model, Msg, init, show, update, view)

import Bootstrap.Button as Button
import Bootstrap.Modal as Modal
import Html exposing (Html, h3, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type alias Model =
    { modalVisibility : Modal.Visibility
    , userGroupId : String
    }


type Msg
    = CloseModal


init : String -> Model
init ugid =
    { modalVisibility = Modal.hidden
    , userGroupId = ugid
    }


show : Model -> Model
show model =
    { model | modalVisibility = Modal.shown }


update : (Msg -> msg) -> Msg -> Model -> Model
update embed msg model =
    case msg of
        CloseModal ->
            { model | modalVisibility = Modal.hidden }


view : (Msg -> msg) -> Model -> Html msg
view embed model =
    Modal.config CloseModal
        |> Modal.large
        |> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [ text "Salesforce action action required" ]
        |> Modal.body []
            [ p [ class "bg-warning", class "p-1", class "rounded" ]
                [ text "You're setting Invoicing type to Invoice. Copy and paste the User Group ID into Salesforce."
                ]
            , h3 [ class "text-center", class "border", class "border-dark", class "rounded", class "p-2", class "w-50", class "m-auto" ] [ text model.userGroupId ]
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.secondary
                , Button.attrs [ onClick CloseModal ]
                ]
                [ text "Ok" ]
            ]
        |> Modal.view model.modalVisibility
        |> Html.map embed
