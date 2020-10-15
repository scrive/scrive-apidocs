module FlashMessage exposing
    ( FlashMessage(..)
    , Model
    , Msg
    , addFlashMessage
    , error
    , init
    , subscriptions
    , success
    , update
    , view
    )

import Bootstrap.Alert as Alert
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import List as L
import Return exposing (..)
import Time as T exposing (Posix)


type alias Model msg =
    { alerts : List AlertModel
    , time : Posix
    , alertNextID : Int
    , tomsg : Msg -> msg
    }


type alias AlertModel =
    { visibility : Alert.Visibility
    , flashType : FlashType
    , message : String
    , id : Int
    , start : Posix
    }


type Msg
    = AlertMsg Int Alert.Visibility
    | Tick Posix


type FlashType
    = Error
    | Success


type FlashMessage
    = FlashMessage FlashType String


error : String -> FlashMessage
error message =
    FlashMessage Error message


success : String -> FlashMessage
success message =
    FlashMessage Success message


init : (Msg -> msg) -> Return msg (Model msg)
init tomsg =
    singleton
        { alerts = []
        , alertNextID = 1
        , time = T.millisToPosix 0
        , tomsg = tomsg
        }


addFlashMessage : FlashMessage -> Model msg -> Model msg
addFlashMessage (FlashMessage flashType message) model =
    let
        newAlert =
            { visibility = Alert.shown
            , flashType = flashType
            , message = message
            , start = model.time
            , id = model.alertNextID
            }
    in
    { model
        | alerts = model.alerts ++ [ newAlert ]
        , alertNextID = model.alertNextID + 1
    }


update : Msg -> Model msg -> Return msg (Model msg)
update msg model =
    singleton <|
        case msg of
            AlertMsg id visibility ->
                { model | alerts = L.map (updateAlert id visibility) model.alerts }

            Tick time ->
                expireAlerts { model | time = time }


updateAlert : Int -> Alert.Visibility -> AlertModel -> AlertModel
updateAlert id visibility alert =
    if alert.id == id then
        { alert | visibility = visibility }

    else
        alert


alertIsExpired : Posix -> AlertModel -> Bool
alertIsExpired time alert =
    T.posixToMillis time > T.posixToMillis alert.start + 10 * 1000


expireAlerts : Model msg -> Model msg
expireAlerts model =
    { model
        | alerts = L.filter (not << alertIsExpired model.time) model.alerts
    }


subscriptions : Model msg -> Sub msg
subscriptions model =
    T.every 10000 (model.tomsg << Tick)


view : Model msg -> Html msg
view model =
    div [ class "flash" ]
        [ div [ class "container" ] <|
            L.map (viewAlert model) model.alerts
        ]


viewAlert : Model msg -> AlertModel -> Html msg
viewAlert model alert =
    Alert.config
        |> (case alert.flashType of
                Success ->
                    Alert.success

                Error ->
                    Alert.danger
           )
        |> Alert.dismissable (model.tomsg << AlertMsg alert.id)
        |> Alert.children
            [ text alert.message
            ]
        |> Alert.view alert.visibility
