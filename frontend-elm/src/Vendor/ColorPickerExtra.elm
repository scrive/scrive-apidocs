-- Copied from https://github.com/simonh1000/elm-colorpicker


module Vendor.ColorPickerExtra exposing
    ( State, Msg, empty, update, view, color2Hex, hex2Color
    , initWithId
    )

{-| An Elm library to help you implement a color picker tool.

@docs State, Msg, empty, update, view, color2Hex, hex2Color

The main picker is for saturation and lightness, while the sliders below are for hie and opacity respectively.

-}

import Color exposing (Color)
import Hex
import Html exposing (Html, div)
import Html.Attributes as Attrs
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (..)
import Svg.Attributes as SvgAttrs exposing (offset, stopColor, stopOpacity, x, x1, x2, y, y1, y2)
import Svg.Events as SvgEvents


widgetWidth =
    200


{-| Opaque type. Needs to be added to your model. You will also need to store a `Color` in your model

    type alias Model =
        { myColour : Color
        , colorPicker : ColorPicker.Model
        }

-}
type State
    = State Model


{-| Initial ColorPicker state

    init =
        { myColour = Color.red
        , colorPicker = ColorPicker.empty
        }

-}
empty : State
empty =
    State blankModel


initWithId : String -> State
initWithId id =
    State
        { blankModel
            | id = id
        }


{-| The model stores the hue because dark colours has indistinguihsable hues.
But the user could easily change their hex between updates so we need to check that this has _probably_
not happened.
-}
type alias Model =
    { mouseTarget : MouseTarget
    , hue : Maybe Float -- 0.0 .. 1.0
    , id : String
    }


blankModel : Model
blankModel =
    { mouseTarget = Unpressed
    , hue = Nothing
    , id = "default"
    }


type MouseTarget
    = Unpressed
    | SatLight Float -- hue, main area
    | HueSlider -- 1st slider
    | OpacitySlider Float -- hue, 2nd slider



-- ------------------------------
-- U P D A T E
-- ------------------------------


{-| Opaque type. These messages are handled by `ColorPicker.update`
-}
type Msg
    = OnMouseDown MouseTarget MouseInfo
    | OnMouseMove MouseTarget MouseInfo
    | OnClick MouseTarget MouseInfo
    | OnMouseUp
    | NoOp -- used to prevent bubbling of messages


{-| On each update, ColorPicker returns its model and (where appropriate) the new selected colo(u)r.

    ColorPickermsg msg ->
        let
            (cp, col) =
                ColorPicker.update msg model.colorPicker
        in
            { model | colorPicker = cp, myColor = Maybe.withDefault model.myColor col }

-}
update : Msg -> Color -> State -> ( State, Maybe Color )
update message col (State model) =
    update_ message col model
        |> Tuple.mapFirst State


update_ : Msg -> Color -> Model -> ( Model, Maybe Color )
update_ message col model =
    let
        handleMouseMove mouseTarget mouseInfo =
            if mouseInfo.mousePressed && model.mouseTarget == mouseTarget then
                ( setHue mouseTarget mouseInfo model, calcNewColour mouseTarget mouseInfo )

            else if not mouseInfo.mousePressed && model.mouseTarget == mouseTarget then
                -- Mouse wss unpressed whie not over a target
                ( setMouseTarget Unpressed model, Nothing )

            else
                ( model, Nothing )

        calcNewColour mouseTarget =
            case mouseTarget of
                SatLight hue ->
                    Just << calcSatLight col (Maybe.withDefault hue model.hue)

                HueSlider ->
                    Just << calcHue col

                OpacitySlider hue ->
                    -- \mouseInfo -> model.hue |> Maybe.map (\h -> calcOpacity col h mouseInfo)
                    Just << calcOpacity col (Maybe.withDefault hue model.hue)

                Unpressed ->
                    \_ -> Nothing
    in
    case message of
        OnMouseDown mouseTarget mouseInfo ->
            ( model
                |> setMouseTarget mouseTarget
                |> setHue mouseTarget mouseInfo
            , calcNewColour mouseTarget mouseInfo
            )

        OnMouseMove mouseTarget mouseInfo ->
            handleMouseMove mouseTarget mouseInfo

        OnClick mouseTarget mouseInfo ->
            ( setHue mouseTarget mouseInfo model, calcNewColour mouseTarget mouseInfo )

        OnMouseUp ->
            ( setMouseTarget Unpressed model, Nothing )

        NoOp ->
            ( model, Nothing )


setMouseTarget : MouseTarget -> Model -> Model
setMouseTarget mouseTarget model =
    { model | mouseTarget = mouseTarget }


setHue : MouseTarget -> MouseInfo -> Model -> Model
setHue mouseTarget mouseInfo model =
    case mouseTarget of
        HueSlider ->
            { model | hue = Just <| toFloat mouseInfo.x / widgetWidth }

        SatLight hue ->
            { model | hue = model.hue |> Maybe.withDefault hue |> Just }

        OpacitySlider hue ->
            { model | hue = model.hue |> Maybe.withDefault hue |> Just }

        _ ->
            model


calcSatLight : Color -> Float -> MouseInfo -> Color
calcSatLight col currHue { x, y, mousePressed } =
    let
        hsla =
            Color.toHsla col
    in
    { hsla
        | hue = currHue
        , saturation = toFloat x / widgetWidth
        , lightness = 1 - toFloat y / 150
    }
        |> Color.fromHsla


calcHue : Color -> MouseInfo -> Color
calcHue col { x, mousePressed } =
    let
        ({ saturation, lightness, alpha } as hsla) =
            Color.toHsla col

        hue =
            toFloat x / widgetWidth

        newCol =
            -- Enable 'escape from black'
            if saturation == 0 && lightness < 1 then
                { hsla | hue = hue, saturation = 0.5, lightness = 128 }

            else
                { hsla | hue = hue }
    in
    newCol |> Color.fromHsla


calcOpacity : Color -> Float -> MouseInfo -> Color
calcOpacity col _ { x, mousePressed } =
    let
        hsla =
            Color.toHsla col
    in
    { hsla | alpha = toFloat x / widgetWidth }
        |> Color.fromHsla



-- ------------------------------
-- V I E W
-- ------------------------------


{-| Renders the color picker on screen
-}
view : Color -> State -> Html Msg
view col (State model) =
    let
        hsla =
            Color.toHsla col

        hue =
            Maybe.withDefault hsla.hue model.hue

        colCss =
            Color.hsl hue 1 0.5
                |> Color.toCssString
    in
    div
        []
        [ div pickerStyles
            [ satLightPalette model.id hue colCss model.mouseTarget
            , pickerIndicator col
            ]
        , div (pickerStyles ++ sliderContainerStyles "hue")
            [ huePalette model.id model.mouseTarget
            , hueMarker hue
            ]

        -- , div (checkedBkgStyles ++ pickerStyles ++ sliderContainerStyles "opacity")
        --     [ opacityPalette hue model
        --     , alphaMarker hsla.alpha
        --     ]
        ]


satLightPalette : String -> Float -> String -> MouseTarget -> Svg Msg
satLightPalette id hue colCss mouseTarget =
    svg
        [ SvgAttrs.width (String.fromInt widgetWidth)
        , SvgAttrs.height "150"
        , SvgAttrs.class "main-picker"
        , SvgAttrs.display "block"
        ]
        [ defs
            []
            [ linearGradient
                [ SvgAttrs.id ("pickerSaturation-" ++ id) ]
                [ stop [ offset "0", stopColor "#808080", stopOpacity "1" ] []
                , stop [ offset "1", stopColor "#808080", stopOpacity "0" ] []
                ]
            , linearGradient
                [ SvgAttrs.id ("pickerBrightness-" ++ id), x1 "0", y1 "0", x2 "0", y2 "1" ]
                [ stop [ offset "0", stopColor "#fff", stopOpacity "1" ] []
                , stop [ offset "0.499", stopColor "#fff", stopOpacity "0" ] []
                , stop [ offset "0.5", stopColor "#000", stopOpacity "0" ] []
                , stop [ offset "1", stopColor "#000", stopOpacity "1" ] []
                ]
            ]
        , rect [ SvgAttrs.width (String.fromInt widgetWidth), SvgAttrs.height "150", SvgAttrs.fill colCss, SvgAttrs.id "picker" ] []
        , rect
            [ SvgAttrs.width (String.fromInt widgetWidth)
            , SvgAttrs.height "150"
            , SvgAttrs.fill <| "url(#pickerSaturation-" ++ id ++ ")"
            ]
            []
        , rect
            ([ SvgAttrs.width (String.fromInt widgetWidth)
             , SvgAttrs.height "150"
             , SvgAttrs.fill <| "url(#pickerBrightness-" ++ id ++ ")"
             ]
                ++ svgDragAttrs mouseTarget (SatLight hue) (OnMouseMove <| SatLight hue)
            )
            []
        ]


{-| pick saturation & lightness
-}
pickerIndicator : Color -> Html Msg
pickerIndicator col =
    let
        adjustment =
            4

        { saturation, lightness } =
            Color.toHsla col

        borderColor =
            if lightness > 0.95 then
                "#cccccc"

            else
                "#ffffff"

        cx_ =
            saturation * widgetWidth - adjustment |> round |> String.fromInt

        cy_ =
            150 - lightness * 150 - adjustment |> round |> String.fromInt
    in
    div
        [ Attrs.style "position" "absolute"
        , Attrs.style "top" (cy_ ++ "px")
        , Attrs.style "left" (cx_ ++ "px")
        , Attrs.style "border-radius" "100%"
        , Attrs.style "border" ("2px solid " ++ borderColor)
        , Attrs.style "width" "6px"
        , Attrs.style "height" "6px"
        , Attrs.style "pointer-events" "none"
        ]
        []



-- --------------------------
-- HUE
-- --------------------------


huePalette : String -> MouseTarget -> Svg Msg
huePalette id mouseTarget =
    let
        mkStop ( os, sc ) =
            stop [ offset os, stopColor sc, stopOpacity "1" ] []

        stops : List ( String, String )
        stops =
            [ ( "0%", "#FF0000" )
            , ( "17%", "#FF00FF" )
            , ( "33%", "#0000FF" )
            , ( "50%", "#00FFFF" )
            , ( "66%", "#00FF00" )
            , ( "83%", "#FFFF00" )
            , ( "100%", "#FF0000" )
            ]
    in
    svg
        (SvgAttrs.class "hue-picker" :: sliderStyles)
        [ defs []
            [ linearGradient
                [ SvgAttrs.id ("gradient-hsv-" ++ id), x1 "100%", y1 "0%", x2 "0%", y2 "0%" ]
                (stops |> List.map mkStop)
            ]
        , rect
            ([ x "0"
             , y "0"
             , SvgAttrs.width (String.fromInt widgetWidth)
             , SvgAttrs.height "100%"
             , SvgAttrs.fill <| "url(#gradient-hsv-" ++ id ++ ")"
             ]
                ++ svgDragAttrs mouseTarget HueSlider (OnMouseMove HueSlider)
            )
            []
        ]



-- --------------------------
-- OPACITY
-- --------------------------


opacityPalette : Float -> Model -> Svg Msg
opacityPalette hue model =
    let
        mkCol op =
            Color.hsla hue 1 0.5 op
                |> Color.toCssString

        grad =
            "linear-gradient(0.25turn, " ++ mkCol 0 ++ ", " ++ mkCol 1 ++ ")"

        overlay =
            [ Attrs.style "background" grad
            , Attrs.style "height" "100%"
            , Attrs.style "width" "100%"
            ]

        mouseTarget =
            OpacitySlider hue
    in
    div (overlay ++ htmlDragAttrs model.mouseTarget mouseTarget (OnMouseMove mouseTarget)) []


{-| Select the hue
-}
hueMarker : Float -> Html Msg
hueMarker lastHue =
    let
        correction =
            4

        xVal =
            -- shift by 4px to center on selected color
            (lastHue * widgetWidth - correction) |> round |> String.fromInt
    in
    div (Attrs.style "left" (xVal ++ "px") :: markerAttrs) []


alphaMarker : Float -> Html Msg
alphaMarker alpha =
    let
        correction =
            4

        xVal =
            -- shift by 4px to center on selected color
            (alpha * widgetWidth - correction) |> round |> String.fromInt
    in
    div (Attrs.style "left" (xVal ++ "px") :: markerAttrs) []



-- --------------------------
-- Event handlers
-- --------------------------


htmlDragAttrs : MouseTarget -> MouseTarget -> (MouseInfo -> Msg) -> List (Svg.Attribute Msg)
htmlDragAttrs currMouseTgt thisTgt onMoveMsg =
    let
        common =
            [ Html.Events.on "mousedown" <| Decode.map (OnMouseDown thisTgt) decodeMouseInfo
            , Html.Events.onMouseUp OnMouseUp
            , onClickHtml <| OnClick thisTgt
            ]
    in
    if currMouseTgt == thisTgt then
        -- for the current target, we also want to track moves
        Html.Events.on "mousemove" (Decode.map onMoveMsg decodeMouseInfo) :: common

    else
        common


onClickHtml : (MouseInfo -> Msg) -> Html.Attribute Msg
onClickHtml msgCreator =
    Html.Events.on "click" (Decode.map msgCreator decodeMouseInfo)


svgDragAttrs : MouseTarget -> MouseTarget -> (MouseInfo -> Msg) -> List (Svg.Attribute Msg)
svgDragAttrs currMouseTgt thisTgt onMoveMsg =
    let
        common =
            [ onMouseDownSvg <| OnMouseDown thisTgt
            , SvgEvents.onMouseUp OnMouseUp
            , onClickSvg <| OnClick thisTgt
            ]
    in
    if currMouseTgt == thisTgt then
        -- for the current target, we also want to track moves
        onMouseMoveSvg onMoveMsg :: common

    else
        common


onMouseDownSvg : (MouseInfo -> Msg) -> Svg.Attribute Msg
onMouseDownSvg msgCreator =
    SvgEvents.on "mousedown" (Decode.map msgCreator decodeMouseInfo)


onMouseMoveSvg : (MouseInfo -> Msg) -> Svg.Attribute Msg
onMouseMoveSvg msgCreator =
    SvgEvents.on "mousemove" (Decode.map msgCreator decodeMouseInfo)


onClickSvg : (MouseInfo -> Msg) -> Svg.Attribute Msg
onClickSvg msgCreator =
    SvgEvents.on "click" (Decode.map msgCreator decodeMouseInfo)


{-| Hack to prevent SVG click events bubble through to rest of app. SVG does not have an onWithOptions
-}
bubblePreventer : Html.Attribute Msg
bubblePreventer =
    Html.Events.stopPropagationOn "click" <| Decode.succeed ( NoOp, True )



-- MouseInfo`


type alias MouseInfo =
    { x : Int
    , y : Int
    , mousePressed : Bool
    }


decodeMouseInfo : Decoder MouseInfo
decodeMouseInfo =
    Decode.map3 MouseInfo
        (Decode.field "offsetX" Decode.int)
        (Decode.field "offsetY" Decode.int)
        (Decode.field "buttons" Decode.int |> Decode.map ((/=) 0))



-- Html Attributes


sliderContainerStyles : String -> List (Html.Attribute msg)
sliderContainerStyles name =
    [ Attrs.style "width" (String.fromInt widgetWidth ++ "px")
    , Attrs.style "height" "12px"
    , Attrs.style "marginTop" "8px"
    , Attrs.class <| "color-picker-slider " ++ name
    ]


checkedBkgStyles : List (Html.Attribute msg)
checkedBkgStyles =
    [ Attrs.style "background-size" "12px 12px"
    , Attrs.style "background-position" "0 0, 0 6px, 6px -6px, -6px 0px"
    , Attrs.style "background-image" "linear-gradient(45deg, #808080 25%, transparent 25%), linear-gradient(-45deg, #808080 25%, transparent 25%), linear-gradient(45deg, transparent 75%, #808080 75%), linear-gradient(-45deg, transparent 75%, #808080 75%)"
    ]


pickerStyles : List (Html.Attribute msg)
pickerStyles =
    [ Attrs.style "cursor" "crosshair"
    , Attrs.style "position" "relative"
    , Attrs.style "border" "1pt solid gray"
    ]


markerAttrs : List (Html.Attribute msg)
markerAttrs =
    [ Attrs.style "position" "absolute"
    , Attrs.style "top" "1px"
    , Attrs.style "bottom" "1px"
    , Attrs.style "border" "1px solid #ddd"
    , Attrs.style "background-color" "#ffffff"
    , Attrs.style "width" "6px"
    , -- this is essental to enable dragging
      Attrs.style "pointer-events" "none"
    ]



-- SVG Attributes


sliderStyles : List (Svg.Attribute msg)
sliderStyles =
    [ SvgAttrs.width (String.fromInt widgetWidth)
    , SvgAttrs.height "100%"
    , SvgAttrs.display "block"
    ]



-- Colour Helpers


{-| Converts `Color` to `String` (with preceding `#`).
Used internally and exposed because the public alternative is a library with multiple dependencies.
-}
color2Hex : Color -> String
color2Hex col =
    let
        { red, green, blue } =
            Color.toRgba col
    in
    [ red, green, blue ]
        |> List.map ((*) 255 >> round >> padHex)
        |> String.join ""
        |> (++) "#"


{-| Converts `String` to `Color`.
Used internally and exposed because the public alternative is a library with multiple dependencies.
-}
hex2Color : String -> Maybe Color
hex2Color s =
    if String.startsWith "#" s then
        hex2Color <| String.dropLeft 1 s

    else
        case String.length s of
            3 ->
                let
                    -- expand 'def' to 'ddeeff'
                    duplicatedDigits =
                        String.split "" s |> List.concatMap (\x -> [ x, x ]) |> String.join ""
                in
                hex2Color duplicatedDigits

            6 ->
                let
                    hex =
                        String.toLower s

                    conv begin end =
                        String.slice begin end >> Hex.fromString >> Result.map (toFloat >> (\x -> x / 255))
                in
                Result.map3 Color.rgb (conv 0 2 hex) (conv 2 4 hex) (conv 4 6 hex)
                    |> Result.toMaybe

            _ ->
                Nothing


padHex : Int -> String
padHex x =
    if x < 16 then
        "0" ++ Hex.toString x

    else
        Hex.toString x
