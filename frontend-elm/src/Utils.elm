module Utils exposing (..)

import Either exposing (Either(..))
import EnumExtra as Enum exposing (Enum)
import FlashMessage exposing (FlashMessage)
import Html exposing (Html, text)
import Http
import Json.Decode as D exposing (Decoder)
import List as L
import Maybe as M
import Parser as P exposing ((|.))
import Platform.Cmd as Cmd
import Result as R
import Task
import Time exposing (Month(..), Posix)
import Time.DateTime exposing (toPosix)
import Time.Iso8601 exposing (toDateTime)
import Url
import Url.Builder as UB exposing (QueryParameter)


type alias Globals msg =
    { xtoken : String
    , cdnBaseUrl : String
    , flashMessage : FlashMessage -> Cmd msg
    , setPageUrlFromModel : Cmd msg
    , gotoUserGroupUsers : String -> Cmd msg
    , gotoDaveDocument : String -> Cmd msg
    , gotoUser : String -> Cmd msg
    , gotoUserGroup : String -> Cmd msg
    , gotoUserGroupAdminTab : Cmd msg
    , gotoUserAdminTab : Cmd msg
    , gotoBrandedDomain : String -> Cmd msg
    , gotoBrandedDomainsTab : Cmd msg
    }


type alias PageUrl =
    { path : List String
    , query : List QueryParameter
    , fragment : Maybe String
    }


mapGlobals : (msg1 -> msg2) -> Globals msg1 -> Globals msg2
mapGlobals liftMessage globals =
    let
        mapper =
            Cmd.map liftMessage
    in
    { xtoken = globals.xtoken
    , cdnBaseUrl = globals.cdnBaseUrl
    , flashMessage = mapper << globals.flashMessage
    , setPageUrlFromModel = mapper globals.setPageUrlFromModel
    , gotoUserGroupUsers = mapper << globals.gotoUserGroupUsers
    , gotoDaveDocument = mapper << globals.gotoDaveDocument
    , gotoUser = mapper << globals.gotoUser
    , gotoUserGroup = mapper << globals.gotoUserGroup
    , gotoUserGroupAdminTab = mapper globals.gotoUserGroupAdminTab
    , gotoUserAdminTab = mapper globals.gotoUserAdminTab
    , gotoBrandedDomain = mapper << globals.gotoBrandedDomain
    , gotoBrandedDomainsTab = mapper globals.gotoBrandedDomainsTab
    }


maybeUpdate : (state -> (state, Cmd cmd)) -> Maybe state -> (Maybe state, Cmd cmd)
maybeUpdate update mState = case mState of
  Just state -> let (newState, cmd) = update state
                in (Just newState, cmd)
  Nothing -> (Nothing, Cmd.none)


emptyPageUrl : PageUrl
emptyPageUrl =
    { path = [], fragment = Nothing, query = [] }


perform : msg -> Cmd msg
perform =
    Task.perform identity << Task.succeed


formBody : Globals msg -> List ( String, String ) -> Http.Body
formBody globals object =
    (object ++ [ ( "xtoken", globals.xtoken ) ])
        |> L.map
            (\( name, value ) ->
                Url.percentEncode name
                    ++ "="
                    ++ Url.percentEncode value
            )
        |> String.join "&"
        |> Http.stringBody "application/x-www-form-urlencoded; charset=UTF-8"


flip : (a -> b -> c) -> (b -> a -> c)
flip f b a =
    f a b


type Status a
    = Failure
    | Loading
    | Success a


fromStatus : Status a -> Maybe a
fromStatus status =
    case status of
        Loading ->
            Nothing

        Failure ->
            Nothing

        Success a ->
            Just a


statusMap : (a -> b) -> Status a -> Status b
statusMap modify status =
    case status of
        Loading ->
            Loading

        Failure ->
            Failure

        Success a ->
            Success <| modify a


statusMerge3 : Status a -> Status b -> Status c -> Status ( a, b, c )
statusMerge3 sA sB sC =
    statusMerge2 (statusMerge2 sA sB) sC |> statusMap (\( ( a, b ), c ) -> ( a, b, c ))


statusMerge2 : Status a -> Status b -> Status ( a, b )
statusMerge2 sA sB =
    case ( sA, sB ) of
        ( Failure, _ ) ->
            Failure

        ( _, Failure ) ->
            Failure

        ( Loading, _ ) ->
            Loading

        ( _, Loading ) ->
            Loading

        ( Success a, Success b ) ->
            Success ( a, b )


datetimeDecoder : Decoder Posix
datetimeDecoder =
    D.string
        |> D.andThen
            (\s ->
                case toDateTime s of
                    Ok datetime ->
                        D.succeed <| toPosix datetime

                    _ ->
                        D.fail "Cannot parse datetime."
            )


firstJust : List (Maybe a) -> Maybe a
firstJust maybes =
    L.head <| L.filterMap identity maybes


stringNonEmpty : String -> Maybe String
stringNonEmpty str =
    ite (String.isEmpty str) Nothing (Just str)

viewError : Html msg
viewError =
    text "Unexpected error, please inform bugs@scrive.com."


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadStatus code ->
            "Server returned HTTP status " ++ String.fromInt code

        Http.BadBody message ->
            "Error parsing response body: " ++ message

        _ ->
            "Encountered HTTP error"



-- if-then-else for avoiding elm-format. Beware, that the branches are not
-- executed lazily anymore.


ite : Bool -> a -> a -> a
ite cond thenValue elseValue =
    if cond then
        thenValue

    else
        elseValue


isInteger : String -> Bool
isInteger numberStr =
    let
        parser =
            P.succeed ()
                |. P.chompIf (\c -> Char.isDigit c && c /= '0')
                |. P.chompWhile Char.isDigit
                |. P.end
    in
    P.run parser numberStr == Ok ()


isJust : Maybe a -> Bool
isJust mA =
    case mA of
        Nothing ->
            False

        Just _ ->
            True


boolToJson : Bool -> String
boolToJson bool =
    ite bool "true" "false"


handleSub : model -> (model -> Maybe sub) -> (sub -> model) -> (sub -> ( sub, Cmd msg )) -> ( model, Cmd msg )
handleSub model getMSub setSub updateSub =
    getMSub model
        |> M.map (updateSub >> (\( m, cmd ) -> ( setSub m, cmd )))
        |> M.withDefault ( model, Cmd.none )



-- SORTING


type alias Sorting column =
    { column : column
    , order : SortOrder
    }


type SortOrder
    = SOAsc
    | SODesc


toggleSorting : column -> Sorting column -> Maybe (Sorting column) -> Sorting column
toggleSorting column default mSorting =
    mSorting
        |> M.withDefault default
        |> (\sorting ->
                if sorting.column == column then
                    { sorting | order = toggleOrder sorting.order }

                else
                    { sorting | column = column, order = SOAsc }
           )


toggleOrder : SortOrder -> SortOrder
toggleOrder order =
    case order of
        SOAsc ->
            SODesc

        SODesc ->
            SOAsc


encodeSortOrder : SortOrder -> String
encodeSortOrder order =
    case order of
        SOAsc ->
            "ascending"

        SODesc ->
            "descending"


enumSortOrder : Enum SortOrder
enumSortOrder =
    Enum.makeEnum [ SOAsc, SODesc ] encodeSortOrder encodeSortOrder


sortIndicator : column -> Sorting column -> Html msg
sortIndicator column sorting =
    if sorting.column == column then
        case sorting.order of
            SOAsc ->
                text " ▼"

            SODesc ->
                text " ▲"

    else
        text ""


mSearchToQuery : Maybe String -> List QueryParameter
mSearchToQuery mSearch =
    mSearch
        |> M.map (\s -> [ UB.string "search" s ])
        |> M.withDefault []


mSortingToQuery : Enum column -> Maybe (Sorting column) -> List QueryParameter
mSortingToQuery enumSortColumn mSorting =
    mSorting
        |> M.map
            (\s ->
                [ UB.string "sort_by" <| Enum.toString enumSortColumn s.column
                , UB.string "order" <| Enum.toString enumSortOrder s.order
                ]
            )
        |> M.withDefault []


mSortingFromSortByOrder : Enum column -> Maybe String -> Maybe String -> Maybe (Sorting column)
mSortingFromSortByOrder enumSortColumn mSortByStr mOrderStr =
    let
        mColumn =
            mSortByStr |> M.andThen (Enum.findEnumValue enumSortColumn >> R.toMaybe)

        mOrder =
            mOrderStr |> M.andThen (Enum.findEnumValue enumSortOrder >> R.toMaybe)
    in
    mColumn |> M.andThen (\column -> mOrder |> M.map (\order -> Sorting column order))



-- TIME


viewMonth : Month -> String
viewMonth month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12

decodeJust : Decoder (Maybe a) -> Decoder a
decodeJust d =
  d |> D.andThen (\ma -> case ma of
          Just a -> D.succeed a
          Nothing -> D.fail "Decoder (Maybe a) return Nothing but we expected Just.")

decodeFullDict : Enum k -> (k -> Decoder v) -> Decoder (Enum.Dict k v)
decodeFullDict e d =
  let verify dict =
        if List.all (\k -> Enum.member k dict) (Enum.allValues e)
        then D.succeed dict
        else D.fail "Decoded dict not full!"
  in decodeDict e d |> D.andThen verify

decodeDict : Enum k -> (k -> Decoder v) -> Decoder (Enum.Dict k v)
decodeDict e d =
  let go : List k -> Decoder (Enum.Dict k v)
      go ks = case ks of
        [] -> D.succeed <| Enum.empty e
        k :: ks_ -> D.maybe (d k)
          |> D.andThen (\mv -> case mv of
              Just v -> D.map (Enum.insert k v) <| go ks_
              Nothing -> go ks_)
  in go <| Enum.allValues e
