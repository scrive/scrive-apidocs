-- Extra bits for Bootstrap.Pagination
-- Based on: https://github.com/rundis/elm-bootstrap/blob/5.2.0/src/Bootstrap/Pagination.elm

module Vendor.PaginationExtra exposing ( view, itemsPerPage, pageNumToOffset )


import Bootstrap.General.HAlign as HAlign
import Bootstrap.Pagination as Pagination exposing (Config)
import Bootstrap.Pagination.Item as Item
import Html exposing (Html, div, text)
import Html.Attributes exposing (attribute)
import Html.Events as Events
import Json.Decode as D
import List as L
import Maybe as M
import Maybe.Extra as M


view : Int -> Maybe Int -> (Int -> msg) -> Html msg
view activePageNum mTotalItems selectedMsg =
    let
        totalItems =
            M.withDefault 0 mTotalItems

        totalPages =
            ceiling (toFloat totalItems / toFloat itemsPerPage)
    in
    if totalItems > 0 then
        Pagination.defaultConfig
            |> Pagination.ariaLabel "Pagination"
            |> Pagination.align HAlign.centerXs
            |> Pagination.small
            |> itemsList
                { selectedMsg = selectedMsg
                , activePageNum = activePageNum
                , totalPages = totalPages
                }
            |> Pagination.view

    else
        div [] []


pageNumToOffset : Int -> Int
pageNumToOffset pageNum =
    (pageNum - 1) * itemsPerPage


itemsPerPage : Int
itemsPerPage =
    100


type alias ListConfig msg =
    { selectedMsg : Int -> msg
    , activePageNum : Int
    , totalPages : Int
    }


-- A simplified version of Pagination.itemsList
itemsList : ListConfig msg -> Config msg -> Config msg
itemsList conf config =
    let
        mPrevPageNum =
            if conf.activePageNum > 1 then
                Just <| conf.activePageNum - 1

            else
                Nothing

        mNextPageNum =
            if conf.activePageNum < conf.totalPages then
                Just <| conf.activePageNum + 1

            else
                Nothing

        onClickPreventDefault msg =
            Events.custom "click" (D.map (\m -> { message = m
                                                , stopPropagation = False
                                                , preventDefault = True
                                                }
                                         ) (D.succeed msg))

        link mOnClickMsg children =
            Item.link
                ([ Html.Attributes.href "#"
                 , Html.Attributes.target "_self"
                 ]
                    ++ M.unwrap [] (\msg -> [ onClickPreventDefault msg ]) mOnClickMsg
                )
                children

        pagesAround =
            5

        minPageNum =
            max 1 (conf.activePageNum - pagesAround)

        maxPageNum =
            min conf.totalPages (conf.activePageNum + pagesAround)

        morePagesItem =
            Item.item
                |> Item.disabled True
                |> link Nothing [ text "..." ]

    in
    ([ Item.item
        |> Item.disabled (M.isNothing mPrevPageNum )
        |> link (M.map conf.selectedMsg mPrevPageNum) [ text "Previous" ]
     ]
        ++ (if minPageNum > 1 then [ morePagesItem ] else [])
        ++ List.map
            (\pageNum ->
                Item.item
                    |> Item.disabled (pageNum == conf.activePageNum)
                    |> link (Just <| conf.selectedMsg pageNum) [ text <| String.fromInt pageNum ]
            )
            (L.range minPageNum maxPageNum)
        ++ (if maxPageNum < conf.totalPages then [ morePagesItem ] else [])
        ++ [ Item.item
                |> Item.disabled (M.isNothing mNextPageNum)
                |> link (M.map conf.selectedMsg mNextPageNum) [ text "Next" ]
           ]
    )
        |> (\xs -> Pagination.items xs config)

