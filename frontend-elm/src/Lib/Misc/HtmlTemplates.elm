module Lib.Misc.HtmlTemplates exposing (evaluateHtmlTemplate)

import Html exposing (Html)

import Html.Parser exposing (Node(..))
import Html.Parser.Util

-- | Templates needn't be well-formed html, so we wrap them in a span.
parseHtmlTemplate : String -> Maybe Node
parseHtmlTemplate str =
  "<span>" ++ str ++ "</span>"
  |>  Html.Parser.run
  |> Result.toMaybe
  |> Maybe.andThen
    (\nodes -> case nodes of
        [span] -> Just span
        _ -> Nothing)

-- | Substitute all the nodes in the tree for which the substitution is defined.
maybeSubstitute : (Node -> Maybe Node) -> Node -> Node
maybeSubstitute sub node =
  case sub node of
    Just replacement -> replacement
    Nothing -> case node of
      Element str attrs nodes ->
        Element str attrs (List.map (maybeSubstitute sub) nodes)
      _ -> node

substituteSpanByClassWithText : {key : String, value : String} -> Node -> Node
substituteSpanByClassWithText {key, value} =
  maybeSubstitute (
    \node -> case node of
      Element str attrs _ ->
        if str == "span" && List.member ("class", key) attrs
        then Just <| Element str attrs [Text value]
        else Nothing
      _ -> Nothing
  )

evaluateHtmlTemplate : List {key : String, value : String} -> String -> Html msg
evaluateHtmlTemplate subs str =
  case parseHtmlTemplate str of
    Nothing -> Html.text str
    Just node ->
      List.foldl substituteSpanByClassWithText node subs
      |> List.singleton
      |> Html.Parser.Util.toVirtualDom
      |> List.head >> Maybe.withDefault (Html.text str)
