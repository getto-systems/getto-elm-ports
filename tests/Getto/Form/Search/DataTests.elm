module Getto.Form.Search.DataTests exposing (..)

import Getto.I18n as I18n
import Getto.Form as Form
import Getto.Form.Edit.Field as EditField
import Getto.Form.Search.Field as Field
import Getto.Form.Search.Data as Data
import Getto.Form.Search.Prepend as Prepend
import Getto.Form.Search.Border as Border
import Getto.Form.Search.Style as Style
import Getto.View.Fa as Fa

import Dict
import List.Extra

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

type Msg
  = Sort String

emptySort =
  { current = Nothing
  , default = Nothing
  , columns = Dict.empty
  }

sortA =
  { current = Just ( "a", Field.ASC )
  , default = Just ( "a", Field.ASC )
  , columns = [ ( "a", Field.ASC  ) ] |> Dict.fromList
  }

emptyModel = { rows = [] }

plainModel =
  { rows =
    [ { a = "a1", b = "b1", c = "c1" }
    , { a = "a2", b = "b2", c = "c2" }
    , { a = "a3", b = "b3", c = "c3" }
    ]
  }

unionModel =
  { data =
    { length = 3
    }
  , rows =
    [ { a = "a1", b = ["b1","b2","b3"] }
    , { a = "a2", b = ["b1","b2"] }
    , { a = "a3", b = [] }
    ]
  }

data model = ( model, model.rows )

suite : Test
suite = describe "pager"
  [ describe "plain"
    [ test "normal" <|
      \_ ->
        let
          cells =
            [ Data.cell "a" Border.none
              ( Style.none [ "head A" |> Form.text ]
              , Style.none [ .a >> H.text ]
              , Prepend.none
              )
            , Data.cell "b" Border.none
              ( Style.none [ "head B" |> Form.text ]
              , Style.none [ .b >> H.text ]
              , Prepend.none
              )
            , Data.cell "c" Border.none
              ( Style.none [ "head C" |> Form.text ]
              , Style.none [ .c >> H.text ]
              , Prepend.none
              )
            ]
        in
          (emptySort, data plainModel) |> Data.table Sort cells |> Expect.equal
            (H.div []
              [ H.table [ A.attribute "_fixedhead" "div-auto-size: none; rows: 1" ]
                [ H.thead []
                  [ H.tr []
                    [ H.th [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "head A" ]
                    , H.th [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "head B" ]
                    , H.th [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "head C" ]
                    ]
                  ]
                , H.tbody []
                  [ H.tr []
                    [ H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "a1" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "b1" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "c1" ]
                    ]
                  , H.tr []
                    [ H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "a2" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "b2" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "c2" ]
                    ]
                  , H.tr []
                    [ H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "a3" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "b3" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "c3" ]
                    ]
                  ]
                ]
              ]
            )

    , test "border" <|
      \_ ->
        let
          cells =
            [ Data.cell "a" Border.left
              ( Style.none [ "head A" |> Form.text ]
              , Style.none [ .a >> H.text ]
              , Prepend.none
              )
            , Data.cell "b" Border.left
              ( Style.none [ "head B" |> Form.text ]
              , Style.none [ .b >> H.text ]
              , Prepend.none
              )
            , Data.cell "c" Border.right
              ( Style.none [ "head C" |> Form.text ]
              , Style.none [ .c >> H.text ]
              , Prepend.none
              )
            ]
        in
          (emptySort, data plainModel) |> Data.table Sort cells |> Expect.equal
            (H.div []
              [ H.table [ A.attribute "_fixedhead" "div-auto-size: none; rows: 1" ]
                [ H.thead []
                  [ H.tr []
                    [ H.th [ A.class "border-left",  A.colspan 1, A.rowspan 1 ] [ H.text "head A" ]
                    , H.th [ A.class "border-left",  A.colspan 1, A.rowspan 1 ] [ H.text "head B" ]
                    , H.th [ A.class "border-right", A.colspan 1, A.rowspan 1 ] [ H.text "head C" ]
                    ]
                  ]
                , H.tbody []
                  [ H.tr []
                    [ H.td [ A.class "border-left",  A.colspan 1, A.rowspan 1 ] [ H.text "a1" ]
                    , H.td [ A.class "border-left",  A.colspan 1, A.rowspan 1 ] [ H.text "b1" ]
                    , H.td [ A.class "border-right", A.colspan 1, A.rowspan 1 ] [ H.text "c1" ]
                    ]
                  , H.tr []
                    [ H.td [ A.class "border-left",  A.colspan 1, A.rowspan 1 ] [ H.text "a2" ]
                    , H.td [ A.class "border-left",  A.colspan 1, A.rowspan 1 ] [ H.text "b2" ]
                    , H.td [ A.class "border-right", A.colspan 1, A.rowspan 1 ] [ H.text "c2" ]
                    ]
                  , H.tr []
                    [ H.td [ A.class "border-left",  A.colspan 1, A.rowspan 1 ] [ H.text "a3" ]
                    , H.td [ A.class "border-left",  A.colspan 1, A.rowspan 1 ] [ H.text "b3" ]
                    , H.td [ A.class "border-right", A.colspan 1, A.rowspan 1 ] [ H.text "c3" ]
                    ]
                  ]
                ]
              ]
            )

    , test "style" <|
      \_ ->
        let
          cells =
            [ Data.cell "a" Border.left
              ( Style.left  [ "head A" |> Form.text ]
              , Style.right [ .a >> H.text ]
              , Prepend.none
              )
            , Data.cell "b" Border.none
              ( Style.none  [ "head B" |> Form.text ]
              , Style.right [ .b >> H.text ]
              , Prepend.none
              )
            , Data.cell "c" Border.none
              ( Style.none  [ "head C" |> Form.text ]
              , Style.right [ .c >> H.text ]
              , Prepend.none
              )
            ]
        in
          (emptySort, data plainModel) |> Data.table Sort cells |> Expect.equal
            (H.div []
              [ H.table [ A.attribute "_fixedhead" "div-auto-size: none; rows: 1" ]
                [ H.thead []
                  [ H.tr []
                    [ H.th [ A.class "is-left border-left", A.colspan 1, A.rowspan 1 ] [ H.text "head A" ]
                    , H.th [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "head B" ]
                    , H.th [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "head C" ]
                    ]
                  ]
                , H.tbody []
                  [ H.tr []
                    [ H.td [ A.class "is-right border-left", A.colspan 1, A.rowspan 1 ] [ H.text "a1" ]
                    , H.td [ A.class "is-right", A.colspan 1, A.rowspan 1 ] [ H.text "b1" ]
                    , H.td [ A.class "is-right", A.colspan 1, A.rowspan 1 ] [ H.text "c1" ]
                    ]
                  , H.tr []
                    [ H.td [ A.class "is-right border-left", A.colspan 1, A.rowspan 1 ] [ H.text "a2" ]
                    , H.td [ A.class "is-right", A.colspan 1, A.rowspan 1 ] [ H.text "b2" ]
                    , H.td [ A.class "is-right", A.colspan 1, A.rowspan 1 ] [ H.text "c2" ]
                    ]
                  , H.tr []
                    [ H.td [ A.class "is-right border-left", A.colspan 1, A.rowspan 1 ] [ H.text "a3" ]
                    , H.td [ A.class "is-right", A.colspan 1, A.rowspan 1 ] [ H.text "b3" ]
                    , H.td [ A.class "is-right", A.colspan 1, A.rowspan 1 ] [ H.text "c3" ]
                    ]
                  ]
                ]
              ]
            )

    , test "prepend" <|
      \_ ->
        let
          cells =
            [ Data.cell "a" Border.none
              ( Style.none [ "head A" |> Form.text ]
              , Style.none [ .a >> H.text ]
              , Prepend.td <| Style.none [ "prepend A" |> Form.text ]
              )
            , Data.cell "b" Border.none
              ( Style.none [ "head B" |> Form.text ]
              , Style.none [ .b >> H.text ]
              , Prepend.none
              )
            , Data.cell "c" Border.none
              ( Style.none [ "head C" |> Form.text ]
              , Style.none [ .c >> H.text ]
              , Prepend.none
              )
            ]
        in
          (emptySort, data plainModel) |> Data.table Sort cells |> Expect.equal
            (H.div []
              [ H.table [ A.attribute "_fixedhead" "div-auto-size: none; rows: 2" ]
                [ H.thead []
                  [ H.tr []
                    [ H.th [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "head A" ]
                    , H.th [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "head B" ]
                    , H.th [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "head C" ]
                    ]
                  , H.tr [ A.class "border-top-single" ]
                    [ H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "prepend A" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] []
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] []
                    ]
                  ]
                , H.tbody []
                  [ H.tr []
                    [ H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "a1" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "b1" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "c1" ]
                    ]
                  , H.tr []
                    [ H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "a2" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "b2" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "c2" ]
                    ]
                  , H.tr []
                    [ H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "a3" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "b3" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "c3" ]
                    ]
                  ]
                ]
              ]
            )

    , test "sort" <|
      \_ ->
        let
          cells =
            [ Data.cell "a" Border.none
              ( Style.none [ "head A" |> Form.text ]
              , Style.none [ .a >> H.text ]
              , Prepend.none
              )
            , Data.cell "b" Border.none
              ( Style.none [ "head B" |> Form.text ]
              , Style.none [ .b >> H.text ]
              , Prepend.none
              )
            , Data.cell "c" Border.none
              ( Style.none [ "head C" |> Form.text ]
              , Style.none [ .c >> H.text ]
              , Prepend.none
              )
            ]
        in
          (sortA, data plainModel) |> Data.table Sort cells |> Expect.equal
            (H.div []
              [ H.table [ A.attribute "_fixedhead" "div-auto-size: none; rows: 1" ]
                [ H.thead []
                  [ H.tr []
                    [ H.th [ A.class "", A.colspan 1, A.rowspan 1 ]
                      [ H.a
                        [ A.href "#"
                        , E.onClick (Sort "a")
                        , A.class "is-active"
                        ]
                        [ H.text "head A", H.text " ", Fa.solid "sort-down" [] ]
                      ]
                    , H.th [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "head B" ]
                    , H.th [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "head C" ]
                    ]
                  ]
                , H.tbody []
                  [ H.tr []
                    [ H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "a1" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "b1" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "c1" ]
                    ]
                  , H.tr []
                    [ H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "a2" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "b2" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "c2" ]
                    ]
                  , H.tr []
                    [ H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "a3" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "b3" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "c3" ]
                    ]
                  ]
                ]
              ]
            )

    , test "group" <|
      \_ ->
        let
          cells =
            [ Data.cell "a" Border.none
              ( [ "head A" |> Form.text ] |> Style.none
              , [ .a >> H.text ] |> Style.none
              , Prepend.none
              )
            , Data.group ([ "group A" |> Form.text ] |> Style.none) Border.none
              [ Data.cell "b" Border.none
                ( [ "head B - group A" |> Form.text ] |> Style.none
                , [ .b >> H.text ] |> Style.none
                , Prepend.none
                )
              , Data.cell "c" Border.none
                ( [ "head C - group A" |> Form.text ] |> Style.none
                , [ .c >> H.text ] |> Style.none
                , Prepend.none
                )
              ]
            , Data.group ([ "group B" |> Form.text ] |> Style.none) Border.none
              [ Data.cell "b" Border.none
                ( [ "head B - group B" |> Form.text ] |> Style.none
                , [ .b >> H.text ] |> Style.none
                , Prepend.none
                )
              , Data.group ([ "group C" |> Form.text ] |> Style.none) Border.none
                [ Data.cell "b" Border.none
                  ( [ "head B - group C" |> Form.text ] |> Style.none
                  , [ .b >> H.text ] |> Style.none
                  , Prepend.none
                  )
                , Data.cell "c" Border.none
                  ( [ "head C - group C" |> Form.text ] |> Style.none
                  , [ .c >> H.text ] |> Style.none
                  , Prepend.none
                  )
                ]
              ]
            ]
        in
          (emptySort, data plainModel) |> Data.table Sort cells |> Expect.equal
            (H.div []
              [ H.table [ A.attribute "_fixedhead" "div-auto-size: none; rows: 3" ]
                [ H.thead []
                  [ H.tr []
                    [ H.th [ A.class "", A.colspan 1, A.rowspan 3 ] [ H.text "head A" ]
                    , H.th [ A.class "", A.colspan 2, A.rowspan 1 ] [ H.text "group A" ]
                    , H.th [ A.class "", A.colspan 3, A.rowspan 1 ] [ H.text "group B" ]
                    ]
                  , H.tr []
                    [ H.th [ A.class "", A.colspan 1, A.rowspan 2 ] [ H.text "head B - group A" ]
                    , H.th [ A.class "", A.colspan 1, A.rowspan 2 ] [ H.text "head C - group A" ]
                    , H.th [ A.class "", A.colspan 1, A.rowspan 2 ] [ H.text "head B - group B" ]
                    , H.th [ A.class "", A.colspan 2, A.rowspan 1 ] [ H.text "group C" ]
                    ]
                  , H.tr []
                    [ H.th [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "head B - group C" ]
                    , H.th [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "head C - group C" ]
                    ]
                  ]
                , H.tbody []
                  [ H.tr []
                    [ H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "a1" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "b1" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "c1" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "b1" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "b1" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "c1" ]
                    ]
                  , H.tr []
                    [ H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "a2" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "b2" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "c2" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "b2" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "b2" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "c2" ]
                    ]
                  , H.tr []
                    [ H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "a3" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "b3" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "c3" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "b3" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "b3" ]
                    , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "c3" ]
                    ]
                  ]
                ]
              ]
            )
    ]
  , test "union" <|
    \_ ->
      let
        cells =
          [ Data.cell "a" Border.none
            ( Style.none [ "head A" |> Form.text ]
            , Style.none [ .a >> H.text ]
            , Prepend.none
            )
          , Data.union .b (.data >> .length) Border.none
            ( Style.none [ "head B" |> Form.text ]
            , Style.none [ (\part row -> String.append row.a part |> H.text) ]
            )
          ]
      in
        (emptySort, data unionModel) |> Data.table Sort cells |> Expect.equal
          (H.div []
            [ H.table [ A.attribute "_fixedhead" "div-auto-size: none; rows: 1" ]
              [ H.thead []
                [ H.tr []
                  [ H.th [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "head A" ]
                  , H.th [ A.class "", A.colspan 3, A.rowspan 1 ] [ H.text "head B" ]
                  ]
                ]
              , H.tbody []
                [ H.tr []
                  [ H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "a1" ]
                  , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "a1b1" ]
                  , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "a1b2" ]
                  , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "a1b3" ]
                  ]
                , H.tr []
                  [ H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "a2" ]
                  , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "a2b1" ]
                  , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "a2b2" ]
                  , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] []
                  ]
                , H.tr []
                  [ H.td [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "a3" ]
                  , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] []
                  , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] []
                  , H.td [ A.class "", A.colspan 1, A.rowspan 1 ] []
                  ]
                ]
              ]
            ]
          )

  , test "empty" <|
    \_ ->
      let
        cells =
          [ Data.cell "a" Border.none
            ( Style.none [ "head A" |> Form.text ]
            , Style.none [ .a >> H.text ]
            , Prepend.none
            )
          , Data.cell "b" Border.none
            ( Style.none [ "head B" |> Form.text ]
            , Style.none [ .b >> H.text ]
            , Prepend.none
            )
          , Data.cell "c" Border.none
            ( Style.none [ "head C" |> Form.text ]
            , Style.none [ .c >> H.text ]
            , Prepend.none
            )
          ]
      in
        (emptySort, data emptyModel) |> Data.table Sort cells |> Expect.equal
          (H.div []
            [ H.table [ A.attribute "_fixedhead" "div-auto-size: none; rows: 1" ]
              [ H.thead []
                [ H.tr []
                  [ H.th [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "head A" ]
                  , H.th [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "head B" ]
                  , H.th [ A.class "", A.colspan 1, A.rowspan 1 ] [ H.text "head C" ]
                  ]
                ]
              , H.tbody []
                [ H.tr []
                  [ H.td [ A.class "", A.colspan 3, A.rowspan 1 ]
                    [ H.p [ A.class "alert" ] [ H.text "I18n.t: load.empty" ] ]
                  ]
                ]
              ]
            ]
          )
  ]
