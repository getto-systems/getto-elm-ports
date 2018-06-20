module Getto.Form.Search.Data exposing
  ( Cell
  , Content
  , Style(..)
  , CellStyle(..)
  , Border(..)
  , BorderSpec
  , Form
  , FormCell(..)
  , cell
  , container
  , group
  , union
  , parts
  , rows
  , table
  , form
  , edit
  , save
  , commit
  )

import Getto.I18n as I18n
import Getto.Form as Form
import Getto.Form.Edit as Edit
import Getto.Form.Edit.Field.Validate as Validate
import Getto.Form.Search.Field as Field
import Getto.View.Fa as Fa

import List.Extra

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E

type Cell model row msg
  = Cell  ( model -> Head msg,        List row -> model -> Column msg,        model -> Prepend msg )
  | Union ( model -> Head msg,        List row -> model -> Column msg,        model -> List (Prepend msg) )
  | Group ( model -> Head msg,        List row -> model -> List (Column msg), model -> List (Prepend msg) )
  | Rows  ( model -> List (Head msg), List row -> model -> Column msg,        model -> List (Prepend msg) )
  | Cells ( model -> List (Head msg), List row -> model -> List (Column msg), model -> List (Prepend msg) )

type alias Content a = ( List a, List Style )
type alias Styled msg = Content (Html msg)

type alias Spec          model msg = Content (Form.Content model msg)
type alias PartSpec part model msg = Content (part -> Form.Content model msg)

type Head msg
  = Head      BorderSpec String (Styled msg)
  | GroupHead BorderSpec (Styled msg) (List (Head msg))
  | UnionHead BorderSpec (Styled msg) Int
  | PartHead  BorderSpec (Styled msg)

type Prepend msg
  = Prepend BorderSpec (Styled msg) CellStyle

type CellStyle
  = TH
  | TD

type Column msg
  = Column      BorderSpec (List (Styled msg))
  | UnionColumn BorderSpec (List (List (Styled msg))) Int
  | RowsColumn  BorderSpec (List (List (Column msg))) Int

type Unit msg
  = Unit      BorderSpec (Styled msg)
  | EmptyUnit BorderSpec (Styled msg) Int
  | UnionUnit BorderSpec (List (Styled msg)) Int
  | RowsUnit  BorderSpec (List (List (Unit msg))) Int

type alias BorderSpec = ( Maybe Border, Maybe Border )

type Border
  = Single
  | Double

type Style
  = Valid Validate.State
  | Left
  | Center
  | Right
  | Top
  | Middle
  | Bottom
  | NoWrap
  | PreWrap
  | Fill

type FormCell model row msg sub
  = FormCell String (List String) BorderSpec
    ( Spec model msg
    , ( Spec row sub
      , Spec row sub
      )
    , ( Spec model msg, CellStyle )
    )
  | FormStaticCell String (List String) BorderSpec
    ( Spec model msg
    , Spec row sub
    , ( Spec model msg, CellStyle )
    )
  | FormButton BorderSpec (Spec row sub)
  | FormButtonSwap BorderSpec
    ( Spec row sub
    , Spec row sub
    )

type alias Form model row msg sub = ( (String -> String), List (FormCell model row msg sub) )


cell
  : String
  -> BorderSpec
  ->
    ( Spec model msg
    , Spec row msg
    , ( Spec model msg, CellStyle )
    )
  -> Cell model row msg
cell sortKey border ( head, column, (prepend,style) ) = Cell
  ( \model      -> Head    border sortKey (head |> toHtml model)
  , \rows model -> Column  border (rows |> List.map (\row -> column |> toHtml row))
  , \model      -> Prepend border (prepend |> toHtml model) style
  )

container
  :  (row -> Edit.Model a)
  -> (Edit.Model a -> sub -> msg)
  -> Form model (Edit.Model a) msg sub
  -> Cell model row msg
container prop msg (i18n,cells) = Cells
  ( \model ->
      cells |> List.map
        (\cell ->
          case cell of
            FormButton     border _ -> emptyHead border
            FormButtonSwap border _ -> emptyHead border
            FormCell       sortKey _ border ( head, _, _ ) -> Head border sortKey (head |> toHtml model)
            FormStaticCell sortKey _ border ( head, _, _ ) -> Head border sortKey (head |> toHtml model)
        )
  , \rows model ->
      cells |> List.map
        (\cell ->
          case cell of
            FormCell _ validationKeys border ( _, column, _ ) ->
              Column border (rows |> List.map (prop >> map msg (toEditHtml validationKeys i18n column)))

            FormStaticCell _ validationKeys border ( _, column, _ ) ->
              Column border (rows |> List.map (prop >> map msg (toSwapHtml (column, column))))

            FormButton border button ->
              Column border (rows |> List.map (prop >> map msg (toSwapHtml (button,button))))

            FormButtonSwap border button ->
              Column border (rows |> List.map (prop >> map msg (toSwapHtml button)))
        )
  , \model ->
      cells |> List.map
        (\cell ->
          case cell of
            FormButton     border _ -> emptyPrepend border
            FormButtonSwap border _ -> emptyPrepend border
            FormCell sortKey _ border ( _, _, (prepend,style) ) ->
              Prepend border (prepend |> toHtml model) style
            FormStaticCell sortKey _ border ( _, _, (prepend,style) ) ->
              Prepend border (prepend |> toHtml model) style
        )
  )

group : Spec model msg -> BorderSpec -> List (Cell model row msg) -> Cell model row msg
group head border cells = Group
  ( \model      -> GroupHead border (head |> toHtml model) (cells |> List.concatMap (cellHeads model))
  , \rows model -> cells |> List.concatMap (cellColumns rows model)
  , \model      -> cells |> List.concatMap (cellPrepends model)
  )

union
  :  (row -> List part)
  -> (model -> Int)
  -> BorderSpec
  ->
    ( Spec model msg
    , PartSpec part row msg
    )
  -> Cell model row msg
union prop toLength border ( head, column ) = Union
  ( \model      -> UnionHead    border (head |> toHtml model) (model |> toLength)
  , \rows model -> UnionColumn  border (column |> toUnionColumnHtml rows prop) (model |> toLength)
  , \model      -> emptyPrepend border |> List.repeat (model |> toLength)
  )

parts
  : (model -> List part)
  -> BorderSpec
  ->
    ( PartSpec part model msg
    , PartSpec part row msg
    , ( PartSpec part model msg, CellStyle )
    )
  -> Cell model row msg
parts prop border ( head, column, (prepend,style) ) = Cells
  ( \model      -> model |> prop |> List.map (\part -> PartHead border (head |> toPartHtml model part))
  , \rows model -> model |> prop |> List.map (\part -> Column   border (column |> toPartColumnHtml rows part))
  , \model      -> model |> prop |> List.map (\part -> Prepend  border (prepend |> toPartHtml model part) style)
  )

rows : (row -> List part) -> List (Cell model part msg) -> Cell model row msg
rows prop cells = Rows
  ( \model -> cells |> List.concatMap (cellHeads model)
  , \rows model ->
    let
      prepends = cells |> List.concatMap (cellPrepends model)
    in
      RowsColumn
        (prepends |> cellBorder)
        (rows |> List.map
          (\row ->
            cells |> List.concatMap (cellColumns (row |> prop) model)
          )
        )
        (prepends |> List.length)
  , \model -> cells |> List.concatMap (cellPrepends model)
  )


emptyHead : BorderSpec -> Head msg
emptyHead border = PartHead border ([],[])

emptyPrepend : BorderSpec -> Prepend msg
emptyPrepend border = Prepend border ([],[]) TD


cellBorder : List (Prepend msg) -> BorderSpec
cellBorder prepends =
  let
    borderStyle position (Prepend border _ _) = border |> position
  in
    ( prepends |> List.head       |> Maybe.andThen (borderStyle Tuple.first)
    , prepends |> List.Extra.last |> Maybe.andThen (borderStyle Tuple.second)
    )


toHtml : model -> Spec model msg -> Styled msg
toHtml model (contents, style) =
  ( contents |> List.map (\content -> model |> content)
  , style
  )

toEditHtml : List String -> (String -> String) -> ( Spec (Edit.Model a) msg, Spec (Edit.Model a) msg ) -> Edit.Model a -> Styled msg
toEditHtml validationKeys i18n column model =
  let
    state    = model |> Edit.validationState validationKeys
    messages = state |> Edit.validationMessages i18n

    (contents,style) = model |> toSwapHtml column
  in
    ( contents ++ (messages |> List.map (\message -> model |> message))
    , style ++ [Valid state]
    )

toSwapHtml : ( Spec (Edit.Model a) msg, Spec (Edit.Model a) msg ) -> Edit.Model a -> Styled msg
toSwapHtml button model =
  button
  |> Edit.swap model
  |> toHtml model

toPartHtml : model -> data -> PartSpec data model msg -> Styled msg
toPartHtml model data (contents, style) =
  ( contents |> List.map (\content -> model |> content data)
  , style
  )

toUnionColumnHtml : List row -> (row -> List part) -> PartSpec part row msg -> List (List (Styled msg))
toUnionColumnHtml rows prop spec = rows |> List.map
  (\row ->
    row |> prop |> List.map (\part -> spec |> toPartHtml row part)
  )

toPartColumnHtml : List row -> part -> PartSpec part row msg -> List (Styled msg)
toPartColumnHtml rows data spec = rows |> List.map (\row -> spec |> toPartHtml row data)


cellHeads : model -> Cell model row msg -> List (Head msg)
cellHeads model cell = model |>
  case cell of
    Cell  (toHead,_,_)  -> toHead >> List.singleton
    Union (toHead,_,_)  -> toHead >> List.singleton
    Group (toHead,_,_)  -> toHead >> List.singleton
    Rows  (toHeads,_,_) -> toHeads
    Cells (toHeads,_,_) -> toHeads

cellColumns : List row -> model -> Cell model row msg -> List (Column msg)
cellColumns rows model cell = model |>
  case cell of
    Cell  (_,toColumn,_)  -> toColumn  rows >> List.singleton
    Union (_,toColumn,_)  -> toColumn  rows >> List.singleton
    Group (_,toColumns,_) -> toColumns rows
    Rows  (_,toColumn,_)  -> toColumn  rows >> List.singleton
    Cells (_,toColumns,_) -> toColumns rows

cellPrepends : model -> Cell model row msg -> List (Prepend msg)
cellPrepends model cell = model |>
  case cell of
    Cell  (_,_,toPrepend)  -> toPrepend >> List.singleton
    Union (_,_,toPrepends) -> toPrepends
    Group (_,_,toPrepends) -> toPrepends
    Rows  (_,_,toPrepends) -> toPrepends
    Cells (_,_,toPrepends) -> toPrepends


map : (row -> msg -> super) -> (row -> Styled msg) -> row -> Styled super
map msg contents row = row |> contents |> mapStyled (msg row)

mapStyled : (a -> b) -> Styled a -> Styled b
mapStyled msg (contents,style) = ( contents |> List.map (H.map msg), style )


table : (String -> msg) -> List (Cell model row msg) -> ( Field.Sort, ( model, List row ) ) -> Html msg
table msg cells (sort,data) =
  let
    (head,body,prepend) = data |> build cells

    thead =
      List.append
        (head |> toHeadRows msg sort |> List.map (H.tr []))
        (prepend |> toPrependRows)

    fixedhead = String.join "; "
      [ "div-auto-size: none"
      , "rows: " ++ (thead |> List.length |> toString)
      ]
  in
    H.div []
      [ H.table [ A.attribute "_fixedhead" fixedhead ]
        [ H.thead [] thead
        , H.tbody [] (body |> toBodyRows |> List.map (H.tr []))
        ]
      ]

toHeadRows : (String -> msg) -> Field.Sort -> ( List (Head msg), Int ) -> List (List (Html msg))
toHeadRows msg sort (heads,rowspan) =
  let
    width head =
      case head of
        UnionHead _ _ colspan -> colspan
        GroupHead _ _ cells   -> cells |> fullWidth
        _ -> 1

    fullWidth =
      List.map width
      >> List.sum
  in
    heads
    |> List.foldl
      (\cell -> fill <|
        case cell of
          Head border sortKey (contents, style) ->
            [ contents
              |> appendSort msg sort sortKey
              |> H.th (attr style border 1 rowspan)
            ] :: []

          UnionHead border (contents, style) colspan ->
            if colspan == 0
              then []
              else
                [ contents
                  |> H.th (attr style border colspan rowspan)
                ] :: []

          PartHead border (contents, style) ->
            [ contents
              |> H.th (attr style border 1 rowspan)
            ] :: []

          GroupHead border (contents, style) cells ->
            [ contents
              |> H.th (attr style border (cells |> fullWidth) 1)
            ] :: ((cells, rowspan - 1) |> toHeadRows msg sort)
      )
      []

appendSort : (String -> msg) -> Field.Sort -> String -> List (Html msg) -> List (Html msg)
appendSort msg sort sortKey contents =
  if sort |> Field.hasSort sortKey |> not
    then contents
    else
      let
        current =
          sort.current
          |> Maybe.andThen
            (\(column,order) ->
              if column == sortKey
                then Just order
                else Nothing
            )
      in
        [ H.a
          ([ A.href "#", E.onClick (msg sortKey) ] ++
            (if current == Nothing
              then []
              else [ A.class "is-active" ]
            )
          )
          (List.append contents
            [ H.text " "
            , case current of
              Nothing         -> Fa.solid "sort"      []
              Just Field.ASC  -> Fa.solid "sort-down" []
              Just Field.DESC -> Fa.solid "sort-up"   []
            ]
          )
        ]

toPrependRows : Maybe (List (Prepend msg)) -> List (Html msg)
toPrependRows prepend =
  case prepend of
    Nothing -> []
    Just cells ->
      cells
      |> List.map
        (\(Prepend border (contents, style) cellStyle) ->
          prependTag cellStyle (attr style border 1 1) contents
        )
      |> H.tr [ A.class "border-top-single" ]
      |> List.singleton

prependTag : CellStyle -> List (H.Attribute msg) -> List (Html msg) -> Html msg
prependTag cellStyle =
  case cellStyle of
    TH -> H.th
    TD -> H.td

toBodyRows : List (List (Unit msg)) -> List (List (Html msg))
toBodyRows = List.concatMap <|
  \row ->
    let
      depth column =
        case column of
          RowsUnit _ rows _ -> rows |> fullDepth
          _ -> 1

      fullDepth = List.map (max depth) >> List.sum

      rowspan = row |> max depth
    in
      row |> List.foldl
        (\col -> fill <|
          case col of
            Unit border (contents, style) ->
              [ [ H.td (attr style border 1 rowspan) contents ]
              ]

            EmptyUnit border (contents, style) colspan ->
              [ [ H.td (attr style border colspan rowspan) contents ]
              ]

            UnionUnit border cols length ->
              [ List.append
                (cols |> List.map
                  (\(contents, style) ->
                    H.td (attr style border 1 rowspan) contents
                  )
                )
                (List.repeat (length - (cols |> List.length))
                  (H.td (attr [] border 1 rowspan) [])
                )
              ]

            RowsUnit border rows colspan ->
              let
                paddingLength = rowspan - (rows |> fullDepth)
                paddingRow =
                  if paddingLength > 0
                    then [ [ H.td (attr [] border colspan paddingLength) [] ] ]
                    else []
              in
                List.append (rows |> toBodyRows) paddingRow
        )
        []

fill : List (List (Html msg)) -> List (List (Html msg)) -> List (List (Html msg))
fill rows acc =
  let
    rowLength = rows |> List.length
    accLength = acc  |> List.length

    length =
      if rowLength > accLength
        then rowLength
        else accLength

    append listLength list = List.append list <|
      List.repeat (length - listLength) []
  in
    List.map2 List.append
      (acc  |> append accLength)
      (rows |> append rowLength)

attr : List Style -> BorderSpec -> Int -> Int -> List (H.Attribute msg)
attr style (left,right) colspan rowspan =
  let
    toSpan f span =
      if span == 1
        then []
        else [f span]

    maybeList f = Maybe.map (f >> List.singleton) >> Maybe.withDefault []
  in
    List.concat
      [ [ A.class <| String.join " " <|
          (List.concat
            [ style |> List.map toStyleClass
            , left  |> maybeList (toBorderClass "border-left")
            , right |> maybeList (toBorderClass "border-right")
            ]
          )
        ]
      , [ colspan |> A.colspan ]
      , [ rowspan |> A.rowspan ]
      ]

toStyleClass : Style -> String
toStyleClass style =
  case style of
    Valid state -> state |> Edit.validationClass
    Left    -> "is-left"
    Center  -> "is-center"
    Right   -> "is-right"
    Top     -> "is-top"
    Middle  -> "is-middle"
    Bottom  -> "is-bottom"
    NoWrap  -> "text-no-wrap"
    PreWrap -> "text-pre-wrap"
    Fill    -> "is-fill"

toBorderClass : String -> Border -> String
toBorderClass prefix border =
  case border of
    Single -> prefix
    Double -> prefix ++ "-double"


build : List (Cell model row msg) -> (model, List row) -> ( ( List (Head msg), Int ), List (List (Unit msg)), Maybe (List (Prepend msg)) )
build cells (model,rows) =
  let
    (head,columns,prepend) = cells |> List.foldl
      (\cell (head,columns,prepend) ->
        case cell of
          Cell ( toHead, toColumn, toPrepend ) ->
            ( List.append head    [ model |> toHead ]
            , List.append columns [ model |> toColumn rows ]
            , List.append prepend [ model |> toPrepend ]
            )
          Union ( toHead, toColumn, toPrepends ) ->
            ( List.append head    [ model |> toHead ]
            , List.append columns [ model |> toColumn rows ]
            , List.append prepend ( model |> toPrepends )
            )
          Group ( toHead, toColumns, toPrepends ) ->
            ( List.append head    [ model |> toHead ]
            , List.append columns ( model |> toColumns rows )
            , List.append prepend ( model |> toPrepends )
            )
          Rows ( toHeads, toColumn, toPrepends ) ->
            ( List.append head    ( model |> toHeads )
            , List.append columns [ model |> toColumn rows ]
            , List.append prepend ( model |> toPrepends )
            )
          Cells ( toHeads, toColumns, toPrepends ) ->
            ( List.append head    ( model |> toHeads )
            , List.append columns ( model |> toColumns rows )
            , List.append prepend ( model |> toPrepends )
            )
      )
      ([],[],[])
  in
    ( head    |> buildHead
    , columns |> buildBody |> withEmptyRow prepend
    , prepend |> buildPrepend
    )

buildHead : List (Head msg) -> ( List (Head msg), Int )
buildHead head =
  let
    depth head =
      case head of
        GroupHead _ _ cells -> 1 + ( cells |> max depth )
        _ -> 1
  in
    ( head, head |> max depth )

buildBody : List (Column msg) -> List (List (Unit msg))
buildBody =
  List.map buildUnit
  >> List.Extra.transpose

withEmptyRow : List (Prepend msg) -> List (List (Unit msg)) -> List (List (Unit msg))
withEmptyRow prepends body =
  if body |> List.isEmpty |> not
    then body
    else
      let
        border  = prepends |> cellBorder
        colspan = prepends |> List.length
        content =
          H.p [ A.class "alert" ]
            [ "load.empty" |> I18n.t |> H.text ]
      in
        [ [ EmptyUnit border ( [ content ], [] ) colspan ] ]

buildPrepend : List (Prepend msg) -> Maybe (List (Prepend msg))
buildPrepend prepend =
  if prepend |> List.any (\(Prepend _ (contents,_) _) -> contents |> List.isEmpty |> not)
    then Just prepend
    else Nothing

buildUnit : Column msg -> List (Unit msg)
buildUnit column =
  case column of
    Column      border cols        -> cols |> List.map (\col -> Unit      border col)
    UnionColumn border cols length -> cols |> List.map (\col -> UnionUnit border col length)
    RowsColumn  border rows length -> rows |> List.map (\row -> RowsUnit  border (row |> buildBody) length)

max : (row -> Int) -> List row -> Int
max depth =
  List.map depth
  >> List.maximum
  >> Maybe.withDefault 1


form : (String -> String) -> List (FormCell model row msg sub) -> Form model row msg sub
form i18n cells = ( i18n, cells )

edit : msg -> Form.Content (Edit.Model a) msg
edit msg =
  Form.html (H.form [ E.onSubmit msg ])
    [ Edit.edit ]

save : ( msg, msg ) -> Form.Content (Edit.Model a) msg
save (save,cancel) =
  Form.html (H.form [ E.onSubmit save ])
    [ Edit.save cancel ]

commit : String -> msg -> Form.Content (Edit.Model a) msg
commit name msg =
  Form.html (H.form [ E.onSubmit msg ])
    [ Edit.commit name, Edit.error ]
