module Getto.Form.Edit.Field exposing
  ( Model
  , Validation
  , Hook
  , Modify(..)
  , fields
  , isEdit
  , isEditSub
  , edit
  , static
  , editSub
  , staticSub
  , string
  , bool
  , radioList
  , checkList
  , file
  , get
  , is
  , value
  , values
  , options
  , picked
  , set
  , put
  , toggle
  , select
  , check
  , sync
  , pick
  , modify
  , maybe
  , reset
  , validateAll
  , validate
  , validationState
  , hasError
  , apply
  , decodeValue
  , encode
  , encodeFields
  , multipartFields
  )
import Getto.Form.Edit.Field.Validate as Validate

import Getto.Rest as Rest
import Getto.Rest.Part as Part
import Getto.Json as Json

import Json.Decode as Decode
import Json.Decode.Extra exposing ((|:))
import Json.Encode as Encode

import Dict exposing ( Dict )
import Focus exposing ( Focus, (=>) )

import FileReader

type alias Model a =
  { a
  | fields : Struct
  }

type alias Struct =
  { id    : Maybe String
  , state : State
  , sub   : Dict String State
  , pairs : Pairs
  }

type alias Pair = ( String, Spec )
type alias Pairs = Dict String ( Spec, Validate.State )

type alias Group = Dict String Bool

type alias Validation a = List ( String, List String, a -> Validate.State )
type alias Hook a       = List ( List String, a -> a )

type State
  = Static
  | Edit

type Spec
  = String String String
  | Bool   Bool   Bool
  | RadioList (Maybe String) (List String) Group
  | CheckList (List String) (List String) Group
  | File (List FileReader.NativeFile)

type Modify
  = Set String
  | Put Bool
  | Toggle
  | Select (Maybe String)
  | Check String
  | Sync (List String)
  | Pick (List FileReader.NativeFile)

type alias SubPair =
  { name  : String
  , state : Bool
  }

fields_ = Focus.create .fields (\f model -> { model | fields = model.fields |> f })
state_  = Focus.create .state  (\f model -> { model | state  = model.state  |> f })
sub_    = Focus.create .sub    (\f model -> { model | sub    = model.sub    |> f })
pairs_  = Focus.create .pairs  (\f model -> { model | pairs  = model.pairs  |> f })

fields : List Pair -> Struct
fields pairs =
  { id    = Nothing
  , state = Static
  , sub   = Dict.empty
  , pairs =
    pairs
    |> List.map
      (\(name,spec) ->
        ( name, ( spec, Validate.Ok ) )
      )
    |> Dict.fromList
  }

string : String -> Spec
string default = String default default

bool : Bool -> Spec
bool default = Bool default default

radioList : Maybe String -> List String -> Spec
radioList default options =
  options
  |> List.map (\item -> (item, Just item == default))
  |> Dict.fromList
  |> RadioList default options

checkList : List String -> List String -> Spec
checkList default options =
  options
  |> List.map (\item -> (item, default |> List.member item))
  |> Dict.fromList
  |> CheckList default options

file : Spec
file = File []


isEdit : Model a -> Bool
isEdit model =
  case model.fields.state of
    Edit   -> True
    Static -> False

isEditSub : String -> Model a -> Bool
isEditSub name model =
  case model.fields.sub |> Dict.get name of
    Just Edit -> True
    _         -> False


get : String -> Model a -> String
get name model =
  case model |> find name of
    Just (String _ value) -> value
    _ -> ""

is : String -> Model a -> Bool
is name model =
  case model |> find name of
    Just (Bool _ value) -> value
    _ -> False

value : String -> Model a -> Maybe String
value name model =
  case model |> find name of
    Just (RadioList _ _ group) -> group |> active
    _ -> Nothing

values : String -> Model a -> List String
values name model =
  case model |> find name of
    Just (CheckList _ _ group) -> group |> actives
    _ -> []

options : String -> Model a -> List String
options name model =
  case model |> find name of
    Just (RadioList _ options _) -> options
    Just (CheckList _ options _) -> options
    _ -> []

picked : String -> Model a -> List FileReader.NativeFile
picked name model =
  case model |> find name of
    Just (File files) -> files
    _ -> []


find : String -> Model a -> Maybe Spec
find name = pairs >> Dict.get name >> Maybe.map Tuple.first

pairs : Model a -> Pairs
pairs = .fields >> .pairs

actives : Group -> List String
actives group =
  group
  |> Dict.toList
  |> List.filterMap
    (\(name,value) ->
      if value
        then Just name
        else Nothing
    )

active : Group -> Maybe String
active = actives >> List.head


edit : Model a -> Model a
edit = to Edit

static : Model a -> Model a
static = to Static

to : State -> Model a -> Model a
to state = Focus.update fields_ <|
  Focus.set state_ state


editSub : String -> Model a -> Model a
editSub = subTo Edit

staticSub : String -> Model a -> Model a
staticSub = subTo Static

subTo : State -> String -> Model a -> Model a
subTo state name = Focus.update (fields_ => sub_) <|
  Dict.update name <| always (Just state)


set : String -> String -> Model a -> Model a
set name value = Focus.update (fields_ => pairs_) <|
  Dict.update name <|
    \fieldValue ->
      case fieldValue of
        Just ((String default _),result) -> Just ((String default value),result)
        _ -> fieldValue

put : String -> Bool -> Model a -> Model a
put name value = Focus.update (fields_ => pairs_) <|
  Dict.update name <|
    \fieldValue ->
      case fieldValue of
        Just ((Bool default _),result) -> Just ((Bool default value),result)
        _ -> fieldValue

toggle : String -> Model a -> Model a
toggle name = Focus.update (fields_ => pairs_) <|
  Dict.update name <|
    \fieldValue ->
      case fieldValue of
        Just ((Bool default val),result) -> Just ((Bool default (not val)),result)
        _ -> fieldValue

select : String -> Maybe String -> Model a -> Model a
select group name = Focus.update (fields_ => pairs_) <|
  Dict.update group <|
    \fieldValue ->
      case fieldValue of
        Just ((RadioList default options group),result) ->
          Just ((RadioList default options (group |> Dict.toList |> List.map
            (\(groupName,groupValue) ->
              ( groupName
              , Just groupName == name
              )
            )
          |> Dict.fromList)),result)
        _ -> fieldValue

check : String -> String -> Model a -> Model a
check group name = Focus.update (fields_ => pairs_) <|
  Dict.update group <|
    \fieldValue ->
      case fieldValue of
        Just ((CheckList default options group),result) ->
          Just ((CheckList default options (group |> Dict.update name (Maybe.map not))),result)
        _ -> fieldValue

sync : String -> List String -> Model a -> Model a
sync group names = Focus.update (fields_ => pairs_) <|
  Dict.update group <|
    \fieldValue ->
      case fieldValue of
        Just ((CheckList default options group),result) ->
          Just ((CheckList default options (group |> Dict.toList |> List.map
            (\(groupName,groupValue) ->
              ( groupName
              , names |> List.member groupName
              )
            )
          |> Dict.fromList)),result)
        _ -> fieldValue

pick : String -> List FileReader.NativeFile -> Model a -> Model a
pick name files = Focus.update (fields_ => pairs_) <|
  Dict.update name <|
    \fieldValue ->
      case fieldValue of
        Just ((File _),result) -> Just ((File files),result)
        _ -> fieldValue

modify : String -> Modify -> Model a -> Model a
modify name modify model =
  case modify of
    Set    value -> model |> set    name value
    Put    value -> model |> put    name value
    Toggle       -> model |> toggle name
    Select value -> model |> select name value
    Check  value -> model |> check  name value
    Sync   value -> model |> sync   name value
    Pick   value -> model |> pick   name value

maybe : (value -> Model a -> Model a) -> Maybe value -> Model a -> Model a
maybe updater = Maybe.map updater >> Maybe.withDefault identity


reset : Model a -> Model a
reset = Focus.update (fields_ => pairs_) <|
  Dict.map <|
    \name (value,result) ->
      ( case value of
        String    default _         -> string    default
        Bool      default _         -> bool      default
        RadioList default options _ -> radioList default options
        CheckList default options _ -> checkList default options
        File                      _ -> file
      , result
      )


validateAll : Validation (Model a) -> Model a -> Model a
validateAll validations model =
  model.fields.pairs
  |> Dict.keys
  |> List.foldl
    (\name -> validate name validations)
    (model |> clearAllValidation)

clearAllValidation : Model a -> Model a
clearAllValidation = Focus.update (fields_ => pairs_) <|
  Dict.map <|
    \name (value,_) -> (value,Validate.Ok)

validate : String -> Validation (Model a) -> Model a -> Model a
validate name validations model =
  let
    list = validations |> List.filter
      (\(target,watches,_) ->
        (target == name) || (watches |> List.member name)
      )

    base = list |> List.foldl
      (\(target,_,_) -> clearValidation target)
      model
  in
    list |> List.foldl
      (\(target,_,validation) ->
        model |> validation |> mergeValidation target
      )
      base

mergeValidation : String -> Validate.State -> Model a -> Model a
mergeValidation name result = updateValidation name <| \old -> Validate.merge [old,result]

clearValidation : String -> Model a -> Model a
clearValidation name = updateValidation name <| \_ -> Validate.Ok

updateValidation : String -> (Validate.State -> Validate.State) -> Model a -> Model a
updateValidation name updater = Focus.update (fields_ => pairs_) <|
  Dict.update name <|
    Maybe.map (\(value,old) -> (value, old |> updater))

validationState : List String -> Model a -> Validate.State
validationState names model =
  let
    dict = model |> pairs
  in
    names
    |> List.filterMap (\name -> dict |> Dict.get name |> Maybe.map Tuple.second)
    |> Validate.merge

hasError : Model a -> Bool
hasError =
  pairs
  >> Dict.toList
  >> List.any
    (\(_,(_,result)) ->
      case result of
        Validate.Err _ -> True
        Validate.Ok    -> False
    )


apply : String -> Hook (Model a) -> Model a -> Model a
apply name hooks model =
  hooks
  |> List.foldl
    (\(targets,hook) ->
      if targets |> List.member name
        then hook
        else identity
    )
    model


storageKey =
  { id    = "id"
  , state = "state"
  , sub   = "sub"
  , pairs = "pairs"
  }

decodeValue : (Model a -> String) -> Maybe Encode.Value -> Model a -> Model a
decodeValue prop storage model =
  let
    id = Just (model |> prop)

    setID = Focus.update fields_ <|
      \struct -> { struct | id = id }
  in
  if id /= (storage |> Maybe.andThen (Json.decodeValue [storageKey.id] Decode.string))
    then model |> setID
    else
      let
        state =
          storage
          |> Maybe.andThen (Json.decodeValue [storageKey.state] Decode.bool)
          |> Maybe.withDefault False

        restoreState =
          if state
            then edit
            else static

        restoreSub model =
          storage
          |> Maybe.andThen (Json.decodeValue [storageKey.sub] (Decode.list decodeSub))
          |> Maybe.withDefault []
          |> List.foldl
            (\sub ->
              if sub.state
                then editSub   sub.name
                else staticSub sub.name
            )
            model
      in
        model
        |> pairs
        |> Dict.toList
        |> List.foldl
          (\(name,(spec,_)) ->
            let
              update decoder setup default =
                case storage |> Maybe.andThen (Json.decodeValue [storageKey.pairs,name] decoder) of
                  Just value -> setup value
                  Nothing    -> setup default
            in
              case spec of
                String    default _   -> update  Decode.string               (set    name) default
                Bool      default _   -> update  Decode.bool                 (put    name) default
                RadioList default _ _ -> update (Decode.maybe Decode.string) (select name) default
                CheckList default _ _ -> update (Decode.list  Decode.string) (sync   name) default
                File              _   -> identity
          )
          (model |> setID |> restoreState |> restoreSub)

decodeSub : Decode.Decoder SubPair
decodeSub =
  Decode.succeed SubPair
  |: (Decode.at ["name"]  Decode.string)
  |: (Decode.at ["state"] Decode.bool)

encode : Model a -> Encode.Value
encode model =
  [ ( storageKey.id,    model.fields.id |> Maybe.map Encode.string |> Maybe.withDefault Encode.null )
  , ( storageKey.state, model |> isEdit |> Encode.bool )
  , ( storageKey.sub,   model |> encodeSub )
  , ( storageKey.pairs, model |> encodeFields |> Encode.object )
  ] |> Encode.object

encodeSub : Model a -> Encode.Value
encodeSub =
  .fields
  >> .sub
  >> Dict.toList
  >> List.map
    (\(name,state) ->
      [ ( "name", name |> Encode.string )
      , ( "state"
        , Encode.bool <|
          case state of
            Edit   -> True
            Static -> False
        )
      ] |> Encode.object
    )
  >> Encode.list

encodeFields : Model a -> Rest.JsonBody
encodeFields =
  let
    value spec =
      case spec of
        String    _   value -> value |> Encode.string
        Bool      _   value -> value |> Encode.bool
        RadioList _ _ group -> group |> active  |> Maybe.map Encode.string |> Maybe.withDefault Encode.null
        CheckList _ _ group -> group |> actives |> List.map  Encode.string |> Encode.list
        File      _         -> Encode.null
  in
    pairs
    >> Dict.toList
    >> List.map (\(name,(spec,_)) -> (name, spec |> value) )

multipartFields : Model a -> Rest.MultipartBody
multipartFields =
  let
    value spec =
      case spec of
        String    _   value -> value |> Part.string
        Bool      _   value -> value |> Part.bool
        RadioList _ _ group -> group |> active  |> Maybe.map Part.string |> Maybe.withDefault Part.null
        CheckList _ _ group -> group |> actives |> List.map  Part.string |> Part.list
        File          files -> files |> Part.file
  in
    pairs
    >> Dict.toList
    >> List.map (\(name,(spec,_)) -> (name, spec |> value) )
