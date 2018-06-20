module Getto.Moment exposing
  ( nop
  , map
  , batch
  , update
  , andThen
  , maybe
  , result
  )

import Focus exposing ( Focus )

type alias Moment  model msg = model -> Next model msg
type alias Next    model msg = ( model, Cmd msg )
type alias Command model msg = model -> Cmd msg

nop : Moment model msg
nop model = ( model, Cmd.none )

map : (a -> b) -> Next model a -> Next model b
map msg (model,cmd) = ( model, cmd |> Cmd.map msg )

batch : List (Command model msg) -> Moment model msg
batch cmds model =
  model ! (cmds |> List.map (\cmd -> model |> cmd))

update : Focus big small -> Moment small msg -> Moment big msg
update prop updater model =
  let
    (small,cmd) = model |> Focus.get prop |> updater
  in
    (model |> Focus.set prop small) ! [cmd]

andThen : (before -> Next after msg) -> Next before msg -> Next after msg
andThen updater (model,cmd) =
  let
    (newModel, newCmd) = model |> updater
  in
    newModel ! [cmd, newCmd]

maybe : (a -> Moment model msg) -> Maybe a -> Moment model msg
maybe f = Maybe.map f >> Maybe.withDefault nop

result : (a -> Moment model msg) -> Result error a -> Moment model msg
result f = Result.map f >> Result.withDefault nop
