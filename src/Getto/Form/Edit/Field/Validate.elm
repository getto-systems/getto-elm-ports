module Getto.Form.Edit.Field.Validate exposing
  ( State(..)
  , merge
  , errors
  , thenError
  , is
  , result
  , notEmpty
  , notEmptyList
  , presence
  , integer
  , positive
  , notNegative
  , negative
  )

type State
  = Ok
  | Err (List String)

merge : List State -> State
merge =
  List.foldr
    (\result acc ->
      case result of
        Ok -> acc
        Err messages ->
          Err ((acc |> errors) ++ messages)
    )
    Ok

errors : State -> List String
errors result =
  case result of
    Ok -> []
    Err errors -> errors


thenError : String -> Bool -> State
thenError message isError =
  if isError
    then Err [message]
    else Ok


is : (value -> Bool) -> value -> Result String value
is validator value =
  if value |> validator
    then Result.Ok value
    else Result.Err "error"

result : String -> Result error value -> State
result error = Result.toMaybe >> (==) Nothing >> thenError error


notEmpty : String -> String -> State
notEmpty error = String.isEmpty >> thenError error

notEmptyList : String -> List a -> State
notEmptyList error = List.isEmpty >> thenError error

presence : String -> Maybe value -> State
presence error = (==) Nothing >> thenError error

integer : String -> String -> State
integer error = String.toInt >> result error

positive : String -> String -> State
positive error = String.toInt >> Result.andThen (is <| (<) 0) >> result error

notNegative : String -> String -> State
notNegative error = String.toInt >> Result.andThen (is <| (<=) 0) >> result error

negative : String -> String -> State
negative error = String.toInt >> Result.andThen (is <| (>) 0) >> result error
