port module Getto.Location exposing
  ( Search
  , redirectTo
  , searchTo
  , entry
  , id
  )

import List.Extra
import Json.Encode as Encode
import Json.Decode as Decode

port redirectTo : String -> Cmd msg
port searchTo : Search -> Cmd msg

type alias Search = List ( String, String )

entry : String -> Search -> Maybe String
entry key = List.Extra.find (Tuple.first >> (==) key) >> Maybe.map Tuple.second

id : Search -> Int
id = entry "id" >> Maybe.withDefault "" >> String.toInt >> Result.withDefault 0
