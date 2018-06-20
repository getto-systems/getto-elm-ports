module Getto.Rest.Search exposing
  ( Response
  , response
  )

import Json.Decode as Decode
import Json.Decode.Extra exposing ((|:))

type alias Response data row =
  { max : Int
  , data : data
  , rows : List row
  }

response : Decode.Decoder data -> Decode.Decoder row -> Decode.Decoder (Response data row)
response data row =
  Decode.succeed Response
  |: (Decode.at ["max"] Decode.int)
  |: (Decode.at ["data"] data)
  |: (Decode.at ["rows"] (Decode.list row))
