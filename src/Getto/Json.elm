module Getto.Json exposing
  ( decodeValue
  , withDefault
  )

import Json.Decode as Decode
import Json.Encode as Encode

decodeValue : List String -> Decode.Decoder a -> Encode.Value -> Maybe a
decodeValue keys decoder =
  Decode.decodeValue (Decode.at keys decoder)
  >> Result.toMaybe

withDefault : a -> Decode.Decoder a -> Decode.Decoder a
withDefault default decoder = Decode.oneOf [decoder, Decode.null default]
