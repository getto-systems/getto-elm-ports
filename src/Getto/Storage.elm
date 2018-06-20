port module Getto.Storage exposing
  ( entry
  , save
  , saveCredential
  , saveMenu
  , saveTerminal
--, saveGlobal -- DO NOT EXPOSE!!
  )

import Json.Encode as Encode
import List.Extra

port savePage   : ( String, Encode.Value ) -> Cmd msg
port saveGlobal : ( String, Encode.Value ) -> Cmd msg

save : String -> Encode.Value -> Cmd msg
save key data = savePage ( key, data )

-- manage global strage keys HERE
saveCredential data = saveGlobal ("credential", data)
saveMenu       data = saveGlobal ("menu",       data)
saveTerminal   data = saveGlobal ("terminal",   data)

entry : String -> List ( String, Encode.Value ) -> Maybe Encode.Value
entry key = List.Extra.find (Tuple.first >> (==) key) >> Maybe.map Tuple.second
