port module Getto.Dom exposing
  ( focusTo
  )

port focusTo : ( String, String, String ) -> Cmd msg
