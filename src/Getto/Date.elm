module Getto.Date exposing
  ( iso8601
  )

import Date.Extra

iso8601 : String -> String
iso8601 =
  Date.Extra.fromIsoString
  >> Maybe.map (Date.Extra.toFormattedString "y-MM-dd")
  >> Maybe.withDefault ""
