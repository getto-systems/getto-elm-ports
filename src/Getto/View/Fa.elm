module Getto.View.Fa exposing
  ( icon
  , solid
  , regular
  , spinner
  , copyright
  )

import Html as H exposing ( Html )
import Html.Attributes as A

type Style
  = Solid
  | Regular

type alias Addition = List String

solid : String -> Addition -> Html msg
solid name = icon Solid name

regular : String -> Addition -> Html msg
regular name = icon Regular name

spinner : Html msg
spinner = solid "spinner" ["spin","pulse"]

copyright : Html msg
copyright = regular "copyright" []

icon : Style -> String -> Addition -> Html msg
icon style name additions =
  H.i
    [ A.class ((style |> toPrefix) :: ((name :: additions) |> prependFa) |> String.join " ")
    ] []

toPrefix : Style -> String
toPrefix style =
  case style of
    Solid   -> "fas"
    Regular -> "far"

prependFa : Addition -> List String
prependFa = List.map (\class -> "fa-" ++ class)
