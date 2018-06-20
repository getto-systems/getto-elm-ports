module Getto.View.Dashboard exposing
  ( Label(..)
  , label
  , alert
  )

import Html as H exposing ( Html )
import Html.Attributes as A

type Label
  = Text   String
  | Small  String Label
  | XSmall String Label
  | Slash

label : List Label -> Html msg
label =
  let
    toContnets content =
      case content of
        Text string -> [ H.text string ]
        Small string content ->
          List.append
            (content |> toContnets)
            [ H.small [] [ H.text string ] ]
        XSmall string content ->
          List.append
            (content |> toContnets)
            [ H.small [] [ H.small [] [ H.text string ] ] ]
        Slash -> [ H.text " / " ]
  in
    List.map (toContnets >> H.em [])
    >> List.intersperse (H.text " ")
    >> H.p []

alert : List String -> Html msg
alert =
  List.map H.text
  >> List.intersperse (H.br [] [])
  >> H.p [ A.class "alert is-danger" ]
