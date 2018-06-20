module Getto.View.Login exposing ( view )
import Getto
import Getto.Login as Login
import Getto.View.Fa as Fa

import Html as H exposing ( Html )
import Html.Attributes as A

view : (Login.Base m -> List (Html msg)) -> Login.Base m -> Html msg
view content model =
  H.div [ A.class "LoginLayout" ]
    [ H.article [] <| List.concat
      [ [ model.info.project |> header
        ]
      , model |> content
      ]
    , model.info.application |> footer
    ]

header : Getto.Project -> Html msg
header project =
  H.header []
    [ H.p []
      [ H.small [] [ H.text project.company ]
      , H.br [] []
      , H.text project.title
      , H.br [] []
      , H.small [] [ H.text project.subTitle ]
      ]
    ]

footer : Getto.Application -> Html msg
footer application =
  H.footer []
    [ Fa.copyright, H.text application.copyright
    , H.text " "
    , H.text "version : ", H.text application.version
    ]
