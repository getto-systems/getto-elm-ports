module Getto.Form.Dialog.Edit exposing
  ( dialog
  )

import Getto.Form as Form
import Getto.Form.Edit as Edit
import Getto.Form.Edit.Field as Field

import Html as H
import Html.Attributes as A

dialog : msg -> List (H.Attribute msg) -> List (Edit.Content a msg) -> List (Edit.Content a msg) -> Edit.Content a msg
dialog update attrs contents appends model = model |>
  if model |> Field.isEdit |> not
    then Form.none
    else
      Form.html (H.section [ A.class "dialog" ])
        [ Form.html (H.section [])
          (List.append
            [ Form.html (H.section [ A.class "edit" ])
              [ Form.html (H.section [])
                [ Edit.editForm update attrs contents
                ]
              ]
            ]
            appends
          )
        ]
