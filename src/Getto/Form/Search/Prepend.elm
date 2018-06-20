module Getto.Form.Search.Prepend exposing
  ( none
  , td
  , th
  )

import Getto.Form.Search.Data as Data
import Getto.Form.Search.Style as Style

type alias Prepend a = ( Data.Content a, Data.CellStyle )

none : Prepend a
none = td <| Style.none []

td : Data.Content a -> Prepend a
td contents = ( contents, Data.TD )

th : Data.Content a -> Prepend a
th contents = ( contents, Data.TH )
