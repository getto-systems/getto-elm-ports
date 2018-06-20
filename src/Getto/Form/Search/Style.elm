module Getto.Form.Search.Style exposing
  ( style
  , none
  , left
  , right
  , center
  , top
  , middle
  , bottom
  , nowrap
  , prewrap
  , fill
  )

import Getto.Form.Search.Data as Data

type alias Styled a = ( a, List Data.Style )

style : List Data.Style -> a -> Styled a
style style contents = ( contents, style )

none : a -> Styled a
none = style []

left : a -> Styled a
left = style [ Data.Left ]

right : a -> Styled a
right = style [ Data.Right ]

center : a -> Styled a
center = style [ Data.Center ]

top : a -> Styled a
top = style [ Data.Top ]

middle : a -> Styled a
middle = style [ Data.Middle ]

bottom : a -> Styled a
bottom = style [ Data.Bottom ]

nowrap : a -> Styled a
nowrap = style [ Data.NoWrap ]

prewrap : a -> Styled a
prewrap = style [ Data.PreWrap ]

fill : a -> Styled a
fill = style [ Data.Fill ]
