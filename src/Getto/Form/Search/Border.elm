module Getto.Form.Search.Border exposing
  ( border
  , none
  , left
  , leftDouble
  , right
  , rightDouble
  , both
  , bothDouble
  )

import Getto.Form.Search.Data as Data

border : Maybe Data.Border -> Maybe Data.Border -> Data.BorderSpec
border left right = ( left, right )

none : Data.BorderSpec
none = border Nothing Nothing

left : Data.BorderSpec
left = border (Just Data.Single) Nothing

leftDouble : Data.BorderSpec
leftDouble = border (Just Data.Double) Nothing

right : Data.BorderSpec
right = border Nothing (Just Data.Single)

rightDouble : Data.BorderSpec
rightDouble = border Nothing (Just Data.Double)

both : Data.BorderSpec
both = border (Just Data.Single) (Just Data.Single)

bothDouble : Data.BorderSpec
bothDouble = border (Just Data.Double) (Just Data.Double)
