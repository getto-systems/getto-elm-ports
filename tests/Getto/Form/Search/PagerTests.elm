module Getto.Form.Search.PagerTests exposing (..)
import Getto.Form.Search.Pager as Pager

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite = describe "pager"
  [ test "zero" <|
    \_ ->
      (1,0) |> Pager.pagers |> Expect.equal
        [ { index = 1
          , ellipsis = False
          , active = False
          }
        ]

  , test "one" <|
    \_ ->
      (1,1) |> Pager.pagers |> Expect.equal
        [ { index = 1
          , ellipsis = False
          , active = True
          }
        ]

  , describe "ellipsis"
    [ test "left" <|
      \_ ->
        (6,6) |> Pager.pagers |> Expect.equal
          [ { index = 1
            , ellipsis = False
            , active = False
            }
          , { index = 2
            , ellipsis = True
            , active = False
            }
          , { index = 3
            , ellipsis = False
            , active = False
            }
          , { index = 4
            , ellipsis = False
            , active = False
            }
          , { index = 5
            , ellipsis = False
            , active = False
            }
          , { index = 6
            , ellipsis = False
            , active = True
            }
          ]

    , test "far left" <|
      \_ ->
        (10,10) |> Pager.pagers |> Expect.equal
          [ { index = 1
            , ellipsis = False
            , active = False
            }
          , { index = 6
            , ellipsis = True
            , active = False
            }
          , { index = 7
            , ellipsis = False
            , active = False
            }
          , { index = 8
            , ellipsis = False
            , active = False
            }
          , { index = 9
            , ellipsis = False
            , active = False
            }
          , { index = 10
            , ellipsis = False
            , active = True
            }
          ]

    , test "right" <|
      \_ ->
        (1,6) |> Pager.pagers |> Expect.equal
          [ { index = 1
            , ellipsis = False
            , active = True
            }
          , { index = 2
            , ellipsis = False
            , active = False
            }
          , { index = 3
            , ellipsis = False
            , active = False
            }
          , { index = 4
            , ellipsis = False
            , active = False
            }
          , { index = 5
            , ellipsis = True
            , active = False
            }
          , { index = 6
            , ellipsis = False
            , active = False
            }
          ]

    , test "far right" <|
      \_ ->
        (1,10) |> Pager.pagers |> Expect.equal
          [ { index = 1
            , ellipsis = False
            , active = True
            }
          , { index = 2
            , ellipsis = False
            , active = False
            }
          , { index = 3
            , ellipsis = False
            , active = False
            }
          , { index = 4
            , ellipsis = False
            , active = False
            }
          , { index = 5
            , ellipsis = True
            , active = False
            }
          , { index = 10
            , ellipsis = False
            , active = False
            }
          ]

    , test "both" <|
      \_ ->
        (6,11) |> Pager.pagers |> Expect.equal
          [ { index = 1
            , ellipsis = False
            , active = False
            }
          , { index = 2
            , ellipsis = True
            , active = False
            }
          , { index = 3
            , ellipsis = False
            , active = False
            }
          , { index = 4
            , ellipsis = False
            , active = False
            }
          , { index = 5
            , ellipsis = False
            , active = False
            }
          , { index = 6
            , ellipsis = False
            , active = True
            }
          , { index = 7
            , ellipsis = False
            , active = False
            }
          , { index = 8
            , ellipsis = False
            , active = False
            }
          , { index = 9
            , ellipsis = False
            , active = False
            }
          , { index = 10
            , ellipsis = True
            , active = False
            }
          , { index = 11
            , ellipsis = False
            , active = False
            }
          ]

    , test "far both" <|
      \_ ->
        (10,20) |> Pager.pagers |> Expect.equal
          [ { index = 1
            , ellipsis = False
            , active = False
            }
          , { index = 6
            , ellipsis = True
            , active = False
            }
          , { index = 7
            , ellipsis = False
            , active = False
            }
          , { index = 8
            , ellipsis = False
            , active = False
            }
          , { index = 9
            , ellipsis = False
            , active = False
            }
          , { index = 10
            , ellipsis = False
            , active = True
            }
          , { index = 11
            , ellipsis = False
            , active = False
            }
          , { index = 12
            , ellipsis = False
            , active = False
            }
          , { index = 13
            , ellipsis = False
            , active = False
            }
          , { index = 14
            , ellipsis = True
            , active = False
            }
          , { index = 20
            , ellipsis = False
            , active = False
            }
          ]
    ]

  , describe "all in window"
    [ test "current in left" <|
      \_ ->
        (1,5) |> Pager.pagers |> Expect.equal
          [ { index = 1
            , ellipsis = False
            , active = True
            }
          , { index = 2
            , ellipsis = False
            , active = False
            }
          , { index = 3
            , ellipsis = False
            , active = False
            }
          , { index = 4
            , ellipsis = False
            , active = False
            }
          , { index = 5
            , ellipsis = False
            , active = False
            }
          ]

    , test "current in center" <|
      \_ ->
        (5,9) |> Pager.pagers |> Expect.equal
          [ { index = 1
            , ellipsis = False
            , active = False
            }
          , { index = 2
            , ellipsis = False
            , active = False
            }
          , { index = 3
            , ellipsis = False
            , active = False
            }
          , { index = 4
            , ellipsis = False
            , active = False
            }
          , { index = 5
            , ellipsis = False
            , active = True
            }
          , { index = 6
            , ellipsis = False
            , active = False
            }
          , { index = 7
            , ellipsis = False
            , active = False
            }
          , { index = 8
            , ellipsis = False
            , active = False
            }
          , { index = 9
            , ellipsis = False
            , active = False
            }
          ]

    , test "current in right" <|
      \_ ->
        (5,5) |> Pager.pagers |> Expect.equal
          [ { index = 1
            , ellipsis = False
            , active = False
            }
          , { index = 2
            , ellipsis = False
            , active = False
            }
          , { index = 3
            , ellipsis = False
            , active = False
            }
          , { index = 4
            , ellipsis = False
            , active = False
            }
          , { index = 5
            , ellipsis = False
            , active = True
            }
          ]
    ]
  ]
