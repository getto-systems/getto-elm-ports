module Getto.Form.Edit.Field.ValidateTests exposing (..)
import Getto.Form.Edit.Field as Field
import Getto.Form.Edit.Field.Validate as Validate

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite = describe "validate field"
  [ test "notEmpty" <|
    \_ ->
      let
        validations =
          [ ( "name", []
            , Field.get "name" >> Validate.notEmpty "name must not empty"
            )
          ]
        field = Field.validateAll validations
          { fields = Field.fields
            [ ("name", Field.string "")
            ]
          }
      in
        field |> Field.validationState ["name"]
        |> Expect.equal (Validate.Err ["name must not empty"])

  , test "notEmptyList" <|
    \_ ->
      let
        validations =
          [ ( "file", []
            , Field.picked "file" >> Validate.notEmptyList "file must not empty"
            )
          ]
        field = Field.validateAll validations
          { fields = Field.fields
            [ ("file", Field.file)
            ]
          }
      in
        field |> Field.validationState ["file"]
        |> Expect.equal (Validate.Err ["file must not empty"])

  , test "presence" <|
    \_ ->
      let
        validations =
          [ ( "gender", []
            , Field.value "gender" >> Validate.presence "select gender"
            )
          ]
        field = Field.validateAll validations
          { fields = Field.fields
            [ ("gender", Field.radioList Nothing ["male","female","trans"])
            ]
          }
      in
        field |> Field.validationState ["gender"]
        |> Expect.equal (Validate.Err ["select gender"])

  , test "integer" <|
    \_ ->
      let
        validations =
          [ ( "count", []
            , Field.get "count" >> Validate.integer "count must integer"
            )
          ]
        field = Field.validateAll validations
          { fields = Field.fields
            [ ("count", Field.string "")
            ]
          }
      in
        field |> Field.validationState ["count"]
        |> Expect.equal (Validate.Err ["count must integer"])

  , test "positive" <|
    \_ ->
      let
        validations =
          [ ( "age", []
            , Field.get "age" >> Validate.positive "age must positive integer"
            )
          ]
        field =
          { fields = Field.fields
            [ ("age", Field.string "")
            ]
          }
      in
        ( field |> Field.set "age" ""   |> Field.validateAll validations |> Field.validationState ["age"]
        , field |> Field.set "age" "1"  |> Field.validateAll validations |> Field.validationState ["age"]
        , field |> Field.set "age" "0"  |> Field.validateAll validations |> Field.validationState ["age"]
        , field |> Field.set "age" "-1" |> Field.validateAll validations |> Field.validationState ["age"]
        ) |> Expect.equal
          ( Validate.Err ["age must positive integer"]
          , Validate.Ok
          , Validate.Err ["age must positive integer"]
          , Validate.Err ["age must positive integer"]
          )

  , test "notNegative" <|
    \_ ->
      let
        validations =
          [ ( "age", []
            , Field.get "age" >> Validate.notNegative "age must not negative integer"
            )
          ]
        field =
          { fields = Field.fields
            [ ("age", Field.string "")
            ]
          }
      in
        ( field |> Field.set "age" ""   |> Field.validateAll validations |> Field.validationState ["age"]
        , field |> Field.set "age" "1"  |> Field.validateAll validations |> Field.validationState ["age"]
        , field |> Field.set "age" "0"  |> Field.validateAll validations |> Field.validationState ["age"]
        , field |> Field.set "age" "-1" |> Field.validateAll validations |> Field.validationState ["age"]
        ) |> Expect.equal
          ( Validate.Err ["age must not negative integer"]
          , Validate.Ok
          , Validate.Ok
          , Validate.Err ["age must not negative integer"]
          )

  , test "negative" <|
    \_ ->
      let
        validations =
          [ ( "count", []
            , Field.get "count" >> Validate.negative "count must negative integer"
            )
          ]
        field =
          { fields = Field.fields
            [ ("count", Field.string "")
            ]
          }
      in
        ( field |> Field.set "count" ""   |> Field.validateAll validations |> Field.validationState ["count"]
        , field |> Field.set "count" "1"  |> Field.validateAll validations |> Field.validationState ["count"]
        , field |> Field.set "count" "0"  |> Field.validateAll validations |> Field.validationState ["count"]
        , field |> Field.set "count" "-1" |> Field.validateAll validations |> Field.validationState ["count"]
        ) |> Expect.equal
          ( Validate.Err ["count must negative integer"]
          , Validate.Err ["count must negative integer"]
          , Validate.Err ["count must negative integer"]
          , Validate.Ok
          )
  ]
