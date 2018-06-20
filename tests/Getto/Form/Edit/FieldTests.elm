module Getto.Form.Edit.FieldTests exposing (..)
import Getto.Form.Edit.Field as Field
import Getto.Form.Edit.Field.Validate as Validate

import Json.Decode as Decode
import Json.Decode.Extra exposing ((|:))
import Json.Encode as Encode

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

type alias DecodeAndEncodeField =
  { name : String
  , address : String
  , over18 : Bool
  , gender : Maybe String
  , role : List String
  }

suite : Test
suite = describe "field utilities"
  [ describe "string field"
    [ test "initial" <|
      \_ ->
        let
          fields =
            { fields = Field.fields [ ("name", Field.string "") ]
            }
        in
          fields |> Field.get "name" |> Expect.equal ""

    , test "with default" <|
      \_ ->
        let
          fields =
            { fields = Field.fields [ ("name", Field.string "John") ]
            }
        in
          fields |> Field.get "name" |> Expect.equal "John"

    , test "set" <|
      \_ ->
        let
          fields =
            { fields = Field.fields [ ("name", Field.string "") ]
            }
        in
          fields |> Field.set "name" "John" |> Field.get "name" |> Expect.equal "John"

    , test "set unknown key" <|
      \_ ->
        let
          fields =
            { fields = Field.fields [ ("name", Field.string "") ]
            }
        in
          fields |> Field.set "unknown" "John" |> Field.get "unknown" |> Expect.equal ""
    ]

  , describe "bool field"
    [ test "initial" <|
      \_ ->
        let
          fields =
            { fields = Field.fields [ ("over18", Field.bool False) ]
            }
        in
          fields |> Field.is "over18" |> Expect.equal False

    , test "with default" <|
      \_ ->
        let
          fields =
            { fields = Field.fields [ ("over18", Field.bool True) ]
            }
        in
          fields |> Field.is "over18" |> Expect.equal True

    , test "put" <|
      \_ ->
        let
          fields =
            { fields = Field.fields [ ("over18", Field.bool False) ]
            }
        in
          fields |> Field.put "over18" True |> Field.is "over18" |> Expect.equal True

    , test "put unknown key" <|
      \_ ->
        let
          fields =
            { fields = Field.fields [ ("over18", Field.string "") ]
            }
        in
          fields |> Field.put "unknown" True |> Field.is "unknown" |> Expect.equal False

    , test "toggle" <|
      \_ ->
        let
          fields =
            { fields = Field.fields [ ("over18", Field.bool False) ]
            }
        in
          fields |> Field.toggle "over18" |> Field.is "over18" |> Expect.equal True

    , test "toggle unknown key" <|
      \_ ->
        let
          fields =
            { fields = Field.fields [ ("over18", Field.string "") ]
            }
        in
          fields |> Field.toggle "unknown" |> Field.is "unknown" |> Expect.equal False
    ]

  , describe "radio list field"
    [ test "initial" <|
      \_ ->
        let
          fields =
            { fields = Field.fields
              [ ("gender", Field.radioList Nothing
                  [ "male"
                  , "female"
                  , "trans"
                  ]
                )
              ]
            }
        in
          fields |> Field.value "gender" |> Expect.equal Nothing

    , test "with default" <|
      \_ ->
        let
          fields =
            { fields = Field.fields
              [ ("gender", Field.radioList (Just "male")
                  [ "male"
                  , "female"
                  , "trans"
                  ]
                )
              ]
            }
        in
          fields |> Field.value "gender" |> Expect.equal (Just "male")

    , test "select" <|
      \_ ->
        let
          fields =
            { fields = Field.fields
              [ ("gender", Field.radioList Nothing
                  [ "male"
                  , "female"
                  , "trans"
                  ]
                )
              ]
            }
        in
          fields
          |> Field.select "gender" (Just "male")
          |> Field.value "gender"
          |> Expect.equal (Just "male")

    , test "select unknown key" <|
      \_ ->
        let
          fields =
            { fields = Field.fields
              [ ("gender", Field.radioList Nothing
                  [ "male"
                  , "female"
                  , "trans"
                  ]
                )
              ]
            }
        in
          fields
          |> Field.select "unknown" (Just "male")
          |> Field.value "gender"
          |> Expect.equal Nothing

    , test "select unknown item" <|
      \_ ->
        let
          fields =
            { fields = Field.fields
              [ ("gender", Field.radioList Nothing
                  [ "male"
                  , "female"
                  , "trans"
                  ]
                )
              ]
            }
        in
          fields
          |> Field.select "gender" (Just "unknown")
          |> Field.value "gender"
          |> Expect.equal Nothing

    , test "select nothing" <|
      \_ ->
        let
          fields =
            { fields = Field.fields
              [ ("gender", Field.radioList (Just "male")
                  [ "male"
                  , "female"
                  , "trans"
                  ]
                )
              ]
            }
        in
          fields
          |> Field.select "gender" Nothing
          |> Field.value "gender"
          |> Expect.equal Nothing
    ]

  , describe "check list field"
    [ test "initial" <|
      \_ ->
        let
          fields =
            { fields = Field.fields
              [ ("gender", Field.checkList []
                  [ "male"
                  , "female"
                  , "trans"
                  ]
                )
              ]
            }
        in
          fields |> Field.values "gender" |> Expect.equal []

    , test "with default" <|
      \_ ->
        let
          fields =
            { fields = Field.fields
              [ ("gender", Field.checkList ["male"]
                  [ "male"
                  , "female"
                  , "trans"
                  ]
                )
              ]
            }
        in
          fields |> Field.values "gender" |> Expect.equal ["male"]

    , test "check" <|
      \_ ->
        let
          fields =
            { fields = Field.fields
              [ ("gender", Field.checkList []
                  [ "male"
                  , "female"
                  , "trans"
                  ]
                )
              ]
            }
        in
          fields
          |> Field.check "gender" "male"
          |> Field.values "gender"
          |> Expect.equal ["male"]

    , test "check unknown key" <|
      \_ ->
        let
          fields =
            { fields = Field.fields
              [ ("gender", Field.checkList []
                  [ "male"
                  , "female"
                  , "trans"
                  ]
                )
              ]
            }
        in
          fields
          |> Field.check "unknown" "male"
          |> Field.values "gender"
          |> Expect.equal []

    , test "check unknown item" <|
      \_ ->
        let
          fields =
            { fields = Field.fields
              [ ("gender", Field.checkList []
                  [ "male"
                  , "female"
                  , "trans"
                  ]
                )
              ]
            }
        in
          fields
          |> Field.check "gender" "unknown"
          |> Field.values "gender"
          |> Expect.equal []

    , test "sync" <|
      \_ ->
        let
          fields =
            { fields = Field.fields
              [ ("gender", Field.checkList []
                  [ "male"
                  , "female"
                  , "trans"
                  ]
                )
              ]
            }
        in
          fields
          |> Field.sync "gender" ["male","female"]
          |> Field.values "gender"
          |> List.sort
          |> Expect.equal ["female","male"]

    , test "sync unknown key" <|
      \_ ->
        let
          fields =
            { fields = Field.fields
              [ ("gender", Field.checkList []
                  [ "male"
                  , "female"
                  , "trans"
                  ]
                )
              ]
            }
        in
          fields
          |> Field.sync "unknown" ["male","female"]
          |> Field.values "gender"
          |> Expect.equal []

    , test "sync unknown item" <|
      \_ ->
        let
          fields =
            { fields = Field.fields
              [ ("gender", Field.checkList []
                  [ "male"
                  , "female"
                  , "trans"
                  ]
                )
              ]
            }
        in
          fields
          |> Field.sync "gender" ["unknown","undefined"]
          |> Field.values "gender"
          |> Expect.equal []
    ]

  , describe "modify"
    [ test "set" <|
      \_ ->
        let
          fields =
            { fields = Field.fields
              [ ("name", Field.string "")
              ]
            }
        in
          fields
          |> Field.modify "name" (Field.Set "John")
          |> Field.get "name"
          |> Expect.equal "John"

    , test "put" <|
      \_ ->
        let
          fields =
            { fields = Field.fields
              [ ("over18", Field.bool False)
              ]
            }
        in
          fields
          |> Field.modify "over18" (Field.Put True)
          |> Field.is "over18"
          |> Expect.equal True

    , test "toggle" <|
      \_ ->
        let
          fields =
            { fields = Field.fields
              [ ("over18", Field.bool False)
              ]
            }
        in
          fields
          |> Field.modify "over18" Field.Toggle
          |> Field.is "over18"
          |> Expect.equal True

    , test "select" <|
      \_ ->
        let
          fields =
            { fields = Field.fields
              [ ("gender", Field.radioList Nothing ["male","female","trans"])
              ]
            }
        in
          fields
          |> Field.modify "gender" (Field.Select <| Just "male")
          |> Field.value "gender"
          |> Expect.equal (Just "male")

    , test "check" <|
      \_ ->
        let
          fields =
            { fields = Field.fields
              [ ("role", Field.checkList [] ["admin","user","system"])
              ]
            }
        in
          fields
          |> Field.modify "role" (Field.Check "admin")
          |> Field.values "role"
          |> Expect.equal ["admin"]

    , test "sync" <|
      \_ ->
        let
          fields =
            { fields = Field.fields
              [ ("role", Field.checkList [] ["admin","user","system"])
              ]
            }
        in
          fields
          |> Field.modify "role" (Field.Sync ["admin","user"])
          |> Field.values "role"
          |> Expect.equal ["admin","user"]
    ]

  , describe "maybe"
    [ test "presence" <|
      \_ ->
        let
          fields =
            { fields = Field.fields
              [ ("name", Field.string "John")
              ]
            }
        in
          fields
          |> Field.maybe (Field.set "name") (Just "Doe")
          |> Field.get "name"
          |> Expect.equal "Doe"

    , test "nothing" <|
      \_ ->
        let
          fields =
            { fields = Field.fields
              [ ("name", Field.string "John")
              ]
            }
        in
          fields
          |> Field.maybe (Field.set "name") Nothing
          |> Field.get "name"
          |> Expect.equal "John"
    ]

  , describe "edit state"
    [ test "default state" <|
      \_ ->
        let
          fields =
            { fields = Field.fields [ ("name", Field.string "") ]
            }
        in
          fields |> Field.isEdit |> Expect.equal False

    , test "to edit" <|
      \_ ->
        let
          fields =
            { fields = Field.fields [ ("name", Field.string "") ]
            }
        in
          fields |> Field.edit |> Field.isEdit |> Expect.equal True

    , test "to static" <|
      \_ ->
        let
          fields =
            { fields = Field.fields [ ("name", Field.string "") ]
            }
        in
          fields |> Field.edit |> Field.static |> Field.isEdit |> Expect.equal False
    ]

  , describe "decode and encode"
    [ test "decodeValue with same id" <|
      \_ ->
        let
          json =
            [ ( "id",    "id" |> Encode.string )
            , ( "state", True |> Encode.bool )
            , ( "pairs"
              , [ ("name",    "John"  |> Encode.string)
              --, ("address", "Tokyo" |> Encode.string) -- missing address in json
                , ("over18",   False  |> Encode.bool)
                , ("gender",  "male"  |> Encode.string)
                , ("role",   ["admin","system"] |> List.map Encode.string |> Encode.list)
                ] |> Encode.object
              )
            ] |> Encode.object
          fields =
            { fields = Field.fields
              [ ("name",    Field.string "")
              , ("address", Field.string "unknown")
              , ("over18",  Field.bool True)
              , ("gender", Field.radioList Nothing
                  [ "male"
                  , "female"
                  , "trans"
                  ]
                )
              , ("role", Field.checkList []
                  [ "admin"
                  , "system"
                  , "user"
                  ]
                )
              ]
            }
            |> Field.decodeValue (always "id") (Just json)

          decoder =
            Decode.succeed DecodeAndEncodeField
            |: (Decode.at ["name"]    Decode.string)
            |: (Decode.at ["address"] Decode.string)
            |: (Decode.at ["over18"]  Decode.bool)
            |: (Decode.at ["gender"] (Decode.maybe Decode.string))
            |: (Decode.at ["role"]   (Decode.list Decode.string))

          expected =
            { name    = "John"
            , address = "unknown"
            , over18  = False
            , gender  = Just "male"
            , role    = ["admin","system"]
            }
        in
          ( fields |> Field.encodeFields |> Encode.object |> Decode.decodeValue decoder
          , fields |> Field.isEdit
          )
          |> Expect.equal
            ( Ok expected
            , True
            )

    , test "decodeValue with different id" <|
      \_ ->
        let
          json =
            [ ( "id",    "id" |> Encode.string )
            , ( "state", True |> Encode.bool )
            , ( "pairs"
              , [ ("name",    "John"  |> Encode.string)
              --, ("address", "Tokyo" |> Encode.string) -- missing address in json
                , ("over18",   False  |> Encode.bool)
                , ("gender",  "male"  |> Encode.string)
                , ("role",   ["admin","system"] |> List.map Encode.string |> Encode.list)
                ] |> Encode.object
              )
            ] |> Encode.object
          fields =
            { fields = Field.fields
              [ ("name",    Field.string "")
              , ("address", Field.string "unknown")
              , ("over18",  Field.bool True)
              , ("gender", Field.radioList Nothing
                  [ "male"
                  , "female"
                  , "trans"
                  ]
                )
              , ("role", Field.checkList []
                  [ "admin"
                  , "system"
                  , "user"
                  ]
                )
              ]
            }
            |> Field.decodeValue (always "another-id") (Just json)

          decoder =
            Decode.succeed DecodeAndEncodeField
            |: (Decode.at ["name"]    Decode.string)
            |: (Decode.at ["address"] Decode.string)
            |: (Decode.at ["over18"]  Decode.bool)
            |: (Decode.at ["gender"] (Decode.maybe Decode.string))
            |: (Decode.at ["role"]   (Decode.list Decode.string))

          expected =
            { name    = ""
            , address = "unknown"
            , over18  = True
            , gender  = Nothing
            , role    = []
            }
        in
          ( fields |> Field.encodeFields |> Encode.object |> Decode.decodeValue decoder
          , fields |> Field.isEdit
          )
          |> Expect.equal
            ( Ok expected
            , False
            )
    ]

  , describe "hook model changed"
    [ test "apply hook" <|
      \_ ->
        let
          field =
            { fields = Field.fields
              [ ("full_name",  Field.string "")
              , ("first_name", Field.string "")
              , ("last_name",  Field.string "")
              ]
            }
          hooks =
            [ ( ["full_name"]
              , \field ->
                case field |> Field.get "full_name" |> String.split " " of
                  [] -> field
                  head :: tail ->
                    field
                    |> Field.set "first_name" head
                    |> Field.set "last_name" (tail |> String.join " ")
              )
            , ( ["first_name","last_name"]
              , \field ->
                field
                |> Field.set "full_name"
                  ((field |> Field.get "first_name") ++ " " ++ (field |> Field.get "last_name"))
              )
            ]
        in
          ( field |> Field.set "full_name" "John Doe" |> Field.apply "full_name"  hooks |> Field.get "first_name"
          , field |> Field.set "full_name" "John Doe" |> Field.apply "full_name"  hooks |> Field.get "last_name"
          , field |> Field.set "first_name" "John"    |> Field.apply "first_name" hooks |> Field.get "full_name"
          , field |> Field.set "last_name"  "Doe"     |> Field.apply "last_name"  hooks |> Field.get "full_name"
          )
          |> Expect.equal
            ( "John"
            , "Doe"
            , "John "
            , " Doe"
            )

    , test "apply unknown key" <|
      \_ ->
        let
          field =
            { fields = Field.fields
              [ ("full_name",  Field.string "")
              , ("first_name", Field.string "")
              , ("last_name",  Field.string "")
              ]
            }
          hooks =
            [ ( ["full_name"]
              , \field ->
                case field |> Field.get "full_name" |> String.split " " of
                  [] -> field
                  head :: tail ->
                    field
                    |> Field.set "first_name" head
                    |> Field.set "last_name" (tail |> String.join " ")
              )
            , ( ["first_name","last_name"]
              , \field ->
                field
                |> Field.set "full_name"
                  ((field |> Field.get "first_name") ++ " " ++ (field |> Field.get "last_name"))
              )
            ]
        in
          field |> Field.set "full_name" "John Doe" |> Field.apply "unknown" hooks
          |> Field.get "first_name" |> Expect.equal ""
    ]

  , describe "validate model"
    [ test "initial" <|
      \_ ->
        let
          field =
            { fields = Field.fields [ ("name", Field.string "") ]
            }
        in
          field |> Field.validationState ["name"] |> Expect.equal Validate.Ok

    , test "ok" <|
      \_ ->
        let
          validations =
            [ ( "name", []
              , Field.get "name" >> Validate.notEmpty "name must not empty"
              )
            ]
          field = Field.validateAll validations
            { fields = Field.fields
              [ ("name",    Field.string "John")
              , ("address", Field.string "")
              ]
            }
        in
          ( field |> Field.validationState ["name"]
          , field |> Field.validationState ["address"]
          )
          |> Expect.equal (Validate.Ok, Validate.Ok)

    , test "error" <|
      \_ ->
        let
          validations =
            [ ( "name", []
              , Field.get "name" >> Validate.notEmpty "name must not empty"
              )
            ]
          field = Field.validateAll validations
            { fields = Field.fields [ ("name", Field.string "") ]
            }
        in
          field |> Field.validationState ["name"] |> Expect.equal (Validate.Err ["name must not empty"])

    , test "unknown key" <|
      \_ ->
        let
          validations =
            [ ( "name", []
              , Field.get "name" >> Validate.notEmpty "name must not empty"
              )
            ]
          field = Field.validateAll validations
            { fields = Field.fields [ ("name", Field.string "") ]
            }
        in
          field |> Field.validationState ["unknown"] |> Expect.equal Validate.Ok

    , test "merge state" <|
      \_ ->
        let
          validations =
            [ ( "name", []
              , Field.get "name" >> Validate.notEmpty "name must not empty"
              )
            , ( "address", []
              , Field.get "address" >> Validate.notEmpty "address must not empty"
              )
            ]
          field = Field.validate "name" validations
            { fields = Field.fields
              [ ("name",    Field.string "")
              , ("address", Field.string "")
              ]
            }
        in
          ( field |> Field.validationState ["name"]
          , field |> Field.validationState ["address"]
          , field |> Field.validationState ["name","address"]
          )
          |> Expect.equal
            ( Validate.Err ["name must not empty"]
            , Validate.Ok
            , Validate.Err ["name must not empty"]
            )

    , test "merge message" <|
      \_ ->
        let
          validations =
            [ ( "name", []
              , Field.get "name" >> Validate.notEmpty "name must not empty"
              )
            , ( "address", []
              , Field.get "address" >> Validate.notEmpty "address must not empty"
              )
            ]
          field = Field.validateAll validations
            { fields = Field.fields
              [ ("name",    Field.string "")
              , ("address", Field.string "")
              ]
            }
        in
          ( field |> Field.validationState ["name"]           |> Validate.errors
          , field |> Field.validationState ["address"]        |> Validate.errors
          , field |> Field.validationState ["name","address"] |> Validate.errors |> List.sort
          )
          |> Expect.equal
            ( ["name must not empty"]
            , ["address must not empty"]
            , ["address must not empty", "name must not empty"]
            )
    ]
  ]
