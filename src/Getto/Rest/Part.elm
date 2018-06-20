module Getto.Rest.Part exposing
  ( Part
  , null
  , string
  , bool
  , list
  , file
  , toParts
  )

import Http

import FileReader

type Part
  = NullPart
  | StringPart String
  | BoolPart Bool
  | ListPart (List Part)
  | FilePart (List FileReader.NativeFile)

null : Part
null = NullPart

string : String -> Part
string = StringPart

bool : Bool -> Part
bool = BoolPart

list : List Part -> Part
list = ListPart

file : List FileReader.NativeFile -> Part
file = FilePart

toParts : ( String, Part ) -> List Http.Part
toParts (key, part) =
  case part of
    NullPart -> []
    StringPart value -> [ Http.stringPart key value ]
    BoolPart   value -> [ Http.stringPart key (value |> toString) ]
    ListPart list ->
      list |> List.concatMap
        (\sub ->
          (key ++ "[]", sub) |> toParts
        )
    FilePart files -> files |> List.map (FileReader.filePart key)
