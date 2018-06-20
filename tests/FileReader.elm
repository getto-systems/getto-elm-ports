module FileReader exposing -- FileReader stub --
  ( NativeFile
  , onFileChange
  , filePart
  )

import Html as H
import Html.Attributes as A

import Http

type alias NativeFile = { dummy : String }

onFileChange : (List NativeFile -> msg) -> H.Attribute msg
onFileChange msg = A.class "on-file-change"

filePart : String -> NativeFile -> Http.Part
filePart name _ = Http.stringPart name "file"
