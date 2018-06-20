module Getto.Login exposing
  ( Base
  )
import Getto

type alias Base m =
  { m
  | info : Getto.Info
  }
