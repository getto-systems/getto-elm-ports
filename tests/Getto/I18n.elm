module Getto.I18n exposing
  ( title
  , menu
  , role
  , rest
  , t
  )

title : String -> String
title = translate "title"

menu : String -> String
menu = translate "menu"

role : String -> String
role = translate "role"

rest : String -> String
rest = translate "rest"

t : String -> String
t = translate "t"

translate : String -> String -> String
translate category key = "I18n." ++ category ++ ": " ++ key
