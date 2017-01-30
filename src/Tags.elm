module Tags exposing (..)

charWidth num = (toString num) ++ "ch"
charHeight num = (toString num) ++ "em"

translate x y = String.concat [ "translate(", toString x, ",", toString y, ")" ]