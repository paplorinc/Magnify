module Tags exposing (..)

em num = (toString num) ++ "em"

translate x y = String.concat [ "translate(", toString x, ",", toString y, ")" ]