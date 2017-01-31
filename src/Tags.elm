module Tags exposing (..)

import Svg
import Svg.Attributes

charWidth num = (toString num) ++ "ch"
charHeight num = (toString num) ++ "em"

transform values = Svg.Attributes.transform <| String.join " " values
translate x y = String.concat [ "translate(", toString x, ",", toString y, ")" ]
scale xy = String.concat [ "scale(", toString xy, ")" ]