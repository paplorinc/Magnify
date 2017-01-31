module Texts exposing (..)

import Basics
import Lists exposing (maximum)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Tags exposing (..)
import Regex
import Tuple2

charCount string = let regexAstralSymbols = Regex.regex "[\\uD800-\\uDBFF][\\uDC00-\\uDFFF]"
                   in Regex.replace Regex.All regexAstralSymbols (\_ -> ".") string
                   |> String.length

splitLines text = String.split "\n" text

mapToSvg lines w_ x_ y_ =
    let x_ = charWidth w_
        dy_ i = charHeight <| case i of 0 -> toFloat (List.length lines - 1) / -2
                                        _ -> 1
    in List.indexedMap (\i line ->
           Svg.tspan [ x x_, dy (dy_ i), fontFamily "monospace" ]
                     [ Svg.text line ])
           lines
