module Texts exposing (..)

import Lists exposing (maximum)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Tags exposing (em)
import Tuple2

splitText text = String.split "\n" text

mapToSvg text x_ y_ =
    let lines = splitText text
        offsetX = em (maximum lines String.length / -2)
        offsetY i = let offset = toFloat (List.length lines - 1) / -4
                    in em <| -(i + offset) * offset
    in List.indexedMap (\i line ->
          Svg.tspan [ x x_, y y_, dx offsetX, dy (offsetY (toFloat i)) ]
                    [ Svg.text line ])
          lines
