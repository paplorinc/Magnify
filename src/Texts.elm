module Texts exposing (..)

import Lists exposing (maxLength)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Tags exposing (em)
import Tuple2

mapToSvg text textX textY = let lines = String.split "\n" text
                                offsetX = em (maxLength lines / -2)
                                offsetY i = let offset = toFloat (List.length lines - 1) / -4
                                            in em <| -(i + offset) * offset
                            in List.indexedMap (\i line -> Svg.tspan [ x textX, y textY, dx offsetX, dy (offsetY (toFloat i)) ] [ Svg.text line ]) lines
