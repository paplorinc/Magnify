module Texts exposing (..)

import Lists exposing (maxLength)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Tags exposing (em)

mapToSvg text textX textY = let
                                lines = String.split "\n" text
                                (offsetX, offsetY) = (maxLength lines, List.length lines - 1) |> Tuple2.mapBoth (\v -> (toFloat v) / 2)
                            in List.indexedMap (\i line -> Svg.tspan [ x textX
                                                                     , y textY
                                                                     , dx <| em -offsetX
                                                                     , dy <| em ((toFloat i - offsetY) * offsetY)
                                                                     ] [ Svg.text line ]) lines
