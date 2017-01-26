module Gui exposing (..)

import Ast exposing (ast)
import Dict
import Html exposing ( Html )
import Html.Attributes exposing (property)
import Json.Decode as Json
import Maybe exposing ( Maybe(..) )
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Task
import Tuple2
import Tuple4
import VirtualDom
import Window

type alias Position = { x : Int, y : Int }
type Msg = Error | WindowSize Window.Size | MouseMove Position

canvas : Int -> Int -> Html Msg -> Svg Msg
canvas w_ h_ background = let
                              w = toString w_
                              h = toString h_
                          in Svg.svg [ width w, height h, preserveAspectRatio "xMinYMin slice" ] [ arrowHead, background ]

arrowHead : Html Msg
arrowHead = Svg.marker [ id "arrowHead"
                       , viewBox "0 0 10 10"
                       , refX "5" , refY "5"
                       , markerUnits "strokeWidth", markerWidth "6", markerHeight "6"
                       , orient "auto"
                       , fill "red"
                       ] [ Svg.path [ d "M 0 0 L 10 5 L 0 10 z" ] [] ]

mapFirstRest : List a -> (a -> b) -> (a -> b) -> List b
mapFirstRest list mapFirst mapRest = case list of
                                          first::rest -> mapFirst first :: List.map mapRest rest
                                          [] -> []

parameter : Int -> Int -> Int -> String -> Svg Msg
parameter x_ y_ width name = let
                                 (lineX1, lineX2, lineY1, lineY2) = (x_, x_ + width, y_, y_) |> Tuple4.mapAll toString
                                 (textX, textY) = (x_ + (width // 2), y_) |> Tuple2.mapBoth toString
                                 format text = let lines = String.split "\n" text
                                                   first line = Svg.text line
                                                   next line  = Svg.tspan [ x textX, dy "0.5em" ] [ Svg.text line ]
                                               in mapFirstRest lines first next
                             in Svg.g [] [ Svg.line [ x1 lineX1, x2 lineX2, y1 lineY1, y2 lineY1, stroke "red", strokeWidth "2" , markerEnd "url(#arrowHead)" ] []
                                         , Svg.text_ [ x textX, y textY, textAnchor "middle" ] (format name) ]


function : String -> Svg Msg
function name = let
                    (x_, y_, w_, h_) = (100, 100, 100, 100)
                    (posX, posY, sizeH, sizeV) = (x_, y_, w_, h_) |> Tuple4.mapAll toString

                    radius = sqrt 100 |> toString

                    (inputs, outputs) = Dict.get name ast
                                        |> Maybe.map (\f -> (f.inputs, f.outputs))
                                        |> Maybe.withDefault ([], Dict.empty)

                    params = let width = 60
                                 spacing = h_ // (1 + List.length inputs)
                                 x = x_ - width
                                 y index = y_ + (index + 1) * spacing
                             in inputs |> List.indexedMap (\i name -> parameter x (y i) width name)

                    returns = let outputValues = Dict.keys outputs
                                  width = 60
                                  x = x_ + w_ - (width // 2)
                                  spacing = h_ // (1 + List.length outputValues)
                                  y = \i -> y_ + (i + 1) * spacing
                              in outputValues |> List.indexedMap (\i name -> parameter x (y i) width name)

                in Svg.g [] ([ Svg.rect [ x posX, y posY, width sizeH, height sizeV, rx radius, ry radius, fill "gray" ] [] ] ++ params ++ returns)
