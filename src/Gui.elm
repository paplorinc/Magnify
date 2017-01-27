module Gui exposing (..)

import Ast exposing (ast)
import Dict
import Html exposing ( Html )
import Html.Attributes exposing (property)
import Json.Decode as Json
import Lists exposing (maxLength)
import Maybe exposing ( Maybe(..) )
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Tags exposing (em)
import Task
import Texts exposing (mapToSvg)
import Tuple2
import Tuple4
import VirtualDom
import Window

type alias Position = { x : Int, y : Int }
type Msg = Error | WindowSize Window.Size | MouseMove Position

canvas : Int -> Int -> Html Msg -> Svg Msg
canvas w_ h_ background = let w = toString w_
                              h = toString h_
                          in Svg.svg [ width w, height h, preserveAspectRatio "xMinYMin slice" ] [ arrowHead, background ]

arrowHead : Html Msg
arrowHead = Svg.marker [ id "arrowHead" , viewBox "0 0 10 10" , refX "5" , refY "5" , markerUnits "strokeWidth", markerWidth "6", markerHeight "6" , orient "auto" , fill "red" ]
                       [ Svg.path [ d "M 0 0 L 10 5 L 0 10 z" ] [] ]


arrow : List String -> Float -> Float -> Float -> Float -> Float -> List (Svg Msg)
arrow values arrowWidth x_ y_ w_ h_ = let yPos index = let size = List.length values |> toFloat
                                                           spacing = h_ / (size + 1)
                                                       in y_ + (1 + toFloat index) * spacing
                                      in values |> List.indexedMap (\i name -> let y = yPos i
                                                                                   x2 = x_ + arrowWidth
                                                                               in parameter x_ x2 y y name)

parameter : Float -> Float -> Float -> Float -> String -> Svg Msg
parameter x_ x2_ y_ y2_ name = let (lineX1, lineX2, lineY1, lineY2) = (x_, x2_, y_, y2_) |> Tuple4.mapAll toString
                                   (textX, textY) = (x_, y_) |> Tuple2.mapBoth toString
                             in Svg.g [] [ Svg.line [ x1 lineX1, x2 lineX2, y1 lineY1, y2 lineY2, stroke "red", strokeWidth "2" , markerEnd "url(#arrowHead)" ] []
                                         , Svg.text_ [ x textX, y textY, textAnchor "start" ] (mapToSvg name textX textY) ]

function : String -> Svg Msg
function name = let (x_, y_, w_, h_) = (100.0, 100.0, 100.0, 100.0)
                    (posX, posY, sizeH, sizeV) = (x_, y_, w_, h_) |> Tuple4.mapAll toString

                    radius = sqrt 100.0 |> toString

                    (inputs, outputs) = Dict.get name ast
                                        |> Maybe.map (\f -> (f.inputs, f.outputs))
                                        |> Maybe.withDefault ([], Dict.empty)

                    arrowWidth = 30
                    params = let x = x_ - arrowWidth
                                 values = inputs
                             in arrow values arrowWidth x y_ w_ h_
                    returns = let x = x_ + w_
                                  values = Dict.keys outputs
                              in arrow values arrowWidth x y_ w_ h_
                in Svg.g [] ([ Svg.rect [ x posX, y posY, width sizeH, height sizeV, rx radius, ry radius, fill "gray" ] [] ] ++ params ++ returns)
