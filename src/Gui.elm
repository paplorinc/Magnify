module Gui exposing (..)

import Ast exposing (ast)
import Dict
import Html
import Html.Attributes exposing (property)
import Json.Decode as Json
import Maybe exposing ( Maybe(..) )
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Task
import Tuple4 exposing (mapAll)
import VirtualDom
import Window

type alias Position = { x : Int, y : Int }
type Msg = Error | WindowSize Window.Size | MouseMove Position

canvas : Int -> Int -> Html.Html Msg -> Svg Msg
canvas w h background = Svg.svg [ w |> toString |> width, h |> toString |> height
                                , preserveAspectRatio "xMinYMin slice"
                                ] [ arrowHead, background ]

arrowHead : Html.Html Msg
arrowHead = Svg.marker [ id "arrowHead"
                    , viewBox "0 0 10 10"
                    , refX "0" , refY "5"
                    , markerUnits "strokeWidth", markerWidth "6", markerHeight "6"
                    , orient "auto"
                    , fill "red"
                    ] [ Svg.path [ d "M 0 0 L 10 5 L 0 10 z" ] []]

parameter : Int -> Int -> Svg Msg
parameter endX endY = Svg.line [ x1 "0", y1 "0" , x2 <| toString endX, y2 <| toString endY
                               , stroke "red", strokeWidth "2"
                               , markerEnd "url(#arrowHead)"
                               ] []

function : String -> Svg Msg
function name = let
                    (inputs, outputs) = Dict.get name ast
                                        |> Maybe.map (\f -> (f.inputs, f.outputs))
                                        |> Maybe.withDefault ([], Dict.empty)

                    (posX, posY, sizeH, sizeV) = mapAll toString (100, 100, 100, 100)
                    radius =  toString <| sqrt 100
                    params = List.map (\i -> "") inputs
                in Svg.rect [ x posX, y posY, width sizeH, height sizeV, rx radius, ry radius
                            , fill "gray"
                            ] [ Svg.text_ [] [Svg.text name] ]
