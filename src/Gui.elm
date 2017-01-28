module Gui exposing (..)

import Ast exposing (Function, ast)
import Dict
import Html exposing ( Html )
import Html.Attributes exposing (property)
import Json.Decode as Json
import Lists exposing (maximum)
import Maybe exposing ( Maybe(..) )
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Tags exposing (em)
import Task
import Texts exposing (mapToSvg, splitText)
import Tuple2
import Tuple4
import VirtualDom
import Window

type alias Position = { x : Float, y : Float }
type Msg = Error
         | WindowSize Window.Size
         | MouseMove Position

canvas : Int -> Int -> Html Msg -> Svg Msg
canvas w_ h_ background =
    let w = toString w_
        h = toString h_
    in Svg.svg [ width w, height h, preserveAspectRatio "xMinYMin slice" ]
               [ arrowHead, background ]

arrowHead : Html Msg
arrowHead = Svg.marker
    [ id "arrowHead" , viewBox "0 0 10 10" , refX "5" , refY "5" , markerUnits "strokeWidth", markerWidth "6", markerHeight "6" , orient "auto" , fill "red" ]
    [ Svg.path [ d "M 0 0 L 10 5 L 0 10 z" ] [] ]


arrow : List String -> Float -> Float -> Float -> List (Svg Msg)
arrow values arrowWidth x_ h_ =
    let yPos index = let size = List.length values |> toFloat
                         spacing = h_ / (size + 1)
                     in (1 + toFloat index) * spacing
    in values |> List.indexedMap (\i name -> let y = yPos i |> em in drawParameter (em x_) (em (x_ + arrowWidth)) y y name)

drawParameter : String -> String -> String -> String -> String -> Svg Msg
drawParameter x1_ x2_ y1_ y2_ name =
    Svg.g [] [ Svg.line [ x1 x1_, x2 x2_, y1 y1_, y2 y2_, stroke "red", strokeWidth "2", markerEnd "url(#arrowHead)" ] []
             , Svg.text_ [ x x1_, y y1_, textAnchor "start" ] (mapToSvg name x1_ y1_) ]

estimateSize : Function -> (Float, Float)
estimateSize f =
    let formattedOutputs = List.map splitText (Dict.keys f.outputs)
        width = maximum formattedOutputs (\s -> maximum s String.length |> round)
        height = Basics.max (List.length f.inputs) (List.length (List.foldl (++) [] formattedOutputs)) |> toFloat
    in (width - 2, height + 1)

drawFunction : Function -> Svg Msg
drawFunction f =
    let (w_, h_) = estimateSize f
        radius = (w_ + h_) / 10 |> sqrt |> em
        arrowWidth = 1
        params = arrow f.inputs arrowWidth 0 h_
        returns = arrow (Dict.keys f.outputs) arrowWidth (w_ + arrowWidth) h_
    in Svg.g [ transform "translate(100,100)"]
             ([ Svg.rect [ x (em arrowWidth), y "0", width <| em w_, height <| em h_, rx radius, ry radius, fill "gray" ] [] ]
             ++ params
             ++ returns)
