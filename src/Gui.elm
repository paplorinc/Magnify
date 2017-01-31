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
import Tags exposing (..)
import Task
import Texts exposing (charCount, mapToSvg, splitLines)
import Tuple2
import Tuple4
import VirtualDom
import Window
import Mouse
import Keyboard

type Msg = ErrorMsg
         | WindowSizeMsg Window.Size
         | MouseMsg Mouse.Position
         | KeyMsg Keyboard.KeyCode

canvas w h background =
    Svg.svg [ width w, height h, preserveAspectRatio "xMinYMin slice" ]
            [ arrowHead, background ]

arrowHead = Svg.marker
    [ id "arrowHead", viewBox "0 0 10 10", refX "5", refY "5", markerUnits "strokeWidth", markerWidth "4", markerHeight "4", orient "auto", fill "red" ]
    [ Svg.path [ d "M 0 0 L 10 5 L 0 10 z" ] [] ]


arrow values arrowWidth w_ x_ h_ =
    let size = List.length values |> toFloat
        spacing = h_ / (size + 1)
    in values |> List.indexedMap (\i name ->
                    let y = (1 + toFloat i) * spacing
                        (x1, x2) = (w_, w_ + arrowWidth) |> Tuple2.mapBoth charWidth
                        (y1, y2) = (y, y) |> Tuple2.mapBoth charHeight
                        text = mapToSvg (splitLines name) w_ x_ y
                    in drawParameter x1 x2 y1 y2 text)

drawParameter x1_ x2_ y1_ y2_ text =
    Svg.g [] [ Svg.line [ x1 x1_, x2 x2_, y1 y1_, y2 y2_, stroke "red", strokeWidth "1.5", markerEnd "url(#arrowHead)" ] []
             , Svg.text_ [ x x1_, y y1_, textAnchor "end", dominantBaseline "middle" ] text ]

estimateSize f =
    let formattedOutputs = List.map splitLines (Dict.keys f.outputs)
        width = maximum (\s -> maximum charCount s |> ceiling) formattedOutputs
        height = let inputLength = f.inputs |> List.length
                     outputLength = formattedOutputs |> List.concat |> List.length
                 in Basics.max inputLength outputLength |> toFloat
    in (width + 1, height)

drawFunction function =
    let (functionWidth, functionHeight) = estimateSize function
        arrowWidth = 2
        radius = (functionWidth + functionHeight) / 20 |> charHeight
        params = let values = function.inputs
                 in arrow values arrowWidth 0 0 functionHeight
        returns = let values = Dict.keys function.outputs
                      w_ = functionWidth + arrowWidth
                  in arrow values arrowWidth w_ arrowWidth functionHeight
    in [ let (x_, y_, w_, h_) = (charWidth arrowWidth, charHeight 0, charWidth functionWidth, charHeight functionHeight)
         in Svg.rect [ x x_, y y_, width w_, height h_, rx radius, ry radius, fill "LightGray", stroke "LightGray", strokeWidth "0.25em" ]
                  [ Svg.title [] [ Svg.text function.name ] ]
       ] ++ params ++ returns
