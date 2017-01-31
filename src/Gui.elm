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
import Texts exposing (charCount, mapToSvg, splitText)
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
    [ id "arrowHead", viewBox "0 0 10 10", refX "5", refY "5", markerUnits "strokeWidth", markerWidth "4", markerHeight "4", orient "auto", fill "red" ]
    [ Svg.path [ d "M 0 0 L 10 5 L 0 10 z" ] [] ]


arrow : List String -> Float -> Float -> Float -> Float -> List (Svg Msg)
arrow values arrowWidth w_ x_ h_ =
    let yPos index = let size = List.length values |> toFloat
                         spacing = h_ / (size + 1)
                     in (1 + index) * spacing
    in values |> List.indexedMap (\i name ->
                     let x1 = w_ |> charWidth
                         x2 = w_ + arrowWidth |> charWidth
                         y = yPos <| toFloat i
                         y1 = y |> charHeight
                         y2 = y |> charHeight
                         text = mapToSvg (splitText name) w_ x_ y
                     in drawParameter x1 x2 y1 y2 text)

drawParameter : String -> String -> String -> String -> List (Svg Msg) -> Svg Msg
drawParameter x1_ x2_ y1_ y2_ text =
    Svg.g [] [ Svg.line [ x1 x1_, x2 x2_, y1 y1_, y2 y2_, stroke "red", strokeWidth "1.5", markerEnd "url(#arrowHead)" ] []
             , Svg.text_ [ x x1_, y y1_, textAnchor "end", dominantBaseline "middle" ] text ]

estimateSize : Function -> (Float, Float)
estimateSize f =
    let formattedOutputs = List.map splitText (Dict.keys f.outputs)
        width = maximum (\s -> maximum charCount s |> ceiling) formattedOutputs
        height = let inputLength = f.inputs |> List.length
                     outputLength = formattedOutputs |> List.concat |> List.length
                 in Basics.max inputLength outputLength |> toFloat
    in (width + 1, height)

drawFunction : Function -> List (Svg Msg)
drawFunction f =
    let (w_, h_) = estimateSize f
        radius = (w_ + h_) / 20 |> charHeight
        arrowWidth = 2
        params = arrow f.inputs arrowWidth 0 0 h_
        returns = arrow (Dict.keys f.outputs) arrowWidth (w_ + arrowWidth) arrowWidth h_
    in [ Svg.rect [ x <| charWidth arrowWidth, y "0", width <| charWidth w_, height <| charHeight h_, rx radius, ry radius, fill "LightGray", stroke "LightGray", strokeWidth "0.25em" ]
                  [ Svg.title [] [ Svg.text f.name ] ]
       ] ++ params ++ returns
