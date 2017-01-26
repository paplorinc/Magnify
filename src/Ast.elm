module Ast exposing (..)

import Json.Decode exposing (Decoder, decodeString, dict, field, float, int, lazy, list, map, map3, oneOf, string)
import Dict exposing (Dict)

inputDecoder : Decoder (List String)
inputDecoder = list string

type Output = OutputString String
            | OutputInt    Int
            | OutputFloat  Float
            | OutputList  (List Output)
            | OutputDict  (Dict String Output)

outputDecoder : Decoder Output
outputDecoder = oneOf [ map OutputString <| string
                      , map OutputInt    <| int
                      , map OutputFloat  <| float
                      , map OutputList   <| list (lazy <| \_ -> outputDecoder)
                      , map OutputDict   <| dict (lazy <| \_ -> outputDecoder)]

type alias Function = { name:    String
                      , inputs:  List String
                      , outputs: Dict String Output}

function : Decoder Function
function = map3 Function (field "name"    <| string)
                         (field "inputs"  <| inputDecoder)
                         (field "outputs" <| dict outputDecoder)

ast : Dict String Function
ast = decodeString (list function) """
[{
  "name":    "solve the quadratic equation (𝕔x² + 𝕓x + 𝕒 = 0)",
  "inputs":  ["𝕔", "𝕓", "𝕒"],
  "outputs": { "-𝕓±√Δ\\n ——\\n 2𝕔":
                 {"divide": [{"plus and minus": [{"negate": ["𝕓"]}
                                                ,{"sqrt": {"subtract":[{"square": ["𝕓"]}
                                                                      ,{"multiply": ["4","𝕔","𝕒"]}]}}]}
                            ,{"multiply": ["2","𝕔"]}]}}
}, {
  "name":    "solve the linear equation (𝕓x + 𝕒 = 0)",
  "inputs":  ["𝕓", "𝕒"],
  "outputs": {"-𝕒\\n——\\n𝕓": {"negate": {"divide": ["𝕒", "𝕓"]}}}
}, {
  "name":    "negate",
  "inputs":  ["𝕒"],
  "outputs": {"-𝕒": {"scala.math.Numeric.negate": ["𝕒"]}}
}, {
  "name":    "add",
  "inputs":  ["𝕒", "𝕓"],
  "outputs": {"𝕒+𝕓": {"scala.math.Numeric.plus": ["𝕒", "𝕓"]}}
}, {
  "name":    "subtract",
  "inputs":  ["𝕒", "𝕓"],
  "outputs": {"𝕒-𝕓": {"scala.math.Numeric.minus": ["𝕒", "𝕓"]}}
}, {
  "name":    "multiply",
  "inputs":  ["𝕒", "𝕓"],
  "outputs": {"𝕒×𝕓": {"scala.math.Numeric.times": ["𝕒", "𝕓"]}}
}, {
  "name":    "divide",
  "inputs":  ["𝕒", "𝕓"],
  "outputs": {"𝕒\\n——\\n𝕓": {"scala.math.Numeric.FloatIsFractional.div": ["𝕒", "𝕓"]}}
}, {
  "name":    "square",
  "inputs":  ["𝕒"],
  "outputs": {"𝕒²": {"multiply": ["𝕒", "𝕒"]}}
}, {
  "name":    "square root",
  "inputs":  ["𝕒"],
  "outputs": {"√𝕒": {"java.lang.Math.sqrt": ["𝕒"]}}
}, {
  "name":    "plus and minus",
  "inputs":  ["𝕒", "𝕓"],
  "outputs": { "𝕒+𝕓": {"add": ["𝕒", "𝕓"]},
               "𝕒-𝕓": {"subtract": ["𝕒", "𝕓"]}
  }
}]""" |> Result.withDefault []
      |> List.map (\x -> (x.name, x))
      |> Dict.fromList
