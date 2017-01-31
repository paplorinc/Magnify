module Main exposing (..)

import Animation exposing (translate)
import Ast exposing (ast, function)
import Dict
import Gui exposing (..)
import Html exposing ( Html )
import Html.Attributes exposing (property)
import Json.Decode as Json
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Task
import Tags
import VirtualDom
import Window
import Mouse
import Keyboard
import Keys
import Mouses
import Windows

type alias Model = { size : Window.Size
                   , pos : Mouse.Position
                   , code : Keyboard.KeyCode
                   , selectedIndex : Int }

main = Html.program { init = init, update = update, subscriptions = subscriptions, view = view }

init = ( { size = Window.Size 600 600, pos = Mouse.Position 0 0, code = 0, selectedIndex = 0 }
       , Task.perform WindowSizeMsg Window.size)

update msg model =
    case msg of
        WindowSizeMsg s -> ( Windows.update model s, Cmd.none )
        MouseMsg p      -> ( Mouses.update model p, Cmd.none )
        KeyMsg c        -> ( Keys.update model c, Cmd.none)
        _               -> Debug.crash "update"

subscriptions model =
    Sub.batch
        [ Window.resizes WindowSizeMsg
        , Mouse.clicks MouseMsg
        , Keyboard.downs KeyMsg ]

view model =
    let w = model.size.width  - 5 |> toString
        h = model.size.height - 5 |> toString
        b = background model
    in Html.div [] [ canvas w h b ]

background model =
    Dict.values ast
 |> List.indexedMap (\i f ->
        let selected = if i == model.selectedIndex then 1.5 else 1
        in drawFunction f |> Svg.g [ Tags.transform [ Tags.translate (50 + i * 120) 50, Tags.scale selected ] ] )
 |> Svg.g []
