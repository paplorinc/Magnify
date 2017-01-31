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

type alias Model = { size : Window.Size, pos : Mouse.Position, code : Keyboard.KeyCode }

getWidth : { b | size : { a | width : number } } -> number
getWidth model  = model.size.width  - 5 -- clientWidth?

getHeight : { b | size : { a | height : number } } -> number
getHeight model = model.size.height - 5

main : Program Never Model Msg
main = Html.program { init = init, update = update, subscriptions = subscriptions, view = view }


init : ( Model, Cmd Msg )
init = ( { size = Window.Size 600 600, pos = Mouse.Position 0 0, code = 0 }
       , Task.perform WindowSizeMsg Window.size)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowSizeMsg s -> ( { model | size = s }, Cmd.none )
        MouseMsg p      -> ( { model | pos = p }, Cmd.none )
        KeyMsg c        -> ( { model | code = c }, Cmd.none)
        _               -> Debug.crash "update"

subscriptions : a -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes WindowSizeMsg
        , Mouse.clicks MouseMsg
        , Keyboard.downs KeyMsg ]

view : Model -> Html Msg
view model = Html.div []
                      [ canvas (getWidth model) (getHeight model) (background model) ]

background : Model -> Svg Msg
background model = Dict.values ast
                |> List.indexedMap (\i f -> drawFunction f |> Svg.g [ transform <| Tags.translate (50 + i * 120) 50 ] )
                |> Svg.g []
