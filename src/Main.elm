module Main exposing (..)

import Dict
import Gui exposing (..)
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

type alias Model    = { size : Window.Size, pos : Position }

getWidth : { b | size : { a | width : number } } -> number
getWidth model  = model.size.width  - 5 -- clientWidth?

getHeight : { b | size : { a | height : number } } -> number
getHeight model = model.size.height - 5

main : Program Never Model Msg
main = Html.program { init = init, update = update, subscriptions = subscriptions, view = view }


init : ( Model, Cmd Msg )
init = ( { size = Window.Size 600 600, pos = Position 0 0 }
       , Task.perform WindowSize Window.size)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
                       WindowSize { width, height } -> ( { model | size = Window.Size width height }, Cmd.none )
                       MouseMove pos                -> ( { model | pos = pos }, Cmd.none )
                       _                            -> Debug.crash "update"

subscriptions : a -> Sub Msg
subscriptions model = Window.resizes WindowSize

view : Model -> Html.Html Msg
view model = Html.div [] [ scene model]

scene : Model -> Html.Html Msg
scene model = canvas (getWidth model) (getHeight model) (background model)

background : Model -> Svg Msg
background model = function "solve the quadratic equation (𝕔x² + 𝕓x + 𝕒 = 0)"
