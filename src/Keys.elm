module Keys exposing (..)

import Keyboard

update model c = { model | selectedIndex =
    case c of
        37 -> Basics.max 0 (model.selectedIndex - 1)
        39 -> model.selectedIndex + 1
        _  ->  model.selectedIndex }