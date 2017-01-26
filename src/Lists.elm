module Lists exposing (..)

maxLength strings = Maybe.withDefault 0 (List.maximum (List.map String.length strings)) |> toFloat