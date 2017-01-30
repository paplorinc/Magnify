module Lists exposing (..)

maximum mapper values = List.map mapper values
                     |> List.maximum
                     |> Maybe.withDefault 0
                     |> toFloat