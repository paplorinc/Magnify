module Lists exposing (..)

maxLength strings = List.map String.length strings
                    |> List.maximum
                    |> Maybe.withDefault 0
                    |> toFloat