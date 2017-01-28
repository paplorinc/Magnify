module Lists exposing (..)

maximum values mapper = List.map mapper values
                        |> List.maximum
                        |> Maybe.withDefault 0
                        |> toFloat