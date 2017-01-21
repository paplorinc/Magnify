module Equation exposing (..)

import List exposing (..)

solve1 : Float -> Float -> Float
solve1 b a = case (b, a) of
                (0, 0) -> 0 -- TODO error
                (0, _) -> 0 -- TODO error
                _      -> -a / b

solve2 : Float -> Float -> Float -> List Float
solve2 c b a = case (c, b, a) of
    (0, _, _) -> [solve1 b a]
    (_, 0, _) -> [-a / c]
    (_, _, 0) -> [0, -b / c]
    _         ->  let delta = b^2 - 4*c*a
                  in if delta > 0 then
                        let op = if b < 0 then (-) else (+)
                            part = op -b (sqrt delta)
                        in [(1/2) * part/c, 2 * a/part]
                     else if delta == 0 then [-b / 2*c]
                     else [0] -- TODO error