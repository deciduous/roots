module Roots.Bitwise exposing (popcount)

import Bitwise


{-| Get the number of 1 bits in a non-negative integer.
-}
popcount : Int -> Int
popcount =
    let
        loop acc n =
            if n == 0 then
                acc

            else
                loop (acc + 1) (n - Bitwise.and n -n)
    in
    loop 0
