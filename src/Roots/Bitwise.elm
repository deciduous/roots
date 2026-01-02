module Roots.Bitwise exposing
    ( isSet
    , popcount
    , set
    , unset
    )

import Bitwise


isSet : Int -> Int -> Bool
isSet bit bits =
    Bitwise.and (Bitwise.shiftLeftBy bit 1) bits /= 0


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


set : Int -> Int -> Int
set bit bits =
    Bitwise.or (Bitwise.shiftLeftBy bit 1) bits


unset : Int -> Int -> Int
unset bit bits =
    Bitwise.and (Bitwise.complement (Bitwise.shiftLeftBy bit 1)) bits
