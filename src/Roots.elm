module Roots exposing
    ( Array
    , Dict
    , isEven
    , isOdd
    )

import Array
import Dict


type alias Array a =
    Array.Array a


type alias Dict k v =
    Dict.Dict k v


isEven : Int -> Bool
isEven n =
    modBy 2 n == 0

isOdd : Int -> Bool
isOdd n =
    modBy 2 n == 1
