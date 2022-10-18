module Roots exposing
    ( Array
    , Dict
    , ifte
    , isEven
    , isOdd
    )

import Array
import Dict


type alias Array a =
    Array.Array a


type alias Dict k v =
    Dict.Dict k v


ifte : Bool -> a -> a -> a
ifte p t f =
    if p then
        t

    else
        f


isEven : Int -> Bool
isEven n =
    modBy 2 n == 0


isOdd : Int -> Bool
isOdd n =
    modBy 2 n == 1
