module Roots.ArrayZipper exposing
    ( ArrayZipper
    , cycleLeft
    , cycleRight
    , empty
    , focus
    , focusOn
    , fromArray
    , fromList
    , index
    , length
    , lengthLeft
    , lengthRight
    , toArray
    , toList
    )

{-| ArrayZipper.

@docs ArrayZipper
@docs cycleLeft
@docs cycleRight
@docs empty
@docs focus
@docs focusOn
@docs fromArray
@docs fromList
@docs index
@docs length
@docs lengthLeft
@docs lengthRight
@docs toArray
@docs toList

-}

import Array exposing (Array)


{-| An array zipper is an array with a focused element, if non-empty.
-}
type ArrayZipper a
    = ArrayZipper
        { array : Array a
        , index_ : Int
        }


{-| An empty array zipper.
-}
empty : ArrayZipper a
empty =
    ArrayZipper
        { array = Array.empty
        , index_ = 0
        }


{-| Get the index.
-}
index : ArrayZipper a -> Int
index (ArrayZipper { index_ }) =
    index_


{-| Get the focus.
-}
focus : ArrayZipper a -> Maybe a
focus (ArrayZipper { array, index_ }) =
    Array.get index_ array


{-| Get the number of elements.
-}
length : ArrayZipper a -> Int
length (ArrayZipper { array }) =
    Array.length array


{-| Get the number of elements to the left of the focus.
-}
lengthLeft : ArrayZipper a -> Int
lengthLeft =
    index


{-| Get the number of elements to the right of the focus.
-}
lengthRight : ArrayZipper a -> Int
lengthRight (ArrayZipper { array, index_ }) =
    max (Array.length array - index_ - 1) 0


{-| Move the focus to the left (with wraparound).
-}
cycleLeft : ArrayZipper a -> ArrayZipper a
cycleLeft ((ArrayZipper { array, index_ }) as zipper) =
    if Array.isEmpty array then
        zipper

    else
        ArrayZipper
            { array = array
            , index_ =
                if index_ == 0 then
                    Array.length array - 1

                else
                    index_ - 1
            }


{-| Move the focus to the right (with wraparound).
-}
cycleRight : ArrayZipper a -> ArrayZipper a
cycleRight ((ArrayZipper { array, index_ }) as zipper) =
    if Array.isEmpty array then
        zipper

    else
        ArrayZipper
            { array = array
            , index_ =
                if index_ == Array.length array - 1 then
                    0

                else
                    index_ + 1
            }


{-| Focus on an index.
-}
focusOn : Int -> ArrayZipper a -> ArrayZipper a
focusOn index0 ((ArrayZipper { array }) as zipper) =
    if Array.isEmpty array then
        zipper

    else
        ArrayZipper
            { array = array
            , index_ = clamp 0 (Array.length array - 1) index0
            }


{-| Convert an array to an array zipper.
-}
fromArray : Array a -> ArrayZipper a
fromArray array =
    ArrayZipper { array = array, index_ = 0 }


{-| Convert a list to an array zipper.
-}
fromList : List a -> ArrayZipper a
fromList list =
    ArrayZipper { array = Array.fromList list, index_ = 0 }


{-| Convert an array zipper to an array.
-}
toArray : ArrayZipper a -> Array a
toArray (ArrayZipper { array }) =
    array


{-| Convert an array zipper to a list.
-}
toList : ArrayZipper a -> List a
toList (ArrayZipper { array }) =
    Array.toList array
