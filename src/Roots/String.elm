module Roots.String exposing
    ( stripPrefix
    , stripPrefix_
    )

{-| String.

@docs stripPrefix stripPrefix_

-}


stripPrefix : String -> String -> Maybe String
stripPrefix prefix string =
    if String.startsWith prefix string then
        Just (String.dropLeft (String.length prefix) string)

    else
        Nothing


stripPrefix_ : String -> String -> String
stripPrefix_ prefix string =
    if String.startsWith prefix string then
        String.dropLeft (String.length prefix) string

    else
        string
