module Roots.String exposing
    ( stripPrefix
    , stripPrefix_
    )

{-| String.

@docs stripPrefix
@docs stripPrefix_

-}


{-| Strip a prefix from a string, and return the resulting string, if the prefix was stripped.
-}
stripPrefix : String -> String -> Maybe String
stripPrefix prefix string =
    if String.startsWith prefix string then
        Just (String.dropLeft (String.length prefix) string)

    else
        Nothing


{-| Strip a prefix from a string, if it exists.
-}
stripPrefix_ : String -> String -> String
stripPrefix_ prefix string =
    if String.startsWith prefix string then
        String.dropLeft (String.length prefix) string

    else
        string
