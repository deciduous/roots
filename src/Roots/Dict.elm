module Roots.Dict exposing
    ( fromArray
    , adjust, upsert
    )

{-| Dict.

@docs fromArray
@docs adjust, upsert

-}

import Array exposing (Array)
import Dict exposing (Dict)


{-| Modify an existing value.
-}
adjust : comparable -> (v -> v) -> Dict comparable v -> Dict comparable v
adjust k f =
    Dict.update k (Maybe.map f)


fromArray : Array ( comparable, v ) -> Dict comparable v
fromArray =
    Array.toList >> Dict.fromList


{-| Modify an existing value, or insert one if it doesn't exist.
-}
upsert : comparable -> v -> (v -> v) -> Dict comparable v -> Dict comparable v
upsert k v0 v1 =
    Dict.update
        k
        (\x ->
            case x of
                Nothing ->
                    Just v0

                Just y ->
                    Just (v1 y)
        )
