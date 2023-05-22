module Roots.Dict exposing
    ( adjust
    , upsert
    )

{-| Dict.

@docs adjust upsert

-}

import Dict exposing (Dict)


adjust : comparable -> (v -> v) -> Dict comparable v -> Dict comparable v
adjust k f =
    Dict.update k (Maybe.map f)


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
