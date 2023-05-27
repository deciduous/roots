module Roots.Dict exposing
    ( adjust
    , upsert
    )

{-| Dict.

@docs adjust
@docs upsert

-}

import Dict exposing (Dict)


{-| Modify an existing value.
-}
adjust : comparable -> (v -> v) -> Dict comparable v -> Dict comparable v
adjust k f =
    Dict.update k (Maybe.map f)


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
