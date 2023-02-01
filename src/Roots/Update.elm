module Roots.Update exposing
    ( Update, empty, sequence
    , pure, command, impure, ask
    , if_, when
    , toUpdate
    )

{-|

@docs Update, empty, sequence
@docs pure, command, impure, ask
@docs if_, when
@docs toUpdate

-}

import Roots.Eff as Eff exposing (Eff)
import Roots.Internal.Eff as Eff
import Roots.Internal.Update as Update


type alias Update e a =
    Update.Update e a


{-| The no-op update.
-}
empty : Update e a
empty =
    Eff.pure


{-| Sequence a list of updates into one, run left-to-right.
-}
sequence : List (Update e a) -> Update e a
sequence =
    let
        f : Update e a -> Update e a -> Update e a
        f upd1 upds0 model0 =
            upds0 model0
                |> Eff.andThen upd1
    in
    List.foldl f empty


{-| Make an update from a pure function.
-}
pure : (a -> a) -> Update e a
pure f x =
    Eff.pure (f x)


{-| Make an update from a command.
-}
command : (a -> Cmd e) -> Update e a
command c x =
    Eff.pure x
        |> Eff.command (c x)


impure : (a -> Eff e a) -> Update e a
impure =
    identity


ask : (a -> Update e a) -> Update e a
ask f x =
    f x x


{-| Make an update conditional on its model.
-}
if_ : (a -> Bool) -> List (Update e a) -> Update e a
if_ p u x =
    if p x then
        sequence u x

    else
        Eff.pure x


{-| Make an update conditional on its model.
-}
when : (a -> Maybe b) -> (b -> List (Update e a)) -> Update e a
when p u x =
    case p x of
        Nothing ->
            Eff.pure x

        Just y ->
            sequence (u y) x


{-| FIXME don't export this once roots defines its own browser application
-}
toUpdate : Update e a -> a -> ( a, Cmd e )
toUpdate u x =
    Eff.toTuple (u x)
