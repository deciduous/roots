module Roots.Update exposing
    ( Update, empty, sequence
    , pure, command, attempt, impure, ask
    , if_, when
    , toUpdate
    )

{-|

@docs Update, empty, sequence
@docs pure, command, attempt, impure, ask
@docs if_, when
@docs toUpdate

-}

import Roots.Eff as Eff exposing (Eff)
import Roots.Internal.Eff as Eff
import Roots.Internal.Update as Update
import Task exposing (Task)


type alias Update e a =
    Update.Update e a


{-| The no-op update.
-}
empty : Update e a
empty =
    []


{-| Sequence a list of updates into one, run left-to-right.
-}
sequence : List (Update e a) -> Update e a
sequence =
    List.concat


{-| Make an update from a pure function.
-}
pure : (a -> a) -> Update e a
pure f =
    [ \x ->
        Eff.Eff
            { model = f x
            , commands = []
            }
    ]


{-| Make an update from a command.
-}
command : (a -> Cmd e) -> Update e a
command c =
    [ \x ->
        Eff.Eff
            { model = x
            , commands = [ c x ]
            }
    ]


{-| Make an update from a task.
-}
attempt : (Result r s -> e) -> (a -> Task r s) -> Update e a
attempt f task =
    command (\x -> Task.attempt f (task x))


impure : (a -> Eff e a) -> Update e a
impure f =
    [ f ]


ask : (a -> Update e a) -> Update e a
ask f =
    [ \x ->
        Eff.pure x
            |> Eff.andThen (f x)
    ]


{-| Make an update conditional on its model.
-}
if_ : (a -> Bool) -> Update e a -> Update e a
if_ p u =
    [ \x ->
        if p x then
            Eff.pure x
                |> Eff.andThen u

        else
            Eff.pure x
    ]


{-| Make an update conditional on its model.
-}
when : (a -> Maybe b) -> (b -> Update e a) -> Update e a
when p u =
    [ \x ->
        case p x of
            Nothing ->
                Eff.pure x

            Just y ->
                Eff.pure x
                    |> Eff.andThen (u y)
    ]


{-| FIXME don't export this once roots defines its own browser application
-}
toUpdate : Update e a -> a -> ( a, Cmd e )
toUpdate u x =
    Eff.pure x
        |> Eff.andThen u
        |> Eff.toTuple
