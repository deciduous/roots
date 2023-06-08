module Roots.Update exposing
    ( Update, empty, sequence
    , pure, command, attempt, eff, ask
    , if_, when
    , overAffineTraversal, overLens
    , toUpdate
    )

{-| Update.

@docs Update, empty, sequence
@docs pure, command, attempt, eff, ask
@docs if_, when
@docs overAffineTraversal, overLens
@docs toUpdate

-}

import Roots.AffineTraversal as AffineTraversal exposing (AffineTraversal_)
import Roots.Eff as Eff exposing (Eff)
import Roots.Internal.Update as Update
import Roots.Lens as Lens exposing (Lens_)
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
    [ \x -> Eff.pure (f x)
    ]


{-| Make an update from a command.
-}
command : (a -> Cmd e) -> Update e a
command c =
    [ \x ->
        Eff.pure x
            |> Eff.command (c x)
    ]


{-| Make an update from a task.
-}
attempt : (Result r s -> e) -> (a -> Task r s) -> Update e a
attempt f task =
    command (\x -> Task.attempt f (task x))


{-| Make an update from an effectful value.
-}
eff : (a -> Eff e a) -> Update e a
eff f =
    [ f ]


ask : (a -> Update e a) -> Update e a
ask f =
    [ \x -> toEff (f x) x ]


{-| Make an update conditional on its model.
-}
if_ : (a -> Bool) -> Update e a -> Update e a
if_ p update =
    [ \x ->
        if p x then
            toEff update x

        else
            Eff.pure x
    ]


{-| Make an update conditional on its model.
-}
when : (a -> Maybe b) -> (b -> Update e a) -> Update e a
when p update =
    [ \x ->
        case p x of
            Nothing ->
                Eff.pure x

            Just y ->
                toEff (update y) x
    ]


{-| Apply an update to the target of an affine traversal.
-}
overAffineTraversal : AffineTraversal_ s a -> Update e a -> Update e s
overAffineTraversal affineTraversal update =
    [ \s ->
        case AffineTraversal.preview affineTraversal s of
            Nothing ->
                Eff.pure s

            Just a ->
                Eff.map
                    (\a1 -> AffineTraversal.set affineTraversal a1 s)
                    (toEff update a)
    ]


{-| Apply an update to the target of a lens.
-}
overLens : Lens_ s a -> Update e a -> Update e s
overLens lens update =
    [ \s ->
        Eff.map
            (\a1 -> Lens.set lens a1 s)
            (toEff update (Lens.view lens s))
    ]


toEff : Update e a -> a -> Eff e a
toEff update x =
    Eff.pure x
        |> Eff.andThen update


{-| FIXME don't export this once roots defines its own browser application
-}
toUpdate : Update e a -> a -> ( a, Cmd e )
toUpdate update x =
    Eff.toTuple (toEff update x)
