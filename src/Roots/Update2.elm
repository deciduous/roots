module Roots.Update2 exposing
    ( noop, also, many
    , apply, command, command_, attempt
    , if_, when
    , overAffineTraversal, overLens
    )

{-| Update.

@docs Update, noop, also, many
@docs apply, command, command_, attempt
@docs if_, when
@docs overAffineTraversal, overLens

-}

import Roots.AffineTraversal as AffineTraversal exposing (AffineTraversal)
import Roots.Eff2 as Eff exposing (Eff)
import Roots.Lens as Lens exposing (Lens)
import Task exposing (Task)


{-| The empty no-op update.
-}
noop : a -> Eff e a
noop x =
    ( x, Cmd.none )


{-| Sequence two updates.
-}
also : (b -> Eff e c) -> (a -> Eff e b) -> (a -> Eff e c)
also g f x =
    Eff.also g (f x)


{-| Sequence a list of updates into one, run left-to-right.
-}
many : List (a -> Eff e a) -> (a -> Eff e a)
many =
    List.foldl also Eff.wrap


{-| Make an update from a function.
-}
apply : (a -> b) -> (a -> Eff e b)
apply f x =
    ( f x, Cmd.none )


{-| Make an update from a command.
-}
command : (a -> Cmd e) -> (a -> Eff e a)
command f x =
    ( x, f x )


{-| Make an update from a command.
-}
command_ : Cmd e -> (a -> Eff e a)
command_ e x =
    ( x, e )


{-| Make an update from a task.
-}
attempt : (Result r s -> e) -> (a -> Task r s) -> (a -> Eff e a)
attempt f task =
    command (\x -> Task.attempt f (task x))


{-| Make an update conditional on its model.
-}
if_ : (a -> Bool) -> (a -> Eff e a) -> (a -> Eff e a)
if_ p f x =
    if p x then
        f x

    else
        ( x, Cmd.none )


{-| Make an update conditional on its model.
-}
when : (a -> Maybe b) -> (b -> (a -> Eff e a)) -> (a -> Eff e a)
when p f x =
    case p x of
        Nothing ->
            ( x, Cmd.none )

        Just y ->
            f y x


{-| Apply an update to the target of an affine traversal.
-}
overAffineTraversal : AffineTraversal s t a b -> (a -> Eff e b) -> (s -> Eff e t)
overAffineTraversal l f s =
    case AffineTraversal.matching l s of
        Ok a ->
            Eff.map (\b -> AffineTraversal.set l b s) (f a)

        Err t ->
            ( t, Cmd.none )


{-| Apply an update to the target of a lens.
-}
overLens : Lens s t a b -> (a -> Eff e b) -> (s -> Eff e t)
overLens l f s =
    Eff.map (\b -> Lens.set l b s) (f (Lens.view l s))
