module Roots.Update3 exposing
    ( Update, run
    , noop, also
    , apply, command, command_, attempt
    , affineTraversal, lens
    )

{-| Update.

@docs Update, run
@docs noop, also, many
@docs apply, command, command_, attempt
@docs overAffineTraversal, overLens

-}

import Roots.AffineTraversal as AffineTraversal exposing (AffineTraversal)
import Roots.Eff2 as Eff exposing (Eff)
import Roots.Lens as Lens exposing (Lens)
import Task exposing (Task)


type alias Update e s t a b =
    (a -> Eff e b) -> (s -> Eff e t)


run : Update e s s a a -> (s -> Eff e s)
run f =
    f (\x -> ( x, Cmd.none ))


{-| The empty no-op update.
-}
noop : Update e a a a a
noop =
    identity


{-| Sequence two updates.
-}
also : (b -> Eff e c) -> Update e a c a b
also g f x =
    Eff.also g (f x)


{-| Make an update from a command.
-}
command : (b -> Cmd e) -> Update e a b a b
command g f x =
    Eff.also (\y -> ( y, g y )) (f x)


{-| Make an update from a command.
-}
command_ : Cmd e -> Update e a b a b
command_ e =
    also (\x -> ( x, e ))


{-| Make an update from a function.
-}
apply : (b -> c) -> Update e a c a b
apply f =
    also (\x -> ( f x, Cmd.none ))


{-
{-| Sequence a list of updates into one, run left-to-right.
-}
many : List (Update e a a) -> Update e a a
many =
    List.foldl also Eff.wrap
-}


{-| Make an update from a task.
-}
attempt : (Result r s -> e) -> (a -> Task r s) -> Update e a a a a
attempt f task =
    command (\x -> Task.attempt f (task x))


{-
{-| Make an update conditional on its model.
-}
if_ : (a -> Bool) -> Update e a a a a
if_ p f x =
    if p x then
        f x

    else
        ( x, Cmd.none )
-}


{-
{-| Make an update conditional on its model.
-}
when : (a -> Maybe b) -> (b -> Update e a a) -> Update e a a
when p f x =
    case p x of
        Nothing ->
            ( x, Cmd.none )
        Just y ->
            f y x
-}


{-| Apply an update to the target of an affine traversal.
-}
affineTraversal : AffineTraversal s t a b -> Update e s t a b
affineTraversal l f s =
    case AffineTraversal.matching l s of
        Ok a ->
            Eff.map (\b -> AffineTraversal.set l b s) (f a)

        Err t ->
            ( t, Cmd.none )


{-| Apply an update to the target of a lens.
-}
lens : Lens s t a b -> Update e s t a b
lens l f s =
    Eff.map (\b -> Lens.set l b s) (f (Lens.view l s))

