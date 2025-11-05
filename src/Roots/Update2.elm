module Roots.Update2 exposing
    ( then_, sequence
    , pure, command, attempt
    , if_, when
    , overAffineTraversal, overLens
    )

{-| Update.

@docs Update, empty, then_, sequence
@docs pure, command, attempt
@docs if_, when
@docs overAffineTraversal, overLens

-}

import Roots.AffineTraversal as AffineTraversal exposing (AffineTraversal)
import Roots.Eff2 as Eff exposing (Eff)
import Roots.Lens as Lens exposing (Lens)
import Task exposing (Task)


{-| Sequence two updates.
-}
then_ : (b -> Eff e c) -> (a -> Eff e b) -> (a -> Eff e c)
then_ g f x =
    Eff.then_ g (f x)


{-| Sequence a list of updates into one, run left-to-right.
-}
sequence : List (a -> Eff e a) -> (a -> Eff e a)
sequence =
    List.foldl then_ Eff.wrap


{-| Make an update from a pure function.
-}
pure : (a -> b) -> (a -> Eff e b)
pure f x =
    Eff.wrap (f x)


{-| Make an update from a command.
-}
command : (a -> Cmd e) -> (a -> Eff e a)
command f x =
    Eff.command (f x) (Eff.wrap x)


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
        Eff.wrap x


{-| Make an update conditional on its model.
-}
when : (a -> Maybe b) -> (b -> (a -> Eff e a)) -> (a -> Eff e a)
when p f x =
    case p x of
        Nothing ->
            Eff.wrap x

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
            Eff.wrap t


{-| Apply an update to the target of a lens.
-}
overLens : Lens s t a b -> (a -> Eff e b) -> (s -> Eff e t)
overLens l f s =
    Eff.map (\b -> Lens.set l b s) (f (Lens.view l s))
