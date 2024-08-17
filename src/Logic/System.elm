module Logic.System exposing
    ( System
    , update, step, step2, step3, step4, step5
    , foldl, foldl2, foldl3, foldl4, foldl5
    , indexedFoldl, indexedFoldl2, indexedFoldl3, indexedFoldl4, indexedFoldl5
    , applyIf, applyMaybe
    )

{-| **System**: main logic driver, that is used to stepping on each game-loop and update `World`

@docs System
@docs update, step, step2, step3, step4, step5

@docs foldl, foldl2, foldl3, foldl4, foldl5
@docs indexedFoldl, indexedFoldl2, indexedFoldl3, indexedFoldl4, indexedFoldl5


# Util

@docs applyIf, applyMaybe

-}

import Array
import Logic.Component as Component
import Logic.Entity exposing (EntityID)
import Logic.Internal exposing (indexedFoldlArray)


{-| Update whole `Component.Set`
-}
update : Component.Spec comp world -> (Component.Set comp -> Component.Set comp) -> System world
update spec f world =
    spec.set (f (spec.get world)) world


{-| -}
type alias System world =
    world -> world


{-| Reduce a `Component.Set` from the left.

Example count how much enemies left in the world:

    enemySet =
        enemySpec.get world

    count =
        foldl (\_ -> (+) 1) enemySet 0

-}
foldl : (comp1 -> acc -> acc) -> Component.Set comp1 -> acc -> acc
foldl f comp1 acc_ =
    Array.foldl
        (\value acc ->
            value |> Maybe.map (\a -> f a acc) |> Maybe.withDefault acc
        )
        acc_
        comp1


{-| Variant of `foldl` that passes the index of the current element to the step function.

`indexedFoldl` is to `foldl` as `List.indexedMap` is to `List.map`.

-}
indexedFoldl : (EntityID -> comp1 -> acc -> acc) -> Component.Set comp1 -> acc -> acc
indexedFoldl f comp1 acc_ =
    indexedFoldlArray
        (\i value acc ->
            value |> Maybe.map (\a -> f i a acc) |> Maybe.withDefault acc
        )
        acc_
        comp1


{-| Step over all entities that have both components and reduce an `Component.Set`s from the left.
-}
foldl2 : (comp1 -> comp2 -> acc -> acc) -> Component.Set comp1 -> Component.Set comp2 -> acc -> acc
foldl2 f comp1 comp2 acc_ =
    indexedFoldlArray
        (\n value acc ->
            Maybe.map2 (\a b -> f a b acc)
                value
                (Component.get n comp2)
                |> Maybe.withDefault acc
        )
        acc_
        comp1


{-| -}
indexedFoldl2 : (EntityID -> comp1 -> comp2 -> acc -> acc) -> Component.Set comp1 -> Component.Set comp2 -> acc -> acc
indexedFoldl2 f comp1 comp2 acc_ =
    indexedFoldlArray
        (\n value acc ->
            Maybe.map2 (\a b -> f n a b acc)
                value
                (Component.get n comp2)
                |> Maybe.withDefault acc
        )
        acc_
        comp1


{-| Same as [`foldl2`](#foldl2) only with 3 components
-}
foldl3 : (comp1 -> comp2 -> comp3 -> acc -> acc) -> Component.Set comp1 -> Component.Set comp2 -> Component.Set comp3 -> acc -> acc
foldl3 f comp1 comp2 comp3 acc_ =
    indexedFoldlArray
        (\n value acc ->
            Maybe.map3 (\a b c -> f a b c acc)
                value
                (Component.get n comp2)
                (Component.get n comp3)
                |> Maybe.withDefault acc
        )
        acc_
        comp1


{-| Same as [`indexedFoldl2`](#indexedFoldl2) only with 3 components
-}
indexedFoldl3 : (EntityID -> comp1 -> comp2 -> comp3 -> acc -> acc) -> Component.Set comp1 -> Component.Set comp2 -> Component.Set comp3 -> acc -> acc
indexedFoldl3 f comp1 comp2 comp3 acc_ =
    indexedFoldlArray
        (\n value acc ->
            Maybe.map3 (\a b c -> f n a b c acc)
                value
                (Component.get n comp2)
                (Component.get n comp3)
                |> Maybe.withDefault acc
        )
        acc_
        comp1


{-| Same as [`foldl2`](#foldl2) only with 4 components
-}
foldl4 :
    (comp1 -> comp2 -> comp3 -> comp4 -> acc -> acc)
    -> Component.Set comp1
    -> Component.Set comp2
    -> Component.Set comp3
    -> Component.Set comp4
    -> acc
    -> acc
foldl4 f comp1 comp2 comp3 comp4 acc_ =
    indexedFoldlArray
        (\n value acc ->
            Maybe.map4 (\a b c d -> f a b c d acc)
                value
                (Component.get n comp2)
                (Component.get n comp3)
                (Component.get n comp4)
                |> Maybe.withDefault acc
        )
        acc_
        comp1


{-| Same as [`indexedFoldl2`](#indexedFoldl2) only with 4 components
-}
indexedFoldl4 :
    (EntityID -> comp1 -> comp2 -> comp3 -> comp4 -> acc -> acc)
    -> Component.Set comp1
    -> Component.Set comp2
    -> Component.Set comp3
    -> Component.Set comp4
    -> acc
    -> acc
indexedFoldl4 f comp1 comp2 comp3 comp4 acc_ =
    indexedFoldlArray
        (\n value acc ->
            Maybe.map4 (\a b c d -> f n a b c d acc)
                value
                (Component.get n comp2)
                (Component.get n comp3)
                (Component.get n comp4)
                |> Maybe.withDefault acc
        )
        acc_
        comp1


{-| Same as [`foldl2`](#foldl2) only with 5 components
-}
foldl5 :
    (comp1 -> comp2 -> comp3 -> comp4 -> acc -> acc)
    -> Component.Set comp1
    -> Component.Set comp2
    -> Component.Set comp3
    -> Component.Set comp4
    -> acc
    -> acc
foldl5 f comp1 comp2 comp3 comp4 acc_ =
    indexedFoldlArray
        (\n value acc ->
            Maybe.map4 (\a b c d -> f a b c d acc)
                value
                (Component.get n comp2)
                (Component.get n comp3)
                (Component.get n comp4)
                |> Maybe.withDefault acc
        )
        acc_
        comp1


{-| Same as [`indexedFoldl2`](#indexedFoldl2) only with 5 components
-}
indexedFoldl5 :
    (EntityID -> comp1 -> comp2 -> comp3 -> comp4 -> comp5 -> acc -> acc)
    -> Component.Set comp1
    -> Component.Set comp2
    -> Component.Set comp3
    -> Component.Set comp4
    -> Component.Set comp5
    -> acc
    -> acc
indexedFoldl5 f comp1 comp2 comp3 comp4 comp5 acc_ =
    indexedFoldlArray
        (\n value acc ->
            Maybe.map5 (\a b c d e -> f n a b c d e acc)
                value
                (Component.get n comp2)
                (Component.get n comp3)
                (Component.get n comp4)
                (Component.get n comp5)
                |> Maybe.withDefault acc
        )
        acc_
        comp1


{-| Single component mapping, Same as`List.map` - only for `Component.Set` inside `World`

    gravitySystem =
        Logic.System.step (Vec2.add gravity) accelerationSpec

-}
step : (comp -> comp) -> Component.Spec comp world -> System world
step f { get, set } world =
    set (get world |> Array.map (Maybe.map f)) world


type alias Acc2 a b =
    { a : Component.Set a
    , b : Component.Set b
    }


{-| Step over all entities that have both components.

Example:

    system =
        Logic.System.step2
            (\( v, _ ) ( p, setP ) -> setP (Vec2.add v p))
            velocitySpec
            positionSpec

-}
step2 :
    (( a, a -> System (Acc2 a b) )
     -> ( b, b -> System (Acc2 a b) )
     -> System (Acc2 a b)
    )
    -> Component.Spec a world
    -> Component.Spec b world
    -> System world
step2 f spec1 spec2 world =
    let
        set1 i a acc =
            { acc | a = Array.set i (Just a) acc.a }

        set2 i b acc =
            { acc | b = Array.set i (Just b) acc.b }

        combined =
            { a = spec1.get world, b = spec2.get world }

        result =
            indexedFoldlArray
                (\n value acc ->
                    Maybe.map2 (\a b -> f ( a, set1 n ) ( b, set2 n ) acc)
                        value
                        (Component.get n acc.b)
                        |> Maybe.withDefault acc
                )
                combined
                combined.a
    in
    world
        |> applyIf (result.a /= combined.a) (spec1.set result.a)
        |> applyIf (result.b /= combined.b) (spec2.set result.b)


type alias Acc3 a b c =
    { a : Component.Set a
    , b : Component.Set b
    , c : Component.Set c
    }


{-| Same as [`step2`](#step2) only with 3 components
-}
step3 :
    (( a, a -> System (Acc3 a b c) )
     -> ( b, b -> System (Acc3 a b c) )
     -> ( c, c -> System (Acc3 a b c) )
     -> System (Acc3 a b c)
    )
    -> Component.Spec a world
    -> Component.Spec b world
    -> Component.Spec c world
    -> System world
step3 f spec1 spec2 spec3 world =
    let
        set1 i a acc =
            { acc | a = Array.set i (Just a) acc.a }

        set2 i b acc =
            { acc | b = Array.set i (Just b) acc.b }

        set3 i c acc =
            { acc | c = Array.set i (Just c) acc.c }

        combined =
            { a = spec1.get world, b = spec2.get world, c = spec3.get world }

        result =
            indexedFoldlArray
                (\n value acc ->
                    Maybe.map3
                        (\a b c -> f ( a, set1 n ) ( b, set2 n ) ( c, set3 n ) acc)
                        value
                        (Component.get n acc.b)
                        (Component.get n acc.c)
                        |> Maybe.withDefault acc
                )
                combined
                combined.a
    in
    world
        |> applyIf (result.a /= combined.a) (spec1.set result.a)
        |> applyIf (result.b /= combined.b) (spec2.set result.b)
        |> applyIf (result.c /= combined.c) (spec3.set result.c)


type alias Acc4 a b c d =
    { a : Component.Set a
    , b : Component.Set b
    , c : Component.Set c
    , d : Component.Set d
    }


{-| Same as [`step2`](#step2) only with 4 components
-}
step4 :
    (( a, a -> System (Acc4 a b c d) )
     -> ( b, b -> System (Acc4 a b c d) )
     -> ( c, c -> System (Acc4 a b c d) )
     -> ( d, d -> System (Acc4 a b c d) )
     -> System (Acc4 a b c d)
    )
    -> Component.Spec a world
    -> Component.Spec b world
    -> Component.Spec c world
    -> Component.Spec d world
    -> System world
step4 f spec1 spec2 spec3 spec4 world =
    let
        set1 i a acc =
            { acc | a = Array.set i (Just a) acc.a }

        set2 i b acc =
            { acc | b = Array.set i (Just b) acc.b }

        set3 i c acc =
            { acc | c = Array.set i (Just c) acc.c }

        set4 i d acc =
            { acc | d = Array.set i (Just d) acc.d }

        combined =
            { a = spec1.get world
            , b = spec2.get world
            , c = spec3.get world
            , d = spec4.get world
            }

        result =
            indexedFoldlArray
                (\n value acc ->
                    Maybe.map4
                        (\a b c d -> f ( a, set1 n ) ( b, set2 n ) ( c, set3 n ) ( d, set4 n ) acc)
                        value
                        (Component.get n acc.b)
                        (Component.get n acc.c)
                        (Component.get n acc.d)
                        |> Maybe.withDefault acc
                )
                combined
                combined.a
    in
    world
        |> applyIf (result.a /= combined.a) (spec1.set result.a)
        |> applyIf (result.b /= combined.b) (spec2.set result.b)
        |> applyIf (result.c /= combined.c) (spec3.set result.c)
        |> applyIf (result.d /= combined.d) (spec4.set result.d)


type alias Acc5 a b c d e =
    { a : Component.Set a
    , b : Component.Set b
    , c : Component.Set c
    , d : Component.Set d
    , e : Component.Set e
    }


{-| Same as [`step2`](#step2) only with 5 components
-}
step5 :
    (( a, a -> System (Acc5 a b c d e) )
     -> ( b, b -> System (Acc5 a b c d e) )
     -> ( c, c -> System (Acc5 a b c d e) )
     -> ( d, d -> System (Acc5 a b c d e) )
     -> ( e, e -> System (Acc5 a b c d e) )
     -> System (Acc5 a b c d e)
    )
    -> Component.Spec a world
    -> Component.Spec b world
    -> Component.Spec c world
    -> Component.Spec d world
    -> Component.Spec e world
    -> System world
step5 f spec1 spec2 spec3 spec4 spec5 world =
    let
        set1 i a acc =
            { acc | a = Array.set i (Just a) acc.a }

        set2 i b acc =
            { acc | b = Array.set i (Just b) acc.b }

        set3 i c acc =
            { acc | c = Array.set i (Just c) acc.c }

        set4 i d acc =
            { acc | d = Array.set i (Just d) acc.d }

        set5 i e acc =
            { acc | e = Array.set i (Just e) acc.e }

        combined =
            { a = spec1.get world
            , b = spec2.get world
            , c = spec3.get world
            , d = spec4.get world
            , e = spec5.get world
            }

        result =
            indexedFoldlArray
                (\n value acc ->
                    Maybe.map5
                        (\a b c d e -> f ( a, set1 n ) ( b, set2 n ) ( c, set3 n ) ( d, set4 n ) ( e, set5 n ) acc)
                        value
                        (Component.get n acc.b)
                        (Component.get n acc.c)
                        (Component.get n acc.d)
                        (Component.get n acc.e)
                        |> Maybe.withDefault acc
                )
                combined
                combined.a
    in
    world
        |> applyIf (result.a /= combined.a) (spec1.set result.a)
        |> applyIf (result.b /= combined.b) (spec2.set result.b)
        |> applyIf (result.c /= combined.c) (spec3.set result.c)
        |> applyIf (result.d /= combined.d) (spec4.set result.d)
        |> applyIf (result.e /= combined.e) (spec5.set result.e)


{-| Just nice helper function to pipe into systems

    update msg world =
        world
            |> system1
            |> applyIf (msg === KeyUp "a") systemMoveLeft
            |> system2

-}
applyIf : Bool -> (a -> a) -> a -> a
applyIf bool f world =
    if bool then
        f world

    else
        world


{-| Same as [`applyIf`](#applyIf), but works with `Maybe`

    update msg world =
        world
            |> system1
            |> applyMaybe (decode saveDecoder msg) loadGame
            |> system2

-}
applyMaybe : Maybe a -> (a -> c -> c) -> c -> c
applyMaybe m f world =
    case m of
        Just a ->
            f a world

        Nothing ->
            world
