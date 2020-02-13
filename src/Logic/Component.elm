module Logic.Component exposing
    ( Set, Spec, empty
    , set, spawn, remove
    , get, get2, update
    , map, filterMap
    , fromList, toList
    , fromDict, toDict
    )

{-| **Component**: the raw data for one aspect of the object, and how it interacts with the world. "Labels the Entity as possessing this particular aspect".

Example:

    type alias Velocity =
        { x : Float, y : Float }

    spec : Component.Spec Velocity { world | v : Component.Set Velocity }
    spec =
        Component.Spec .v (\comps world -> { world | v = comps })

    empty : Component.Set Velocity
    empty =
        Component.empty

@docs Set, Spec, empty


# Manipulations

@docs set, spawn, remove
@docs get, get2, update
@docs map, filterMap


# List

@docs fromList, toList


# Dict

@docs fromDict, toDict

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Logic.Internal exposing (indexedFoldlArray)


{-| Component storage, the main building block of the world
-}
type alias Set comp =
    Array (Maybe comp)


type alias EntityID =
    Int


{-| Component specification, how to get `Component.Set` from the world and set back into the world (mainly used by Systems)
-}
type alias Spec comp world =
    { get : world -> Set comp
    , set : Set comp -> world -> world
    }


{-| Create an empty `Component.Set` - mostly used to init component sets in the world.
-}
empty : Set comp
empty =
    Array.empty


{-| Remove component from `Component.Set` by `EntityID`, or return unchanged if component not in `Set`.
-}
remove : EntityID -> Set a -> Set a
remove entityID components =
    Array.set entityID Nothing components


{-| Safe way to create a component, same as `set`, only if an index is out of range `Component.Set` will be stretched.

    test =
        -- Just 5
        empty |> spawn 5 10 |> get 5

-}
spawn : EntityID -> a -> Set a -> Set a
spawn id value components =
    if id - Array.length components < 0 then
        Array.set id (Just value) components

    else
        Array.append components (Array.repeat (id - Array.length components) Nothing)
            |> Array.push (Just value)


{-| Set the component at a particular index. Returns an updated `Component.Set`. If the index is out of range, the `Component.Set` is unaltered.

    test =
        -- Nothing
        empty |> set 5 10 |> get 5

-}
set : EntityID -> a -> Set a -> Set a
set id value components =
    Array.set id (Just value) components


{-| Get component for `EntityId`.
-}
get : EntityID -> Set comp -> Maybe comp
get id =
    Array.get id >> Maybe.withDefault Nothing


{-| Get components Tuple for `EntityId`.
-}
get2 : EntityID -> Set comp -> Set comp2 -> Maybe ( comp, comp2 )
get2 id set1 set2 =
    Maybe.map2 Tuple.pair
        (get id set1)
        (get id set2)


{-| Filter out certain components.
-}
filterMap : (comp -> Maybe comp) -> Set comp -> Set comp
filterMap f comps =
    Array.map (Maybe.andThen f) comps


{-| Apply a function on every component in a `Component.Set`.

    map sqrt (fromList [ ( 0, 1 ), ( 1, 4 ), ( 2, 9 ) ])
        |> (==) fromList [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ) ]

-}
map : (comp -> comp) -> Set comp -> Set comp
map f comps =
    Array.map (Maybe.map f) comps


{-| Update Component by `EntityID`.
-}
update : EntityID -> (comp -> comp) -> Set comp -> Set comp
update i f =
    Logic.Internal.update i (Maybe.map f)


{-| Create a `Component.Set` from a `List`.

**Note**: Useful for data serialization.

-}
fromList : List ( EntityID, a ) -> Set a
fromList =
    List.foldl (\( index, value ) components -> spawn index value components) empty


{-| Convert a `Component.Set` into an association list of id-component pairs, sorted by id.

**Note**: Useful for data deserialization.

-}
toList : Set a -> List ( EntityID, a )
toList =
    indexedFoldlArray (\i a acc -> Maybe.map (\a_ -> ( i, a_ ) :: acc) a |> Maybe.withDefault acc) []


{-| Create a `Component.Set` from a dictionary.

**Note**: Useful for data serialization.

-}
fromDict : Dict EntityID a -> Set a
fromDict =
    Dict.foldl (\index value components -> spawn index value components) empty


{-| Create a dictionary from a `Component.Set`.

**Note**: Useful for data deserialization.

-}
toDict : Set a -> Dict EntityID a
toDict =
    indexedFoldlArray
        (\i a acc ->
            Maybe.map (\a_ -> Dict.insert i a_ acc) a
                |> Maybe.withDefault acc
        )
        Dict.empty
