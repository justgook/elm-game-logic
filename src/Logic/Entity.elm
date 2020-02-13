module Logic.Entity exposing (EntityID, create, with, remove)

{-| **Entity**: The entity is a general-purpose object. It only consists of a unique ID. They "tag every coarse game object as a separate item".
Example:

    Entity.create id world
        |> Entity.with ( positionSpec, positionComponent )
        |> Entity.with ( velocitySpec, velocityComponent )

@docs EntityID, create, with, remove

-}

import Array
import Logic.Component as Component


{-| -}
type alias EntityID =
    Int


{-| Start point for spawning `Entity`

    Entity.create id world
        |> Entity.with ( positionSpec, positionComponent )
        |> Entity.with ( velocitySpec, velocityComponent )

-}
create : EntityID -> world -> ( EntityID, world )
create id world =
    ( id, world )


{-| Way to create `Entity` destruction functions, should pipe in all possible component specs.
It also can be used to just disable (remove) some components from an entity.

    remove =
        Entity.remove positionSpec
            >> Entity.remove velocitySpec

    newWorld =
        remove ( id, world )

-}
remove : Component.Spec comp world -> ( EntityID, world ) -> ( EntityID, world )
remove spec ( entityID, world ) =
    ( entityID, spec.set (Array.set entityID Nothing (spec.get world)) world )


{-| Set component to spawn with a new entity

    Entity.create ( id, world )
        |> Entity.with ( positionSpec, positionComponent )
        |> Entity.with ( velocitySpec, velocityComponent )

-}
with : ( Component.Spec comp world, comp ) -> ( EntityID, world ) -> ( EntityID, world )
with ( spec, component ) ( entityID, world ) =
    let
        updatedComponents =
            spec.get world
                |> Component.spawn entityID component

        updatedWorld =
            spec.set updatedComponents world
    in
    ( entityID, updatedWorld )
