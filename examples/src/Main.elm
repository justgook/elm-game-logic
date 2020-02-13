module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (style)
import Logic.Component as Component
import Logic.Entity as Entity
import Logic.System as System exposing (System, applyIf)


main : Program () World Float
main =
    Browser.element
        { init = \_ -> ( spawn world, Cmd.none )
        , update = \_ w -> ( system velocitySpec positionSpec w, Cmd.none )
        , subscriptions = \_ -> Browser.Events.onAnimationFrameDelta identity
        , view =
            \w ->
                System.foldl
                    (\( px, py ) acc ->
                        div
                            [ style "width" "30px"
                            , style "height" "30px"
                            , style "position" "absolute"
                            , style "top" "0"
                            , style "left" "0"
                            , style "background" "red"
                            , style "transform" ("translate(" ++ String.fromInt px ++ "px, " ++ String.fromInt py ++ "px)")
                            ]
                            []
                            :: acc
                    )
                    (positionSpec.get w)
                    []
                    |> div []
        }


system : Component.Spec ( Int, Int ) World -> Component.Spec ( Int, Int ) World -> System World
system spec1 spec2 w =
    System.step2
        (\( ( vx, vy ), setVel ) ( ( px, py ), setPos ) acc ->
            let
                x =
                    vx + px

                y =
                    vy + py
            in
            acc
                |> setPos ( x, y )
                |> applyIf (x < 0) (setVel ( abs vx, vy ))
                |> applyIf (x > w.windowWidth) (setVel ( abs vx * -1, vy ))
                |> applyIf (y < 0) (setVel ( vx, abs vy ))
                |> applyIf (y > w.windowHeight) (setVel ( vx, abs vy * -1 ))
        )
        spec1
        spec2
        w


type alias World =
    { position : Component.Set ( Int, Int )
    , velocity : Component.Set ( Int, Int )
    , windowWidth : Int
    , windowHeight : Int
    }


world : World
world =
    { position = Component.empty
    , velocity = Component.empty
    , windowWidth = 800
    , windowHeight = 600
    }


spawn : World -> World
spawn w_ =
    List.range 0 10
        |> List.foldl
            (\i ->
                Entity.create i
                    >> Entity.with ( positionSpec, ( i * 3, i * 5 ) )
                    >> Entity.with ( velocitySpec, ( modBy 3 i + 1, modBy 5 i + 1 ) )
                    >> Tuple.second
            )
            w_


positionSpec : Component.Spec ( Int, Int ) { world | position : Component.Set ( Int, Int ) }
positionSpec =
    Component.Spec .position (\comps w -> { w | position = comps })


velocitySpec : Component.Spec ( Int, Int ) { world | velocity : Component.Set ( Int, Int ) }
velocitySpec =
    Component.Spec .velocity (\comps w -> { w | velocity = comps })
