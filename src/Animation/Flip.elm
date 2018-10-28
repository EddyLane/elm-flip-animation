module Animation.Flip exposing
    ( Configuration, RenderConfig
    , State, init
    , render, animate
    , subscriptions
    )

{-| Provides a simple way of achieving FLIP style animations.


# Configuration

@docs Configuration, RenderConfig


# State

@docs State, init


# Animate

@docs render, animate


# Subscriptions

@docs subscriptions

-}

import Animation
import Animation.Spring.Presets exposing (Spring)
import Browser.Events
import DOM
import Dict exposing (Dict)
import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (attribute, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Set exposing (Set)



---- CONFIGURATION ----


{-| Configuration for the flip animation
-}
type alias Configuration child msg =
    { id : child -> String
    , updateMsg : State -> Cmd msg -> msg
    , animateMsg : String -> Animation.Msg -> msg
    , getBoundingClientRects : Encode.Value -> Cmd msg
    , gotBoundingClientRects : (Decode.Value -> msg) -> Sub msg
    , spring : Spring
    }



---- STATE ----


{-| Opaque type for the state of the animation
-}
type State
    = State StateRec


type alias StateRec =
    { animations : Dict String Animation.State }


{-| Init a fresh flip animation
-}
init : Configuration child msg -> List child -> ( State, Cmd msg )
init config children =
    ( State { animations = Dict.empty }
    , updatePositions config children
    )


{-| Update the position of an element in the flip animation
-}
animate : Animation.Msg -> String -> State -> State
animate animationMsg id (State state) =
    State { state | animations = Dict.update id (Maybe.map (Animation.update animationMsg)) state.animations }


updatePositions : Configuration child msg -> List child -> Cmd msg
updatePositions config children =
    config.getBoundingClientRects
        (children
            |> List.map config.id
            |> List.foldl Set.insert Set.empty
            |> Encode.set Encode.string
        )



---- VIEW ----


{-| Render configuration for the flip animation, including the overall config

    Flip.render
        { config = flipConfig
        , children = model.children
        , state = model.flip
        , childElement = always div
        , childAttrs = always []
        , childContents = .label >> text
        }

-}
type alias RenderConfig child msg =
    { config : Configuration child msg
    , state : State
    , children : List child
    , childAttrs : child -> List (Attribute msg)
    , childElement : child -> List (Attribute msg) -> List (Html msg) -> Html msg
    , childContents : child -> List (Html.Html msg)
    }


flipAttributeName : String
flipAttributeName =
    "data-elm-flip-id"


{-| Render a flip animation.

More info on method and reasoning soon!

-}
render : RenderConfig child msg -> List (Html msg)
render renderConfig =
    List.concat
        [ List.map (renderVisibleChild renderConfig) renderConfig.children
        , List.map (renderHiddenChild renderConfig) renderConfig.children
        ]


renderHiddenChild : RenderConfig child msg -> child -> Html msg
renderHiddenChild { config, childAttrs, childElement, childContents } child =
    childElement child
        (List.concat
            [ childAttrs child
            , [ attribute flipAttributeName (config.id child)
              , style "visibility" "hidden"
              ]
            ]
        )
        (childContents child)


renderVisibleChild : RenderConfig child msg -> child -> Html msg
renderVisibleChild { config, state, childAttrs, childElement, childContents } child =
    let
        (State { animations }) =
            state
    in
    case Dict.get (config.id child) animations of
        Just animation ->
            childElement child
                (List.concat
                    [ childAttrs child
                    , Animation.render animation
                    , [ style "position" "absolute" ]
                    ]
                )
                (childContents child)

        Nothing ->
            text ""



---- SUBSCRIPTIONS ----


{-| Flip subscriptions
-}
subscriptions : Configuration child msg -> State -> List child -> Sub msg
subscriptions config (State state) children =
    let
        forceUpdate =
            config.updateMsg (State state) (updatePositions config children)
    in
    Sub.batch
        [ gotBoundingRectsSubs config state
        , animateSubs config state
        , Browser.Events.onResize (\_ _ -> forceUpdate)
        , Browser.Events.onAnimationFrame (\_ -> forceUpdate)
        ]


gotBoundingRectsSubs : Configuration child msg -> StateRec -> Sub msg
gotBoundingRectsSubs config state =
    config.gotBoundingClientRects
        (Decode.decodeValue (Decode.list gotBoundingClientRectsDecoder)
            >> Result.map (updateAnimations config state)
            >> Result.withDefault (updateAnimations config state [])
        )


animateSubs : Configuration child msg -> StateRec -> Sub msg
animateSubs config state =
    state.animations
        |> Dict.toList
        |> List.map
            (\( id, animation ) ->
                Animation.subscription (config.animateMsg id) [ animation ]
            )
        |> Sub.batch


type alias GotBoundingClientRect =
    { id : String
    , rectangle : DOM.Rectangle
    }


gotBoundingClientRectsDecoder : Decode.Decoder GotBoundingClientRect
gotBoundingClientRectsDecoder =
    Decode.map2 GotBoundingClientRect
        (Decode.field "id" Decode.string)
        (Decode.field "rectangle"
            (Decode.map4 DOM.Rectangle
                (Decode.field "top" Decode.float)
                (Decode.field "left" Decode.float)
                (Decode.field "width" Decode.float)
                (Decode.field "height" Decode.float)
            )
        )


updateAnimations : Configuration child msg -> StateRec -> List GotBoundingClientRect -> msg
updateAnimations config state positions =
    config.updateMsg
        (State { state | animations = List.foldl (updateAnimation config.spring) state.animations positions })
        Cmd.none


updateAnimation : Spring -> GotBoundingClientRect -> Dict String Animation.State -> Dict String Animation.State
updateAnimation spring { id, rectangle } animations =
    let
        animationList =
            [ Animation.translate (Animation.px rectangle.left) (Animation.px rectangle.top)
            , Animation.width (Animation.px rectangle.width)
            , Animation.height (Animation.px rectangle.height)
            ]
    in
    Dict.update id
        (\maybeAnim ->
            case maybeAnim of
                Just animation ->
                    Just <|
                        Animation.interrupt [ Animation.to animationList ] animation

                Nothing ->
                    Just <|
                        Animation.styleWith (Animation.spring spring) animationList
        )
        animations
