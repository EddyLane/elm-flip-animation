module Animation.Flip exposing (Configuration, State, animate, init, render, subscriptions, updatePositions)

import Animation
import Animation.Spring.Presets exposing (Spring)
import DOM
import Dict exposing (Dict)
import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (attribute, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Set exposing (Set)



---- CONFIGURATION ----


type alias Configuration child msg =
    { id : child -> String
    , childAttrs : child -> List (Attribute msg)
    , childElement : child -> List (Attribute msg) -> List (Html msg) -> Html msg
    , childContents : child -> List (Html.Html msg)
    , updateMsg : State -> Cmd msg -> msg
    , animateMsg : String -> Animation.Msg -> msg
    , getBoundingClientRects : Encode.Value -> Cmd msg
    , gotBoundingClientRects : (Decode.Value -> msg) -> Sub msg
    , spring : Spring
    }



---- STATE ----


type State
    = State StateRec


type alias StateRec =
    { animations : Dict String Animation.State }


init : Configuration child msg -> List child -> ( State, Cmd msg )
init config children =
    ( State { animations = Dict.empty }
    , updatePositions config children
    )


updatePositions : Configuration child msg -> List child -> Cmd msg
updatePositions config children =
    config.getBoundingClientRects
        (children
            |> List.map config.id
            |> List.foldl Set.insert Set.empty
            |> Encode.set Encode.string
        )


animate : Animation.Msg -> String -> State -> State
animate animationMsg id (State state) =
    State { state | animations = Dict.update id (Maybe.map (Animation.update animationMsg)) state.animations }



---- VIEW ----


flipAttributeName : String
flipAttributeName =
    "data-elm-flip-id"


render : Configuration child msg -> State -> List child -> List (Html msg)
render config (State state) children =
    List.concat
        [ List.map (renderVisibleChild config state) children
        , List.map (renderHiddenChild config) children
        ]


renderHiddenChild : Configuration child msg -> child -> Html msg
renderHiddenChild { id, childAttrs, childElement, childContents } child =
    childElement child
        (List.concat
            [ childAttrs child
            , [ attribute flipAttributeName (id child)
              , style "visibility" "hidden"
              ]
            ]
        )
        (childContents child)


renderVisibleChild : Configuration child msg -> StateRec -> child -> Html msg
renderVisibleChild { id, childAttrs, childElement, childContents } { animations } child =
    case Dict.get (id child) animations of
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


subscriptions : Configuration child msg -> State -> Sub msg
subscriptions config (State state) =
    Sub.batch
        [ gotBoundingRectsSubs config state
        , animateSubs config state
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
