port module Main exposing (Model, Msg(..), init, main, update, view)

import Animation
import Animation.Flip as Flip
import Animation.Spring.Presets as Presets
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Random.List



---- PORTS ----


port getBoundingClientRects : Encode.Value -> Cmd msg


port gotBoundingClientRects : (Decode.Value -> msg) -> Sub msg



---- MODEL ----


type alias Model =
    { flip : Flip.State
    , children : List FlipItem
    }


type alias FlipItem =
    { id : String, label : String }


flipConfig : Flip.Configuration FlipItem Msg
flipConfig =
    { id = .id
    , childAttrs = always [ class "panel-block" ]
    , childElement = always div
    , childContents = \child -> [ text child.label ]
    , updateMsg = UpdateFlip
    , animateMsg = AnimateFlip
    , getBoundingClientRects = getBoundingClientRects
    , gotBoundingClientRects = gotBoundingClientRects
    , spring = Presets.stiff
    }


init : ( Model, Cmd Msg )
init =
    let
        flipItems =
            [ { id = "1", label = "I am the first child" }
            , { id = "2", label = "I am the second child" }
            , { id = "3", label = "I am the third child" }
            , { id = "4", label = "I am the fourth child" }
            , { id = "5", label = "I am the fifth child" }
            , { id = "6", label = "I am the sixth child" }
            , { id = "7", label = "I am the seventh child" }
            , { id = "8", label = "I am the eighth child" }
            , { id = "9", label = "I am the nineth child" }
            ]

        ( flip, flipCmd ) =
            Flip.init flipConfig flipItems
    in
    ( { flip = flip
      , children = flipItems
      }
    , flipCmd
    )



---- UPDATE ----


type Msg
    = NoOp
    | UpdateFlip Flip.State (Cmd Msg)
    | AnimateFlip String Animation.Msg
    | Shuffle
    | Shuffled (List FlipItem)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateFlip flip flipCmd ->
            ( { model | flip = flip }
            , flipCmd
            )

        AnimateFlip id subMsg ->
            ( { model | flip = Flip.animate subMsg id model.flip }
            , Cmd.none
            )

        Shuffle ->
            ( model
            , Random.generate Shuffled (Random.List.shuffle model.children)
            )

        Shuffled children ->
            ( { model | children = children }
            , Flip.updatePositions flipConfig children
            )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container m-t-lg" ]
        [ div [ class "columns" ]
            [ div [ class "column" ]
                [ viewListPanel model ]
            , div [ class "column is-one-quarter" ]
                [ button [ class "button is-fullwidth is-info", onClick Shuffle ] [ text "Shuffle" ]
                ]
            ]
        ]


viewListPanel : Model -> Html Msg
viewListPanel model =
    nav [ class "panel", style "position" "relative" ] (Flip.render flipConfig model.flip model.children)



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Flip.subscriptions flipConfig model.flip



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
