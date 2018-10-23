module Main exposing (Model, Msg(..), init, main, update, view)

import Animation.Flip as Flip
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, src)



---- MODEL ----


type alias Model =
    {}


type alias FlipItem =
    { id : String, label : String }


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container m-t-lg" ]
        [ div [ class "columns" ]
            [ div [ class "column" ]
                [ viewListPanel model ]
            , div [ class "column is-one-quarter" ]
                [ button [ class "button is-fullwidth is-info" ] [ text "Shuffle" ]
                ]
            ]
        ]


viewListPanel : Model -> Html msg
viewListPanel model =
    nav [ class "panel" ] <|
        Flip.render
            { id = .id
            , childAttrs = always [ class "panel-block" ]
            , childElement = always div
            , childContents = \child -> [ text child.label ]
            }
            [ { id = "first-element", label = "I am the first child" }
            , { id = "second-element", label = "I am the second child" }
            , { id = "third-element", label = "I am the third child" }
            ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
