port module Main exposing (Model, Msg(..), init, main, update, view)

import Animation
import Animation.Flip as Flip
import Animation.Spring.Presets as Presets
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, classList, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
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
    , viewMode : ViewMode
    , searchInputValue : String
    }


type alias FlipItem =
    { id : String
    , label : String
    , icon : String
    }


type ViewMode
    = List
    | Grid


flipConfig : Flip.Configuration FlipItem Msg
flipConfig =
    { id = .id
    , updateMsg = UpdateFlip
    , animateMsg = AnimateFlip
    , getBoundingClientRects = getBoundingClientRects
    , gotBoundingClientRects = gotBoundingClientRects
    , spring =
        { stiffness = 210
        , damping = 20
        }
    }


init : ( Model, Cmd Msg )
init =
    let
        flipItems =
            [ { id = "1", label = "Get rid of all of the spiders", icon = "fa-spider" }
            , { id = "2", label = "Learn to write Haskell", icon = "fa-language" }
            , { id = "3", label = "Water the plants", icon = "fa-seedling" }
            , { id = "4", label = "Eat a cookie ", icon = "fa-cookie" }
            , { id = "5", label = "I can't see anything", icon = "fa-low-vision" }
            , { id = "6", label = "I love to eat Nandos", icon = "fa-grin-hearts" }
            , { id = "7", label = "Front page of the internet", icon = "fa-globe" }
            ]

        ( flip, flipCmd ) =
            Flip.init flipConfig flipItems
    in
    ( { flip = flip
      , children = flipItems
      , viewMode = List
      , searchInputValue = ""
      }
    , flipCmd
    )



---- UPDATE ----


type Msg
    = UpdateFlip Flip.State (Cmd Msg)
    | AnimateFlip String Animation.Msg
    | Shuffle
    | Shuffled (List FlipItem)
    | SetViewMode ViewMode
    | SetSearchInputValue String


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
            , Cmd.none
            )

        SetViewMode viewMode ->
            ( { model | viewMode = viewMode }
            , Cmd.none
            )

        SetSearchInputValue searchInputValue ->
            ( { model | searchInputValue = searchInputValue }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container m-t-lg" ]
        [ div [ class "columns" ]
            [ div [ class "column" ]
                [ viewListPanel model ]
            , div [ class "column is-one-quarter" ]
                [ button
                    [ class "button is-fullwidth is-info m-b-sm"
                    , onClick Shuffle
                    ]
                    [ span [ class "icon p-r-lg" ]
                        [ i [ class "fas fa-random" ] []
                        ]
                    , text "Shuffle"
                    ]
                ]
            ]
        ]


viewListPanel : Model -> Html Msg
viewListPanel model =
    nav
        [ class "panel"
        , style "background-color" "white"
        , style "max-height" "600px"
        , style "height" "auto"
        , style "transition" "max-height 3s ease-out"
        ]
        [ p [ class "panel-heading" ] [ text "Animated stuff" ]
        , div [ class "panel-block" ]
            [ p [ class "control has-icons-left" ]
                [ input
                    [ class "input is-small"
                    , placeholder "search"
                    , type_ "text"
                    , value model.searchInputValue
                    , onInput SetSearchInputValue
                    ]
                    []
                , span [ class "icon is-small is-left" ]
                    [ i [ class "fas fa-search" ]
                        []
                    ]
                ]
            ]
        , p [ class "panel-tabs" ]
            [ a
                [ classList [ ( "is-active", model.viewMode == List ) ]
                , onClick (SetViewMode List)
                ]
                [ span [ class "icon" ]
                    [ i [ class "fas fa-list" ] []
                    ]
                , text "List"
                ]
            , a
                [ classList [ ( "is-active", model.viewMode == Grid ) ]
                , onClick (SetViewMode Grid)
                ]
                [ span [ class "icon" ]
                    [ i [ class "fas fa-th-large" ] []
                    ]
                , text "Grid"
                ]
            ]
        , div (flipContainerAttributes model.viewMode) <|
            Flip.render
                { config = flipConfig
                , children = model.children
                , state = model.flip
                , childElement = always div
                , childAttrs = flipItemAttributes model.viewMode model.searchInputValue
                , childContents = flipItemContents model.viewMode model.searchInputValue
                }
        ]


flipContainerAttributes : ViewMode -> List (Html.Attribute Msg)
flipContainerAttributes viewMode =
    case viewMode of
        Grid ->
            [ style "position" "relative"
            , style "display" "flex"
            , style "flex-flow" "row wrap"
            ]

        List ->
            [ style "position" "relative" ]


flipItemContents : ViewMode -> String -> FlipItem -> List (Html.Html Msg)
flipItemContents viewMode filterQuery flipItem =
    [ span [ class "panel-icon" ]
        [ i
            [ class "fas"
            , class flipItem.icon
            ]
            []
        ]
    , text flipItem.label
    ]


flipItemAttributes : ViewMode -> String -> FlipItem -> List (Html.Attribute Msg)
flipItemAttributes viewMode filterQuery flipItem =
    case viewMode of
        Grid ->
            [ class "panel-block"
            , style "flex" "auto"
            , flipItemClassList filterQuery flipItem
            ]

        List ->
            [ class "panel-block"
            , flipItemClassList filterQuery flipItem
            ]


flipItemClassList : String -> FlipItem -> Html.Attribute Msg
flipItemClassList filterQuery flipItem =
    classList
        [ ( "flip-item-hidden"
          , if filterQuery == "" then
                False

            else
                not <| String.contains (String.toLower filterQuery) (String.toLower flipItem.label)
          )
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Flip.subscriptions flipConfig model.flip model.children



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
