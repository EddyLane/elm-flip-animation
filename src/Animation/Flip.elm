module Animation.Flip exposing (ChildConfig, State, render)

import Animation
import Dict exposing (Dict)
import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (attribute)



---- STATE ----


type State
    = State StateRec


type alias StateRec =
    { animations : Dict String Animation.State }



---- VIEW ----


type alias ChildConfig child msg =
    { id : child -> String
    , childAttrs : child -> List (Attribute msg)
    , childElement : child -> List (Attribute msg) -> List (Html msg) -> Html msg
    , childContents : child -> List (Html.Html msg)
    }


render : ChildConfig child msg -> List child -> List (Html msg)
render config children =
    List.map (renderChild config) children


renderChild : ChildConfig child msg -> child -> Html msg
renderChild { id, childAttrs, childElement, childContents } child =
    childElement child
        (List.concat [ childAttrs child, [ attribute "data-flip-id" (id child) ] ])
        (childContents child)
