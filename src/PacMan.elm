module PacMan exposing (main)

import Browser
import Html exposing (Html, div, node, text)
import Html.Attributes exposing (class, style)
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (fill, height, width, x, y)



-----------------------
-- GLOBAL VARIABLES --
-----------------------


fieldWidth : Int
fieldWidth =
    500


fieldHeight : Int
fieldHeight =
    500



------------
-- MODELS --
------------


type alias Model =
    Int



-----------------------
-- STYLESHEETS (CSS) --
-----------------------


wrapperCss : List (Html.Attribute msg)
wrapperCss =
    [ Html.Attributes.style "width" "100%"
    , Html.Attributes.style "height" "auto"
    ]


headlineCss : List (Html.Attribute msg)
headlineCss =
    [ Html.Attributes.style "width" "500px"
    , Html.Attributes.style "height" "auto"
    , Html.Attributes.style "background-color" "#000"
    , Html.Attributes.style "border" "10px solid #000"
    , Html.Attributes.style "color" "#fff"
    , Html.Attributes.style "margin" "3em auto 0em"
    , Html.Attributes.style "padding" "0em"
    , Html.Attributes.style "display" "flex"
    , Html.Attributes.style "justify-content" "space-between"
    ]


textCss : List (Html.Attribute msg)
textCss =
    [ Html.Attributes.style "text-align" "center"
    , Html.Attributes.style "font-family" "VT323, monospace"
    , Html.Attributes.style "font-weight" "bold"
    ]


gameCss : List (Html.Attribute msg)
gameCss =
    [ Html.Attributes.style "width" (String.fromInt fieldWidth ++ "px")
    , Html.Attributes.style "height" (String.fromInt fieldHeight ++ "px")
    , Html.Attributes.style "margin" "0em auto"
    , Html.Attributes.style "border-left" "10px solid #000"
    , Html.Attributes.style "border-right" "10px solid #000"
    , Html.Attributes.style "background-color" "#000"
    , Html.Attributes.style "display" "block"
    ]


styleContents : String
styleContents =
    """
    @import url('https://fonts.googleapis.com/css2?family=VT323&display=swap');
    body {
        background-color: black;
    }
    .headline {
        font-family: 'VT323', monospace;
    }
    """



----------
-- INIT --
----------


initialModel : Model
initialModel =
    0


type Msg
    = Test



------------
-- UPDATE --
------------


update : Msg -> Model -> Model
update msg model =
    case msg of
        Test ->
            model + 1



----------
-- VIEW --
----------


view : Model -> Html Msg
view model =
    node "main"
        []
        [ node "style" [] [ text styleContents ]
        , div (class "wrapper" :: wrapperCss)
            [ div (class "headline" :: headlineCss)
                [ div (textCss ++ [ Html.Attributes.style "text-transform" "uppercase" ]) [ Html.text "High score" ]
                , div textCss [ Html.text "0" ]
                , div textCss [ Html.text "500x500" ]
                ]
            , svg gameCss
                [ rect
                    [ x (String.fromInt 0)
                    , y (String.fromInt 0)
                    , Svg.Attributes.width (String.fromInt 100)
                    , Svg.Attributes.height (String.fromInt 100)
                    , fill "red"
                    ]
                    []
                ]
            ]
        ]



-------------------
-- MAIN PROGRAMM --
-------------------


main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, view = view, update = update }



---------------
-- FUNCTIONS --
---------------
