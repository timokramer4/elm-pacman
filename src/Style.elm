module Style exposing (gameChildCss, gameCss, ghostSvgCss, headlineCss, messageCss, pacmanSvgCss, styleContents, textCss, wrapperCss)

import Html
import Html.Attributes exposing (id, style)
import Settings exposing (fieldSettings, ghostSettings, pacSettings)



-----------------------
-- STYLESHEETS (CSS) --
-----------------------


wrapperCss : List (Html.Attribute msg)
wrapperCss =
    [ Html.Attributes.style "width" "100%"
    , Html.Attributes.style "height" "auto"
    , Html.Attributes.style "position" "absolute"
    , Html.Attributes.style "top" "50%"
    , Html.Attributes.style "transform" "translateY(-50%)"
    ]


headlineCss : List (Html.Attribute msg)
headlineCss =
    [ Html.Attributes.style "width" "500px"
    , Html.Attributes.style "height" "auto"
    , Html.Attributes.style "background-color" "#000"
    , Html.Attributes.style "border" "10px solid #000"
    , Html.Attributes.style "color" "#fff"
    , Html.Attributes.style "margin" "0em auto"
    , Html.Attributes.style "padding" "0em"
    , Html.Attributes.style "display" "flex"
    , Html.Attributes.style "justify-content" "space-between"
    ]


textCss : List (Html.Attribute msg)
textCss =
    [ Html.Attributes.style "text-align" "center"
    , Html.Attributes.style "font-family" "VT323, monospace"
    , Html.Attributes.style "font-weight" "bold"
    , Html.Attributes.style "font-size" "1.5em"
    ]


gameCss : List (Html.Attribute msg)
gameCss =
    [ Html.Attributes.style "position" "relative"
    , Html.Attributes.style "width" (String.fromInt fieldSettings.width ++ "px")
    , Html.Attributes.style "height" (String.fromInt fieldSettings.height ++ "px")
    , Html.Attributes.style "margin" "0em auto"
    , Html.Attributes.style "border-left" "10px solid #000"
    , Html.Attributes.style "border-right" "10px solid #000"
    , Html.Attributes.style "background-color" "#000"
    , Html.Attributes.style "display" "block"
    ]


gameChildCss : List (Html.Attribute msg)
gameChildCss =
    [ Html.Attributes.style "position" "absolute"
    , Html.Attributes.style "top" "0"
    , Html.Attributes.style "left" "0"
    , Html.Attributes.style "width" (String.fromInt fieldSettings.width ++ "px")
    , Html.Attributes.style "height" (String.fromInt fieldSettings.height ++ "px")
    , Html.Attributes.style "overflow" "hidden"
    ]


pacmanSvgCss : List (Html.Attribute msg)
pacmanSvgCss =
    [ id "pacman"
    , Html.Attributes.style "position" "absolute"
    , Html.Attributes.style "width" (String.fromInt pacSettings.ratio ++ "px")
    , Html.Attributes.style "height" (String.fromInt pacSettings.ratio ++ "px")
    ]


ghostSvgCss : List (Html.Attribute msg)
ghostSvgCss =
    [ id "ghost"
    , Html.Attributes.style "position" "absolute"
    , Html.Attributes.style "width" (String.fromInt ghostSettings.ratio ++ "px")
    , Html.Attributes.style "height" (String.fromInt ghostSettings.ratio ++ "px")
    ]


messageCss : List (Html.Attribute msg)
messageCss =
    [ Html.Attributes.style "left" "0px"
    , Html.Attributes.style "top" "267px"
    , Html.Attributes.style "position" "relative"
    , Html.Attributes.style "color" "#ffcc00"
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
