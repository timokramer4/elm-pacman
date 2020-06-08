module PacMan exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Dict exposing (Dict, get)
import Html exposing (Html, div, img, node, text)
import Html.Attributes exposing (class, id, src, style)
import Json.Decode exposing (..)
import Svg exposing (Svg, circle, path, polygon, svg)
import Svg.Attributes exposing (cx, cy, d, fill, points, r, transform, x, y)
import Time exposing (every)



-----------------------
-- GLOBAL VARIABLES --
-----------------------


fieldWidth : Float
fieldWidth =
    500


fieldHeight : Float
fieldHeight =
    500


gameColor : String
gameColor =
    "#3498DB"


pacSettings : { ratio : Float }
pacSettings =
    { ratio = 30
    }


movement : Float
movement =
    1


runMesh : Dict Int Line
runMesh =
    Dict.fromList
        [ ( 1, Line { x = 168, y = 283 } { x = 332, y = 283 } )
        , ( 2, Line { x = 168, y = 187 } { x = 168, y = 283 } ) 
        , ( 3, Line { x = 332, y = 187 } { x = 168, y = 187 } )
        , ( 4, Line { x = 332, y = 283 } { x = 332, y = 187 } )
        , ( 5, Line { x = 24, y = 29 } { x = 222, y = 29 } )
        , ( 6, Line { x = 24, y = 29 } { x = 24, y = 140 } )
        , ( 7, Line { x = 24, y = 92 } { x = 477, y = 92 } )
        , ( 8, Line { x = 114, y = 29 } { x = 114, y = 426 } )
        , ( 9, Line { x = 222, y = 29 } { x = 222, y = 92 } )
        , ( 10, Line { x = 278, y = 29 } { x = 278, y = 92 } )
        , ( 11, Line { x = 278, y = 29 } { x = 477, y = 29 } )
        , ( 12, Line { x = 386, y = 29 } { x = 386, y = 426 } )
        , ( 13, Line { x = 477, y = 29 } { x = 477, y = 140 } )
        , ( 14, Line { x = 477, y = 140 } { x = 386, y = 140 } )
        , ( 15, Line { x = 331, y = 92 } { x = 331, y = 140 } )
        , ( 16, Line { x = 331, y = 140 } { x = 278, y = 140 } )
        , ( 17, Line { x = 278, y = 140 } { x = 278, y = 187 } )
        , ( 18, Line { x = 169, y = 92 } { x = 169, y = 140 } )
        , ( 19, Line { x = 169, y = 140 } { x = 222, y = 140 } )
        , ( 20, Line { x = 222, y = 140 } { x = 222, y = 187 } )
        , ( 21, Line { x = -, y = - } { x = -, y = - } )
        , ( 22, Line { x = -, y = - } { x = -, y = - } )
        , ( 23, Line { x = -, y = - } { x = -, y = - } )
        , ( 24, Line { x = -, y = - } { x = -, y = - } )
        , ( 25, Line { x = -, y = - } { x = -, y = - } )
        , ( 26, Line { x = -, y = - } { x = -, y = - } )
        , ( 27, Line { x = -, y = - } { x = -, y = - } )
        , ( 28, Line { x = -, y = - } { x = -, y = - } )
        , ( 29, Line { x = -, y = - } { x = -, y = - } )
        , ( 30, Line { x = -, y = - } { x = -, y = - } )
        , ( 31, Line { x = 24, y = 426 } { x = 114, y = 426 } )
        , ( 32, Line { x = 477, y = 473 } { x = 24, y = 473 } )
        , ( 33, Line { x = -, y = - } { x = -, y = - } )
        , ( 34, Line { x = 169, y = 426 } { x = 222, y = 426 } )
        , ( 35, Line { x = 222, y = 426 } { x = 222, y = 473 } )
        , ( 36, Line { x = 477, y = 473 } { x = 24, y = 426 } )
        , ( 37, Line { x = -, y = - } { x = -, y = - } )
        , ( 38, Line { x = 279, y = 426 } { x = 331, y = 426 } )
        , ( 39, Line { x = 279, y = 426 } { x = 279, y = 473 } )
        , ( 40, Line { x = -, y = - } { x = -, y = - } )
        , ( 41, Line { x = -, y = - } { x = -, y = - } )
        , ( 42, Line { x = -, y = - } { x = -, y = - } )
        , ( 43, Line { x = 386, y = 426 } { x = 477, y = 426 } )
        , ( 44, Line { x = 477, y = 473 } { x = 477, y = 426 } )
        , ( 45, Line { x = -, y = - } { x = -, y = - } )
        , ( 46, Line { x = 24, y = 140 } { x = 114, y = 140 } )
        ]



------------
-- MODELS --
------------


type alias Point =
    { x : Float
    , y : Float
    }


type alias Line =
    { start : Point
    , end : Point
    }


type alias PacMan =
    { position : Point
    , rotation : Int
    , state : State
    , highScore : Float
    , highScore2 : Float
    }


type Direction
    = Up
    | Down
    | Left
    | Right



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
    , Html.Attributes.style "width" (String.fromFloat fieldWidth ++ "px")
    , Html.Attributes.style "height" (String.fromFloat fieldHeight ++ "px")
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
    , Html.Attributes.style "width" (String.fromFloat fieldWidth ++ "px")
    , Html.Attributes.style "height" (String.fromFloat fieldHeight ++ "px")
    , Html.Attributes.style "overflow" "hidden"
    ]


pacmanSvgCss : List (Html.Attribute msg)
pacmanSvgCss =
    [ id "pacman"
    , Html.Attributes.style "position" "absolute"
    , Html.Attributes.style "width" (String.fromFloat pacSettings.ratio ++ "px")
    , Html.Attributes.style "height" (String.fromFloat pacSettings.ratio ++ "px")
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


initialModel : PacMan
initialModel =
    { position = { x = 250, y = 283 }
    , state = Running Right
    , rotation = 0
    , highScore = 0
    , highScore2 = 0
    }


type Msg
    = MoveDirection Direction
    | Nothing
    | NoMoving


type State
    = Running Direction
    | Stopped Direction



------------
-- UPDATE --
------------


update : Msg -> PacMan -> ( PacMan, Cmd Msg )
update msg pac =
    case msg of
        MoveDirection d ->
            case d of
                Left ->
                    if outOfBounds pac then
                        ( { pac | position = changeXPosition fieldWidth pac, state = Running d, rotation = 180 }, Cmd.none )

                    else if getMesh runMesh { x = pac.position.x - movement, y = pac.position.y } then
                        ( { pac | position = changeXPosition (pac.position.x - movement) pac, state = Running d, rotation = 180 }, Cmd.none )

                    else
                        ( { pac | highScore = pac.position.x, highScore2 = pac.position.y }, Cmd.none )

                -- update NoMoving pac
                Right ->
                    if outOfBounds pac then
                        ( { pac | position = changeXPosition 0 pac, state = Running d, rotation = 0 }, Cmd.none )

                    else if getMesh runMesh { x = pac.position.x + movement, y = pac.position.y } then
                        ( { pac | position = changeXPosition (pac.position.x + movement) pac, state = Running d, rotation = 0 }, Cmd.none )

                    else
                        ( { pac | highScore = pac.position.x, highScore2 = pac.position.y }, Cmd.none )

                Up ->
                    if outOfBounds pac then
                        ( { pac | position = changeYPosition fieldHeight pac, state = Running d, rotation = -90 }, Cmd.none )

                    else if getMesh runMesh { x = pac.position.x, y = pac.position.y - movement } then
                        ( { pac | position = changeYPosition (pac.position.y - movement) pac, state = Running d, rotation = -90 }, Cmd.none )

                    else
                        ( { pac | highScore = pac.position.x, highScore2 = pac.position.y }, Cmd.none )

                Down ->
                    if outOfBounds pac then
                        ( { pac | position = changeYPosition 0 pac, state = Running d, rotation = 90 }, Cmd.none )

                    else if getMesh runMesh { x = pac.position.x, y = pac.position.y + movement } then
                        ( { pac | position = changeYPosition (pac.position.y + movement) pac, state = Running d, rotation = 90 }, Cmd.none )

                    else
                        ( { pac | highScore = pac.position.x, highScore2 = pac.position.y }, Cmd.none )

        Nothing ->
            case pac.state of
                Running d ->
                    ( { pac | state = Stopped d }, Cmd.none )

                Stopped d ->
                    ( { pac | state = Running d }, Cmd.none )

        NoMoving ->
            ( pac, Cmd.none )



----------
-- VIEW --
----------


view : PacMan -> Html Msg
view pac =
    node "main"
        []
        [ node "style" [] [ text styleContents ]
        , div (class "wrapper" :: wrapperCss)
            [ div (class "headline" :: headlineCss)
                [ div (textCss ++ [ Html.Attributes.style "text-transform" "uppercase" ]) [ Html.text "High score" ]
                , div textCss [ Html.text ("(" ++ String.fromFloat pac.highScore ++ ", " ++ String.fromFloat pac.highScore2 ++ ")") ]
                , div textCss [ Html.text "500x500" ]
                ]
            , div
                gameCss
                [ svg
                    (gameChildCss
                        ++ [ id "gameField" ]
                    )
                    [ circle [ cx "330", cy "283", r "5", fill "red" ] []
                    , path [ fill gameColor, d "M200.3,74.7h-65c-2.8,0-5-2.3-5-5V50.3c0-2.8,2.2-5,5-5h65c2.8,0,5,2.3,5,5v19.3  C205.3,72.4,203.1,74.7,200.3,74.7z" ] []
                    , path [ fill gameColor, d "M364,74.7h-65c-2.8,0-5-2.3-5-5V50.3c0-2.8,2.3-5,5-5h65c2.8,0,5,2.3,5,5v19.3C369,72.4,366.8,74.7,364,74.7z" ] []
                    , path [ fill gameColor, d "M92,74.7H44.7c-2.8,0-5-2.3-5-5V50.3c0-2.8,2.3-5,5-5H92c2.8,0,5,2.3,5,5v19.3C97,72.4,94.8,74.7,92,74.7z" ] []
                    , path [ fill gameColor, d "M92,122.3H44.7c-2.8,0-5-2.3-5-5v-4.7c0-2.8,2.3-5,5-5H92c2.8,0,5,2.3,5,5v4.7C97,120.1,94.8,122.3,92,122.3z" ] []
                    , path [ fill gameColor, d "M455.3,123.3H408c-2.8,0-5-2.3-5-5v-4.7c0-2.8,2.3-5,5-5h47.3c2.8,0,5,2.3,5,5v4.7  C460.3,121.1,458.1,123.3,455.3,123.3z" ] []
                    , path [ fill gameColor, d "M455.3,74.7H408c-2.8,0-5-2.3-5-5V50.3c0-2.8,2.3-5,5-5h47.3c2.8,0,5,2.3,5,5v19.3  C460.3,72.4,458.1,74.7,455.3,74.7z" ] []
                    , path [ fill gameColor, d "M309.3,108.7H189.7c-2.8,0-5,2.3-5,5v4.7c0,2.8,2.3,5,5,5H235c2.8,0,5,2.2,5,5v37.3c0,2.8,2.3,5,5,5h10.3  c2.8,0,5-2.3,5-5v-37.3c0-2.8,2.2-5,5-5h44c2.8,0,5-2.3,5-5v-4.7C314.3,110.9,312.1,108.7,309.3,108.7z" ] []
                    , path [ fill gameColor, d "M200.3,155.3H157c-2.8,0-5-2.2-5-5v-36.7c0-2.8-2.3-5-5-5h-11.7c-2.8,0-5,2.3-5,5v99.7c0,2.8,2.3,5,5,5H147  c2.8,0,5-2.3,5-5v-38.7c0-2.8,2.2-5,5-5h43.3c2.8,0,5-2.3,5-5v-4.3C205.3,157.6,203.1,155.3,200.3,155.3z" ] []
                    , path [ fill gameColor, d "M300,170.7l43.3,0c2.8,0,5,2.2,5,5l0,36.7c0,2.8,2.3,5,5,5l11.7,0c2.8,0,5-2.3,5-5l0-99.7c0-2.8-2.3-5-5-5  h-11.7c-2.8,0-5,2.3-5,5v38.7c0,2.8-2.2,5-5,5l-43.3,0c-2.8,0-5,2.3-5,5v4.3C295,168.4,297.3,170.7,300,170.7z" ] []
                    , path [ fill gameColor, d "M309.3,300H189.7c-2.8,0-5,2.3-5,5v4.7c0,2.8,2.3,5,5,5H235c2.8,0,5,2.2,5,5V357c0,2.8,2.3,5,5,5h10.3  c2.8,0,5-2.3,5-5v-37.3c0-2.8,2.2-5,5-5h44c2.8,0,5-2.3,5-5V305C314.3,302.2,312.1,300,309.3,300z" ] []
                    , path [ fill gameColor, d "M364,312.7h-10c-2.8,0-5-2.3-5-5V256c0-2.8,2.3-5,5-5h10c2.8,0,5,2.3,5,5v51.7C369,310.4,366.8,312.7,364,312.7  z" ] []
                    , path [ fill gameColor, d "M146,312.7h-10c-2.8,0-5-2.3-5-5V256c0-2.8,2.3-5,5-5h10c2.8,0,5,2.3,5,5v51.7C151,310.4,148.8,312.7,146,312.7  z" ] []
                    , path [ fill gameColor, d "M200.3,360.7h-65c-2.8,0-5-2.3-5-5v-3.3c0-2.8,2.2-5,5-5h65c2.8,0,5,2.3,5,5v3.3  C205.3,358.4,203.1,360.7,200.3,360.7z" ] []
                    , path [ fill gameColor, d "M365,360.7h-65c-2.8,0-5-2.3-5-5v-3.3c0-2.8,2.2-5,5-5h65c2.8,0,5,2.3,5,5v3.3C370,358.4,367.8,360.7,365,360.7  z" ] []
                    , path [ fill gameColor, d "M309.3,395.3H189.7c-2.8,0-5,2.3-5,5v4.7c0,2.8,2.3,5,5,5H235c2.8,0,5,2.2,5,5v37.3c0,2.8,2.3,5,5,5h10.3  c2.8,0,5-2.3,5-5V415c0-2.8,2.2-5,5-5h44c2.8,0,5-2.3,5-5v-4.7C314.3,397.6,312.1,395.3,309.3,395.3z" ] []
                    , path [ fill gameColor, d "M200.3,442.7H156c-2.8,0-5-2.2-5-5v-38.3c0-2.8-2.3-5-5-5h-10c-2.8,0-5,2.3-5,5v38.3c0,2.8-2.2,5-5,5H44  c-2.8,0-5,2.2-5,5v3.3c0,2.8,2.3,5,5,5h156.3c2.8,0,5-2.3,5-5v-3.3C205.3,444.9,203.1,442.7,200.3,442.7z" ] []
                    , path [ fill gameColor, d "M300,442.7h44.3c2.8,0,5-2.2,5-5v-38.3c0-2.8,2.3-5,5-5h10c2.8,0,5,2.3,5,5v38.3c0,2.8,2.2,5,5,5h82  c2.8,0,5,2.2,5,5v3.3c0,2.8-2.3,5-5,5H300c-2.8,0-5-2.3-5-5v-3.3C295,444.9,297.3,442.7,300,442.7z" ] []
                    , path [ fill gameColor, d "M92,347.3h-7.7h-4H44c-2.8,0-5,2.2-5,5v3.3c0,2.8,2.3,5,5,5h26.3c2.8,0,5,2.2,5,5V404c0,2.8,2.3,5,5,5H92  c2.8,0,5-2.3,5-5v-51.7C97,349.6,94.8,347.3,92,347.3z" ] []
                    , path [ fill gameColor, d "M408.3,347.3h7.7h4h36.3c2.8,0,5,2.2,5,5v3.3c0,2.8-2.3,5-5,5H430c-2.8,0-5,2.2-5,5V404c0,2.8-2.3,5-5,5h-11.7  c-2.8,0-5-2.3-5-5v-51.7C403.3,349.6,405.6,347.3,408.3,347.3z" ] []

                    -- Outer lines and inner cave
                    , path [ fill gameColor, d "M407.7,208.5v-42c0-2.8,2.2-5,5-5h83.9c1.5,0,2.7-1.2,2.7-2.7V8.9c0-1.5-1.2-2.7-2.7-2.7H4.8  c-1.5,0-2.7,1.2-2.7,2.7v150.4c0,1.5,1.2,2.7,2.7,2.7h81.8c2.8,0,5,2.2,5,5V208c0,2.8-2.2,5-5,5H0v5.4h94.3c1.5,0,2.7-1.2,2.7-2.7  v-56.3c0-1.5-1.2-2.7-2.7-2.7H12.5c-2.8,0-5-2.2-5-5v-135c0-2.8,2.2-5,5-5h221c2.8,0,5,2.2,5,5v53c0,2.8,2.2,5,5,5H255  c2.8,0,5-2.2,5-5v-53c0-2.8,2.2-5,5-5h223.9c2.8,0,5,2.2,5,5v134.5c0,2.8-2.2,5-5,5H405c-1.5,0-2.7,1.2-2.7,2.7v57.1  c0,0.1,0,0.1,0,0.2c0,0.1,0,0.1,0,0.2c0,1.5,1.2,2.7,2.7,2.7h95v-5.4h-87.3C409.9,213.5,407.7,211.3,407.7,208.5z" ] []
                    , path [ fill gameColor, d "M411.4,256.7H500v-5.3h-94.5c-1.2,0-2.2,1.2-2.2,2.7c0,0,0,0,0,0c0,0,0,0,0,0v57.3c0,1.5,1.2,2.7,2.7,2.7h85.4  c1.5,0,2.7,1.2,2.7,2.7v72.6c0,2.8-2.2,5-5,5h-24.9c-2.8,0-5,2.2-5,5v4.7c0,2.8,2.2,5,5,5h24.9c2.8,0,5,2.2,5,5v73.1  c0,1.5-1.2,2.7-2.7,2.7H10.1c-1.5,0-2.7-1.2-2.7-2.7V414c0-2.8,2.2-5,5-5h25.1c2.8,0,5-2.2,5-5v-4.7c0-2.8-2.2-5-5-5H12.4  c-2.8,0-5-2.2-5-5v-72.1c0-1.5,1.2-2.7,2.7-2.7h84.2c0.1,0,0.2,0,0.3,0c0.1,0,0.2,0,0.3,0c1.5,0,2.7-1.2,2.7-2.7v-57  c0-1.5-1.2-2.7-2.7-2.7H0v5.4h89.6c1.5,0,2.7,1.2,2.7,2.7v46.2c0,1.5-1.2,2.7-2.7,2.7H4.7c-1.5,0-2.7,1.2-2.7,2.7v180.6  c0,1.5,1.2,2.7,2.7,2.7h492.1c1.5,0,2.7-1.2,2.7-2.7V311.3c0-1.5-1.2-2.7-2.7-2.7h-85.4c-1.5,0-2.7-1.2-2.7-2.7v-46.6  C408.7,257.9,409.9,256.7,411.4,256.7z" ] []
                    , polygon [ fill gameColor, points "309.9,204.5 266.6,204.5 266.6,209.3 309.9,209.3 309.9,260.9 190.1,260.9 190.1,209.3 233,209.3   233,204.5 190.1,204.5 185.4,204.5 185.4,209.3 185.4,260.9 185.4,265.7 190.1,265.7 309.9,265.7 314.6,265.7 314.6,260.9   314.6,209.3 314.6,204.5 " ] []
                    ]
                , div
                    (gameChildCss
                        ++ [ id "pacmanArea" ]
                    )
                    [ img (pacmanSvgCss ++ [ src "https://upload.wikimedia.org/wikipedia/commons/thumb/4/49/Pacman.svg/972px-Pacman.svg.png", Html.Attributes.style "top" (String.fromFloat (pac.position.y - pacSettings.ratio / 2) ++ "px"), Html.Attributes.style "left" (String.fromFloat (pac.position.x - pacSettings.ratio / 2) ++ "px"), Html.Attributes.style "transform" ("rotate(" ++ String.fromInt pac.rotation ++ "deg)") ])
                        []
                    ]
                ]
            , div (class "headline" :: headlineCss)
                [ div (textCss ++ [ Html.Attributes.style "text-transform" "uppercase" ]) [ Html.text "Leben:" ]
                , div (textCss ++ [ Html.Attributes.style "text-align" "left" ]) [ Html.text "3" ]
                , div (textCss ++ [ Html.Attributes.style "text-transform" "uppercase" ]) [ Html.text "Früchte:" ]
                , div textCss [ Html.text "Kirsche" ]
                ]
            ]
        ]



-------------------
-- Subscriptions --
-------------------


subscriptions : PacMan -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        , case model.state of
            Running d ->
                Time.every 20 (\_ -> MoveDirection d)

            _ ->
                Sub.none
        ]



-------------------
-- MAIN PROGRAMM --
-------------------


main : Program () PacMan Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        , update = update
        }



---------------
-- FUNCTIONS --
---------------
-- key functions


keyDecoder : Json.Decode.Decoder Msg
keyDecoder =
    Json.Decode.map toKey (Json.Decode.field "key" Json.Decode.string)


toKey : String -> Msg
toKey string =
    case string of
        "ArrowUp" ->
            MoveDirection Up

        "ArrowDown" ->
            MoveDirection Down

        "ArrowLeft" ->
            MoveDirection Left

        "ArrowRight" ->
            MoveDirection Right

        _ ->
            Nothing


changeXPosition : Float -> PacMan -> Point
changeXPosition value pac =
    let
        oldPosition =
            pac.position
    in
    { oldPosition | x = value }


changeYPosition : Float -> PacMan -> Point
changeYPosition value pac =
    let
        oldPosition =
            pac.position
    in
    { oldPosition | y = value }


outOfBounds : PacMan -> Bool
outOfBounds pac =
    pac.position.x < 0 || pac.position.x > fieldWidth || pac.position.y < 0 || pac.position.y > fieldHeight


getMesh : Dict Int Line -> Point -> Bool
getMesh mesh pos =
    Dict.foldl ((\x -> checkPath x) pos) False mesh


checkPath : Point -> Int -> Line -> Bool -> Bool
checkPath pos _ line e =
    (pos.x >= min line.start.x line.end.x && pos.x <= max line.start.x line.end.x && pos.y >= min line.start.y line.end.y && pos.y <= max line.start.y line.end.y) || e
