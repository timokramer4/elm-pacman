module PacMan exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Dict exposing (Dict, get)
import Html exposing (Html, div, img, node, text)
import Html.Attributes exposing (class, id, src, style)
import Json.Decode exposing (..)
import Json.Encode exposing (bool, float, int)
import List exposing (..)
import Svg exposing (Svg, circle, path, polygon, rect, svg)
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


pointStep : Float
pointStep =
    15


pointSize : Float
pointSize =
    5


pointMesh : Dict Int Point
pointMesh =
    Dict.fromList
        [ ( 1, { x = 25, y = 25 } )
        , ( 2, { x = 115, y = 25 } )
        , ( 3, { x = 220, y = 25 } )
        , ( 4, { x = 280, y = 25 } )
        , ( 5, { x = 385, y = 25 } )
        , ( 6, { x = 475, y = 25 } )
        , ( 7, { x = 25, y = 85 } )
        , ( 8, { x = 175, y = 85 } )
        , ( 9, { x = 220, y = 85 } )
        , ( 10, { x = 280, y = 85 } )
        , ( 11, { x = 325, y = 85 } )
        , ( 12, { x = 475, y = 85 } )
        , ( 13, { x = 25, y = 145 } )
        , ( 14, { x = 115, y = 145 } )
        , ( 15, { x = 175, y = 145 } )
        , ( 16, { x = 220, y = 145 } )
        , ( 17, { x = 280, y = 145 } )
        , ( 18, { x = 325, y = 145 } )
        , ( 19, { x = 385, y = 145 } )
        , ( 20, { x = 475, y = 145 } )
        , ( 21, { x = 175, y = 190 } )
        , ( 22, { x = 220, y = 190 } )
        , ( 23, { x = 280, y = 190 } )
        , ( 24, { x = 325, y = 190 } )
        , ( 25, { x = 0 - pacSettings.ratio / 2, y = 235 } )
        , ( 26, { x = 175, y = 235 } )
        , ( 27, { x = 325, y = 235 } )
        , ( 28, { x = 500 + pacSettings.ratio / 2, y = 235 } )
        , ( 29, { x = 175, y = 280 } )
        , ( 30, { x = 325, y = 280 } )
        , ( 31, { x = 25, y = 325 } )
        , ( 32, { x = 175, y = 325 } )
        , ( 33, { x = 220, y = 325 } )
        , ( 34, { x = 280, y = 325 } )
        , ( 35, { x = 325, y = 325 } )
        , ( 36, { x = 475, y = 325 } )
        , ( 37, { x = 25, y = 370 } )
        , ( 38, { x = 55, y = 370 } )
        , ( 39, { x = 115, y = 370 } )
        , ( 40, { x = 175, y = 370 } )
        , ( 41, { x = 220, y = 370 } )
        , ( 42, { x = 280, y = 370 } )
        , ( 43, { x = 325, y = 370 } )
        , ( 44, { x = 385, y = 370 } )
        , ( 45, { x = 445, y = 370 } )
        , ( 46, { x = 475, y = 370 } )
        , ( 47, { x = 25, y = 430 } )
        , ( 48, { x = 55, y = 430 } )
        , ( 49, { x = 115, y = 430 } )
        , ( 50, { x = 175, y = 430 } )
        , ( 51, { x = 220, y = 430 } )
        , ( 52, { x = 280, y = 430 } )
        , ( 53, { x = 325, y = 430 } )
        , ( 54, { x = 385, y = 430 } )
        , ( 55, { x = 445, y = 430 } )
        , ( 56, { x = 475, y = 430 } )
        , ( 57, { x = 25, y = 475 } )
        , ( 58, { x = 220, y = 475 } )
        , ( 59, { x = 280, y = 475 } )
        , ( 60, { x = 475, y = 475 } )
        ]


getPoint : Int -> Point
getPoint i =
    case get i pointMesh of
        Just point ->
            point

        _ ->
            { x = 0, y = 0 }


runMesh : Dict Int Line
runMesh =
    Dict.fromList
        [ ( 1, Line (getPoint 29) (getPoint 30) )
        , ( 2, Line (getPoint 21) (getPoint 32) )
        , ( 3, Line (getPoint 21) (getPoint 24) )
        , ( 4, Line (getPoint 24) (getPoint 35) )
        , ( 5, Line (getPoint 1) (getPoint 3) )
        , ( 6, Line (getPoint 1) (getPoint 13) )
        , ( 7, Line (getPoint 7) (getPoint 12) )
        , ( 8, Line (getPoint 2) (getPoint 49) )
        , ( 9, Line (getPoint 3) (getPoint 9) )
        , ( 10, Line (getPoint 4) (getPoint 10) )
        , ( 11, Line (getPoint 4) (getPoint 6) )
        , ( 12, Line (getPoint 5) (getPoint 54) )
        , ( 13, Line (getPoint 6) (getPoint 20) )
        , ( 14, Line (getPoint 19) (getPoint 20) )
        , ( 15, Line (getPoint 11) (getPoint 18) )
        , ( 16, Line (getPoint 17) (getPoint 18) )
        , ( 17, Line (getPoint 17) (getPoint 23) )
        , ( 18, Line (getPoint 8) (getPoint 15) )
        , ( 19, Line (getPoint 15) (getPoint 16) )
        , ( 20, Line (getPoint 16) (getPoint 22) )
        , ( 21, Line (getPoint 25) (getPoint 26) )
        , ( 22, Line (getPoint 27) (getPoint 28) )
        , ( 23, Line (getPoint 31) (getPoint 33) )
        , ( 24, Line (getPoint 34) (getPoint 36) )
        , ( 25, Line (getPoint 39) (getPoint 44) )
        , ( 26, Line (getPoint 33) (getPoint 41) )
        , ( 27, Line (getPoint 34) (getPoint 42) )
        , ( 28, Line (getPoint 31) (getPoint 37) )
        , ( 29, Line (getPoint 37) (getPoint 38) )
        , ( 30, Line (getPoint 38) (getPoint 48) )
        , ( 31, Line (getPoint 47) (getPoint 49) )
        , ( 32, Line (getPoint 57) (getPoint 60) )
        , ( 33, Line (getPoint 40) (getPoint 50) )
        , ( 34, Line (getPoint 50) (getPoint 51) )
        , ( 35, Line (getPoint 51) (getPoint 58) )
        , ( 36, Line (getPoint 57) (getPoint 47) )
        , ( 37, Line (getPoint 43) (getPoint 53) )
        , ( 38, Line (getPoint 52) (getPoint 53) )
        , ( 39, Line (getPoint 52) (getPoint 59) )
        , ( 40, Line (getPoint 36) (getPoint 46) )
        , ( 41, Line (getPoint 45) (getPoint 46) )
        , ( 42, Line (getPoint 45) (getPoint 55) )
        , ( 43, Line (getPoint 54) (getPoint 56) )
        , ( 44, Line (getPoint 56) (getPoint 60) )
        , ( 45, Line (getPoint 13) (getPoint 14) )
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
    , nextDir : Direction
    , score : Float
    }


type Direction
    = Up
    | Down
    | Left
    | Right
    | None



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
    , nextDir = Right
    , rotation = 0
    , score = 0
    }


type Msg
    = MoveDirection Direction
    | Nothing
    | NoMoving
    | ChangeDirection Direction


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

                    else if checkDir pac d || checkDir pac pac.nextDir then
                        if checkDir pac pac.nextDir && pac.nextDir /= d then
                            ( { pac | state = Running pac.nextDir, nextDir = None }, Cmd.none )

                        else
                            ( { pac | position = changeXPosition (pac.position.x - movement) pac, state = Running d, rotation = 180 }, Cmd.none )

                    else
                        update NoMoving pac

                Right ->
                    if outOfBounds pac then
                        ( { pac | position = changeXPosition 0 pac, state = Running d, rotation = 0 }, Cmd.none )

                    else if checkDir pac d || checkDir pac pac.nextDir then
                        if checkDir pac pac.nextDir && pac.nextDir /= d then
                            ( { pac | state = Running pac.nextDir, nextDir = None }, Cmd.none )

                        else
                            ( { pac | position = changeXPosition (pac.position.x + movement) pac, state = Running d, rotation = 0 }, Cmd.none )

                    else
                        update NoMoving pac

                Up ->
                    if outOfBounds pac then
                        ( { pac | position = changeYPosition fieldHeight pac, state = Running d, rotation = -90 }, Cmd.none )

                    else if checkDir pac d || checkDir pac pac.nextDir then
                        if checkDir pac pac.nextDir && pac.nextDir /= d then
                            ( { pac | state = Running pac.nextDir, nextDir = None }, Cmd.none )

                        else
                            ( { pac | position = changeYPosition (pac.position.y - movement) pac, state = Running d, rotation = -90 }, Cmd.none )

                    else
                        update NoMoving pac

                Down ->
                    if outOfBounds pac then
                        ( { pac | position = changeYPosition 0 pac, state = Running d, rotation = 90 }, Cmd.none )

                    else if checkDir pac d || checkDir pac pac.nextDir then
                        if checkDir pac pac.nextDir && pac.nextDir /= d then
                            ( { pac | state = Running pac.nextDir, nextDir = None }, Cmd.none )

                        else
                            ( { pac | position = changeYPosition (pac.position.y + movement) pac, state = Running d, rotation = 90 }, Cmd.none )

                    else
                        update NoMoving pac

                _ ->
                    update NoMoving pac

        Nothing ->
            case pac.state of
                Running d ->
                    ( { pac | state = Stopped d }, Cmd.none )

                Stopped d ->
                    ( { pac | state = Running d }, Cmd.none )

        NoMoving ->
            ( pac, Cmd.none )

        ChangeDirection d ->
            ( { pac | nextDir = d }, Cmd.none )



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
                , div textCss [ Html.text (String.fromFloat pac.score) ]
                , div textCss [ Html.text "500x500" ]
                ]
            , div
                gameCss
                [ svg
                    (gameChildCss
                        ++ [ id "gameField" ]
                    )
                    (getMeshForPoints runMesh
                        ++ [ path [ fill gameColor, d "M200.3,74.7h-65c-2.8,0-5-2.3-5-5V50.3c0-2.8,2.2-5,5-5h65c2.8,0,5,2.3,5,5v19.3  C205.3,72.4,203.1,74.7,200.3,74.7z" ] []
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
                    )
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
subscriptions pac =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        , case pac.state of
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
            ChangeDirection Up

        "ArrowDown" ->
            ChangeDirection Down

        "ArrowLeft" ->
            ChangeDirection Left

        "ArrowRight" ->
            ChangeDirection Right

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


checkDir : PacMan -> Direction -> Bool
checkDir pac d =
    case d of
        Left ->
            getMesh runMesh { x = pac.position.x - movement, y = pac.position.y }

        Right ->
            getMesh runMesh { x = pac.position.x + movement, y = pac.position.y }

        Up ->
            getMesh runMesh { x = pac.position.x, y = pac.position.y - movement }

        Down ->
            getMesh runMesh { x = pac.position.x, y = pac.position.y + movement }

        _ ->
            False


getMesh : Dict Int Line -> Point -> Bool
getMesh mesh pos =
    List.foldl ((\x -> checkPath x) pos) False (Dict.values mesh)


checkPath : Point -> Line -> Bool -> Bool
checkPath pos line e =
    (pos.x >= min line.start.x line.end.x && pos.x <= max line.start.x line.end.x && pos.y >= min line.start.y line.end.y && pos.y <= max line.start.y line.end.y) || e


getMeshForPoints : Dict Int Line -> List (Svg Msg)
getMeshForPoints mesh =
    pointsToSvg (List.foldl createPoints [] (Dict.values mesh))


createPoints : Line -> List Point -> List Point
createPoints line pointList =
    let
        startPoint : Point
        startPoint =
            { x = min line.start.x line.end.x, y = min line.start.y line.end.y }

        endPoint : Point
        endPoint =
            { x = max line.start.x line.end.x, y = max line.start.y line.end.y }

        currentPoint : Point
        currentPoint =
            moveToWards startPoint endPoint pointStep
    in
    if startPoint /= endPoint then
        startPoint :: createPoints (Line currentPoint endPoint) pointList

    else
        currentPoint :: pointList


moveToWards : Point -> Point -> Float -> Point
moveToWards from to lenght =
    { x = from.x + min lenght (to.x - from.x), y = from.y + min lenght (to.y - from.y) }


pointsToSvg : List Point -> List (Svg Msg)
pointsToSvg points =
    indexedMap createRectSvg points


createRectSvg : Int -> Point -> Svg Msg
createRectSvg _ point =
    rect
        [ x (String.fromFloat (point.x - pointSize / 2))
        , y (String.fromFloat (point.y - pointSize / 2))
        , Svg.Attributes.width (String.fromFloat pointSize)
        , Svg.Attributes.height (String.fromFloat pointSize)
        , fill "red"
        ]
        []


indexedMap : (Int -> a -> b) -> List a -> List b
indexedMap func list =
    (\( v, _ ) -> v) (List.foldl (\x ( ys, l ) -> ( func l x :: ys, l + 1 )) ( [], 0 ) list)
