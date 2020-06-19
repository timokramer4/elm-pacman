module PacMan exposing (main)

import Array exposing (length)
import Browser
import Browser.Events exposing (onKeyDown)
import Dict exposing (Dict, get, member)
import Html exposing (Html, div, img, node, text)
import Html.Attributes exposing (class, id, src, style)
import Json.Decode exposing (..)
import List exposing (..)
import List.Unique exposing (filterDuplicates)
import String exposing (length)
import Svg exposing (Svg, circle, path, polygon, rect, svg)
import Svg.Attributes exposing (cx, cy, d, fill, points, r, x, y)
import Time exposing (every)



-----------------------
-- GLOBAL VARIABLES --
-----------------------


fieldSettings : { width : Float, height : Float, borderColor : String }
fieldSettings =
    { width = 500
    , height = 500
    , borderColor = "#3498DB"
    }


pacSettings : { ratio : Float }
pacSettings =
    { ratio = 25
    }


movement : Float
movement =
    1


scoreSettings : { item : Float, pill : Float, fruit : Float }
scoreSettings =
    { item = 10
    , pill = 50
    , fruit = 50
    }


itemSettings : { fill : String, step : Float, size : Float }
itemSettings =
    { fill = "#FFAAA5"
    , step = 15
    , size = 5
    }


pillSettings : { fill : String, radius : Float }
pillSettings =
    { fill = "#FFAAA5"
    , radius = 6
    }


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
        , ( 25, { x = -5, y = 235 } )
        , ( 26, { x = 175, y = 235 } )
        , ( 27, { x = 325, y = 235 } )
        , ( 28, { x = 505, y = 235 } )
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


pillsList : List Point
pillsList =
    [ { x = 25, y = 115 }
    , { x = 55, y = 400 }
    , { x = 385, y = 55 }
    , { x = 325, y = 430 }
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


type alias Game =
    { pPosition : Point
    , pRotation : Int
    , state : State
    , nextDir : Direction
    , score : Float
    , eatablePoints : List Point
    , pills : List Point
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
    , Html.Attributes.style "width" (String.fromFloat fieldSettings.width ++ "px")
    , Html.Attributes.style "height" (String.fromFloat fieldSettings.height ++ "px")
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
    , Html.Attributes.style "width" (String.fromFloat fieldSettings.width ++ "px")
    , Html.Attributes.style "height" (String.fromFloat fieldSettings.height ++ "px")
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


initialModel : Game
initialModel =
    { pPosition = { x = 250, y = 280 }
    , state = Running Right
    , nextDir = Right
    , pRotation = 0
    , score = 0
    , eatablePoints = substractList pillsList (filterDuplicates (List.foldl createPoints [] (Dict.values runMesh)))
    , pills = pillsList
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


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        MoveDirection d ->
            case d of
                Left ->
                    if outOfBounds game then
                        ( { game | pPosition = changeXPosition fieldSettings.width game, state = Running d, pRotation = 180 }, Cmd.none )

                    else if checkDir game d || checkDir game game.nextDir then
                        if checkDir game game.nextDir && game.nextDir /= d then
                            ( { game | state = Running game.nextDir, nextDir = None }, Cmd.none )

                        else
                            ( checkEatable { game | pPosition = changeXPosition (game.pPosition.x - movement) game, state = Running d, pRotation = 180 }, Cmd.none )

                    else
                        update NoMoving game

                Right ->
                    if outOfBounds game then
                        ( { game | pPosition = changeXPosition 0 game, state = Running d, pRotation = 0 }, Cmd.none )

                    else if checkDir game d || checkDir game game.nextDir then
                        if checkDir game game.nextDir && game.nextDir /= d then
                            ( { game | state = Running game.nextDir, nextDir = None }, Cmd.none )

                        else
                            ( checkEatable { game | pPosition = changeXPosition (game.pPosition.x + movement) game, state = Running d, pRotation = 0 }, Cmd.none )

                    else
                        update NoMoving game

                Up ->
                    if outOfBounds game then
                        ( { game | pPosition = changeYPosition fieldSettings.height game, state = Running d, pRotation = -90 }, Cmd.none )

                    else if checkDir game d || checkDir game game.nextDir then
                        if checkDir game game.nextDir && game.nextDir /= d then
                            ( { game | state = Running game.nextDir, nextDir = None }, Cmd.none )

                        else
                            ( checkEatable { game | pPosition = changeYPosition (game.pPosition.y - movement) game, state = Running d, pRotation = -90 }, Cmd.none )

                    else
                        update NoMoving game

                Down ->
                    if outOfBounds game then
                        ( { game | pPosition = changeYPosition 0 game, state = Running d, pRotation = 90 }, Cmd.none )

                    else if checkDir game d || checkDir game game.nextDir then
                        if checkDir game game.nextDir && game.nextDir /= d then
                            ( { game | state = Running game.nextDir, nextDir = None }, Cmd.none )

                        else
                            ( checkEatable { game | pPosition = changeYPosition (game.pPosition.y + movement) game, state = Running d, pRotation = 90 }, Cmd.none )

                    else
                        update NoMoving game

                _ ->
                    update NoMoving game

        Nothing ->
            case game.state of
                Running d ->
                    ( { game | state = Stopped d }, Cmd.none )

                Stopped d ->
                    ( { game | state = Running d }, Cmd.none )

        NoMoving ->
            ( game, Cmd.none )

        ChangeDirection d ->
            ( { game | nextDir = d }, Cmd.none )



----------
-- VIEW --
----------


view : Game -> Html Msg
view game =
    node "main"
        []
        [ node "style" [] [ text styleContents ]
        , div (class "wrapper" :: wrapperCss)
            [ div (class "headline" :: headlineCss)
                [ div (textCss ++ [ Html.Attributes.style "text-transform" "uppercase" ]) [ Html.text "High score" ]
                , div textCss [ Html.text (String.fromFloat game.score) ]
                , div textCss [ Html.text "500x500" ]
                ]
            , div
                gameCss
                [ svg
                    (gameChildCss
                        ++ [ id "gameField" ]
                    )
                    (pointsToSvg game.eatablePoints 1
                        ++ pointsToSvg game.pills 2
                        ++ [ path [ fill fieldSettings.borderColor, d "M94,70.7H43.7c-2.8,0-5-2.3-5-5V42.3c0-2.8,2.3-5,5-5H94c2.8,0,5,2.3,5,5v23.3C99,68.4,96.8,70.7,94,70.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M200.3,70.7h-66c-2.8,0-5-2.3-5-5V42.3c0-2.8,2.2-5,5-5h66c2.8,0,5,2.3,5,5v23.3 C205.3,68.4,203.1,70.7,200.3,70.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M366.1,71.6h-67.5c-3,0-5.3-2.3-5.3-5V43.2c0-2.8,2.5-5,5.3-5h67.5c3,0,5.3,2.3,5.3,5v23.3 C371.5,69.3,369.1,71.6,366.1,71.6z" ] []
                           , path [ fill fieldSettings.borderColor, d "M94.6,131.6H42.4c-2.8,0-5-2.6-5-5.6v-21.3c0-3.2,2.3-5.6,5-5.6h52.3c2.8,0,5,2.6,5,5.6v21.3 C99.6,129.1,97.5,131.6,94.6,131.6z" ] []
                           , path [ fill fieldSettings.borderColor, d "M457,70.1h-52.5c-3,0-5.3-2.3-5.3-5V42.7c0-2.8,2.5-5,5.3-5H457c3,0,5.3,2.3,5.3,5V65 C462.3,67.8,460,70.1,457,70.1z" ] []
                           , path [ fill fieldSettings.borderColor, d "M199.7,158.3h-35.4c-2.4,0-4.3-2.2-4.3-5v-49.8c0-2.8-2-5-4.3-5h-22.1c-2.4,0-4.3,2.3-4.3,5v112.8 c0,2.8,2,5,4.3,5h22.1c2.4,0,4.3-2.3,4.3-5v-34.7c0-2.8,1.9-5,4.3-5h35.4c2.4,0,4.3-2.3,4.3-5v-8.3 C204,160.6,202.1,158.3,199.7,158.3z" ] []
                           , path [ fill fieldSettings.borderColor, d "M305.8,98.6H193.2c-2.8,0-5,2.3-5,5v22.7c0,2.8,2.3,5,5,5h36.3c2.8,0,5,2.2,5,5v35.3c0,2.8,2.3,5,5,5h18.3 c2.8,0,5-2.3,5-5v-35.3c0-2.8,2.2-5,5-5h38c2.8,0,5-2.3,5-5v-22.7C310.8,100.8,308.6,98.6,305.8,98.6z" ] []
                           , path [ fill fieldSettings.borderColor, d "M298.2,175.7h36.4c2.4,0,4.2,2.2,4.2,5v33.7c0,2.8,1.9,5,4.2,5h24.8c2.4,0,4.2-2.3,4.2-5l-0.9-111.9 c0-2.8-1.9-5-4.2-5h-23.8c-2.4,0-4.2,2.3-4.2,5l-0.1,49.9c0,2.8-1.8,5-4.2,5h-36.4c-2.4,0-4.2,2.3-4.2,5v8.3 C294,173.4,295.9,175.7,298.2,175.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M457,131.7h-50.5c-3,0-5.3-2.3-5.3-5v-22.4c0-2.8,2.5-5,5.3-5H457c3,0,5.3,2.3,5.3,5v22.3 C462.3,129.4,460,131.7,457,131.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M155,311.7h-21c-2.8,0-5-2.3-5-5V255c0-2.8,2.3-5,5-5h21c2.8,0,5,2.3,5,5v51.7 C160,309.4,157.8,311.7,155,311.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M307.3,294H193.7c-2.8,0-5,2.3-5,5v7.7c0,2.8,2.3,5,5,5H228c2.8,0,5,2.2,5,5V351c0,2.8,2.3,5,5,5h22.3 c2.8,0,5-2.3,5-5v-34.3c0-2.8,2.2-5,5-5h37c2.8,0,5-2.3,5-5V299C312.3,296.2,310.1,294,307.3,294z" ] []
                           , path [ fill fieldSettings.borderColor, d "M366,312.7h-22c-2.8,0-5-2.3-5-5V255c0-2.8,2.3-5,5-5h22c2.8,0,5,2.3,6,5v52.7   C371,310.4,368.8,312.7,366,312.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M94,340.3H81.3h-1H43c-2.8,0-5,2.2-5,5v7.3c0,2.8,2.3,5,5,5h21.3c2.8,0,5,2.2,5,5V411c0,2.8,2.3,5,5,5H94 c2.8,0,5-2.3,5-5v-65.7C99,342.6,96.8,340.3,94,340.3z" ] []
                           , path [ fill fieldSettings.borderColor, d "M202.3,356.7h-69c-2.8,0-5-2.3-5-5v-8.3c0-2.8,2.2-5,5-5h69c2.8,0,5,2.3,5,5v8.3   C207.3,354.4,205.1,356.7,202.3,356.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M368,355.7h-69c-2.8,0-5-2.3-5-5v-7.3c0-2.8,2.2-5,5-5h69c2.8,0,5,2.3,5,5v7.3   C373,353.4,370.8,355.7,368,355.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M404.3,338.3h4.7h5h42.3c2.8,0,5,2.2,5,5v7.3c0,2.8-2.3,5-5,5H435c-2.8,0-5,2.2-5,5V412c0,2.8-2.3,5-5,5h-20.7 c-2.8,0-5-2.3-5-5v-68.7C399.3,340.6,401.6,338.3,404.3,338.3z" ] []
                           , path [ fill fieldSettings.borderColor, d "M202,442.7h-37c-2.8,0-5-2.2-5-5v-50.3c0-2.8-2.3-5-5-5h-20.9c-2.8,0-5,2.3-5,5v50.3c0,2.8-2.2,5-5,5H44 c-2.8,0-5,2.2-5,5v9.3c0,2.8,2.3,5,5,5H202c2.8,0,5-2.3,5-5v-9.3C207,444.9,204.8,442.7,202,442.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M298.9,443.7h35.5c2.7,0,4.9-2.2,4.9-5v-52.3c0-2.8,2.3-5,4.9-5H366c2.7,0,4.9,2.3,4.9,5v52.3 c0,2.8,2.2,5,4.9,5h81.5c2.7,0,4.9,2.2,4.9,5v8.3c0,2.8-2.3,5-4.9,5H298.9c-2.7,0-4.9-2.3-4.9-5v-8.3 C294,445.9,296.3,443.7,298.9,443.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M304.3,383.3H195.7c-2.8,0-5,2.3-5,5V411c0,2.8,2.3,5,5,5H229c2.8,0,5,3.2,5,6v35.3c0,2.8,2.3,5,5,5h21.3 c2.8,0,5-2.3,5-5V422c0-2.8,2.2-6,5-6h34c2.8,0,5-2.3,5-5v-22.7C309.3,385.6,307.1,383.3,304.3,383.3z" ] []

                           -- Outer lines and inner cave
                           , path [ fill fieldSettings.borderColor, d "M403.7,211.2v-43.8c0-2.9,2.2-5.1,5-5.1h84.9c1.5,0,2.7-1.2,2.7-2.7V7.9c0-1.5-1.2-2.7-2.7-2.7H6.8 c-1.5,0-2.7,1.2-2.7,2.7v152.2c0,1.5,1.2,2.7,2.7,2.7h82.8c2.8,0,5,2.2,5,5.1v41.8c0,2.9-2.2,6.1-5,6.1H0v5.5h97.3 c1.5,0,2.7-1.2,2.7-2.7v-58.3c0-1.5-1.2-2.7-2.7-2.7H16.5c-2.8,0-5-2.2-5-5.1V15.9c0-2.9,2.2-5.1,5-5.1h211c2.8,0,5,2.2,5,5.1v51 c0,2.9,2.2,5.1,5,5.1H261c2.8,0,5-2.2,5-5.1v-51c0-2.9,2.2-5.1,5-5.1h212.9c2.8,0,5,2.2,5,5.1v136c0,2.9-2.2,5.1-5,5.1H401 c-1.5,0-2.7,1.2-2.7,2.7v59.1c0,0.1,0,0.1,0,0.2c0,0.1,0,0.1,0,0.2c0,1.5,1.2,2.7,2.7,2.7h99v-5.5h-91.3 C405.9,216.3,403.7,214.1,403.7,211.2z" ] []
                           , polygon [ fill fieldSettings.borderColor, points "306.9,203.5 263.5,203.5 263.5,208.3 306.9,208.3 306.9,261.9 192.4,261.9 192.4,208.3 234.5,208.3 234.5,203.5 192.4,203.5 188.4,203.5 188.4,208.3 188.4,261.9 188.4,266.7 192.4,266.7 306.9,266.7 311,266.7 311,261.9 311,208.3 311,203.5" ] []
                           , path [ fill fieldSettings.borderColor, d "M406.4,254.4H500V249h-99.5c-1.2,0-2.2,1.2-2.2,2.7l0,0l0,0v56.3c0,1.5,1.2,2.7,2.7,2.7h85.4 c1.5,0,2.7,1.2,2.7,2.7v65.9c0,2.9-2.2,5.1-5,5.1h-19.9c-2.8,0-5,2.2-5,5.1v20.8c0,2.9,2.2,5.1,5,5.1h19.9c2.8,0,5,2.2,5,5.1v66.4 c0,1.5-1.2,2.7-2.7,2.7H14.1c-1.5,0-2.7-1.2-2.7-2.7v-66.5c0-2.9,2.2-4.1,5-4.1h20.1c2.8,0,5-2.2,5-5.1v-21.8c0-2.9-2.2-5.1-5-5.1 H16.4c-2.8,0-5-2.2-5-5.1V314c0-1.5,1.2-2.7,2.7-2.7h83.2c0.1,0,0.2,0,0.3,0c0.1,0,0.2,0,0.3,0c1.5,0,2.7-1.2,2.7-2.7v-57 c0-1.5-1.2-2.7-2.7-2.7H0v5.5h92.6c1.5,0,2.7,1.2,2.7,2.7v46c0,1.5-1.2,2.7-2.7,2.7H6.7c-1.5,0-2.7,1.2-2.7,2.7v183.9 c0,1.5,1.2,2.7,2.7,2.7h487.1c1.5,0,2.7-1.2,2.7-2.7V308c0-1.5-1.2-2.7-2.7-2.7h-87.4c-1.5,0-2.7-1.2-2.7-2.7V257 C403.7,255.6,404.9,254.4,406.4,254.4z" ] []
                           ]
                    )
                , div
                    (gameChildCss
                        ++ [ id "pacmanArea" ]
                    )
                    [ img (pacmanSvgCss ++ [ src "https://upload.wikimedia.org/wikipedia/commons/thumb/4/49/Pacman.svg/972px-Pacman.svg.png", Html.Attributes.style "top" (String.fromFloat (game.pPosition.y - pacSettings.ratio / 2) ++ "px"), Html.Attributes.style "left" (String.fromFloat (game.pPosition.x - pacSettings.ratio / 2) ++ "px"), Html.Attributes.style "transform" ("rotate(" ++ String.fromInt game.pRotation ++ "deg)") ])
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


subscriptions : Game -> Sub Msg
subscriptions game =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        , case game.state of
            Running d ->
                Time.every 20 (\_ -> MoveDirection d)

            _ ->
                Sub.none
        ]



-------------------
-- MAIN PROGRAMM --
-------------------


main : Program () Game Msg
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


changeXPosition : Float -> Game -> Point
changeXPosition value game =
    let
        oldPosition =
            game.pPosition
    in
    { oldPosition | x = value }


changeYPosition : Float -> Game -> Point
changeYPosition value game =
    let
        oldPosition =
            game.pPosition
    in
    { oldPosition | y = value }


outOfBounds : Game -> Bool
outOfBounds game =
    game.pPosition.x < 0 || game.pPosition.x > fieldSettings.width || game.pPosition.y < 0 || game.pPosition.y > fieldSettings.height


checkDir : Game -> Direction -> Bool
checkDir game d =
    case d of
        Left ->
            getMesh runMesh { x = game.pPosition.x - movement, y = game.pPosition.y }

        Right ->
            getMesh runMesh { x = game.pPosition.x + movement, y = game.pPosition.y }

        Up ->
            getMesh runMesh { x = game.pPosition.x, y = game.pPosition.y - movement }

        Down ->
            getMesh runMesh { x = game.pPosition.x, y = game.pPosition.y + movement }

        _ ->
            False


getMesh : Dict Int Line -> Point -> Bool
getMesh mesh pos =
    List.foldl ((\x -> checkPath x) pos) False (Dict.values mesh)


checkPath : Point -> Line -> Bool -> Bool
checkPath pos line e =
    (pos.x >= min line.start.x line.end.x && pos.x <= max line.start.x line.end.x && pos.y >= min line.start.y line.end.y && pos.y <= max line.start.y line.end.y) || e


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
            moveToWards startPoint endPoint itemSettings.step
    in
    if startPoint /= endPoint then
        startPoint :: createPoints (Line currentPoint endPoint) pointList

    else
        currentPoint :: pointList


moveToWards : Point -> Point -> Float -> Point
moveToWards from to lenght =
    { x = from.x + min lenght (to.x - from.x), y = from.y + min lenght (to.y - from.y) }


checkEatable : Game -> Game
checkEatable game =
    let
        checkCurrentPoint : List Point -> List Point
        checkCurrentPoint lp =
            case lp of
                [] ->
                    []

                x :: xs ->
                    if x == game.pPosition then
                        checkCurrentPoint xs

                    else
                        x :: checkCurrentPoint xs

        localListItems : List Point
        localListItems =
            checkCurrentPoint game.eatablePoints

        localListPills : List Point
        localListPills =
            checkCurrentPoint game.pills
    in
    if List.length game.pills == List.length localListPills && List.length game.eatablePoints == List.length localListItems then
        game

    else if List.length game.pills /= List.length localListPills then
        { game | pills = localListPills, score = game.score + scoreSettings.pill }

    else
        { game | eatablePoints = localListItems, score = game.score + scoreSettings.item }


pointsToSvg : List Point -> Int -> List (Svg Msg)
pointsToSvg points mode =
    -- mode : 1 -> eatable, 2-> pills
    case mode of
        1 ->
            indexedMap createItemSvg points

        2 ->
            indexedMap createPillSvg points

        _ ->
            []


createItemSvg : Int -> Point -> Svg Msg
createItemSvg _ point =
    rect
        [ x (String.fromFloat (point.x - itemSettings.size / 2))
        , y (String.fromFloat (point.y - itemSettings.size / 2))
        , Svg.Attributes.width (String.fromFloat itemSettings.size)
        , Svg.Attributes.height (String.fromFloat itemSettings.size)
        , fill itemSettings.fill
        ]
        []


createPillSvg : Int -> Point -> Svg Msg
createPillSvg _ point =
    circle
        [ cx (String.fromFloat point.x)
        , cy (String.fromFloat point.y)
        , r (String.fromFloat pillSettings.radius)
        , fill pillSettings.fill
        ]
        []


indexedMap : (Int -> a -> b) -> List a -> List b
indexedMap func list =
    (\( v, _ ) -> v) (List.foldl (\x ( ys, l ) -> ( func l x :: ys, l + 1 )) ( [], 0 ) list)



substractList : List Point -> List Point -> List Point
substractList a b =
    List.filter (\x -> not (List.member x a)) b
